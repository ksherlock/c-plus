// Cplus project, translation phases 1 and 2: character-level transformations.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_CHAR_DECODE_H
#define CPLUS_CHAR_DECODE_H

#include "formats.h"

#include <sstream>
#include <cstring>

namespace cplus {

template< typename output, typename ... config >
class decode_stage : public stage< output, config ... > {
protected:
	using decode_stage::stage::stage;
	
	lex_decode_state lex_state() {
		lex_decode_state ret = {};
		this->template pass< pass_policy::optional >( ret );
		return ret;
	}
};

template< typename output >
class char_decoder : public decode_stage< output, char_decoder_config > {
	using char_decoder::stage::pass;
	
	enum state { normal, cr, tri1, trigraph, backslash, ucn } state = normal;
	
	std::vector< raw_char > input_buffer;
	
	void reset() noexcept {
		state = normal;
		input_buffer.clear();
	}
	void shift( raw_char in, enum state next ) noexcept {
		state = next;
		input_buffer.push_back( std::move( in ) ); // Would be better to reserve, but max size is 10.
	}
	
public:
	using char_decoder::decode_stage::decode_stage;
	
	void flush() {
		CPLUS_FINALLY ( reset(); )
		pass( std::make_move_iterator( input_buffer.begin() ), std::make_move_iterator( input_buffer.end() ) );
		CPLUS_DO_FINALLY
	}
	
	void operator () ( raw_char const & in ) {
		for (;; flush() ) switch ( state ) { // continue; means unshift and retry from normal state.
		case normal:
			switch ( in ) {
			case '?':
				if ( this->get_config().disable_trigraphs || this->lex_state() == lex_decode_state::raw ) goto normal;
				return shift( in, tri1 );
				
			case '\\':
				if ( this->lex_state() == lex_decode_state::raw ) goto normal;
				return shift( in, backslash );
				
			case '\r':
				state = cr;
				pass( raw_char( '\n', in ) );
				return;
				
			default: normal:
				return pass( in );
			}
		
		case cr: // Discard a \n following \r.
			state = normal;
			if ( in == '\n' ) return;
			else continue;
			
		case tri1:
			if ( in == '?' ) return shift( in, trigraph );
			else continue;
			
		case trigraph:
			static char const	tri_source[] =	"='()!<>-/",
								tri_dest[] =	"#^[]|{}~\\";
			if ( char const *sp = std::strchr( tri_source, in ) ) {
				{
					CPLUS_FINALLY ( reset(); )
					this->template pass< pass_policy::optional >( delimiter< struct trigraph, delimiter_sense::open >( std::move( input_buffer.front() ) ) );
					this->template pass< pass_policy::optional >( delimiter< struct trigraph, delimiter_sense::close >( in ) );
					CPLUS_DO_FINALLY
				}
				char dest = tri_dest[ sp - tri_source ];
				if ( dest != '\\' ) pass( raw_char{ static_cast< std::uint8_t >( dest ), in } ); // Locate at discriminating symbol.
				else shift( { static_cast< std::uint8_t >( dest ), in }, backslash );
				
			} else if ( in == '?' ) { // May be a trigraph preceded by a "?".
				CPLUS_FINALLY (
					input_buffer.erase( input_buffer.begin() );
					shift( in, trigraph ); // Shift new "?" but remain in same state.
				)
				pass( std::move( input_buffer.front() ) ); // Flush only non-trigraph "?".
				CPLUS_DO_FINALLY
			
			} else continue; // Not a trigraph.
			return;
		
		case backslash:
			switch ( in ) {
			case 'u':
			case 'U':
				if ( this->lex_state() == lex_decode_state::escape ) continue;
				return shift( in, ucn );
				
			case '\r': case '\n': { // phase 2
				CPLUS_FINALLY (
					reset();
					if ( in == '\r' ) state = cr;
				)
				this->template pass< pass_policy::optional >( line_splice( std::move( input_buffer.front() ) ) );
				CPLUS_DO_FINALLY
				return;
			}
			default: continue;
			}
			
		case ucn:
			if ( ! std::isxdigit( in ) ) continue;
			
			shift( in, state );
			if ( input_buffer.size() == ( input_buffer[1] == 'u'? 6 : 10 ) ) {
				CPLUS_FINALLY ( reset(); )
				
				char32_t ucn_acc = 0;
				for ( auto cit = input_buffer.begin() + 2; cit != input_buffer.end(); ++ cit ) {
					ucn_acc <<= 4;
					if ( * cit >= '0' && * cit <= '9' ) ucn_acc |= * cit - '0';
					else if ( * cit >= 'a' && * cit <= 'f' ) ucn_acc |= * cit - 'a' + 10;
					else if ( * cit >= 'A' && * cit <= 'F' ) ucn_acc |= * cit - 'A' + 10;
				}
				this->template pass< pass_policy::optional >( delimiter< struct ucn, delimiter_sense::open >( input_buffer.front() ) );
				this->template pass< pass_policy::optional >( delimiter< struct ucn, delimiter_sense::close >( std::move( input_buffer.back() ) ) );
				pass( cplus::ucn( ucn_acc, std::move( input_buffer.front() ) ) );
				CPLUS_DO_FINALLY
			}
			return;
		}
	}
};

template< typename output >
class utf8_transcoder : public decode_stage< output > {
	raw_codepoint result;
	char32_t min;
	int len = 0;
	
	void non_ascii( raw_char const &c ) { // Decode outside operator () to keep ASCII fast.
		this->pass( utf8_char( c, c ) ); // Pass through.
		
		if ( len == 0 ) {
			this->template diagnose< diagnose_policy::fatal, utf8_error >( c < 0xC0 || c >= 0xFE, c );
			len = 1;
			while ( c & ( 0x40 >> len ) ) ++ len;
			result = { static_cast< char32_t >( ( c + ( 0x80 >> len ) ) & 0xFF ), c };
			min = 1 << ( len > 1? len * 5 + 1 : 7 ); // The lowest result which merits this many bytes.
		} else {
			if ( c < 0x80 || c >= 0xC0 ) {
				flush(); // Abort and reset.
				return (*this) ( c ); // Retry.
			}
			result = result << 6 | ( c & 0x3F );
			if ( -- len != 0 ) return;
			this->template diagnose< diagnose_policy::pass, utf8_error >( result < min, result ); // Identifier identity issues may result from using invalid UTF-8.
			this->pass( std::move( result ) );
		}
	}
public:
	using utf8_transcoder::decode_stage::decode_stage;
	
	template< typename cont >
	void operator () ( raw_codepoint const & in, cont && propagate ) { // Encode e.g. UCNs.
		if ( in < 0x80 ) {
			this->pass( utf8_char( static_cast< std::uint8_t >( in ), in ) );
	
		} else { // convert ucn_acc to UTF-8
			int unicode_remaining = 6; // number of UTF-8 trailer chars
			for ( int shift = 31; in >> shift == 0; shift -= 5 ) -- unicode_remaining;
	
			this->pass( utf8_char( static_cast< std::uint8_t >( 0x7F80 >> unicode_remaining | in >> unicode_remaining * 6 ), in ) );
			while ( unicode_remaining -- ) {
				this->pass( utf8_char( static_cast< std::uint8_t >( 0x80 | ( ( in >> unicode_remaining * 6 ) & 0x3F ) ), in ) );
			}
		}
		propagate();
	}
	
	void operator () ( raw_char const & c ) {
		if ( len != 0 || ( c >= 0x80 && this->lex_state() != lex_decode_state::raw ) ) non_ascii( c );
		else this->pass( c );
	}
	void flush() {
		CPLUS_FINALLY (
			result = {};
			len = 0;
		)
		this->template diagnose< diagnose_policy::pass, utf8_error >( len != 0, std::move( result ) );
	}
};

}

#endif
