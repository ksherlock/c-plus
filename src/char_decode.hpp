// Cplus project, translation phases 1 and 2: character-level transformations.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_CHAR_DECODE_H
#define CPLUS_CHAR_DECODE_H

#include "formats.h"

#include <sstream>
#include <cstring>

namespace cplus {

template< typename output >
class char_decoder : public stage< output, char_decoder_config > {
	using char_decoder::stage::pass;
	
	enum state { normal, cr, tri1, trigraph, backslash, tri_backslash, ucn, tri_ucn } state = normal;
	
	std::vector< raw_char > input_buffer;
	
	void reset() noexcept {
		state = normal;
		input_buffer.clear();
	}
	void shift( raw_char in, enum state next ) noexcept {
		state = next;
		input_buffer.push_back( std::move( in ) ); // Would be better to reserve, but max size is 10.
	}
	
	lex_decode_state lex_state() {
		lex_decode_state ret = {};
		this->template pass< pass_policy::optional >( ret );
		return ret;
	}
	
public:
	using char_decoder::stage::stage;
	
	void flush() {
		CPLUS_FINALLY ( reset(); )
		if ( state == tri_backslash || state == tri_ucn ) { // A ??/ trigraph shifted a backslash.
			pass( mapped_char< struct trigraph >{ '\\', input_buffer.front() } );
			pass( input_buffer.begin() + 1, input_buffer.end() );
			
		} else pass( input_buffer.begin(), input_buffer.end() );
		CPLUS_DO_FINALLY
	}
	
	void operator () ( raw_char const & in ) {
		for (;; flush() ) switch ( state ) { // continue; means unshift and retry from normal state.
		case normal:
			switch ( in ) {
			case '?':
				if ( this->get_config().disable_trigraphs || lex_state() == lex_decode_state::raw ) goto normal;
				return shift( in, tri1 );
				
			case '\\':
				if ( lex_state() == lex_decode_state::raw ) goto normal;
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
				if ( dest != '\\' ) pass( mapped_char< struct trigraph >{ static_cast< std::uint8_t >( dest ), in } ); // Locate at discriminating symbol.
				else shift( { static_cast< std::uint8_t >( dest ), in }, tri_backslash );
				
			} else if ( in == '?' ) { // May be a trigraph preceded by a "?".
				CPLUS_FINALLY (
					input_buffer.erase( input_buffer.begin() );
					shift( in, trigraph ); // Shift new "?" but remain in same state.
				)
				pass( std::move( input_buffer.front() ) ); // Flush only non-trigraph "?".
				CPLUS_DO_FINALLY
			
			} else continue; // Not a trigraph.
			return;
		
		case backslash: case tri_backslash:
			switch ( in ) {
			case 'u':
			case 'U':
				if ( lex_state() == lex_decode_state::escape ) continue;
				return shift( in, state == tri_backslash? tri_ucn : ucn );
				
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
			
		case ucn: case tri_ucn:
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
				
				if ( ucn_acc < 0x80 ) {
					pass( mapped_char< struct ucn >( static_cast< std::uint8_t >( ucn_acc ), input_buffer.front() ) );
			
				} else { // convert ucn_acc to UTF-8
					int unicode_remaining = 6; // number of UTF-8 trailer chars
					for ( int shift = 31; ucn_acc >> shift == 0; shift -= 5 ) -- unicode_remaining;
			
					pass( mapped_char< struct ucn >( static_cast< std::uint8_t >( 0x7F80 >> unicode_remaining | ucn_acc >> unicode_remaining * 6 ), input_buffer.front() ) );
					while ( unicode_remaining -- ) {
						pass( mapped_char< struct ucn >( static_cast< std::uint8_t >( 0x80 | ( ( ucn_acc >> unicode_remaining * 6 ) & 0x3F ) ), input_buffer.front() ) );
					}
				}
				this->template pass< pass_policy::optional >( cplus::ucn( ucn_acc, std::move( input_buffer.front() ) ) );
				CPLUS_DO_FINALLY
			}
			return;
		}
	}
};

}

#endif
