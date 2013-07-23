// Cplus project, translation phases 1 and 2: character-level transformations.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_CHAR_DECODE
#define CPLUS_CHAR_DECODE

#include "formats.h"

#include <sstream>
#include <cstring>

namespace cplus {

template< typename output_iterator >
class char_decoder : public stage< output_iterator, char_decoder_config >,
	pp_char_source_import_base {
	
	using char_decoder::stage::pass;
	
	enum states { tri1 = pp_char_source_last, backslash, msdos };
	int state;
	
	int unicode_remaining; // used by UCN state machine
	char32_t ucn_acc;
	
	pp_char shift_buffer[ 9 ], *shift_p; // worst case is "\U1234567Z"
	
	void shift_reset()
		{ shift_p = shift_buffer; }
	
	void shift( raw_char const &c )
		{ * shift_p ++ = { c, normal }; }
	
	void unshift() {
		auto && guard = util::finally( [this]() noexcept { shift_reset(); } );
		pass( shift_buffer, shift_p );
		static_cast< void >( guard );
	}
	
	lex_decode_state get_decode_state() {
		lex_decode_state ret = {};
		this->template pass< pass_policy::optional >( ret );
		return ret;
	}
	
public:
	template< typename ... args >
	char_decoder( args && ... a )
		: char_decoder::stage( std::forward< args >( a ) ... ),
		state{ normal }, shift_p{ shift_buffer } {}
	
	void flush()
		{ unshift(); }
	
	void operator()( raw_char const &c ) {
		for (;;) switch ( state ) {
		case normal:
			switch ( c.c ) {
			case '?':	if ( this->get_config().disable_trigraphs
							|| get_decode_state() == lex_decode_state::raw ) goto normal;
						state = tri1;		shift( c );		return;
			case '\\':	if ( get_decode_state() == lex_decode_state::raw ) goto normal;
						state = backslash;	shift( c );		return;
			default: normal:				pass( c );		return;
			}
		
		case tri1:
			switch ( c.c ) {
			case '?':	state = trigraph;	shift( c );		return;
			default:	state = normal;		unshift();		continue;
			}
		
		case trigraph:
			static char const	tri_source[] =	"='()!<>-",
								tri_dest[] =	"#^[]|{}~";
			if ( char const *sp = std::strchr( tri_source, c.c ) ) {
				shift_reset();
				state = normal; // reset state
				pass( pp_char{ { static_cast< std::uint8_t >( tri_dest[ sp - tri_source ] ), c }, trigraph } ); // pass trigraph, located at discriminating symbol
				return;
			} else if ( c.c == '/' ) {
				shift_reset();
				* shift_p ++ = { { static_cast< std::uint8_t >( '\\' ), c }, trigraph };
				state = backslash;
				return;
			} else if ( c.c == '?' ) { // may be a trigraph preceded by a "?"
				auto && guard = util::finally( [&]() noexcept {
					shift_p = std::move( shift_buffer + 1, shift_p, shift_buffer );
					shift( c.c ); // shift current "?"
				} );
				pass( * shift_buffer ); // unshift only non-trigraph "?"
				
				static_cast< void >( guard );
				return; // remain in same state
			
			} else { // not a trigraph
						state = normal;		unshift();		continue;
			}
		
		case backslash:
			switch ( c.c ) {
			case 'u':
			case 'U':	if ( get_decode_state() == lex_decode_state::escape ) {
						state = normal;		unshift();		continue;
						}
						ucn_acc = 0;
						unicode_remaining = c.c == 'u'? 4 : 8;
						state = ucn;		shift( c );		return;
						
			case '\r':	state = msdos;		shift( c );		return; // phase 2
			
			case '\n': splice: {
					auto && guard = util::finally( [this]() noexcept {
						state = normal;		shift_reset();
					} );
					this->template pass< pass_policy::optional >( line_splice( std::move( shift_buffer[0] ) ) );
					static_cast< void >( guard );
															return; // phase 2
				}
			default:	state = normal;		unshift();		continue;
			}
		
		case msdos: // phase 2. Delete any \CRLF, not checking for consistency.
			switch ( c.c ) {
			case '\n':	goto splice;
			default:	state = normal;		unshift();		continue;
			}
			
		case ucn:
			{
				int digit;
				if ( c.c >= '0' && c.c <= '9' ) digit = c.c - '0';
				else if ( c.c >= 'a' && c.c <= 'f' ) digit = c.c - 'a' + 10;
				else if ( c.c >= 'A' && c.c <= 'F' ) digit = c.c - 'A' + 10;
				else {
						state = normal;		unshift();		continue;
				}
				ucn_acc = ( ucn_acc << 4 ) | digit;
			}
			
			if ( -- unicode_remaining ) {
				shift( c );
				return;
			} else {
				auto && guard = util::finally( [this]() noexcept {
					state = normal;
					shift_reset();
				} );
				if ( ucn_acc < 0x80 ) {
					pass( pp_char{ { static_cast< std::uint8_t >( ucn_acc ), shift_buffer[0] }, ucn } );
				
				} else { // convert ucn_acc to UTF-8
					unicode_remaining = 6; // number of UTF-8 trailer chars
					for ( int shift = 31; ucn_acc >> shift == 0; shift -= 5 ) -- unicode_remaining;
				
					pass( pp_char{ { static_cast< std::uint8_t >( 0x7F80 >> unicode_remaining | ucn_acc >> unicode_remaining * 6 ), shift_buffer[0] }, ucn } );
					while ( unicode_remaining -- ) {
						pass( pp_char{ { static_cast< std::uint8_t >( 0x80 | ( ( ucn_acc >> unicode_remaining * 6 ) & 0x3F ) ), shift_buffer[0] }, ucn } );
					}
				}
				static_cast< void >( guard );
				return;
			}
		}
	}
};

}

#endif
