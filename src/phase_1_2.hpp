// Cplus project, translation phases 1 and 2: character-level transformations.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_PHASE_1_2
#define CPLUS_PHASE_1_2

#include "formats.h"

#include <sstream>
#include <cstring>

namespace cplus {

template< typename output_iterator >
class phase1_2 : public stage< output_iterator, phase1_2_config >,
	pp_char_source_import_base {
	
	enum states { tri1 = pp_char_source_last, backslash, msdos, rstring };
	int state;
	
	int unicode_remaining; // used by UCN state machine
	char32_t ucn_acc;
	bool inhibit_ucn;
	
	pp_char shift_buffer[ 9 ], *shift_p; // worst case is "\U1234567Z"
	
	void shift_reset()
		{ shift_p = shift_buffer; }
	
	void shift( raw_char const &c )
		{ * shift_p ++ = { c, normal }; }
	
	void unshift() {
		inhibit_ucn = false;
		for ( pp_char *p = shift_buffer; p != shift_p; ++ p ) {
			try {
				phase1_2::stage::pass( * p );
			} catch ( inhibit_ucn_notification &n ) { // follows backslash, which is always shifted
				if ( p == shift_p - 1 ) inhibit_ucn = true;
			}
		}
		shift_reset();
	}
	
	void pass( raw_char const &c ) try {
		inhibit_ucn = false;
		if ( state == normal ) phase1_2::stage::pass( c );
		else phase1_2::stage::pass( pp_char{ c, static_cast< pp_char_source >( state ) } );
	} catch ( raw_string_notification &n ) { // follows quote, which is never shifted
		state = n.entering? (int) rstring : normal;
	}
	
public:
	template< typename ... args >
	phase1_2( args && ... a )
		: phase1_2::stage( std::forward< args >( a ) ... ),
		state{ normal }, inhibit_ucn{ false }, shift_p{ shift_buffer } {}
	
	void flush()
		{ unshift(); }
	
	void operator()( raw_char const &c ) {
		for (;;) switch ( state ) {
		case normal:
			switch ( c.c ) {
			case '?':	if ( this->get_config().disable_trigraphs ) goto normal;
						state = tri1;		shift( c );		return;
			case '\\':	state = backslash;	shift( c );		return;
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
				pass( { static_cast< std::uint8_t >( tri_dest[ sp - tri_source ] ), c } ); // pass trigraph state to client...
				shift_reset();
				state = normal; // ... before resetting state
				return;
			} else if ( c.c == '/' ) {
				shift_reset();
				* shift_p ++ = { { static_cast< std::uint8_t >( '\\' ), c }, trigraph };
				state = backslash;
				return;
			} else if ( c.c == '?' ) { // may be a trigraph preceded by a "?"
				phase1_2::stage::pass( * shift_buffer ); // unshift only that "?"
				shift_p = std::move( shift_buffer + 1, shift_p, shift_buffer );
				shift( c.c ); // shift current "?"
				return; // remain in same state
			
			} else { // not a trigraph
						state = normal;		unshift();		continue;
			}
		
		case backslash:
			switch ( c.c ) {
			case 'u':
			case 'U':	if ( inhibit_ucn ) {
						state = normal;		unshift();		continue;
						}
						ucn_acc = 0;
						unicode_remaining = c.c == 'u'? 4 : 8;
						state = ucn;		shift( c );		return;
			case '\r':	state = msdos;		shift( c );		return; // phase 2
			case '\n':	state = normal;		shift_reset();	return; // phase 2
			default:	state = normal;		unshift();		continue;
			}
		
		case msdos: // phase 2. Delete any \CRLF, not checking for consistency.
			switch ( c.c ) {
			case '\n':	state = normal;		shift_reset();	return;
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
			} else if ( ucn_acc < 0x80 ) {
				pass( { static_cast< std::uint8_t >( ucn_acc ), shift_buffer[0] } );
				
			} else { // convert ucn_acc to UTF-8
				unicode_remaining = 6; // number of UTF-8 trailer chars
				for ( int shift = 31; ucn_acc >> shift == 0; shift -= 5 ) -- unicode_remaining;
				
				pass( { static_cast< std::uint8_t >( 0x7F80 >> unicode_remaining | ucn_acc >> unicode_remaining * 6 ), shift_buffer[0] } );
				while ( unicode_remaining -- ) {
					pass( { static_cast< std::uint8_t >( 0x80 | ( ( ucn_acc >> unicode_remaining * 6 ) & 0x3F ) ), shift_buffer[0] } );
				}
			}
			state = normal;
			shift_reset();
			return;
		
		case rstring:
			return pass( c );
		}
	}
};

}

#endif
