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
	location_t file_pos;
	int state;
	
	int unicode_remaining; // used by UCN state machine
	char32_t ucn_acc;
	bool inhibit_ucn;
	
	pp_char shift_buffer[ 9 ], *shift_p; // worst case is "\U1234567Z"
	
	void shift_reset()
		{ shift_p = shift_buffer; }
	
	void shift( std::uint8_t c )
		{ * shift_p ++ = pp_char{ c, file_pos, normal }; }
	
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
	
	void pass( std::uint8_t c ) try {
		inhibit_ucn = false;
		phase1_2::stage::pass( pp_char{ c, file_pos, static_cast< pp_char_source >( state ) } );
	} catch ( raw_string_notification &n ) { // follows quote, which is never shifted
		state = n.entering? (int) rstring : normal;
	}
	
public:
	template< typename ... args >
	phase1_2( args && ... a )
		: phase1_2::stage( std::forward< args >( a ) ... ),
		file_pos{ 1 }, state{ normal }, inhibit_ucn{ false } {
		shift_reset();
	}
	
	void flush() // append newline, §2.2/2. Don't execute a last-line directive ending in "\".
		{ (*this)( '\n' ); }
	
	void operator()( std::uint8_t c ) {
		if ( c == '\n' ) {
			file_pos = ( file_pos + 1 ) & file_location::line_mask;
		} else {
			file_pos += file_location::column_increment;
		}
		
		for (;;) switch ( state ) {
		case normal:
			switch ( c ) {
			case '?':	if ( this->get_config().disable_trigraphs ) goto normal;
						state = tri1;		shift( c );		return;
			case '\\':	state = backslash;	shift( c );		return;
			default: normal:				pass( c );		return;
			}
		
		case tri1:
			switch ( c ) {
			case '?':	state = trigraph;	shift( c );		return;
			default:	state = normal;		unshift();		continue;
			}
		
		case trigraph:
			static char const	tri_source[] =	"='()!<>-",
								tri_dest[] =	"#^[]|{}~";
			if ( char const *sp = std::strchr( tri_source, c ) ) {
				shift_reset();
				pass( tri_dest[ sp - tri_source ] ); // pass trigraph state to client...
				state = normal; // ... before resetting state
				return;
			} else if ( c == '/' ) {
				state = backslash;
				shift( '\\' );
				shift_buffer->s = trigraph;
				return;
			} else if ( c == '?' ) { // may be a trigraph preceded by a "?"
				phase1_2::stage::pass( * shift_buffer ); // unshift only that "?"
				shift_p = std::move( shift_buffer + 1, shift_p, shift_buffer );
				shift( c ); // shift current "?"
				return; // remain in same state
			
			} else { // not a trigraph
						state = normal;		unshift();		continue;
			}
		
		case backslash:
			switch ( c ) {
			case 'u':
			case 'U':	if ( inhibit_ucn ) {
						state = normal;		unshift();		continue;
						}
						ucn_acc = 0;
						unicode_remaining = c == 'u'? 4 : 8;
						state = ucn;		shift( c );		return;
			case '\r':	state = msdos;		shift( c );		return; // phase 2
			case '\n':	state = normal;		shift_reset();	return; // phase 2
			default:	state = normal;		unshift();		continue;
			}
		
		case msdos: // phase 2. Delete any \CRLF, not checking for consistency.
			switch ( c ) {
			case '\n':	state = normal;		shift_reset();	return;
			default:	state = normal;		unshift();		continue;
			}
			
		case ucn:
			{
				int digit;
				if ( c >= '0' && c <= '9' ) digit = c - '0';
				else if ( c >= 'a' && c <= 'f' ) digit = c - 'a' + 10;
				else if ( c >= 'A' && c <= 'F' ) digit = c - 'A' + 10;
				else {
						state = normal;		unshift();		continue;
				}
				ucn_acc = ( ucn_acc << 4 ) | digit;
			}
			
			if ( -- unicode_remaining ) {
				shift( c );
				return;
			} else if ( ucn_acc < 0x80 ) {
				pass( ucn_acc );
				
			} else { // convert ucn_acc to UTF-8
				unicode_remaining = 6; // number of UTF-8 trailer chars
				for ( int shift = 31; ucn_acc >> shift == 0; shift -= 5 ) -- unicode_remaining;
				
				pass( std::uint8_t( 0x7F80 >> unicode_remaining
								| std::uint64_t( ucn_acc ) >> unicode_remaining * 6 ) );
				while ( unicode_remaining -- ) {
					pass( std::uint8_t( 0x80 | ( ( ucn_acc >> unicode_remaining * 6 ) & 0x3F ) ) );
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
