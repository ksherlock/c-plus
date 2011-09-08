// Cplus project, translation phases 1 and 2: character-level transformations.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_PHASE_1_2
#define CPLUS_PHASE_1_2

#include "formats.h"
#include "constants.h"

#include <cstring>

namespace cplus {

template< typename output_iterator >
class phase1_2 {
	output_iterator cont;
	
	enum class states { CPLUS_PP_CHAR_SOURCES, tri1, backslash, msdos, rstring };
	std::streamoff file_pos;
	std::size_t newlines_removed; // how many to reinsert at the next newline
	states state;
	
	int unicode_remaining; // used by UCN state machine
	uint32_t ucn_acc;
	bool inhibit_ucn;
	
	pp_char shift_buffer[ 9 ], *shift_p; // worst case is "\U1234567"
	
	pp_char make_output( uint8_t c )
		{ return { c, file_pos, static_cast< pp_char_source >( state ) }; }
	
	void shift_reset()
		{ shift_p = shift_buffer; }
	
	void shift( uint8_t c )
		{ * shift_p ++ = pp_char{ c, file_pos, pp_char_source::normal }; }
	
	void unshift() {
		inhibit_ucn = false;
		for ( pp_char *p = shift_buffer; p != shift_p; ++ p ) {
			try {
				* cont ++ = * p;
			} catch ( inhibit_ucn_notification &n ) {
				if ( p == shift_p - 1 ) inhibit_ucn = true;
			}
		}
		shift_reset();
	}
	
	void pass( uint8_t c ) try {
		inhibit_ucn = false;
		* cont ++ = make_output( c );
	} catch ( raw_string_notification &n ) {
		state = n.entering? states::rstring : states::normal;
	}
	
public:
	template< typename ... args >
	phase1_2( args && ... a )
		: cont( std::forward< args >( a ) ... ),
		file_pos( 0 ), newlines_removed( 0 ), state( states::normal ),
		inhibit_ucn( false ) { shift_reset(); }

	void operator()( uint8_t c ) {
		++ file_pos; // Don't point at the middle of a UTF-8 sequence.
		
		for (;;) switch ( state ) {
		case states::normal:
			switch ( c ) {
			case '?':	state = states::tri1;		shift( c );		return;
			case '\\':	state = states::backslash;	shift( c );		return;
			case '\n':	for ( ; newlines_removed; -- newlines_removed ) pass( '\n' );
													pass( c );		return;
			default:								pass( c );		return;
			}
		
		case states::tri1:
			switch ( c ) {
			case '?':	state = states::trigraph;	shift( c );		return;
			default:	state = states::normal;		unshift();		continue;
			}
		
		case states::trigraph:
			static char const	tri_source[] =	"=/\'()!<>-",
								tri_dest[] =	"#\\^[]|{}~";
			if ( char const *sp = std::strchr( tri_source, c ) ) {
				shift_reset();
				c = tri_dest[ sp - tri_source ];
				if ( c != '\\' ) {
					pass( c ); // pass trigraph state to client...
					state = states::normal; // ... before resetting state
					return;
				} else {
					state = states::backslash;
					shift( c );
					shift_buffer->s = pp_char_source::trigraph;
					return;
				}
			} else if ( c == '?' ) { // may be a trigraph preceded by a "?"
				* cont ++ = * shift_buffer; // unshift only that "?"
				shift_p = std::move( shift_buffer + 1, shift_p, shift_buffer );
				shift( c ); // shift current "?"
				return; // remain in same state
			
			} else { // not a trigraph
						state = states::normal;		unshift();		continue;
			}
		
		case states::backslash:
			switch ( c ) {
			case 'U':	if ( inhibit_ucn ) {
						state = states::normal;		unshift();		continue;
						}
						ucn_acc = 0; unicode_remaining = 8;
						state = states::ucn;		shift( c );		return;
			case 'u':	if ( inhibit_ucn ) {
						state = states::normal;		unshift();		continue;
						}
						ucn_acc = 0; unicode_remaining = 4;
						state = states::ucn;		shift( c );		return;
			case '\r':	state = states::msdos;		shift( c );		return; // phase 2
			case '\n':	++ newlines_removed;
						state = states::normal;		shift_reset();	return; // phase 2
			default:	state = states::normal;		unshift();		continue;
			}
		
		case states::msdos: // phase 2. Delete any \CRLF, not checking for consistency.
			switch ( c ) {
			case '\n':	++ newlines_removed;
						state = states::normal;		shift_reset();	return;
			default:	state = states::normal;		unshift();		continue;
			}
			
		case states::ucn:
			{
				int digit;
				if ( c >= '0' && c <= '9' ) digit = c - '0';
				else if ( c >= 'a' && c <= 'f' ) digit = c - 'a' + 10;
				else if ( c >= 'A' && c <= 'F' ) digit = c - 'A' + 10;
				else {
						state = states::normal;		unshift();		continue;
				}
				ucn_acc = ( ucn_acc << 4 ) | digit;
				
				if ( -- unicode_remaining ) {
					shift( c );
					return;
				}
			}
			
			if ( ucn_acc >= 0xD800 && ucn_acc <= 0xDFFF ) {
				throw error( file_pos,
					"Universal-character-name corresponds to a surrogate pair code point (§2.3/2). "
					"If specifying UTF-16, encode the desired Unicode character and the pair will "
					"be generated. Otherwise, try a hexadecimal escape sequence \"\\xDNNN\"." );
			} else if ( ucn_acc < 0x80 ) {
				pass( ucn_acc );
				
			} else { // convert ucn_acc to UTF-8
				unicode_remaining = 6; // number of UTF-8 trailer chars
				for ( int shift = 31; ucn_acc >> shift == 0; shift -= 5 ) -- unicode_remaining;
				
				pass( uint8_t( 0x7F80 >> unicode_remaining
								| uint64_t( ucn_acc ) >> unicode_remaining * 6 ) );
				while ( unicode_remaining -- ) {
					pass( uint8_t( 0x80 | ( ( ucn_acc >> unicode_remaining * 6 ) & 0x3F ) ) );
				}
			}
			state = states::normal;
			shift_reset();
			return;
		
		case states::rstring:
			return pass( c );
		}
	}
	
	friend void finalize( phase1_2 &o ) {
		o( '\n' ); // append newline, §2.2/2. Don't execute a last-line directive ending in "\".
		finalize( o.cont );
	}
};

}

#endif
