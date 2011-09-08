// Cplus project, translation phase 3: generate preprocessing tokens.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_PHASE_3
#define CPLUS_PHASE_3

#include "util.h"
#include "formats.h"
#include "constants.h"

#include <algorithm>
#include <sstream>

namespace cplus {

template< typename output_iterator >
class phase3 {
	output_iterator cont;
	
	enum states {
		CPLUS_PP_TOKEN_TYPES, escape, exponent, ud_suffix, rstring, line_comment, block_comment
	} state, state_after_space;
	
	pp_token token;
	
	pp_char input_buffer[ 8 ], *input_buffer_p; // worst case is %:%\UFFFFFFFF
	std::string const *punct_lower, *punct_upper, *punct_match;
	
	std::size_t rstring_term_start, rstring_seq_len; // offset, length of rstring termination sequence
	
	util::wstring_convert utf8;
	void shift( pp_char const &in ) { // defer a UTF-8 character or punctuator
		* input_buffer_p ++ = in;
	}
	void unshift( pp_char const &in ) { // accept a UTF-8 sequence
		for ( pp_char *p = input_buffer; p != input_buffer_p; ++ p ) {
			token.s += p->c;
		}
		token.s += in.c; // always include final char
		input_buffer_p = input_buffer;
	}
	
	void pass() {
		/*	Newline characters also override state_after_space, but to see a
			newline, state must equal ws. So it's handled in "case ws:". */
		if ( token.type != pp_token_type::ws ) state_after_space = ws;
		* cont ++ = std::move( token ); // not sure if move helps
		token.s.clear();
		token.type = pp_token_type( state = state_after_space );
	}
	
public:
	template< typename ... args >
	phase3( args && ... a )
		: cont( std::forward< args >( a ) ... ),
		state( ws ), state_after_space( ws ),
		token( pp_token{ pp_token_type::ws, "\n", 0  } ),
		input_buffer_p( input_buffer ) {}
	
	void operator()( pp_char const &in ) {
		char32_t c;
		switch ( state ) {
		default:
			try {
				std::u32string s = utf8.from_bytes( in.c );
				if ( s.empty() ) return shift( in );
				else if ( state != string && state != char_lit && state != escape ) {
					if ( in.s == pp_char_source::ucn && char_in_set( char_set::basic_source, s[0] ) ) {
						throw error( in.p, "Please do not encode basic source text "
											"in universal-character-names (§2.3/2)." );
					} else if ( ( s[0] <= 0x1F && ! char_in_set( char_set::space, s[0] ) )
							|| ( s[0] >= 0x7F && s[0] <= 0x9F ) ) {
						throw error( in.p, "Stray control character (§2.3/2)." );
					}
				}
				c = s[0];
			} catch ( std::range_error & ) {
				throw error( in.p, "Malformed UTF-8." );
			}
			break;
		case rstring: case block_comment: case line_comment:
			c = in.c;
		}
		
		for (;;) switch ( state ) {
		case ws:
			if ( c == '\n' ) {
				if ( token == pp_constants::space ) token.s.clear();
				token.s += '\n';
				return;
			} else if ( char_in_set( char_set::space, c ) ) {
				if ( token.s.empty() ) {
					token = pp_constants::space;
					token.p = in.p;
				}
				return;
			
			} else {
				bool at_newline = false;
				if ( ! token.s.empty() ) {
					at_newline = token.s[0] == '\n';
					if ( at_newline ) state_after_space = ws;
					pass(); // empty whitespace tokens are filtered and do not use state_after_space
				}
				
				token.p = in.p;
				if ( state != ws ) {
					continue;
				}
				unshift( in );
				
				if ( char_in_set( char_set::punct, c ) ) {
					if ( char_in_set( char_set::multipunct, c ) ) {
						// tentatively set type to directive if it could possibly be one
						token.type = at_newline? pp_token_type::directive : pp_token_type::punct;
						state = punct;
						punct_lower = multichar_punctuators;
						punct_upper = multichar_punctuators_end;
						punct_match = nullptr;
					} else {
						token.type = pp_token_type::punct;
						pass();
					}
				} else if ( char_in_set( char_set::digit, c ) ) {
					token.type = pp_token_type( state = num );
				} else if ( char_in_set( char_set::initial, c ) ) {
					token.type = pp_token_type( state = id );
				} else if ( c == '\"' ) {
					token.type = pp_token_type( state = string );
				} else if ( c == '\'' ) {
					token.type = pp_token_type( state = char_lit );
				} else { // "other", such as a stray backslash
					token.type = pp_token_type::misc;
					pass(); // chars not in alpha may still be catenated to an id or header-name
				}
				return;
			}
		
		case id: // includes string and char literal suffixes
		case directive:
			if ( char_in_set( char_set::identifier, c ) ) {
				unshift( in );
				return;
			} else {
				std::string const *id_punct
					= std::lower_bound( id_punctuators, std::end( id_punctuators ), token.s );
				if ( id_punct != std::end( id_punctuators ) && token.s == * id_punct ) { // §2.13/1
					if ( id_punct == include_directive ) {
						if ( state == directive ) {
							pass();
							token.type = pp_token_type( state = state_after_space = header_name );
							continue;
						} else goto got_id;
					} else {
						token.type = pp_token_type::punct;
						pass();
						continue;
					}
				} else if ( c == '"'
					&& std::binary_search( string_prefixes, std::end( string_prefixes ), token.s ) ) {
					if ( token.s.back() == 'R' ) {
						unshift( in );
						state = rstring;
						token.type = pp_token_type::string;
						rstring_term_start = std::string::npos;
						throw raw_string_notification{ true };
					} else {
						unshift( in );
						token.type = pp_token_type( state = string );
						return;
					}
				} else if ( c == '\'' && ( token.s == "U" || token.s == "L" || token.s == "u" ) ) {
					token.type = pp_token_type( state = char_lit );
					unshift( in );
					return;
				
				} else got_id: {
					pass();
					continue;
				}
			}
		
		case num:
			if ( char_in_set( char_set::identifier, c ) || c == '.' ) {
				unshift( in ); // Standard unclear about combining diacritics with numerals.
				if ( c == 'E' || c == 'e' ) state = exponent;
				return;
			} else {
				pass();
				continue;
			}
		
		case exponent:
			if ( c == '+' || c == '-' ) {
				unshift( in );
				state = num;
				return;
			} else {
				state = num;
				continue;
			}
		
		case punct:
			if ( char_in_set( char_set::multipunct, c ) ) {
				token.s += c; // don't unshift because the buffer is being used for punct
				
				punct_lower = std::lower_bound( punct_lower, punct_upper, token.s );
				++ token.s.back();
				punct_upper = std::lower_bound( punct_lower, punct_upper, token.s );
				-- token.s.back();
				
				if ( punct_lower == punct_upper ) { // no possible match
					//token.s.pop_back(); // backtrack
					token.s.erase( token.s.size() - 1 ); // G++ doesn't have pop_back yet
					goto punct_done;
				} else {
					if ( punct_lower->size() == token.s.size() ) {
						punct_match = punct_lower;
					}
					shift( in );
					return;
				}
			} else if ( token.s == "." && char_in_set( char_set::digit, c ) ) {
				token.type = pp_token_type( state = num );
				unshift( in );
				return;
			
			} else punct_done: {
				int match_size;
				if ( ! punct_match ) {
					match_size = 1;
				
				} else if ( punct_match == cplus::block_comment ) {
					state = block_comment;
					token.s.clear();
					input_buffer_p = input_buffer;
					continue;
				
				} else if ( punct_match == cplus::line_comment ) {
					state = line_comment;
					token.s.clear();
					input_buffer_p = input_buffer;
					continue;
				
				} else { // Common case! punct_match != nullptr and not a comment:
					match_size = punct_match->size();
					
					if ( punct_match == less_scope ) { // edge case of §2.5/3:
						// group "<: ::" and "<: :>" specially or "< ::" by default.
						match_size = c == ':' || c == '>'? 2 : 1;
					}
				}
				token.s.erase( match_size ); // make token a copy of punct_match
				
				if ( token.type == pp_token_type::directive ) {
					if ( punct_match == alt_hash || token.s == "#" ) {
						pass();
						state_after_space = directive;
						if ( char_in_set( char_set::initial, c ) ) {
							token.type = pp_token_type( state = directive ); // kludge
						}
						goto punct_passed;
					} else token.type = pp_token_type::punct;
				}
				pass();
			punct_passed:
				pp_char *pend = input_buffer_p;
				input_buffer_p = input_buffer;
				for ( pp_char *p = input_buffer + match_size - 1; p != pend; ++ p ) {
					(*this)( * p ); // retry input that wasn't handled before
				}
				return (*this)( in ); // send UTF-8 back to utf8_decode (again)
			}
		
		case block_comment:
			if ( token.s.empty() ) {
				if ( c == '*' ) token.s += c;
				return;
			} else if ( c == '/' ) {
				token.type = pp_token_type( state = ws );
				token.s = ' ';
				return;
			} else {
				token.s.clear();
				return;
			}
		
		case line_comment:
			if ( c == '\n' ) {
				token.type = pp_token_type( state = ws );
				token.s = '\n';
				return;
			} else {
				return;
			}
		
		case rstring:
			unshift( in );
			if ( rstring_term_start == std::string::npos ) {
				if ( c == '(' ) {
					rstring_term_start = 0; // kludge to advance sub-state
					return;
				}
				if ( char_in_set( char_set::space, c ) || c == ')' || c == '\\'
						|| ! char_in_set( char_set::basic_source, c ) )
					throw error( in.p, "Raw string termination sequence must consist of "
						"alphanumeric characters and C-language punctuation except parens "
						"and backslash (§2.14.5)." );
				if ( token.s.size() - token.s.find( '"' ) == 18 )
					throw error( in.p, "Raw string termination sequence may contain at "
						"most 16 characters (§2.14.5/2)." );
				return;
			} else if ( c == ')' ) {
				rstring_term_start = token.s.size(); // termination begins at next char
				rstring_seq_len = 0;
				return;
			} else if ( rstring_term_start != 0 ) {
				// match the char to the corresponding sequence at beginning of the token
				uint8_t begin_seq_char = token.s[ token.s.find( '"' ) + 1 + rstring_seq_len ];
				if ( c == '"' && begin_seq_char == '(' ) {
					state = ud_suffix;
					throw raw_string_notification{ false };
					
				} else if ( begin_seq_char != c ) {
					rstring_term_start = 0; // mismatch, start over at next ")"
					return;
				} else {
					++ rstring_seq_len;
					return;
				}
			}
			return;
		
		case string:
		case char_lit:
			/*if ( c > 0x10FFFF && ( token.s[0] == 'U' || token.s[0] == 'u' ) ) {
				throw error( in.p, "Universal-character-names within a char16_t or char32_t literal must "
					"be ≤ 0x10FFFF, i.e. in Unicode range (§2.14.5/15). Try a hex escape sequence instead."
			} -- this should be done later */
			if ( in.s == pp_char_source::ucn && char_in_set( char_set::basic_source, c ) ) {
				goto unmap_ucn; // Basic source UCNs are special, and must be processed like escapes.
			}
			unshift( in );
			switch ( c ) {
			case '\"':	if ( state == string ) state = ud_suffix; return;
			case '\'':	if ( state == char_lit ) state = ud_suffix; return;
			case '\\':	state = escape; throw inhibit_ucn_notification();
			case '\n':	throw error( in.p, "Use \\n instead of embedding a newline in a literal." );
			default:	return;
			}
		
		// Don't map escape sequences yet, as that depends on execution charset.
		case escape:
			// But do *unmap* UCNs, since eg "\$" = "\\u0024" greedily matches the backslash escape first.
			if ( in.s == pp_char_source::ucn ) throw error( in.p, "ICE: failed to inhibit UCN conversion." );
			if ( ! char_in_set( char_set::basic_source, c ) ) {
			unmap_ucn:
				int digits = c >= 0x10000? 8 : 4;
				token.s += digits == 4? "\\u" : "\\U";
				std::ostringstream s;
				s.width( digits );
				s.fill( '0' );
				s.setf( std::ios::hex, std::ios::basefield );
				s.setf( std::ios::uppercase );
				s << static_cast< uint32_t >( c );
				token.s += s.str();
				input_buffer_p = input_buffer;
			} else unshift( in );
			state = static_cast< states >( token.type );
			return;
		
		case ud_suffix: // This only checks the first char to see if a suffix exists.
			if ( char_in_set( char_set::initial, c ) ) {
				state = id;
				continue;
			} else {
				if ( ! token.s.empty() ) pass();
				token.type = pp_token_type( state = ws );
				continue;
			}
		
		case header_name:
			if ( token.s.size() >= 2 && ( ( token.s[0] == '<' && token.s.back() == '>' )
									|| ( token.s[0] == '"' && token.s.back() == '"' ) ) ) {
				if ( c == '\n' ) { // Obtained header-name token. Either return it...
					input_buffer_p = input_buffer;
					pass();
					continue;
				} else if ( char_in_set( char_set::space, c ) ) { // ... or hold on to it...
					if ( input_buffer_p == input_buffer ) shift( in );
					return; // remember whether there was trailing space, not how much
				} else {
					goto header_name_retokenize; // ... or reject it and redo.
				}
			} else if ( c == '\n' ) header_name_retokenize: {
				unshift( in );
				/*	Re-match against §16.2/4. If characters were specified as trigraphs or UCNs,
					file positioning will be approximate. */
				std::streamoff p = token.p;
				std::string redo;
				swap( token.s, redo );
				token.type = pp_token_type( state = ws );
				for ( uint8_t c : std::move( redo ) ) (*this)( pp_char{ c, p ++ } );
				return;
			} else {
				unshift( in );
				return;
			}
		
		default: ;
		}
	}
	
	friend void finalize( phase3 &o ) {
		if ( o.state == block_comment || o.state == line_comment )
			throw error( o.token.p, "Unterminated comment." );
		if ( o.state == string || o.state == char_lit || o.state == rstring )
			throw error( o.token.p, "Unterminated literal." );
		if ( o.state == header_name ) // possible by ending file with a backslash
			throw error( o.token.p, "Unterminated header name." );
		if ( o.state != ws ) throw error( o.token.p, "ICE: Phase 3 terminated in unexpected state." );
		
		if ( ! o.token.s.empty() ) o.pass();
		finalize( o.cont );
	}
};

}

#endif
