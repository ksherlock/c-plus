// Cplus project, translation phase 3: generate preprocessing tokens.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_LEX_H
#define CPLUS_LEX_H

#include "formats.h"
#include "constants.h"

#include <sstream>

namespace cplus {

int fast_dispatch = 0, slow_dispatch = 0, slow_histo[ 20 ];

namespace char_set {
	constexpr char_bitmap ascii_alnum_not_exponent = {
		0x00, 0x00, 0x00, 0x00,
		0x00, 0x00, 0xFF, 0xC0,
		0x7B, 0xFF, 0xFF, 0xE1,
		0x7B, 0xFF, 0xFF, 0xE0,
	};
	constexpr char_bitmap line_space = {
		0x00, 0x40, 0x00, 0x00,
		0x80, 0x00, 0x00, 0x00,
	};
	constexpr char_bitmap anything_on_line = {
		0xFF, 0xC3, 0xFF, 0xFF, // Exclude newline, vertical tab, form feed, carriage return.
		0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xFF, 0xFF, 0xFF,
	};
	constexpr char_bitmap not_block_termination = {
		0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xDE, 0xFF, 0xFF, // Exclude asterisk and slash.
		0xFF, 0xFF, 0xFF, 0xFF,
		0xFF, 0xFF, 0xFF, 0xFF,
	};
	
	constexpr char_bitmap const *safe_chars[] = {
		&none /* ws */, &ascii_alnum /* id */, &ascii_alnum_not_exponent /* num */, &none /* todo, multipunct for punct */,
		&none, &none, &none, &none, /* string_lit, char_lit, header_name, misc */
		&none, &none, &ascii_alnum, &none, &none, /* escape, exponent, directive, ud_suffix, rstring */
		&line_space /* space_run */, &space /* after_newline */,
		&anything_on_line /* line_comment */, &none /* line_comment_check */, &not_block_termination /* block_comment */, &none /* initial */
	};
}

template< typename output_iterator >
class lexer : public stage< output_iterator, lexer_config >,
	token_type_import_base {
	
	enum states {
		escape = token_type_last, exponent, directive, ud_suffix, rstring, space_run, after_newline, line_comment, line_comment_check, block_comment, initial
	};
	int state, state_after_space;
	bool in_directive; // only serves to diagnose whitespace restriction of §16/4
	
	line_splice splice_first; // the first line splice, if any, seen during the last token
	std::size_t splice_count = 0;
	
	cplus::token token;
	
	std::vector< pp_char > input_buffer;
	string const *punct_lower, *punct_upper, *punct_match;
	
	std::size_t rstring_term_start, rstring_seq_len; // offset, length of rstring termination sequence
	
	util::utf8_convert utf8;
	void shift( pp_char const &in ) { // defer a UTF-8 character or punctuator
		input_buffer.push_back( in );
	}
	void unshift( pp_char const &in ) { // accept a UTF-8 sequence
		for ( auto && p : input_buffer ) token.s += p.c;
		token.s += in.c; // always include final char
		input_buffer.clear();
	}
	
	void pass() {
		/*	Newline characters also override state_after_space, but to see a
			newline, state must equal ws. So it's handled in "case ws:". */
		if ( token.type != ws ) state_after_space = ws;
		auto && guard = util::finally( [ this ]() noexcept {
			token = {};
			token.type = state = state_after_space;
		} );
		lexer::stage::pass( std::move( token ) );
		
		static_cast< void >( guard );
	}
	
	void general_path( raw_char const &in, pp_char_source in_s ) {
		char32_t &c = utf8.result;
		if ( state == block_comment || state == line_comment || ( state == header_name && ( token.type == block_comment || token.type == space_run ) ) ) {
			// Disable UTF-8 decoding where possible, but avoid generating diagnostics on non-characters.
			c = in.c;
		} else {
			bool undo_multibyte;
			try {
				try {
					if ( ! utf8.in( in.c ) ) return shift( in );
					else {
						this->template diagnose< diagnose_policy::pass, error >( c >= 0xD800 && c <= 0xDFFF, in,
							"This is a surrogate pair code point (§2.3/2). If specifying UTF-16, " // message assumes UTF-16 hasn't been encoded in UTF-8
							"encode the desired Unicode character and the pair will be generated. Otherwise, try a hexadecimal escape sequence \"\\xDnnn\"." );
						if ( state != rstring && state != string_lit && state != char_lit && state != escape ) {
							this->template diagnose< diagnose_policy::pass, error >(
								in_s == pp_char_source::ucn && char_in_set( char_set::basic_source, c ),
								in, "Please do not encode basic source text in universal-character-names (§2.3/2)." );
							this->template diagnose< diagnose_policy::fatal, error >(
								( c <= 0x1F && ! char_in_set( char_set::space, c ) ) || ( c >= 0x7F && c <= 0x9F ),
								in, "Stray control character (§2.3/2)." );
						}
					}
				} catch ( util::utf8_convert::error & e ) { // get rid of whatever formerly appeared to be valid UTF-8.
					undo_multibyte = e.incomplete;
					throw error( in, "Malformed UTF-8." );
				} catch ( ... ) {
					undo_multibyte = c >= 0x80;
					throw;
				}
			} catch ( ... ) {
				if ( undo_multibyte ) {
					while ( static_cast< std::uint8_t >( token.s.back() ) < 0xC0 ) token.s.pop_back();
					token.s.pop_back();
				}
				throw;
			}
		}
		
		for (;;) switch ( state ) {
		case initial:
			state = after_newline;
			
		case ws: // every other state returns here after pass(), even if a space character hasn't been seen
			if ( splice_count != 0 ) { // Reinsert line splices seen during last token as whitespace, if preserve_space is set.
				token.construct::operator = ( std::move( splice_first ) );
				do token.s += "\\\n"; while ( -- splice_count );
			}
		case space_run:
			if ( c == '\n' || c == '\r' ) { // quietly allow CR on the assumption it precedes LF.
				if ( in_directive ) {
					if ( this->get_config().preserve_space && ! token.s.empty() ) {
						pass(); // Directive includes space up to newline.
					}
					lexer::stage::template pass< pass_policy::optional >( delimiter< struct directive, delimiter_sense::close >( in ) );
					in_directive = false;
				}
				if ( token.s.empty() ) token.construct::operator = ( in );
				if ( ! this->get_config().preserve_space ) token.assign_content( pp_constants::newline ); // Newline has precedence over space in non-preserving mode.
				state = after_newline;
				state_after_space = ws; // Cancel looking for directive or header name.
			} else if ( char_in_set( char_set::space, c ) && token.s.empty() ) {
				token.construct::operator = ( in );
				if ( ! this->get_config().preserve_space ) token.assign_content( pp_constants::space );
				state = space_run;
			}
		case after_newline:
			if ( char_in_set( char_set::space, c ) ) {
				if ( this->get_config().preserve_space ) {
					assert ( token.s.get_allocator() == this->get_config().stream_pool );
					unshift( in );
				}
				this->template diagnose< diagnose_policy::pass, error >( in_directive && ( c == '\v' || c == '\f' ),
					token, "Only space and horizontal tab allowed in whitespace outside comments in a directive (§16/4)." );
				return;
			
			} else {
				if ( ! token.s.empty() ) { // Filter empty whitespace tokens, but always advance state to state_after_space.
					if ( state == after_newline ) state_after_space = after_newline; // Newline cancels state_after_space, i.e. directive and header name detection.
					token.type = ws; // In case token.type = state_after_space assigned after_newline.
					pass();
				} else state = state_after_space;
				
				token.construct::operator = ( in );
				if ( state != ws && state != after_newline ) {
					token.type = state;
					continue; // Go to state_after_space.
				}
			redispatch: // Come back from state_after_space rejection.
				unshift( in );
				
				if ( char_in_set( char_set::punct, c ) ) {
					if ( char_in_set( char_set::multipunct, c ) ) {
						// tentatively set type to directive if it could possibly be the opening #
						token.type = state == after_newline? static_cast< int >( directive ) : static_cast< int >( punct );
						state = punct;
						punct_lower = multichar_punctuators;
						punct_upper = multichar_punctuators_end;
						punct_match = nullptr;
						
					} else { // question mark, comma, semicolon, tilde, parens, braces, brackets
						token.type = punct;
						pass();
					}
				} else if ( char_in_set( char_set::digit, c ) ) {
					token.type = state = num;
				} else if ( char_in_set( char_set::initial, c ) ) {
					token.type = state = id;
				} else if ( c == '\"' ) {
					token.type = state = string_lit;
				} else if ( c == '\'' ) {
					token.type = state = char_lit;
				} else { // "other", such as a stray backslash
					token.type = misc;
					pass(); // chars not in alpha may still be catenated to an id or header-name
				}
				return;
			}
		
		case directive:
			token.type = id;
		case id: // Includes ud-suffix after initial character.
			if ( char_in_set( char_set::identifier, c ) ) {
				unshift( in );
				return;
			} else {
				string const *id_punct
					= std::lower_bound( id_punctuators, std::end( id_punctuators ), token.s );
				if ( id_punct != std::end( id_punctuators ) && token.s == * id_punct ) { // §2.13/1
					if ( id_punct == include_directive ) {
						if ( state == directive ) {
							pass();
							state_after_space = header_name;
							continue;
						} else goto got_id;
					} else {
						token.type = punct;
						pass();
						continue;
					}
				} else if ( c == '"'
						&& std::binary_search( string_lit_prefixes, std::end( string_lit_prefixes ), token.s ) ) {
					if ( token.s.back() == 'R' ) {
						unshift( in );
						state = rstring;
						token.type = string_lit;
						rstring_term_start = std::string::npos;
						return;
					} else {
						unshift( in );
						token.type = state = string_lit;
						return;
					}
				} else if ( c == '\''
						&& std::binary_search( char_lit_prefixes, std::end( char_lit_prefixes ), token.s ) ) {
					token.type = state = char_lit;
					unshift( in );
					return;
				
				} else {
					if ( ( token.type == string_lit && token.s.back() != '"' )
						|| ( token.type == char_lit && token.s.back() != '\'' ) ) { // an alternative token cannot be a ud-suffix
						std::string suffix( token.s );
						suffix.erase( 0, suffix.rfind( token.type == string_lit? '"' : '\'' ) + 1 );
						string suffix_s( suffix.c_str(), this->get_config().stream_pool ); // preempts token for writability
						string const *id_punct = std::lower_bound( id_punctuators, std::end( id_punctuators ), suffix_s );
						if ( id_punct != std::end( id_punctuators ) && suffix_s == * id_punct
								&& id_punct != include_directive ) {
							token.s.assign( token.s.begin(), token.s.end() - suffix_s.size() ); // preempts suffix_s
							pass();
							
							token.s = std::move( suffix_s ); // suffix_s is passed not as writer
							token.type = punct;
							pass();
						}
					}
				got_id:
					if ( token.s.empty() ) goto redispatch; // state_after_space is still directive
					else pass();
					continue;
				}
			}
		
		case num:
			if ( char_in_set( char_set::identifier, c ) || c == '.' ) {
				unshift( in ); // Standard unclear about non-initial chars (combining diacritics) with numerals.
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
					token.s.pop_back(); // backtrack
					goto punct_done;
				
				} else if ( punct_lower == cplus::block_comment ) {
					state = block_comment;
					if ( ! this->get_config().preserve_space ) token.s = pp_constants::space.s;
					input_buffer.clear();
					return;
				
				} else if ( punct_lower == cplus::line_comment ) {
					state = line_comment;
					// token will be discarded if not preserving space
					input_buffer.clear();
					return;
				
				} else {
					if ( punct_lower->size() == token.s.size() ) {
						punct_match = punct_lower;
					}
					shift( in );
					return;
				}
			} else if ( token.s == pp_constants::dot.s && char_in_set( char_set::digit, c ) ) {
				token.type = state = num;
				unshift( in );
				return;
			
			} else punct_done: {
				int match_size;
				if ( ! punct_match ) {
					match_size = 1;
				} else if ( punct_match == less_scope ) {
					// edge case of §2.5/3: group "<: ::" and "<: :>" specially or "< ::" by default.
					match_size = c == ':' || c == '>'? 2 : 1;
				} else {
					match_size = punct_match->size();
				}
				token.s.resize( match_size ); // make token a copy of punct_match
				
				std::vector< pp_char > retry;
				retry.swap( input_buffer ); // Buffer will be clear if any pass throws (and meaningful chars lost).
				
				if ( token.type == directive ) {
					token.type = punct;
					if ( punct_match == hash_alt || token == pp_constants::stringize ) {
						lexer::stage::template pass< pass_policy::optional >( delimiter< struct directive, delimiter_sense::open >( token ) );
						pass();
						state_after_space = directive;
						in_directive = true;
						
						goto punct_passed;
					}
				}
				pass();
			punct_passed:
				std::for_each( retry.begin() + match_size - 1, retry.end(), std::ref( * this ) ); // Retry non-matched punctuation and UTF-8 lead sequence.
				return (*this)( std::move( in ) );
			}
		
		case block_comment:
			if ( this->get_config().preserve_space ) token.s += c; // no UTF-8 decoding in comments
			
			if ( ! input_buffer.empty() && c == '/' && in_s != pp_char_source::ucn ) {
				/*	If we got here from /​* being a pseudo punctuator, looking for #, then
					this is still the beginning of the line, and continue looking for a #. */
				state = token.type == directive? (int) after_newline : space_run;
				token.type = ws;
			}
			input_buffer.clear();
			
			if ( c == '*' && in_s != pp_char_source::ucn ) shift( in ); // advance to next sub-state
			return;
			
		case line_comment_check:
			this->template diagnose< diagnose_policy::pass, error >( char_in_set( char_set::space, c ) || in_s == pp_char_source::ucn,
				in, "Only space allowed between vertical tab or form feed and line comment terminating newline (§2.8/1)." );
		case line_comment:
			if ( c == '\n' && in_s != pp_char_source::ucn ) {
				token.type = state = ws;
				continue;
			} else {
				if ( this->get_config().preserve_space ) token.s += c;
				if ( ( c == '\v' || c == '\f' ) && in_s != pp_char_source::ucn ) state = line_comment_check;
				return;
			}
			
		case rstring:
			unshift( in );
			if ( rstring_term_start == std::string::npos ) {
				if ( c == '(' ) {
					rstring_term_start = 0; // kludge to advance sub-state
					return;
				}
				this->template diagnose< diagnose_policy::pass, error >(
					char_in_set( char_set::space, c ) || c == ')' || c == '\\' || ! char_in_set( char_set::basic_source, c ),
					token, "Raw string delimiter sequence must consist of alphanumeric characters and C-language punctuation except parens "
					"and backslash (§2.14.5)." );
				this->template diagnose< diagnose_policy::pass, error >( token.s.size() - token.s.find( '"' ) == 18,
					token, "Raw string delimiter sequence may contain at most 16 characters (§2.14.5/2)." );
				return;
			} else if ( c == ')' ) {
				rstring_term_start = token.s.size(); // termination begins at next char
				rstring_seq_len = 0;
				return;
			} else if ( rstring_term_start != 0 ) {
				// match the char to the corresponding sequence at beginning of the token
				std::uint8_t begin_seq_char = token.s[ token.s.find( '"' ) + 1 + rstring_seq_len ];
				if ( c == '"' && begin_seq_char == '(' ) {
					state = ud_suffix;
					return;
					
				} else if ( begin_seq_char != c ) {
					rstring_term_start = 0; // mismatch, start over at next ")"
					return;
				} else {
					++ rstring_seq_len;
					return;
				}
			}
			return;
		
		case string_lit:
		case char_lit:
			if ( in_s == pp_char_source::ucn && char_in_set( char_set::basic_source, c ) ) {
				goto unmap_ucn; // Basic source UCNs are special, and must be processed like escapes.
			}
			unshift( in );
			switch ( c ) {
			case '\"':	if ( state == string_lit ) state = ud_suffix; return;
			case '\'':	if ( state == char_lit ) state = ud_suffix; return;
			case '\\':	state = escape; return;
			case '\n':
				this->template diagnose< diagnose_policy::pass, error >( true, token, "Use \\n instead of embedding a newline in a literal (§2.14.5)." ); return;
			default:	return;
			}
		
		// Don't map escape sequences yet, as that depends on execution charset.
		case escape:
			// But do *unmap* UCNs, since eg "\$" = "\\u0024" greedily matches the backslash escape first.
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
				//token.s += s.str();
				token.s += s.str().c_str();
				input_buffer.clear();
			} else unshift( in );
			state = static_cast< states >( token.type );
			this->template diagnose< diagnose_policy::pass, error >( in_s == pp_char_source::ucn, token, "ICE: failed to inhibit UCN conversion." );
			return;
		
		case ud_suffix: // This only checks the first char to see if a suffix exists.
			if ( char_in_set( char_set::initial, c ) ) {
				state = id;
				continue;
			} else {
				if ( ! token.s.empty() ) pass();
				token.type = state = ws;
				continue;
			}
		
			/*	A header-name ceases to be a header-name if it's followed by another token. So we must scan to the end of line.
				But the usual whitespace handling would pass the whitespace before the state machine could return to header_name.
				Shift the header-name characters, then scan whitespace into token until \n. If anything goes wrong, retokenize. */
		case header_name:
			if ( token.type == header_name ) {
				if ( c == '\n' ) return header_name_retry( in );
				
				else if ( input_buffer.empty() ) {
					if ( c != '<' && c != '\"' ) {
						goto redispatch; // state_after_space is already header_name
					}
				} else if ( ( input_buffer[0].c == '<' && c == '>' )
						 || ( input_buffer[0].c == '\"' && c == '\"' ) ) {
					token.type = state_after_space = ws;
				}
				shift( in );
				return;
				
			} else if ( token.type == ws ) {
				if ( c == '\n' ) pass_header_name: {
					auto trailing_ws = std::move( token.s ); // save any trailing space
					token.s.clear();
					
					token.type = header_name; // restore header name into token
					for ( auto && p : input_buffer ) token.s.push_back( p.c );
					input_buffer.clear(); // like unshift() but doesn't include current char
					
					auto state_after_space_back = state_after_space; // schedule possible continuation to line comment mode
					pass(); // pass header name (and reset to whitespace mode)
					if ( this->get_config().preserve_space ) token.s = std::move( trailing_ws ); // restore saved space
					token.construct::operator = ( in ); // approximately locate the space at the newline or //
					token.type = ws;
					state = state_after_space_back; // continue to line comment mode if "//" sent us here
					
					continue;
				
				} else if ( c == '/' ) {
					shift( in );
					token.type = punct;
				
				} else if ( char_in_set( char_set::space, c ) ) {
					token.s += c;
					this->template diagnose< diagnose_policy::pass, error >( c == '\v' || c == '\f',
						token, "Only space and horizontal tab allowed in whitespace outside comments in a directive (§16/4)." );
				
				} else return header_name_retry( in );
				return;
			
			} else if ( token.type == punct ) {
				if ( c == '/' ) {
					token.s += "//";
					state_after_space = line_comment;
					input_buffer.pop_back();
					goto pass_header_name;
				
				} else if ( c == '*' ) {
					token.type = space_run;
					token.s += "/*";
					input_buffer.pop_back();
					return;
				
				} else {
					auto slash( std::move( input_buffer.back() ) );
					input_buffer.pop_back();
					header_name_retry( slash ); // worst case: #include <xyz>/=
					return (*this)( in );
				}
			
			} else if ( token.type == space_run ) {
				if ( in_s != pp_char_source::ucn ) token.type = block_comment;
				token.s += c;
				
			} else if ( token.type == block_comment ) {
				if ( token.s.back() == '*' && c == '/' && in_s != pp_char_source::ucn ) {
					token.type = ws;
				
				} else if ( in_s == pp_char_source::ucn ) {
					token.type = space_run;
				}
				token.s += c;
			}
			return;
		}
	}
	
	void header_name_retry( pp_char const & in ) {
		token.type = state = state_after_space = ws;
		auto trailing_ws = std::move( token ); // Save any trailing space.
		token.s.clear();
		
		std::vector< pp_char > input_back;
		swap( input_back, input_buffer );
		for ( auto && p : input_back ) (*this)( p );
		
		if ( ! trailing_ws.s.empty() ) { // Restore saved space into a single token; locations of individual comments are lost.
			(*this)( { { ' ', std::move( trailing_ws ) }, pp_char_source::normal } );
			if ( this->get_config().preserve_space ) token = std::move( trailing_ws );
		}
		(*this)( in );
	}
	
public:
	template< typename ... args >
	lexer( args && ... a )
		: lexer::stage( std::forward< args >( a ) ... ),
		state( initial ), state_after_space( after_newline ), in_directive( false ) {}
	
	void operator() ( raw_char const &in ) {
		if ( char_in_set( * char_set::safe_chars[ state ], in.c ) ) {
			//++ fast_dispatch;
			input_buffer.clear();
			if ( state < space_run || this->get_config().preserve_space ) {
				unshift( in );
			}
		} else {
			//++ slow_dispatch;
			//++ slow_histo[ state ];
			general_path( in, pp_char_source::normal );
		}
	}
	
	void operator() ( pp_char const &in )
		{ general_path( in, in.s ); }
	
	void operator() ( lex_decode_state & s ) { // s must be initialized to "normal" or caller won't handle absence of this stage.
		switch ( state ) {
			case rstring: s = lex_decode_state::raw; break;
			case escape: s = lex_decode_state::escape; break;
		}
	}
	
	void operator() ( line_splice in ) {
		if ( ! this->get_config().preserve_space ) return;
		if ( state == line_comment || state == block_comment || state == space_run || state == after_newline || ( state == ws && ! token.s.empty() ) ) {
			token.s += "\\\n";
		} else if ( splice_count ++ == 0 ) {
			splice_first = std::move( in );
		}
	}
	
	void flush() {
		this->template diagnose< diagnose_policy::pass, error >(
			state == block_comment || ( state == header_name && ( token.type == block_comment || token.type == space_run ) ),
			token, "Unterminated comment." );
		this->template diagnose< diagnose_policy::fatal, error >(
			state == string_lit || state == char_lit || state == rstring,
			token, "Unterminated literal." );
		
		(*this)( '\n' );
		
		this->template diagnose< diagnose_policy::pass, error >(
			state != ws && state != after_newline && state != space_run,
			token, "ICE: Phase 3 terminated in unexpected state." ); // Pass whatever is left; this may result from later code modification or extension.
		
		if ( ! token.s.empty() ) pass();
	}
};

}

#endif
