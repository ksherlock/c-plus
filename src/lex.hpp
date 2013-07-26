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
		0xFF, 0xC7, 0xFF, 0xFF, // Exclude newline, vertical tab, form feed.
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
	int multibyte_count = 0; // How many unprocessed multibyte bytes are at end of token.
	
	std::vector< raw_char > input_buffer;
	string const *punct_lower, *punct_upper, *punct_match;
	std::function< void() > input_replay = []{}; // Used for header-name corner case.
	
	std::size_t rstring_term_start, rstring_seq_len; // offset, length of rstring termination sequence
	
	void shift( raw_char in ) { // Defer a UTF-8 byte, punctuator, or header-name character.
		input_buffer.push_back( std::move( in ) );
	}
	void accept( char_t in )
		{ token.s += in; }
	
	void multibyte_reject() noexcept {
		token.s.erase( token.s.end() - multibyte_count, token.s.end() );
		multibyte_count = 0;
	}
	std::string multibyte_retrieve() {
		std::string ret( token.s.end() - multibyte_count, token.s.end() );
		multibyte_reject();
		return ret;
	}
	
	void pass() {
		/*	Newline characters also override state_after_space, but to see a
			newline, state must equal ws. So it's handled in "case ws:". */
		if ( token.type != ws ) state_after_space = ws;
		CPLUS_FINALLY (
			token = {};
			token.type = state = state_after_space;
		)
		lexer::stage::pass( std::move( token ) );
		
		CPLUS_DO_FINALLY
	}
	
	void general_path( raw_char const &in ) {
		for (;;) switch ( state ) {
		case initial:
			state = after_newline;
			
		case ws: // every other state returns here after pass(), even if a space character hasn't been seen
			if ( splice_count != 0 ) { // Reinsert line splices seen during last token as whitespace, if preserve_space is set.
				token.construct::operator = ( std::move( splice_first ) );
				do token.s += "\\\n"; while ( -- splice_count );
			}
		case space_run:
			if ( in == '\n' || in == '\r' ) { // quietly allow CR on the assumption it precedes LF.
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
			} else if ( char_in_set( char_set::space, in ) && token.s.empty() ) {
				token.construct::operator = ( in );
				if ( ! this->get_config().preserve_space ) token.assign_content( pp_constants::space );
				state = space_run;
			}
		case after_newline:
			if ( char_in_set( char_set::space, in ) ) {
				if ( this->get_config().preserve_space ) {
					assert ( token.s.get_allocator() == this->get_config().stream_pool );
					accept( in );
				}
				this->template diagnose< diagnose_policy::pass, error >( in_directive && ( in == '\v' || in == '\f' ),
					in, "Only space and horizontal tab allowed in whitespace outside comments in a directive (§16/4)." );
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
				accept( in );
				
				if ( char_in_set( char_set::punct, in ) ) {
					if ( char_in_set( char_set::multipunct, in ) ) {
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
				} else if ( char_in_set( char_set::digit, in ) ) {
					token.type = state = num;
				} else if ( char_in_set( char_set::initial, in ) ) {
					token.type = state = id;
				} else if ( in == '\"' ) {
					token.type = state = string_lit;
				} else if ( in == '\'' ) {
					token.type = state = char_lit;
				} else { // "other", such as a stray backslash
					this->template diagnose< diagnose_policy::pass, error >( ( in <= 0x1F && ! char_in_set( char_set::space, in ) ) || in == 0x7F,
						in, "Stray control character (§2.3/2)." ); // Problem: valid range stopping at 0x80 presumes UTF-8.
					this->template diagnose< diagnose_policy::fatal, error >( in >= 0x80, in, "ICE: encoded bytes processed as ASCII." );
					token.type = misc;
					pass(); // chars not in alpha may still be catenated to an id or header-name
				}
				return;
			}
		
		case directive:
			token.type = id;
		case id: // Includes ud-suffix after initial character.
			if ( char_in_set( char_set::identifier, in ) ) {
				accept( in );
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
				} else if ( in == '"'
						&& std::binary_search( string_lit_prefixes, std::end( string_lit_prefixes ), token.s ) ) {
					if ( token.s.back() == 'R' ) {
						accept( in );
						state = rstring;
						token.type = string_lit;
						rstring_term_start = std::string::npos;
						return;
					} else {
						accept( in );
						token.type = state = string_lit;
						return;
					}
				} else if ( in == '\''
						&& std::binary_search( char_lit_prefixes, std::end( char_lit_prefixes ), token.s ) ) {
					token.type = state = char_lit;
					accept( in );
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
			if ( char_in_set( char_set::identifier, in ) || in == '.' ) {
				accept( in ); // Standard unclear about non-initial chars (combining diacritics) with numerals.
				if ( in == 'E' || in == 'e' ) state = exponent;
				return;
			} else {
				pass();
				continue;
			}
		
		case exponent:
			if ( in == '+' || in == '-' ) {
				accept( in );
				state = num;
				return;
			} else {
				state = num;
				continue;
			}
		
		case punct:
			if ( char_in_set( char_set::multipunct, in ) ) {
				accept( in ); // don't accept because the buffer is being used for punct
				
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
			} else if ( token.s == pp_constants::dot.s && char_in_set( char_set::digit, in ) ) {
				token.type = state = num;
				accept( in );
				return;
			
			} else punct_done: {
				finish_punct();
				return (*this)( std::move( in ) );
			}
		
		case block_comment:
			if ( this->get_config().preserve_space ) accept( in ); // no UTF-8 decoding in comments
			
			if ( ! input_buffer.empty() && in == '/' ) {
				/*	If we got here from /​* being a pseudo punctuator, looking for #, then
					this is still the beginning of the line, and continue looking for a #. */
				state = token.type == directive? (int) after_newline : space_run;
				token.type = ws;
			}
			input_buffer.clear();
			
			if ( in == '*' ) shift( in ); // advance to next sub-state
			return;
			
		case line_comment_check:
			this->template diagnose< diagnose_policy::pass, error >( ! char_in_set( char_set::space, in ),
				in, "Only space allowed between vertical tab or form feed and line comment terminating newline (§2.8/1)." );
		case line_comment:
			if ( in == '\n' ) {
				token.type = state = ws;
				continue;
			} else {
				if ( this->get_config().preserve_space ) accept( in );
				if ( in == '\v' || in == '\f' ) state = line_comment_check;
				return;
			}
			
		case rstring:
			accept( in );
			if ( rstring_term_start == std::string::npos ) {
				if ( in == '(' ) {
					rstring_term_start = 0; // kludge to advance sub-state
					return;
				}
				this->template diagnose< diagnose_policy::pass, error >(
					char_in_set( char_set::space, in ) || in == ')' || in == '\\' || ! char_in_set( char_set::basic_source, in ),
					in, "Raw string delimiter sequence must consist of alphanumeric characters and C-language punctuation except parens "
					"and backslash (§2.14.5)." );
				this->template diagnose< diagnose_policy::pass, error >( token.s.size() - token.s.find( '"' ) == 18,
					in, "Raw string delimiter sequence may contain at most 16 characters (§2.14.5/2)." );
				return;
			} else if ( in == ')' ) {
				rstring_term_start = token.s.size(); // termination begins at next char
				rstring_seq_len = 0;
				return;
			} else if ( rstring_term_start != 0 ) {
				// match the char to the corresponding sequence at beginning of the token
				std::uint8_t begin_seq_char = token.s[ token.s.find( '"' ) + 1 + rstring_seq_len ];
				if ( in == '"' && begin_seq_char == '(' ) {
					state = ud_suffix;
					return;
					
				} else if ( begin_seq_char != in ) {
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
			accept( in );
			switch ( in ) {
			case '\"':	if ( state == string_lit ) state = ud_suffix; return;
			case '\'':	if ( state == char_lit ) state = ud_suffix; return;
			case '\\':	state = escape; return;
			case '\n':
				this->template diagnose< diagnose_policy::pass, error >( true, in, "Use \\n instead of embedding a newline in a literal (§2.14.5)." ); return;
			default:	return;
			}
		
		// Don't map escape sequences yet, as that depends on execution charset.
		case escape:
			// But do *unmap* UCNs, since eg "\$" = "\\u0024" greedily matches the backslash escape first.
			state = static_cast< states >( token.type );
			if ( ! char_in_set( char_set::basic_source, in ) ) return unmap_ucn( in, in );
			else return accept( in );
		
		case ud_suffix: // This only checks the first char to see if a suffix exists.
			if ( char_in_set( char_set::initial, in ) ) {
				state = id;
				continue;
			} else {
				if ( ! token.s.empty() ) pass();
				token.type = state = ws;
				continue;
			}
		
			/*	A header-name ceases to be a header-name if it's followed by another token. So we must scan to the end of line.
				But the usual whitespace handling would pass the whitespace before the state machine could return to header_name.
				Shift the header-name characters, then scan whitespace into token until \n. If anything goes wrong, retokenize.
				Good: rescanned characters have proper locations. Bad: whitespace after an invalidated name is lost. */
		case header_name:
			if ( token.type == header_name ) {
				if ( in == '\n' ) return header_name_retry( in );
				
				else if ( input_buffer.empty() ) {
					if ( in != '<' && in != '\"' ) {
						goto redispatch; // state_after_space is already header_name
					}
				} else if ( ( input_buffer[0] == '<' && in == '>' )
						 || ( input_buffer[0] == '\"' && in == '\"' ) ) {
					token.type = state_after_space = ws;
				}
				shift( in );
				auto input_replay_head = std::move( input_replay );
				input_replay = [ this, in, input_replay_head ] { input_replay_head(); general_path( in ); };
				return;
				
			} else if ( token.type == ws ) {
				if ( in == '\n' ) pass_header_name: {
					input_replay = []{};
					auto trailing_ws = std::move( token.s ); // save any trailing space
					token.s.clear();
					
					token.type = header_name; // restore header name into token
					for ( auto && p : input_buffer ) token.s.push_back( p );
					input_buffer.clear(); // like accept() but doesn't include current char
					
					auto state_after_space_back = state_after_space; // schedule possible continuation to line comment mode
					pass(); // pass header name (and reset to whitespace mode)
					if ( this->get_config().preserve_space ) token.s = std::move( trailing_ws ); // restore saved space
					token.construct::operator = ( in ); // approximately locate the space at the newline or //
					token.type = ws;
					state = state_after_space_back; // continue to line comment mode if "//" sent us here
					
					continue;
				
				} else if ( in == '/' ) {
					shift( in );
					token.type = punct;
				
				} else if ( char_in_set( char_set::space, in ) ) {
					accept( in );
					this->template diagnose< diagnose_policy::pass, error >( in == '\v' || in == '\f',
						in, "Only space and horizontal tab allowed in whitespace outside comments in a directive (§16/4)." );
				
				} else return header_name_retry( in );
				return;
			
			} else if ( token.type == punct ) {
				if ( in == '/' ) {
					token.s += "//";
					state_after_space = line_comment;
					input_buffer.pop_back();
					goto pass_header_name;
				
				} else if ( in == '*' ) {
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
				token.type = block_comment;
				accept( in );
				
			} else if ( token.type == block_comment ) {
				if ( token.s.back() == '*' && in == '/' ) {
					token.type = ws;
				}
				accept( in );
			}
			return;
		}
	}
	
	void finish_punct() {
		int match_size;
		if ( ! punct_match ) {
			match_size = 1;
		} else if ( punct_match == less_scope ) {
			match_size = 1; // Edge case of §2.5/3: Lex < :: instead of <: : .
		} else if ( punct_match == alt_bracket_scope || punct_match == alt_brackets ) {
			match_size = 2; // But do not interfere with <: :> or <: :: which are valid.
		} else {
			match_size = punct_match->size();
		}
		token.s.resize( match_size ); // make token a copy of punct_match
		
		std::vector< raw_char > retry;
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
	}
	void header_name_retry() {
		state = state_after_space = ws;
		token = {};
		
		input_buffer.clear();
		input_replay();
		input_replay = []{};
	}
	void header_name_retry( raw_char const & in ) {
		header_name_retry();
		(*this)( in ); // Do not pass space previously stored in token.s inside a UTF-8 sequence.
	}
	
	void unmap_ucn( char32_t c, construct loc ) {
		(*this) ( raw_char( '\\', loc ) );
		int digits = c >= 0x10000? 8 : 4;
		(*this) ( raw_char( digits == 4? 'u' : 'U', loc ) );
		std::ostringstream s;
		s.width( digits );
		s.fill( '0' );
		s.setf( std::ios::hex, std::ios::basefield );
		s.setf( std::ios::uppercase );
		s << static_cast< std::uint32_t >( c );
		for ( char c : s.str() ) (*this) ( raw_char( c, loc ) );
	}
	
	lex_decode_state decode_state() {
		switch ( state ) {
		case escape:						return lex_decode_state::escape;
		case header_name:					if ( token.type == block_comment || token.type == space_run )
		case block_comment: case line_comment: case line_comment_check:
		case rstring:							return lex_decode_state::raw;
		default:							return lex_decode_state::normal;
		}
	}
	
public:
	template< typename ... args >
	lexer( args && ... a )
		: lexer::stage( std::forward< args >( a ) ... ),
		state( initial ), state_after_space( after_newline ), in_directive( false ) {}
	
	void operator() ( raw_char const &in ) {
		if ( char_in_set( * char_set::safe_chars[ state ], in ) ) {
			//++ fast_dispatch;
			input_buffer.clear();
			if ( state < space_run || this->get_config().preserve_space ) {
				accept( in );
			}
		} else {
			//++ slow_dispatch;
			//++ slow_histo[ state ];
			general_path( in );
		}
	}
	
	void operator () ( multibyte_char in )
		{ accept( in ); ++ multibyte_count; }
	
	void operator () ( raw_codepoint const & in ) {
		if ( multibyte_count == 1 ) {
			raw_char in_char( token.s.back(), in );
			multibyte_reject();
			return (*this) ( std::move( in_char ) );
		}
		CPLUS_FINALLY ( multibyte_reject(); ) // Set multibyte_count = 0 to accept the sequence.
		
		this->template diagnose< diagnose_policy::pass, error >( in <= 0x9F, in, "Stray control character (§2.3/2)." );
		this->template diagnose< diagnose_policy::pass, error >( in >= 0xD800 && in <= 0xDFFF, in,
			"This is a surrogate pair code point (§2.3/2). If specifying UTF-16, " // message assumes UTF-16 hasn't been encoded in UTF-8
			"encode the desired Unicode character and the pair will be generated. Otherwise, try a hexadecimal escape sequence \"\\xDnnn\"." );
		
		switch ( state ) {
		case id: case directive: case num: case exponent:
			if ( state == exponent ) state = num;
			if ( char_in_set( char_set::identifier, in ) ) goto accept;
		case ud_suffix:
			if ( char_in_set( char_set::initial, in ) ) goto accept;
			goto reinitialize;
			
		case string_lit: case char_lit: case rstring: accept:
			multibyte_count = 0;
			break;
			
		case escape:
			state = static_cast< int >( token.type );
			unmap_ucn( in, in );
			break;
			
		case block_comment: case line_comment: case line_comment_check: comment:
			if ( state == block_comment ) input_buffer.clear();
			this->template diagnose< diagnose_policy::pass, error >( state == line_comment_check, in, "Ugh." );
			if ( this->get_config().preserve_space ) multibyte_count = 0;
			break;
			
		case header_name: case punct: default: reinitialize:
			std::string multibyte_seq;
			
			if ( token.type == header_name ) { // All this handles only header names.
				for ( int i = 0; i != multibyte_count; ++ i ) {
					char_t byte = token.s[ token.s.size() - multibyte_count + i ];
					shift( { byte } );
					auto input_replay_head = std::move( input_replay );
					input_replay = [ this, byte, in, input_replay_head ] { input_replay_head(); (*this) ( multibyte_char{ byte, in } ); };
				}
				auto input_replay_head = std::move( input_replay );
				input_replay = [ this, in, input_replay_head ] { input_replay_head(); (*this) ( in ); };
				return;
			} else if ( token.type == block_comment ) {
				goto comment;
			} else if ( state == header_name ) {
				multibyte_seq = multibyte_retrieve();
				header_name_retry(); // Goes to punct state for <xyz> style include.
			}
			if ( multibyte_seq.empty() ) multibyte_seq = multibyte_retrieve();
			
			while ( state == punct ) finish_punct();
		
			if ( ! token.s.empty() ) pass();
			token = { char_in_set( char_set::initial, in )? token_type::id : token_type::misc, std::move( multibyte_seq ), in };
			if ( token.type == misc ) pass();
			else state = id;
		}
		
		CPLUS_DO_FINALLY
	}
	
	void operator () ( ucn in ) {
		auto prev_decode_state = decode_state();
		auto prev_state = state;
		(*this) ( static_cast< raw_codepoint const & >( in ) );
		
		this->template diagnose< diagnose_policy::pass, error >( prev_decode_state != lex_decode_state::normal,
			in, "ICE: Interpreted a UCN by accident." )
		|| this->template diagnose< diagnose_policy::pass, error >(
			char_in_set( char_set::basic_source, in ) && prev_state != string_lit && prev_state != char_lit,
			in, "Please do not encode basic source text in universal-character-names (§2.3/2)." );
	}
	
	void operator () ( multibyte_error && e ) {
		multibyte_reject();
		this->template diagnose< diagnose_policy::pass, multibyte_error >( true, std::move( e ) );
	}
	
	void operator () ( lex_decode_state & s ) // s must be initialized to "normal" or caller won't handle absence of this stage.
		{ s = decode_state(); }
	
	void operator () ( line_splice in ) {
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
		
		(*this)( raw_char( '\n' ) );
		
		this->template diagnose< diagnose_policy::pass, error >(
			state != ws && state != after_newline && state != space_run,
			token, "ICE: Phase 3 terminated in unexpected state." ); // Pass whatever is left; this may result from later code modification or extension.
		
		if ( ! token.s.empty() ) pass();
	}
};

}

#endif
