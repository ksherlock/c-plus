// Cplus project, translation phase 4: execute preprocessing directives and macros and the like.
// copyright David Krauss, created 8/29/11

#ifndef CPLUS_PHASE_4
#define CPLUS_PHASE_4

#include "macro.hpp"

#include <sstream>
#include <fstream>

namespace cplus {

std::string destringize( std::string &&in ) { // As described in §16.9. Also used for default-form headers.
	in = in.substr( in.find( '"' ) + 1, in.rfind( '"' ) - in.find( '"' ) - 1 ); // delete prefix AND suffix
	if ( ! in.empty() ) for ( std::string::size_type n = 0; n < in.size() - 1; ++ n ) {
		if ( in[ n ] == '\\' && ( in[ n+1 ] == '"' || in[ n+1 ] == '\\' ) ) in.erase( n, 1 );
	}
	return in;
}

template< typename output_iterator >
class pragma_filter { // Replaces pragma operator "calls" with destringized directive tokens.
	output_iterator cont;
public:
	enum { normal, open_paren, string, close_paren } state;
	
	template< typename ... args >
	pragma_filter( args && ... a )
		: cont( std::forward< args >( a ) ... ), state( normal ) {}
	
	void operator() ( pp_token &&in ) {
		if ( in.type == pp_token_type::ws ) return cont( std::move( in ) );
		
		switch ( state ) {
		case normal:		if ( in == pp_constants::pragma_operator ) state = open_paren;
							else cont( std::move( in ) );
							return;
							
		case open_paren:	state = string;
							if ( in != pp_constants::lparen )
								throw error( in.p, "_Pragma operand must be in parentheses (§16.9)." );
							return;
							
		case string:		state = close_paren;
							if ( in.type != pp_token_type::string )
								throw error( in.p, "_Pragma operand must be a string (§16.9)." );
							in.s = destringize( std::move( in.s ) );
							in.type = pp_token_type::directive;
							return cont( std::move( in ) ); // TODO: throw select pragmas to Phases 1-4
							
		case close_paren:	state = normal;
							if ( in != pp_constants::rparen )
								throw error( in.p, "Only one argument to _Pragma allowed (§16.9)." );
							return;
		}
	}
	friend void finalize( pragma_filter &o ) {
		if ( o.state != normal ) throw error( 0, "Unterminated _Pragma." );
		finalize( o.cont );
	}
};

template< typename output_iterator >
class phase4 : substitution_phase< util::output_iterator_from_functor< pragma_filter< output_iterator > > > {
	enum { normal, space, newline, directive, skip_if, skip_else } state;
	
	typedef substitution_phase< util::output_iterator_from_functor< pragma_filter< output_iterator > > > base_type;
	using base_type::input; // share state with macro context subobject
	using base_type::macros; // and macro substitution driver subobject
	
	std::size_t line_number, skip_depth, conditional_depth;
	
	pp_tokens process_macros( pp_tokens::iterator first, pp_tokens::iterator last ) {
		pp_tokens ret;
		
		macro_context< util::output_iterator_from_functor<
			macro_filter< std::back_insert_iterator< pp_tokens > > > > local( macros, this->callers, ret );
		while ( pp_constants::skip_space( first, last ) != last ) {
			local( std::move( * first ++ ) );
			if ( first != last && first->type == pp_token_type::ws ) local( std::move( * first ++ ) );
		}
		finalize( local );
		return ret;
	}
	
	void handle_directive() {
		using pp_constants::skip_space;
		
		pp_tokens::iterator pen = input.begin();
		if ( skip_space( pen, input.end() ) == input.end() ) {
			// null directive
			
		} else if ( * pen == pp_constants::define_directive ) {
			auto macro = normalize_macro_definition( std::move( input ) );
			auto definition = macros.insert( macro );
			if ( definition.second == false && * definition.first != macro )
				throw error( macro.second.front().p, "Definition does not match previous definition." );
			
		} else if ( * pen == pp_constants::undef_directive ) {
			if ( skip_space( ++ pen, input.end() ) == input.end() )
				throw error( input.back().p, "Expected an identifier before end of line." );
			if ( pen->type != pp_token_type::id )
				throw error( pen->p, "Expected an identifier." );
			if ( std::binary_search( pp_constants::reserved_macro_names,
				std::end( pp_constants::reserved_macro_names ), pen->s ) ) throw error( pen->p,
				"Invalid use of a reserved identifier (§17.6.4.3.1/2, 17.6.4.3.2/1)." );
			
			macros.erase( pen->s );
			
			if ( skip_space( ++ pen, input.end() ) != input.end() )
				throw error( input.back().p, "Expected end of line." );
		
		} else if ( * pen == pp_constants::error_directive ) {
			throw error( pen->p, "Error directive (§16.5)." );
		
		} else if ( * pen == pp_constants::pragma_directive ) { // convert #pragma to _Pragma()
			auto local( util::make_output_iterator( ref( * this ) ) );
			pp_tokens seq = { pp_constants::pragma_operator, pp_constants::lparen,
					{ pp_token_type::id, "__CPLUS_STRINGIZE__" }, pp_constants::lparen },
				term = { pp_constants::rparen, pp_constants::rparen };
			
			auto pos_back = input.back().p;
			skip_space( ++ pen, input.end() );
			std::move( pen, input.end(), std::back_inserter( seq ) );
			input.clear();
			
			state = normal;
			std::move( seq.begin(), seq.end(), local );
			
			if ( this->paren_depth != 1 ) // not infallible, but #pragma is implementation-defined anyway
				throw error( pos_back, "Parens must balance in #pragma." );
			std::move( term.begin(), term.end(), local ); // copy of const would be appropriate but crashes G++
		
		} else if ( * pen == pp_constants::else_directive || * pen == pp_constants::endif_directive
					|| ( * pen == pp_constants::elif_directive && state != skip_if ) ) {
			if ( conditional_depth == 0 ) throw error( pen->p, "This directive must terminate a conditional block." );
			-- conditional_depth;
			if ( * pen != pp_constants::endif_directive ) state = skip_else;
			
			if ( * pen != pp_constants::elif_directive && skip_space( ++ pen, input.end() ) != input.end() )
				throw error( pen->p, "This directive takes no arguments." );
			
		} else if ( * pen == pp_constants::if_directive || * pen == pp_constants::elif_directive
					|| * pen == pp_constants::ifdef_directive || * pen == pp_constants::ifndef_directive ) {
			
			// Normalize. "ifdef" => "defined", "ifndef " => "!defined", "if" and "elif" just get erased.
			if ( * pen == pp_constants::ifdef_directive || * pen == pp_constants::ifndef_directive ) {
				if ( * pen == pp_constants::ifndef_directive ) {
					* pen ++ = { pp_token_type::punct, "!" };
					if ( pen->type != pp_token_type::ws ) throw error( pen->p, "Expected space." ); // hah
				}
				* pen = pp_constants::defined_operator;
				if ( skip_space( ++ pen, input.end() )->type != pp_token_type::id
					|| skip_space( ++ pen, input.end() ) != input.end() ) throw error( pen->p,
					"#ifdef and #ifndef accept only a single identifier argument." );
			} else * pen = pp_constants::space;
			
			for ( pen = input.begin(); pen != input.end(); ++ pen ) { // evaluate defined( id ) operator
				if ( * pen == pp_constants::defined_operator ) {
					* pen = pp_constants::space;
					bool paren;
					if ( skip_space( ++ pen, input.end() ) == input.end() ) goto bad_defined_expr;
					paren = * pen == pp_constants::lparen;
					if ( paren && skip_space( ++ pen, input.end() ) == input.end() ) goto bad_defined_expr;
					if ( pen->type != pp_token_type::id ) goto bad_defined_expr;
					pen->type = pp_token_type::num;
					pen->s = macros.count( pen->s )? "1" : "0"; // perform the actual operation
					if ( paren && ( skip_space( ++ pen, input.end() ) == input.end()
						|| * pen != pp_constants::rparen ) ) bad_defined_expr:
						throw error( pen[ -1 ].p, "defined operator does not match \"defined <id>\" "
													"or \"defined ( <id> )\" (§16.1/1)." );
				}
			}
			input = process_macros( input.begin(), input.end() );
			
			for ( pen = input.begin(); pen != input.end(); ++ pen ) { // evaluate defined( id ) operator
				if ( pen->type == pp_token_type::id && pen->s != "true" && pen->s != "false" ) {
					pen->type = pp_token_type::num;
					pen->s = "0";
				}
			}
			
			if ( skip_space( pen = input.begin(), input.end() ) == input.end() )
				throw error( pen[ -1 ].p, "No controlling expression for conditional directive." );
			
			if ( * pen == pp_token{ pp_token_type::num, "0" } ) state = skip_if; // TODO actually evaluate.
			else {
				state = normal;
				++ conditional_depth;
			}
		} else if ( * pen == pp_constants::include_directive || * pen == pp_constants::line_directive ) {
			// Handle directives allowing macros in arguments. This cannot be the default (§16/6).
			
			input = process_macros( pen, input.end() );
			pen = input.begin();
			
			if ( * pen == pp_constants::include_directive ) {
				handle_header( pen );
			
			} else if ( * pen == pp_constants::line_directive ) {
				if ( skip_space( ++ pen, input.end() ) == input.end() )
					throw error( input.front().p, "Expected line number." );
				
				std::istringstream s( std::move( pen->s ) );
				s >> line_number;
				if ( ! s || ! s.ignore().eof() || line_number == 0 ) throw error( pen->p,
					"Line number must be a decimal literal between 1 and 2^31-1 (§16.4/3)." );
				
				if ( skip_space( ++ pen, input.end() ) != input.end() ) {
					if ( pen->type != pp_token_type::string || pen->s.front() != '"' || pen->s.back() != '"' )
						throw error( pen->p, "Filename must be a simple string (§16.4/4)." );
					
					macros[ "__FILE__" ][ 1 ] = std::move( * pen );
					
					if ( skip_space( ++ pen, input.end() ) != input.end() )
						throw error( pen->p, "Extra tokens after replacement filename." );
				}
			}
		} else throw error( pen->p, "Non-directive." );
		
		input.clear();
	}
	
	void handle_header( pp_tokens::iterator pen ) {
		using pp_constants::skip_space;
		
		if ( skip_space( ++ pen, input.end() ) == input.end() )
			throw error( input.front().p, "Expected header name." );
		
		std::string name;
		if ( pen->type == pp_token_type::header_name ) name = pen->s;
		
		else if ( pen->type == pp_token_type::string ) {
			name = std::string( "\"" ) + destringize( std::move( pen->s ) ) + "\"";
		
		} else {
			auto end = input.rbegin();
			skip_space( end, decltype( end )( pen ) ); // skip trailing space
			while ( pen != end.base() ) {
				name += pen ++ ->s;
			}
			-- pen;
			if ( name.front() != '<' || name.back() != '>' ) throw error( pen->p,
				"Expected a sequence of tokens between \"<\" and \">\"." );
		}
		
		if ( skip_space( ++ pen, input.end() ) != input.end() ) throw error( input.back().p,
			"Extra tokens after header name." );
		
		bool use_user_paths = name.front() == '"';
		name = name.substr( 1, name.size() - 2 );
		if ( name.empty() ) throw error( pen->p, "Empty header name." );
		
		std::filebuf header;
		if ( name[0] == '/' ) { // it looks like an absolute path. TODO, deal with other path separators
			if ( ! header.open( name, std::ios::in | std::ios::binary ) ) {
				name.erase( 0, 1 );
			}
		}
		if ( ! header.is_open() ) { // before the path set, look in same directory as current file
			std::string file_relative = macros[ "__FILE__" ][ 1 ].s;
			std::size_t pathlen = file_relative.rfind( '/' );
			if ( pathlen == std::string::npos ) pathlen = 1; // file directory = working directory
			file_relative = file_relative.substr( 1, pathlen - 1 );
			file_relative += name;
			
			header.open( file_relative, std::ios::in | std::ios::binary );
		}
		if ( ! header.is_open() ) {
			std::string const path_sets[] = { config::search_paths_user, config::search_paths_system };
			for ( auto path_set = use_user_paths? path_sets : path_sets + 1;
				path_set != std::end( path_sets ); ++ path_set ) {
				for ( auto const &path : macros[ * path_set ] ) {
					if ( path.type == pp_token_type::ws ) continue;
					if ( path.type != pp_token_type::string || path.s.front() != '"' || path.s.back() != '"' )
						throw error( pen->p, "Compiler driver error: invalid pathname in header search set." );
					
					if ( header.open( path.s.substr( 1, path.s.size() - 2 ) + name,
						std::ios::in | std::ios::binary ) ) goto opened;
				}
			}
			throw error( pen->p, "File not found." );
		}
	opened:
		
		auto context_state = std::tie( macros[ "__FILE__" ][ 1 ].s, line_number );
		std::tuple< std::string, std::size_t > context_backup = context_state;
		
		name.insert( 0, 1, '"' ); // do a simple stringize, not converting UCNs (preserve meaning, not spelling)
		for ( std::size_t n = 1; n < name.size(); ++ n ) {
			if ( name[ n ] == '\\' || name[ n ] == '"' ) {
				name.insert( n ++, 1, '\\' );
			}
		}
		name += '"';
		context_state = std::make_tuple( name, 1 );
		
		// Connect a new lexer to self and process the file. this->operator() will be called recursively.
		util::output_iterator_from_functor< phase1_2<
			util::output_iterator_from_functor< phase3<
			util::output_iterator_from_functor< std::reference_wrapper< phase4 > >
			> > > > local( * this );
		
		input.clear();
		std::copy( std::istreambuf_iterator< char >( & header ), std::istreambuf_iterator< char >(),
					ref( local ) ); // not sure ref is needed, G++ wants to use forwarding ctor as copy
		finalize( local );
		
		context_state = context_backup;
		state = newline;
	}
	
public:
	template< typename ... args >
	phase4( args && ... a )
		: base_type( {
			{ "__CPLUS_PATHS_USER__", { pp_constants::space } },
			{ "__CPLUS_PATHS_SYSTEM__", { pp_constants::space } },
			{ "__DATE__", { pp_constants::space } },
			{ "__FILE__", { pp_constants::space, pp_constants::empty_string } },
			{ "__TIME__", { pp_constants::space } },
			{ "__CPLUS_STRINGIZE__", { pp_constants::variadic, pp_constants::rparen,
										pp_constants::stringize, pp_constants::variadic } } },
			std::forward< args >( a ) ... ),
		state( normal ), line_number( 0 ) {
		
		char time_str_buf[ 26 ], *time_cstr = time_str_buf;
		std::time_t time_scalar = std::time( nullptr );
		util::ctime( &time_scalar, time_cstr );
		std::string time_str( time_cstr );
		
		macros[ "__TIME__" ].push_back( { pp_token_type::string,
			std::string( "\"" ) + time_str.substr( 11, 8 ) + "\"" } );
		
		time_str.erase( 11, 9 );
		macros[ "__DATE__" ].push_back( { pp_token_type::string,
			std::string( "\"" ) + time_str.substr( 4, 11 ) + "\"" } );
	}
	
	phase4( phase4 && ) = delete;
	phase4( phase4 const & ) = delete;
	
	void operator()( pp_token &&in ) {
		using pp_constants::skip_space;
		
		switch ( state ) {
		case space: got_space:
		case newline:
			if ( in.type == pp_token_type::ws ) {
				if ( in.s[ 0 ] == '\n' ) {
					state = newline;
					line_number += in.s.size();
				}
				return; // condense whitespace
			}
			if ( state == newline ) {
				macros[ "__LINE__" ] = { pp_constants::space, {
					pp_token_type::num,
					static_cast< std::ostringstream & >( std::ostringstream() << line_number ).str(),
					in.p + in.s.size() // fake line number is positioned at beginning of line
				} };
			}
			base_type::operator()( pp_constants::space );
			state = normal; // fallthrough:
		case normal:
			if ( in.type == pp_token_type::directive ) { // consume and discard the #
				if ( this->paren_depth )
					throw error( in.p, "A macro invokation cannot span a directive (§16.3/11)." );
				this->flush(); // clear macro processing state
				
				state = directive;
				return;
			}
			if ( in.type == pp_token_type::ws ) {
				state = space;
				goto got_space;
			}
			base_type::operator()( std::move( in ) );
			return;
		
		case directive:
			if ( in.s[ 0 ] == '\n' ) {
				handle_directive();
				if ( state == skip_if || state == skip_else ) goto count_skipped_lines;
				else goto got_space;
			} else {
				input.push_back( std::move( in ) );
				return;
			}
		
		case skip_if:
		case skip_else:
			if ( in.s[0] == '\n' ) count_skipped_lines:
				line_number += in.s.size(); // count lines while waiting
			
			else if ( ( in.type == pp_token_type::directive && in.s != "#" ) || ! input.empty() ) {
				input.push_back( std::move( in ) ); // collect a directive
				return;
			}
			if ( input.empty() ) return; // finished if no directive yet
			
			if ( input.front() == pp_constants::if_directive || input.front() == pp_constants::ifdef_directive
				|| input.front() == pp_constants::ifndef_directive ) {
				++ skip_depth;
			
			} else if ( skip_depth == 0 ) {
				if ( input.front() == pp_constants::endif_directive
					|| ( input.front() == pp_constants::else_directive && state == skip_if ) ) {
					if ( input.front() == pp_constants::else_directive ) ++ conditional_depth;
					state = newline;
				
				} else if ( input.front() == pp_constants::elif_directive && state == skip_if ) {
					handle_directive();
				}
			} else if ( input.front() == pp_constants::endif_directive ) {
				-- skip_depth;
			}
			input.clear();
			return;
		}
	}
	
	friend void finalize( phase4 &o ) {
		if ( o.state == directive ) throw error( o.input.front().p, "Unterminated directive." );
		
		finalize( static_cast< base_type & >( o ) );
	}
};

}

#endif
