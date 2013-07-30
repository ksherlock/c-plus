// Cplus project, translation phase 4: execute preprocessing directives and macros and the like.
// copyright David Krauss, created 8/29/11

#ifndef CPLUS_PREPROCESS_H
#define CPLUS_PREPROCESS_H

#include "macro.hpp"

#include <sstream>
#include <stack>

namespace cplus {

namespace pp_constants {
	token const stringize_macro{ token_type::id, "#" }; // Use illegal identifier for internal #pragma support macro.
	tokens const guard_default{ zero }; // An empty header file is unconditionally guarded.
}

inline std::string destringize( std::string in ) { // As described in §16.9. Also used for default-form headers.
	in = in.substr( in.find( '"' ) + 1, in.rfind( '"' ) - in.find( '"' ) - 1 ); // delete prefix AND suffix
	if ( ! in.empty() ) for ( std::string::size_type n = 0; n < in.size() - 1; ++ n ) {
		if ( in[ n ] == '\\' && ( in[ n+1 ] == '"' || in[ n+1 ] == '\\' ) ) in.erase( n, 1 );
	}
	return in;
}

inline uintmax_t rudimentary_evaluate( tokens::iterator &pen, int min_precedence = 0 ) {
	using namespace pp_constants;
	typedef uintmax_t eval_t; // need a discriminated union of intmax_t and uintmax_t
	typedef std::map< string, std::pair< eval_t (*)( eval_t, eval_t ), int > > op_map;
	static op_map binary_operators {
	#define SIMPLE_OP( op, prec ) { # op, { []( eval_t l, eval_t r ){ return static_cast< eval_t >( l op r ); }, prec } }
	#define DIVISION_OP( op, prec ) { # op, { []( eval_t l, eval_t r ){ return r == 0 ? throw std::domain_error( "Division by zero." ) : l op r; }, prec } }
		SIMPLE_OP( *, 10 ), DIVISION_OP( /, 10 ), DIVISION_OP( %, 10 ),
		SIMPLE_OP( +, 9 ), SIMPLE_OP( -, 9 ),
		SIMPLE_OP( <<, 8 ), SIMPLE_OP( >>, 8 ),
		SIMPLE_OP( <=, 7 ), SIMPLE_OP( >=, 7 ), SIMPLE_OP( <, 7 ), SIMPLE_OP( >, 7 ),
		SIMPLE_OP( ==, 6 ), SIMPLE_OP( !=, 6 ), SIMPLE_OP( not_eq, 6 ),
		SIMPLE_OP( &, 5 ), SIMPLE_OP( bitand, 5 ),
		SIMPLE_OP( ^, 4 ), SIMPLE_OP( xor, 4 ),
		SIMPLE_OP( |, 3 ), SIMPLE_OP( bitor, 3 ),
		SIMPLE_OP( &&, 2 ), SIMPLE_OP( and, 2 ),
		SIMPLE_OP( ||, 1 ), SIMPLE_OP( or, 1 )
	#undef SIMPLE_OP
	#undef DIVISION_OP
	};
	static std::map< string, eval_t (*)( eval_t ) > unary_operators {
	#define SIMPLE_OP( op ) { # op, []( eval_t v ){ return static_cast< eval_t >( op v ); } }
		SIMPLE_OP( + ), SIMPLE_OP( - ), SIMPLE_OP( ! ), SIMPLE_OP( ~ ),
		SIMPLE_OP( not ), SIMPLE_OP( compl )
	#undef SIMPLE_OP
	};
	
	auto begin = pen;
	while ( unary_operators.count( pen->s ) ) ++ pen;
	auto prefix_end = pen;
	
	eval_t acc;
	if ( * pen == lparen ) {
		acc = rudimentary_evaluate( ++ pen );
		if ( * pen != rparen ) throw error( * pen, "Expected \")\"." );
	} else if ( pen->type == token_type::char_lit ) { // Very poor, but compliant, QOI. Wait for std::wstring_convert.
		acc = pen->s[ pen->s.find( '\'' ) + 1 ]; // Just use first byte of UTF-8 or multichar literal. Incorrectly gets backslash of escape seq.
	} else if ( pen->type == token_type::num ) {
		std::istringstream s( pen->s );
		s.unsetf( std::ios::basefield ); // Enable hex and octal notations.
		s >> acc;
		
		bool got_u = false, got_l = false;
		for ( int ch; ( ch = s.get() ) != std::istringstream::traits_type::eof(); ) switch ( ch ) {
		case 'l': case 'L':
			if ( got_l ) goto bad_num;
			got_l = true;
			if ( s.peek() == ch ) s.ignore();
			break;
		case 'u': case 'U':
			if ( got_u ) goto bad_num;
			got_u = true;
			break;
		default: bad_num:
			throw error( * pen, "Not an integer." );
		}
		
	} else if ( * pen == true_value || * pen == false_value ) {
		acc = * pen == true_value;
	} else throw error( * pen, "Expected a number." );
	
	while ( prefix_end != begin ) acc = unary_operators[ ( -- prefix_end ) ->s ]( acc );
	
	++ pen;
	while ( pen->s != "$" && * pen != rparen && * pen != colon ) {
		auto op = binary_operators.find( pen->s );
		if ( * pen == conditional ) {
			auto middle = rudimentary_evaluate( ++ pen );
			if ( * pen != colon ) throw error( * pen, "Expected \":\"." );
			auto end = rudimentary_evaluate( ++ pen ); // goes to end of expression
			acc = acc? middle : end; // This is the final result; loop terminates now.
		} else if ( op != binary_operators.end() ) {
			int prec = std::get< 1 >( op->second );
			if ( prec < min_precedence ) return acc;
			token const & op_token = * pen;
			try { acc = std::get< 0 >( op->second )( acc, rudimentary_evaluate( ++ pen, prec + 1 ) ); }
			catch ( std::domain_error &e ) { throw error( op_token, e.what() ); }
		} else throw error( * pen, "Expected an operator." );
	}
	return acc;
}

template< typename output_iterator >
class preprocessor
	: public derived_stage< substitution_phase< output_iterator >, preprocessor_config, lexer_config > {
	typedef typename preprocessor::derived_stage derived_stage;
	
	using derived_stage::input; // share state with macro context subobject
	using derived_stage::macros; // and macro substitution driver subobject
	
	enum { entering, normal, skip_if, skip_else } state;
	bool directive = false;
	
	unsigned skip_depth, conditional_depth;
	std::stack< bool, std::vector< bool > > else_stack; // which levels of conditional compilation are in #else groups
	
	struct guard_detector {
		bool valid;
		string name;
		tokens guard;
		std::size_t depth; // overall conditional depth
	} guard_detect;
	std::map< string, tokens const > guarded_headers; // from header (initial) __FILE__ string to guard expression
	
	tokens process_macros( tokens::iterator first, tokens::iterator last, bool preserve_defined_operator = false ) {
		tokens ret;
		
		// Make "defined" a macro which prevents expansion of its single argument. Imitate GCC's extension, manual §4.2.3.
		typename preprocessor::macro_context_info::name_map::iterator preserver_it;
		preserve_defined_operator = preserve_defined_operator && ! macros.count( pp_constants::defined_operator.s ); // If user-defined macro already exists, use that instead, and don't undefine it.
		if ( preserve_defined_operator ) { // #define defined(x)defined(x##<recursion stop><recursion stop>)
			using namespace pp_constants; // Catenating first recursion stop ensures that no space precedes second.
			preserver_it = this->insert_macro_definition( tokens{ define_directive, defined_operator, lparen, { token_type::id, "x" },
				rparen, defined_operator, lparen, { token_type::id, "x" }, concat, recursion_stop, recursion_stop, rparen }, this->get_config().macro_pool );
		}
		CPLUS_FINALLY (
			if ( preserve_defined_operator ) macros.erase( preserver_it ); // #undef defined
		)
		
		auto filter = pile< macro_filter >( [ &ret ]( token && in ) { ret.push_back( std::move( in ) ); } );
		pile< macro_context >(
			static_cast< macro_context_info & >( * this ),
			util::function< void( token && ), void( error && ) >( filter.template pass_function< token && >(), this->template pass_function< error && >() )
		).pass( std::make_move_iterator( first ), std::make_move_iterator( last ) );
		
		CPLUS_DO_FINALLY
		return ret;
	}
	
	void validate_else( token const &directive ) {
		this->template diagnose< diagnose_policy::fatal, error >(
			( directive == pp_constants::else_directive || directive == pp_constants::elif_directive ) && else_stack.top() == true,
			directive, "An #else or #elif group cannot follow another #else group." );
		else_stack.top() = directive == pp_constants::else_directive;
	}
	
	void handle_directive() {
		using pp_constants::skip_space;
		
		input.front().assign_content( pp_constants::placemarker ); // Clear the redundant # token.
		tokens::iterator pen = input.begin() + 1;
		skip_space( pen, input.end() );
		
		bool is_include = false; // handle_header does its own input.clear() before entering the header.
		CPLUS_FINALLY ( if ( ! is_include ) input.clear(); )
		
		if ( pen != input.end() && ( ( conditional_depth == guard_detect.depth // at file top level,
				&& * pen != pp_constants::ifndef_directive && * pen != pp_constants::if_directive ) // any wrong directive
			|| ( conditional_depth == guard_detect.depth + 1 // or an alternative to the guard
				&& ( * pen == pp_constants::elif_directive || * pen == pp_constants::else_directive ) ) ) ) {
			guard_detect.valid = false; // cancels guard detection
		}
		
		if ( pen == input.end() ) {
			// null directive
			
		} else if ( * pen == pp_constants::define_directive ) {
			this->insert_macro_definition( std::move( input ), this->get_config().macro_pool );
			
		} else if ( * pen == pp_constants::undef_directive ) {
			do this->template diagnose< diagnose_policy::fatal, error >( skip_space( ++ pen, input.end() ) == input.end(),
				input.back(), "Expected an identifier before end of line." );
			while ( this->template diagnose< diagnose_policy::pass, error >( pen->type != token_type::id,
				* pen, "Expected an identifier." ) );
			this->template diagnose< diagnose_policy::fatal, error >( pen->s == pp_constants::file_macro.s || pen->s == pp_constants::line_macro.s,
				* pen, "Cannot #undef this macro (§16.8/4)." );
			
			macros.erase( pen->s );
			
			this->template diagnose< diagnose_policy::pass, error >( skip_space( ++ pen, input.end() ) != input.end(),
				input.back(), "Expected end of line." );
			
		} else if ( this->template diagnose< diagnose_policy::pass, error >( * pen == pp_constants::error_directive,
			* pen, "Error directive (§16.5)." ) ) ; // Rely on driver to show actual message.
		
		else if ( * pen == pp_constants::pragma_directive ) { // convert #pragma to _Pragma()
			tokens arg;
			arg.swap( input );
			
			tokens intro = { pp_constants::pragma_operator, pp_constants::lparen, pp_constants::stringize_macro, pp_constants::lparen };
			this->pass( std::make_move_iterator( intro.begin() ), std::make_move_iterator( intro.end() ) );
			this->pass( std::make_move_iterator( ++ pen ), std::make_move_iterator( arg.end() ) ); // Swapping container preserves iterators.
			
			this->template diagnose< diagnose_policy::pass, error >( this->paren_depth != 1, // Not infallible, but #pragma is implementation-defined anyway.
				arg.back(), "Parens must balance in #pragma." );
			while ( this->paren_depth != 0 ) {
				this->pass( util::val( pp_constants::rparen ) ); // Close the stringize macro (and whatever the user left unbalanced).
			}
			this->pass( util::val( pp_constants::rparen ) ); // Terminate the _Pragma operator which is not a macro.
			
		} else if ( * pen == pp_constants::else_directive || * pen == pp_constants::endif_directive
					|| ( * pen == pp_constants::elif_directive && state != skip_if ) ) {
			this->template diagnose< diagnose_policy::fatal, error >( conditional_depth == 0, * pen, "This directive must terminate a conditional block." );
			-- conditional_depth;
			if ( * pen == pp_constants::endif_directive ) else_stack.pop();
			else state = skip_else;
			
			validate_else( * pen );
			
			this->template diagnose< diagnose_policy::pass, error >( * pen != pp_constants::elif_directive && skip_space( ++ pen, input.end() ) != input.end(),
				util::make_implicit_thunk( [&]{ return * pen; } ), "This directive takes no arguments." );
			
		} else if ( * pen == pp_constants::if_directive || * pen == pp_constants::elif_directive
					|| * pen == pp_constants::ifdef_directive || * pen == pp_constants::ifndef_directive ) {
			bool condition = false;
			CPLUS_FINALLY (
				if ( condition ) {
					if ( state == skip_if ) state = entering;
					++ conditional_depth;
				} else state = skip_if;
				else_stack.push( false ); // Not strictly exception-safe, but this only allocates one bit.
				if ( std::uncaught_exception() && guard_detect.depth == conditional_depth ) guard_detect.valid = false;
			)
			
			// 1st pass: normalize. "ifdef" => "defined", "ifndef " => "!defined", "if" and "elif" just get erased.
			if ( * pen == pp_constants::ifdef_directive || * pen == pp_constants::ifndef_directive ) {
				if ( * pen == pp_constants::ifndef_directive ) {
					* pen ++ = pp_constants::bang;
					this->template diagnose< diagnose_policy::pass, error >( pen->type != token_type::ws, * pen, "Expected space." ); // hah
				}
				* pen = pp_constants::defined_operator;
				do this->template diagnose< diagnose_policy::fatal, error >( skip_space( ++ pen, input.end() ) == input.end(),
					input.back(), "Expected an identifier before end of line." );
				while ( this->template diagnose< diagnose_policy::pass, error >( pen->type != token_type::id,
					* pen, "Expected an identifier." ) && ( * pen = pp_constants::space, true ) );
				
				if ( this->template diagnose< diagnose_policy::pass, error >( skip_space( ++ pen, input.end() ) != input.end(),
					util::make_implicit_thunk( [&]{ return * pen; } ), "#ifdef and #ifndef accept only a single identifier argument." ) ) {
					std::fill( pen, input.end(), pp_constants::space );
				}
			} else * pen = pp_constants::space;
			
			condition = evaluate_condition( input.begin(), input.end() );
			
			if ( guard_detect.valid && guard_detect.depth == conditional_depth ) {
				if ( guard_detect.guard == pp_constants::guard_default ) {
					guard_detect.guard.assign( std::make_move_iterator( input.begin() + 1 ), std::make_move_iterator( input.end() ) );
					for ( auto & t : guard_detect.guard ) t = t.reallocate( this->get_config().macro_pool );
				} else {
					guard_detect.valid = false;
				}
			}
			CPLUS_DO_FINALLY
			
		} else if ( * pen == pp_constants::include_directive || * pen == pp_constants::line_directive ) {
			// Handle directives allowing macros in arguments. This cannot be the default (§16/6).
			input = process_macros( pen, input.end() );
			pen = input.begin();
			
			if ( * pen == pp_constants::include_directive ) {
				is_include = true;
				handle_header( header_access_from_input( pen ) ); // flushes directive input and obtains new input
			
			} else if ( * pen == pp_constants::line_directive ) {
				std::ptrdiff_t physical_line = this->line_number( * pen ); // signed to prevent overflow below
				
				this->template diagnose< diagnose_policy::fatal, error >( skip_space( ++ pen, input.end() ) == input.end(),
					input.back(), "Expected line number." );
				
				std::istringstream s( pen->s ); // decimal-only is default
				std::int32_t line_number = 0;
				this->template diagnose< diagnose_policy::pass, error >( ! ( s >> line_number ) || ! s.ignore().eof(),
					* pen, "Line number must be a decimal literal between 1 and 2^31-1 (§16.4/3)." );
				
				if ( line_number != 0 ) { // User may set filename only using #line 0 "name".
					this->presumed.line_displacement = line_number - physical_line - 1;
					state = entering; // print new line number
				}
				if ( skip_space( ++ pen, input.end() ) != input.end() ) {
					this->template diagnose< diagnose_policy::fatal, error >( pen->type != token_type::string_lit || pen->s.front() != '"' || pen->s.back() != '"',
						* pen, "Filename must be a simple string (§16.4/4)." );
					
					this->presumed.filename = std::move( * pen );
					state = entering; // print new file name
					
					this->template diagnose< diagnose_policy::pass, error >( skip_space( ++ pen, input.end() ) != input.end(),
						util::make_implicit_thunk( [&]{ return * pen; } ), "Extra tokens after replacement filename." );
				}
			}
		} else {
			this->template diagnose< diagnose_policy::pass, error >( true, * pen, "Non-directive." );
		}
		
		CPLUS_DO_FINALLY // Clear input buffer.
	}
	
	bool evaluate_condition( tokens::const_iterator first, tokens::const_iterator last ) {
		tokens expr( first, last );
		tokens::iterator pen;
		
		replace_defined_operator( expr );
		expr = process_macros( expr.begin(), expr.end(), true ); // 3rd pass: macro replacement.
		replace_defined_operator( expr ); // for compatibility with GCC, Apple headers
		
		for ( pen = expr.begin(); pen != expr.end(); ++ pen ) { // 4th pass: squash identifiers.
			if ( pen->type == token_type::id && pen->s != "true" && pen->s != "false" ) {
				pen->assign_content( pp_constants::zero );
			}
		}
		
		if ( this->template diagnose< diagnose_policy::pass, error >( pp_constants::skip_space( pen = expr.begin(), expr.end() ) == expr.end(),
			util::make_implicit_thunk( [&]{ return pen[ -1 ]; } ), "No controlling expression for conditional directive." ) ) return true; // Just take a guess ;v) .
		
		expr.erase( std::remove_if( expr.begin(), expr.end(),
									[]( token const &t ){ return t.type == token_type::ws; } ), expr.end() );
		expr.push_back( { token_type::punct, "$", util::val< construct const & >( expr.back() ) } );
		
		pen = expr.begin(); // 5th pass (!): actually evaluate.
		bool result = rudimentary_evaluate( pen );
		this->template diagnose< diagnose_policy::pass, error >( pen != expr.end() - 1, * pen, "Expected end of expression." );
		return result;
	}
	void replace_defined_operator( tokens &expr ) {
		for ( auto pen = expr.begin(); pen != expr.end(); ++ pen ) { // 2nd pass: evaluate defined( id ) operator.
			if ( * pen == pp_constants::defined_operator ) {
				tokens::iterator defined_it = pen, arg_it;
				
				if ( this->template diagnose< diagnose_policy::pass, error >(
					! ( ( arg_it = pp_constants::skip_space( ++ pen, expr.end() ) ) != expr.end() && arg_it->type == token_type::id ) // defined x
					&& ! ( * pen == pp_constants::lparen // defined ( x )
						&& ( arg_it = pp_constants::skip_space( ++ pen, expr.end() ) ) != expr.end() && arg_it->type == token_type::id
						&& pp_constants::skip_space( ++ pen, expr.end() ) != expr.end() && * pen == pp_constants::rparen
					),
					util::make_implicit_thunk( [&]{ return pen == expr.end()? expr.back() : * pen; } ),
					"defined operator does not match \"defined <identifier>\" or \"defined ( <identifier> )\" (§16.1/1, 4)." ) )
					continue; // Let subsequent pass macro-replace it or clobber to 0.
				
				defined_it->assign_content( macros.count( arg_it->s )? pp_constants::one : pp_constants::zero ); // Exception safety: copies a string.
				expr.erase( defined_it + 1, pen + 1 );
				pen = defined_it;
			}
		}
	}
	
	struct header_access { token source; std::string path; token presumed; };
	header_access header_access_from_input( tokens::iterator pen ) {
		CPLUS_FINALLY ( input.clear(); )
		using pp_constants::skip_space;
		
		this->template diagnose< diagnose_policy::fatal, error >( skip_space( ++ pen, input.end() ) == input.end(),
			input.back(), "Expected header name." );
		
		std::string name;
		if ( pen->type == token_type::header_name ) name = pen->s;
		
		else if ( pen->type == token_type::string_lit ) {
			name = std::string( "\"" ) + destringize( std::move( pen->s ) ) + "\"";
		
		} else {
			auto end = input.rbegin();
			skip_space( end, decltype( end )( pen ) ); // skip trailing space
			auto name_begin = pen;
			for ( ; pen != end.base(); ++ pen ) {
				if ( pen->type != token_type::ws ) name += pen->s;
				else if ( token_semantic_equal( * pen, pp_constants::space ) ) name += ' ';
			}
			while ( name.back() == ' ' ) {
				name.pop_back();
			}
			pen = name_begin + 1;
			this->template diagnose< diagnose_policy::fatal, error >( name.front() != '<' || name.back() != '>' || name.size() == 2, * pen,
				"Expected a sequence of tokens between \"<\" and \">\"." );
		}
		
		bool use_user_paths = name.front() == '"';
		name = name.substr( 1, name.size() - 2 );
		this->template diagnose< diagnose_policy::fatal, error >( name.empty(), * pen, "Empty header name." );
		
		std::filebuf header;
		if ( name[0] == '/' ) { // it looks like an absolute path. TODO, deal with other path separators
			if ( header.open( name.c_str(), std::ios::in | std::ios::binary ) ) goto opened;
			name.erase( 0, 1 );
		}
		if ( ! header.is_open() ) { // before the path set, look in same directory as current file
			std::string path = this->presumed.filename.s; // TODO use backup of original path, not affected by #line
			std::size_t pathlen = path.rfind( '/' );
			if ( pathlen == std::string::npos ) pathlen = 1; // file directory = working directory
			path = path.substr( 1, pathlen - 1 ) + name;
			
			if ( header.open( path.c_str(), std::ios::in | std::ios::binary ) ) {
				name = path;
			}
		}
		if ( ! header.is_open() ) {
			std::vector< string > const *path_sets[] = { & this->get_config().user_paths, & this->get_config().system_paths };
			for ( auto path_set = use_user_paths? & path_sets[0] : & path_sets[1];
				! this->template diagnose< diagnose_policy::fatal, error >(
					path_set == std::end( path_sets ), * pen, "File not found." );
				++ path_set ) {
				for ( std::string path : ** path_set ) {
					path = path + name;
					if ( header.open( path.c_str(), std::ios::in | std::ios::binary ) ) {
						name = path;
						goto opened;
					}
				}
			}
		}
	opened:
		header_access ret {
			pen->reallocate( this->get_config().macro_pool ), // Source token.
			std::move( name ), // Access path.
			{ token_type::string_lit, { "\"", this->get_config().macro_pool } } // Stringized presumed __FILE__ name, filled in below.
		};
		ret.presumed.construct::operator = ( ret.source );
		for ( char c : ret.path ) {
			if ( c == '\\' || c == '"' ) ret.presumed.s += '\\';
			ret.presumed.s += c;
		}
		ret.presumed.s += '"';
		
		this->template diagnose< diagnose_policy::pass, error >( ( pen->type == token_type::string_lit || pen->type == token_type::header_name )
			&& skip_space( ++ pen, input.end() ) != input.end(), input.back(), "Extra tokens after header name." );
		
		CPLUS_DO_FINALLY // Clear input buffer.
		return ret;
	}
	
	void handle_header( header_access access ) {
		auto guard_it = guarded_headers.find( access.presumed.s );
		if ( guard_it != guarded_headers.end()
			&& this->template diagnose< diagnose_policy::pass, error >(
				evaluate_condition( guard_it->second.begin(), guard_it->second.end() ), // Use condition result for flow control.
				access.source, "Warning: multiply including guarded header" ) )
			return;
		
		auto context_backup = std::move( this->presumed );
		auto guard_detect_backup = std::move( guard_detect );
		CPLUS_FINALLY(
			this->presumed = std::move( context_backup ); // Restore __FILE__, __LINE__, and guard detection of calling header.
			guard_detect = std::move( guard_detect_backup );
			
			if ( state == normal ) state = entering; // Re-enter calling header.
		)
		this->presumed = macro_context_info::presumptions{ std::move( access.presumed ) };
		guard_detect = { true, this->presumed.filename.s, pp_constants::guard_default, conditional_depth };
		
		state = entering;
		instantiate( std::make_shared< inclusion >( std::move( access.path ), std::move( access.source ) ),
			pile< char_decoder, lexer >( * this ) );
		preprocessor::macro_context::flush(); // Invocations cannot span file boundaries.
		
		if ( guard_detect.valid && guard_detect.depth == conditional_depth ) {
			guard_current_header();
		}
		
		CPLUS_DO_FINALLY
	}
	void guard_current_header()
		{ guarded_headers.insert( { guard_detect.name, std::move( guard_detect.guard ) } ); }
	
public:
	template< typename ... args >
	preprocessor( args && ... a ) : preprocessor( {}, std::forward< args >( a ) ... ) {}
	
	template< typename ... args >
	preprocessor( std::vector< tokens > init_macro_definitions /* Token sequences of directives with leading #'s stripped. */, args && ... a )
		: derived_stage( ( init_macro_definitions.push_back( {
			pp_constants::define_directive, pp_constants::stringize_macro, // #define #(...)#__VA_ARGS__
				pp_constants::lparen, pp_constants::variadic_decl, pp_constants::rparen, pp_constants::stringize, pp_constants::variadic
		} ), init_macro_definitions ), std::forward< args >( a ) ... ),
		state( normal ), skip_depth( 0 ), conditional_depth( 0 ), guard_detect{ false } {
		
		char time_str_buf[ 26 ], *time_cstr = time_str_buf;
		std::time_t time_scalar = std::time( nullptr );
		util::ctime( &time_scalar, time_cstr );
		std::string time_str( time_cstr );
		
		this->insert_macro_definition( { pp_constants::define_directive, { token_type::id, "__TIME__" }, pp_constants::space,
			{ token_type::string_lit, ( std::string( "\"" ) + time_str.substr( 11, 8 ) + "\"" ).c_str() } }, literal_pool );
		
		time_str.erase( 11, 9 );
		this->insert_macro_definition( { pp_constants::define_directive, { token_type::id, "__DATE__" }, pp_constants::space,
			{ token_type::string_lit, ( std::string( "\"" ) + time_str.substr( 4, 11 ) + "\"" ).c_str() } }, literal_pool );
		
		struct pp_master_config : config_pragma_base {
			preprocessor *master;
			pragma_handler_list pragma_handlers() {
				static pragma_handler_list ret = {
					{ "once", pragma_function( [this]( tokens &&in ) {
						if ( ! in.empty() ) throw error( in[ 0 ], "This directive does not take an argument." ); // diagnose() not available in a pragma handler
						this->master->guard_detect.guard = pp_constants::guard_default;
						this->master->guard_current_header();
					} ) }
				};
				return ret;
			}
		};
		this->cont.template get_config< pp_master_config >().master = this; // restrictive to assume cont has config manager
	}
	
	token const & directive_id() {
		auto directive_it = input.begin() + 1; // First character is #.
		return pp_constants::skip_space( directive_it, input.end() ) != input.end()? * directive_it : pp_constants::placemarker;
	}
	
	void operator () ( delimiter< struct directive, delimiter_sense::open > in ) {
		this->template diagnose< diagnose_policy::pass, error >( this->paren_depth != 0, in, "A macro invocation cannot span a directive (§16.3/11)." );
		
		if ( input.empty() ) input.push_back( pp_constants::placemarker ); // Ensure there is whitespace before the directive.
		this->preprocessor::macro_context::flush(); // Flush uncalled function.
		directive = true;
		this->template pass< pass_policy::optional >( std::move( in ) );
	}
	void operator () ( delimiter< struct directive, delimiter_sense::close > in ) {
		directive = false;
		if ( state == normal ) {
			handle_directive();
		} else {
			CPLUS_FINALLY ( input.clear(); ) // Don't let exceptions leave garbage in the buffer.
			
			token const &directive = directive_id();
			if ( directive == pp_constants::if_directive || directive == pp_constants::ifdef_directive
				|| directive == pp_constants::ifndef_directive ) {
				++ skip_depth;
			
			} else if ( skip_depth == 0 ) {
				validate_else( directive );
				
				if ( directive == pp_constants::endif_directive
					|| ( directive == pp_constants::else_directive && state == skip_if ) ) {
					if ( directive == pp_constants::else_directive ) ++ conditional_depth;
					else else_stack.pop(); // handle endif
					
					state = entering;
					
					auto pen = input.begin() + 1;
					pp_constants::skip_space( pen, input.end() );
					this->template diagnose< diagnose_policy::pass, error >( pp_constants::skip_space( ++ pen, input.end() ) != input.end(), // Standard is unclear on this requirement.
						util::make_implicit_thunk( [&]{ return * pen; } ), "This directive takes no arguments." );
				} else if ( directive == pp_constants::elif_directive && state == skip_if ) {
					handle_directive();
				}
			} else if ( directive == pp_constants::endif_directive ) {
				-- skip_depth;
			}
		}
		this->template pass< pass_policy::optional >( std::move( in ) );
	}
	
	void operator () ( token && in ) {
		switch ( state ) {
		case entering: // one-shot state simply inserts whitespace #line directive if preserving space
			if ( this->token_config.preserve_space && in.get_parent< input_source >() ) {
				using namespace pp_constants;
				tokens comment_directive{ { token_type::ws, "#line " }, { line_macro.type, line_macro.s, in }, space, file_macro, in.s[0] != '\n'? newline : placemarker };
				for ( auto &t : process_macros( comment_directive.begin(), comment_directive.end() ) ) {
					t.type = token_type::ws;
					this->pass( std::move( t ) );
				}
			}
			state = normal;
		
		case normal:
			this->template diagnose< diagnose_policy::pass, error >( in == pp_constants::variadic && ( ! directive || directive_id() != pp_constants::define_directive ),
				in, "The identifier __VA_ARGS__ is reserved (§16.3/5)." );
			
			if ( ! directive ) {
				if ( guard_detect.depth == conditional_depth && in.type != token_type::ws ) {
					guard_detect.valid = false; // Anything besides a directive outside guard cancels guard detection.
				}
				this->pass( std::move( in ) );
			} else { // Accumulate tokens of a complete directive, then execute.
				input.push_back( std::move( in ) );
			}
			break;
		
		case skip_if: // Skip_if includes skipping elif *before* taken group.
		case skip_else: // Skip_else includes skipping elif *after* taken group.
			if ( ! directive ) break;
			input.push_back( std::move( in ) ); // Collect tokens from after # until before newline.
			break;
		}
	}
	
	void flush() {
		this->template diagnose< diagnose_policy::pass, error >( state == skip_if || state == skip_else || conditional_depth != 0,
			input.empty()? construct() : input.back(), "Expected #endif." );
		if ( directive ) { // If there is no terminating newline, it isn't a directive. Reprocess the tokens as garbage.
			tokens redo;
			redo.swap( input );
			this->pass( std::make_move_iterator( redo.begin() ), std::make_move_iterator( redo.end() ) );
		}
		state = normal;
	}
};

template< typename output_iterator > // Replaces pragma operator "calls" with destringized directive tokens.
class pragma_filter
	: public derived_stage< config_manager< output_iterator > > {
	config_pragma_base::pragma_map pragmas; // must be initialized before config3 - will be tough to handle in framework
	
	token pragma_token; // only for reporting non-termination error
public:
	enum { normal, open_paren, string_lit, close_paren } state;
	
	template< typename config_ret_type >
	config_ret_type &get_config() {
		typedef typename std::remove_const< config_ret_type >::type config_type;
		bool inserting = pragma_filter::config_manager::registry.count( typeid( config_type ) ) == 0;
		config_type &ret = pragma_filter::config_manager::template get_config< config_type >();
		if ( inserting ) {
			pragmas.insert( ret.pragma_handlers() );
		}
		return ret;
	}
	
	template< typename ... args >
	pragma_filter( args && ... a )
		: pragma_filter::derived_stage( std::forward< args >( a ) ... ), state( normal ) {}
	
	void operator() ( token &&in ) {
		if ( in.type == token_type::ws ) {
			this->pass( std::move( in ) );
			return;
		}
		switch ( state ) {
		case normal:		if ( in == pp_constants::pragma_operator ) {
								state = open_paren;
								pragma_token = std::move( in );
							} else this->pass( std::move( in ) );
							return;
							
		case open_paren:	state = normal;
							this->template diagnose< diagnose_policy::fatal, error >( in != pp_constants::lparen,
								in, "_Pragma operand must be in parentheses (§16.9)." );
							state = string_lit;
							return;
		case string_lit: {
							state = close_paren;
							this->template diagnose< diagnose_policy::fatal, error >( in.type != token_type::string_lit,
								in, "_Pragma operand must be a string (§16.9)." );
							
							tokens args;
							instantiate( std::make_shared< raw_text< string > >( destringize( in.s ), in ),
								pile< utf8_transcoder, lexer >( get_config< lexer_config >(),
									util::function< void( token && ), void( error && ) >(
										[ & args ]( token && t ){ args.push_back( std::move( t ) ); },
										this->template pass_function< error && >()
									) ) );
							auto pen = args.begin();
							if ( pp_constants::skip_space( pen, args.end() ) == args.end() ) return;
							
							auto target = pragmas.find( pen->s );
							if ( target != pragmas.end() ) {
								* pen = pp_constants::space; // consume initial token
								args.erase( std::remove_if( args.begin(), args.end(),
									[]( token const &t ) { return t.type == token_type::ws; } ), args.end() );
								try {
									return target->second( std::move( args ) );
								} catch ( propagate_pragma & ) { goto propagate; }
							} else propagate: {
								this->template pass< pass_policy::optional >( pragma( std::move( in.s ), std::move( in ) ) );
								return;
							}
						}
		case close_paren:	pragma_token = token();
							if ( ! this->template diagnose< diagnose_policy::pass, error >( in != pp_constants::rparen,
								in, "Only one argument to _Pragma allowed (§16.9)." ) ) {
								state = normal; // Continue complaining until the close paren.
							}
							return;
		}
	}
	void flush()
		{ this->template diagnose< diagnose_policy::pass, error >( state != normal, pragma_token, "Unterminated _Pragma." ); }
};

template< typename output >
class space_condenser : public stage< output, preprocessor_config, lexer_config > {
	token acc = empty_acc();
	token empty_acc() { return { token_type::ws, string{ this->template get_config< preprocessor_config const >().macro_pool } }; }
public:
	using space_condenser::stage::stage;
	
	void operator () ( token && in ) {
		if ( ! this->template get_config< lexer_config const >().preserve_space ) return this->pass( std::move( in ) );
		
		if ( in.type != token_type::ws ) {
			flush();
			this->pass( std::move( in ) );
		} else {
			if ( acc.s.empty() ) acc.construct::operator = ( std::move( in ) );
			acc.s += std::move( in.s );
		}
	}
	
	void flush() {
		if ( acc.s.empty() ) return;
		this->pass( std::move( acc ) );
		acc = empty_acc();
	}
};

}

#endif
