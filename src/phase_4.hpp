// Cplus project, translation phase 4: execute preprocessing directives and macros and the like.
// copyright David Krauss, created 8/29/11

#ifndef CPLUS_PHASE_4
#define CPLUS_PHASE_4

#include "macro.hpp"

#include <sstream>
#include <fstream>
#include <stack>

namespace cplus {

namespace pp_constants {
	token const stringize_macro{ token_type::id, "#" }; // Use illegal identifier for internal #pragma support macro.
	token const line_marker = { token_type::ws, "#line " };
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
	typedef std::map< string, std::pair< std::function< eval_t( eval_t, eval_t ) >, int > > op_map;
	static op_map binary_operators {
	#define SIMPLE_OP( op, prec ) { # op, { []( eval_t l, eval_t r ){ return l op r; }, prec } }
		SIMPLE_OP( *, 10 ), SIMPLE_OP( /, 10 ), SIMPLE_OP( %, 10 ),
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
	};
	static std::map< string, std::function< eval_t( eval_t ) > > unary_operators {
	#define SIMPLE_OP( op ) { # op, []( eval_t v ){ return op v; } }
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
		if ( s.peek() == 'u' || s.peek() == 'U' ) s.ignore();
		if ( ( s.peek() == 'l' || s.peek() == 'L' ) && s.peek() == s.str().back() ) s.ignore( 2 ); // 2 may be excessive, triggered by eg 1L
		if ( ! s.ignore().eof() ) throw error( * pen, "Not an integer." );
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
			acc = std::get< 0 >( op->second )( acc, rudimentary_evaluate( ++ pen, prec + 1 ) );
		} else throw error( * pen, "Expected an operator." );
	}
	return acc;
}

template< typename output_iterator >
class phase4
	: public derived_stage< substitution_phase< output_iterator >, phase4_config, phase3_config > {
	typedef typename phase4::derived_stage derived_stage;
	
	using derived_stage::input; // share state with macro context subobject
	using derived_stage::macros; // and macro substitution driver subobject
	
	enum { entering, normal, directive, skip_if, skip_else } state;
	
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
		if ( preserve_defined_operator ) { // #define defined(x)defined(x##<recursion stop><recursion stop>)
			using namespace pp_constants; // Catenating first recursion stop ensures that no space precedes second.
			static tokens const defined_preserver = { { token_type::id, "x" }, rparen, defined_operator,
				lparen, { token_type::id, "x" }, concat, recursion_stop, recursion_stop, rparen
			};
			// If user-defined macro already exists, use that instead, and don't undefine it.
			preserve_defined_operator = macros.insert( { pp_constants::defined_operator.s, defined_preserver } ).second;
		}
		
		macro_context< macro_filter< std::back_insert_iterator< tokens > > >
			local( static_cast< macro_context_info & >( * this ), ret );
		pass( std::make_move_iterator( first ), std::make_move_iterator( last ), local );
		local.flush();
		
		if ( preserve_defined_operator ) macros.erase( pp_constants::defined_operator.s ); // #undef defined
		
		return ret;
	}
	
	void validate_else( token const &directive ) {
		if ( ( directive == pp_constants::else_directive || directive == pp_constants::elif_directive )
				&& else_stack.top() == true )
			throw error( directive, "An #else or #elif group cannot follow another #else group." );
		else_stack.top() = directive == pp_constants::else_directive;
	}
	
	void handle_directive() {
		using pp_constants::skip_space;
		
		tokens::iterator pen = input.begin() + 1;
		skip_space( pen, input.end() );
		
		if ( pen != input.end() && ( ( conditional_depth == guard_detect.depth // at file top level,
				&& * pen != pp_constants::ifndef_directive && * pen != pp_constants::if_directive ) // any wrong directive
			|| ( conditional_depth == guard_detect.depth + 1 // or an alternative to the guard
				&& ( * pen == pp_constants::elif_directive || * pen == pp_constants::else_directive ) ) ) ) {
			guard_detect.valid = false; // cancels guard detection
		}
		
		if ( pen == input.end() ) {
			// null directive
			
		} else if ( * pen == pp_constants::define_directive ) {
			auto macro = normalize_macro_definition( std::move( input ), this->get_config().macro_pool );
			auto definition = macros.insert( macro );
			if ( definition.second == false && * definition.first != macro )
				throw error( macro.second.front(), "Macro definition does not match previous definition." );
			
		} else if ( * pen == pp_constants::undef_directive ) {
			if ( skip_space( ++ pen, input.end() ) == input.end() )
				throw error( input.back(), "Expected an identifier before end of line." );
			if ( pen->type != token_type::id )
				throw error( * pen, "Expected an identifier." );
			if ( pen->s == pp_constants::file_macro.s || pen->s == pp_constants::line_macro.s )
				throw error( * pen, "Cannot #undef this macro (§16.8/4)." );
			
			macros.erase( pen->s );
			
			if ( skip_space( ++ pen, input.end() ) != input.end() )
				throw error( input.back(), "Expected end of line." );
			
		} else if ( * pen == pp_constants::error_directive ) {
			throw error( * pen, "Error directive (§16.5)." ); // Rely on driver to show actual message.
		
		} else if ( * pen == pp_constants::pragma_directive ) { // convert #pragma to _Pragma()
			auto local( ref( * this ) );
			tokens intro = { pp_constants::pragma_operator, pp_constants::lparen,
					pp_constants::stringize_macro, pp_constants::lparen },
				arg( std::move( input ) ),
				term = { pp_constants::rparen, pp_constants::rparen, std::move( pen[ -1 ] ) }; // end with whitespace
			
			pass( std::make_move_iterator( intro.begin() ), std::make_move_iterator( intro.end() ), local );
			
			auto pos_back = arg.back();
			skip_space( ++ pen, arg.end() ); // does moving a container preserve iterators into it?
			pass( std::make_move_iterator( pen ), std::make_move_iterator( arg.end() ), local );
			if ( this->paren_depth != 1 ) // not infallible, but #pragma is implementation-defined anyway
				throw error( pos_back, "Parens must balance in #pragma." );
			
			pass( std::make_move_iterator( term.begin() ), std::make_move_iterator( term.end() ), local );
		
		} else if ( * pen == pp_constants::else_directive || * pen == pp_constants::endif_directive
					|| ( * pen == pp_constants::elif_directive && state != skip_if ) ) {
			if ( conditional_depth == 0 ) throw error( * pen, "This directive must terminate a conditional block." );
			-- conditional_depth;
			if ( * pen == pp_constants::endif_directive ) else_stack.pop();
			else state = skip_else;
			
			validate_else( * pen );
			
			if ( * pen != pp_constants::elif_directive && skip_space( ++ pen, input.end() ) != input.end() )
				throw error( * pen, "This directive takes no arguments." );
			
		} else if ( * pen == pp_constants::if_directive || * pen == pp_constants::elif_directive
					|| * pen == pp_constants::ifdef_directive || * pen == pp_constants::ifndef_directive ) {
			token saved_space = std::move( input.front() );
			input.front() = pp_constants::space;
			
			// 1st pass: normalize. "ifdef" => "defined", "ifndef " => "!defined", "if" and "elif" just get erased.
			if ( * pen == pp_constants::ifdef_directive || * pen == pp_constants::ifndef_directive ) {
				if ( * pen == pp_constants::ifndef_directive ) {
					* pen ++ = pp_constants::bang;
					if ( pen->type != token_type::ws ) throw error( * pen, "Expected space." ); // hah
				}
				* pen = pp_constants::defined_operator;
				if ( skip_space( ++ pen, input.end() )->type != token_type::id
					|| skip_space( ++ pen, input.end() ) != input.end() ) throw error( * pen,
					"#ifdef and #ifndef accept only a single identifier argument." );
			} else * pen = pp_constants::space;
			
			if ( guard_detect.valid && guard_detect.depth == conditional_depth ) {
				if ( guard_detect.guard == pp_constants::guard_default ) {
					for ( auto &t : input ) t = t.reallocate( this->get_config().macro_pool );
					guard_detect.guard = input;
				} else {
					guard_detect.valid = false;
				}
			}
			
			if ( ! evaluate_condition( input.begin(), input.end() ) ) state = skip_if;
			else {
				if ( state == skip_if ) state = entering;
				++ conditional_depth;
			}
			else_stack.push( false );
			input = { std::move( saved_space ) };
		} else if ( * pen == pp_constants::include_directive || * pen == pp_constants::line_directive ) {
			// Handle directives allowing macros in arguments. This cannot be the default (§16/6).
			std::iter_swap( input.begin(), pen - 1 ); // drop whitespace after the # token
			input = process_macros( pen - 1, input.end() );
			pen = input.begin() + 1;
			
			if ( * pen == pp_constants::include_directive ) {
				return handle_header( pen ); // flushes directive input and obtains new input
			
			} else if ( * pen == pp_constants::line_directive ) {
				std::ptrdiff_t physical_line = this->line_number( * pen ); // signed to prevent overflow below
				
				if ( skip_space( ++ pen, input.end() ) == input.end() )
					throw error( input.front(), "Expected line number." );
				
				std::istringstream s( pen->s ); // decimal-only is default
				int32_t line_number = * std::istream_iterator< int32_t >( s );
				if ( ! s || ! s.ignore().eof() || line_number == 0 ) throw error( * pen,
					"Line number must be a decimal literal between 1 and 2^31-1 (§16.4/3)." );
				
				this->presumed.line_displacement = line_number - physical_line - 1;
				
				if ( skip_space( ++ pen, input.end() ) != input.end() ) {
					if ( pen->type != token_type::string_lit || pen->s.front() != '"' || pen->s.back() != '"' )
						throw error( * pen, "Filename must be a simple string (§16.4/4)." );
					
					this->presumed.filename = std::move( * pen );
					
					if ( skip_space( ++ pen, input.end() ) != input.end() )
						throw error( * pen, "Extra tokens after replacement filename." );
				}
				
				state = entering; // print new line number
			}
		} else {
			throw error( * pen, "Non-directive." );
		}
		
		input.resize( 1 ); // preceding whitespace continues accumulation
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
		
		if ( pp_constants::skip_space( pen = expr.begin(), expr.end() ) == expr.end() )
			throw error( pen[ -1 ], "No controlling expression for conditional directive." );
		
		expr.erase( std::remove_if( expr.begin(), expr.end(),
									[]( token const &t ){ return t.type == token_type::ws; } ), expr.end() );
		expr.push_back( { token_type::punct, "$", util::val< construct const & >( expr.back() ) } );
		
		pen = expr.begin(); // 5th pass (!): actually evaluate.
		bool result = rudimentary_evaluate( pen );
		if ( pen != expr.end() - 1 ) throw error( * pen, "Expected end of expression." );
		return result;
	}
	void replace_defined_operator( tokens &expr ) const {
		for ( auto pen = expr.begin(); pen != expr.end(); ++ pen ) { // 2nd pass: evaluate defined( id ) operator.
			if ( * pen == pp_constants::defined_operator ) {
				* pen = pp_constants::space;
				if ( pp_constants::skip_space( ++ pen, expr.end() ) == expr.end() ) bad_defined_expr:
					throw error( pen[ -1 ], "defined operator does not match \"defined <id>\" "
												"or \"defined ( <id> )\" (§16.1/1)." );
				bool paren = * pen == pp_constants::lparen;
				if ( ( paren && pp_constants::skip_space( ++ pen, expr.end() ) == expr.end() )
					|| pen->type != token_type::id ) goto bad_defined_expr;
				// perform the actual operation
				pen->assign_content( macros.count( pen->s )? pp_constants::one : pp_constants::zero );
				
				if ( paren && ( pp_constants::skip_space( ++ pen, expr.end() ) == expr.end()
					|| * pen != pp_constants::rparen ) ) goto bad_defined_expr;
			}
		}
	}
	
	void handle_header( tokens::iterator pen ) {
		using pp_constants::skip_space;
		
		if ( skip_space( ++ pen, input.end() ) == input.end() )
			throw error( input.front(), "Expected header name." );
		
		std::string name;
		if ( pen->type == token_type::header_name ) name = pen->s;
		
		else if ( pen->type == token_type::string_lit ) {
			name = std::string( "\"" ) + destringize( std::move( pen->s ) ) + "\"";
		
		} else {
			auto end = input.rbegin();
			skip_space( end, decltype( end )( pen ) ); // skip trailing space
			auto name_begin = pen;
			while ( pen != end.base() ) {
				name += pen ++ ->s;
			}
			-- pen;
			if ( name.front() != '<' || name.back() != '>' || name.size() == 2 ) throw error( * pen,
				"Expected a sequence of tokens between \"<\" and \">\"." );
			pen = name_begin + 1;
		}
		
		bool use_user_paths = name.front() == '"';
		name = name.substr( 1, name.size() - 2 );
		if ( name.empty() ) throw error( * pen, "Empty header name." );
		
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
				path_set != std::end( path_sets ); ++ path_set ) {
				for ( std::string path : ** path_set ) {
					path = path + name;
					if ( header.open( path.c_str(), std::ios::in | std::ios::binary ) ) {
						name = path;
						goto opened;
					}
				}
			}
			throw error( * pen, "File not found." );
		}
	opened:
		token name_r = pen->reallocate( this->get_config().macro_pool ),
			name_s{ token_type::string_lit, string{ "\"", this->get_config().macro_pool }, * pen };
		for ( char c : name ) {
			if ( c == '\\' || c == '"' ) name_s.s += '\\';
			name_s.s += c;
		}
		name_s.s += '"';
		
		if ( ( pen->type == token_type::string_lit || pen->type == token_type::header_name )
			&& skip_space( ++ pen, input.end() ) != input.end() ) throw error( input.back(), "Extra tokens after header name." );
		input.resize( 1 );
		
		auto context_backup = this->presumed;
		this->presumed = macro_context_info::presumptions{ name_s };
		
		auto guard = guarded_headers.find( name_s.s );
		if ( guard == guarded_headers.end() || evaluate_condition( guard->second.begin(), guard->second.end() ) ) {
			if ( guard != guarded_headers.end() ) {
				this->pass_or_throw< error >( name_r, "Warning: multiply including guarded header" );
			}
			auto guard_detect_backup = std::move( guard_detect );
			guard_detect = { true, name_s.s, pp_constants::guard_default, conditional_depth };
			
			state = entering;
			
			instantiate( std::make_shared< inclusion >( name, name_r ),
				phase1_2< phase3< std::reference_wrapper< phase4 > > >( * this ) );
			
			state = entering;
			
			if ( guard_detect.valid && guard_detect.depth == conditional_depth ) {
				guard_current_header();
			}
			guard_detect = std::move( guard_detect_backup );
		}
		
		this->presumed = context_backup;
	}
	void guard_current_header()
		{ guarded_headers.insert( { guard_detect.name, std::move( guard_detect.guard ) } ); }
	
public:
	template< typename ... args >
	phase4( args && ... a )
		: derived_stage( macro_context_info::name_map{
			{ pp_constants::stringize_macro.s, tokens{ pp_constants::variadic, pp_constants::rparen,
										pp_constants::stringize, pp_constants::variadic } },
			{ pp_constants::file_macro.s, tokens{ pp_constants::space } },
			{ pp_constants::line_macro.s, tokens{ pp_constants::space } } },
			std::forward< args >( a ) ... ),
		state( normal ), skip_depth( 0 ), conditional_depth( 0 ), guard_detect{ false } {
		
		char time_str_buf[ 26 ], *time_cstr = time_str_buf;
		std::time_t time_scalar = std::time( nullptr );
		util::ctime( &time_scalar, time_cstr );
		std::string time_str( time_cstr );
		
		macros.insert( { "__TIME__", { pp_constants::space, { token_type::string_lit,
			( std::string( "\"" ) + time_str.substr( 11, 8 ) + "\"" ).c_str() } } } );
		
		time_str.erase( 11, 9 );
		macros.insert( { "__DATE__", { pp_constants::space, { token_type::string_lit,
			( std::string( "\"" ) + time_str.substr( 4, 11 ) + "\"" ).c_str() } } } );
		
		struct pp_master_config : config_pragma_base {
			phase4 *master;
			pragma_handler_list pragma_handlers() {
				static pragma_handler_list ret = {
					{ "once", pragma_function( [this]( tokens &&in ) {
						if ( ! in.empty() ) throw error( in[ 0 ], "This directive does not take an argument." );
						this->master->guard_detect.guard = pp_constants::guard_default;
						this->master->guard_current_header();
					} ) }
				};
				return ret;
			}
		};
		this->cont.template get_config< pp_master_config >().master = this; // restrictive to assume cont has config manager
	}
	
	void operator() ( token &&in ) {
		using pp_constants::skip_space;
		
		if ( in == pp_constants::variadic && std::find( input.begin(), input.end(), pp_constants::define_directive ) == input.end() )
			throw error( in, "The identifier __VA_ARGS__ is reserved (§16.3/5)." );
		
		switch ( state ) {
		case entering: // one-shot state simply inserts whitespace #line directive if preserving space
			if ( this->token_config.preserve_space && in.get_parent< input_source >() ) {
				using namespace pp_constants;
				tokens comment_directive{ line_marker, { line_macro.type, line_macro.s, in }, space, file_macro, in.s[0] != '\n'? newline : placemarker };
				for ( auto &t : process_macros( comment_directive.begin(), comment_directive.end() ) ) {
					t.type = token_type::ws;
					phase4::base::operator() ( std::move( t ) );
				}
			}
			state = normal;
		
		case normal:
			if ( in.type != token_type::ws ) {
				if ( guard_detect.depth == conditional_depth // anything besides a directive outside guard
						&& ( in.type != token_type::directive || input.size() > 1 ) ) {
					guard_detect.valid = false; // cancels guard detection
				}
				if ( in.type == token_type::directive ) { // consume and discard the #
					if ( this->paren_depth != 0 )
						throw error( in, "A macro invocation cannot span a directive (§16.3/11)." );
					
					this->pass( std::make_move_iterator( input.begin() ), std::make_move_iterator( input.end() - 1 ) ); // flush uncalled function
					input.erase( input.begin(), input.end() - 1 ); // save trailing space token (guaranteed to exist)
					state = directive;
					return;
				}
			}
			phase4::base::operator() ( std::move( in ) );
			return;
		
		case directive:
			if ( in.s[ 0 ] == '\n' ) {
				std::string newline( std::move( in.s ) ); // defer trailing space, for #include
				string( std::move( in.s ) ); // remove it from pool (don't clog stream)
				
				state = normal;
				handle_directive();
				
				in.s = newline.c_str();
				return (*this)( std::move( in ) ); // "retry" the newline token
			} else {
				input.push_back( std::move( in ) );
				return;
			}
		
		case skip_if: // skip_if includes skipping elif *before* taken group
		case skip_else: // skip_else includes skipping elif *after* taken group
			if ( ( in.type == token_type::directive && in != pp_constants::hash ) // if just encountered a directive
					|| ( input.size() != 1 && in.s[0] != '\n' ) ) { // or inside a directive
				input.push_back( std::move( in ) ); // collect it from directive identifier until before newline
				return;
			}
			if ( input.size() == 1 ) return; // finished if no directive yet
			
			token const &directive = input[ 1 ];
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
					
					auto pen = input.begin() + 2;
					if ( skip_space( pen, input.end() ) != input.end() ) // Standard is unclear on this requirement.
						throw error( * pen, "This directive takes no arguments." );
				} else if ( directive == pp_constants::elif_directive && state == skip_if ) {
					handle_directive();
				}
			} else if ( directive == pp_constants::endif_directive ) {
				-- skip_depth;
			}
			input.resize( 1 );
			return;
		}
	}
	
	void flush() {
		if ( state == skip_if || state == skip_else || conditional_depth != 0 )
			throw error( input.front(), "Expected #endif." );
		if ( state == directive ) input.clear(); // last line was directive terminated with backslash.
	}
};

template< typename output_iterator > // Replaces pragma operator "calls" with destringized directive tokens.
class pragma_filter
	: public derived_stage< config_manager< output_iterator > > {
	config_pragma_base::pragma_map pragmas; // must be initialized before config3 - will be tough to handle in framework
	
	token pragma_token; // only for reporting non-termination error
public:
	enum { normal, open_paren, string, close_paren } state;
	
	template< typename config_ret_type >
	config_ret_type &get_config() {
		typedef typename std::remove_const< config_ret_type >::type config_type;
		bool inserting = pragma_filter::base::registry.count( typeid( config_type ) ) == 0;
		config_type &ret = pragma_filter::base::template get_config< config_type >();
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
							
		case open_paren:	state = string;
							if ( in != pp_constants::lparen )
								throw error( in, "_Pragma operand must be in parentheses (§16.9)." );
							return;
		case string:	{
							state = close_paren;
							if ( in.type != token_type::string_lit )
								throw error( in, "_Pragma operand must be a string (§16.9)." );
							
							tokens args;
							instantiate( std::make_shared< raw_text >( destringize( in.s ), in ),
								phase3< std::back_insert_iterator< tokens > >( get_config< phase3_config >(), args ) );
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
								in.type = token_type::directive;
								this->pass( std::move( in ) );
								return;
							}
						}
		case close_paren:	state = normal;
							pragma_token = token();
							if ( in != pp_constants::rparen )
								throw error( in, "Only one argument to _Pragma allowed (§16.9)." );
							return;
		}
	}
	void flush()
		{ if ( state != normal ) throw error( pragma_token, "Unterminated _Pragma." ); }
};

}

#endif
