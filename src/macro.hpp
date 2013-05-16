// Cplus project, preprocessor macro engine.
// copyright David Krauss, created 9/2/11

#include "util.h"
#include "formats.h"
#include "constants.h"
#include "phase_3.hpp"

#include <map>
#include <unordered_map>

namespace cplus {

template< typename output_iterator >
class substitution_phase;

struct macro_context_info { // information that determines how macros are expanded, shared between contexts
	phase3_config const &token_config;
	
	typedef std::unordered_map< string, tokens const > name_map;
	name_map macros;
	struct presumptions { // Specific to current file, errors result if not empty and macro invokation spans EOF.
		std::int32_t line_displacement; // controlled by #line directive
		token filename; // also set by #line; if empty, use token::source
	} presumed;
};

template< typename output_iterator > // either another macro_context (via std::function) or a macro_filter
class macro_context : public stage< output_iterator > {
	typedef macro_context_info::name_map name_map;
	macro_context_info const &common; // points to sibling subobject of phase4 next to root context

	typedef std::vector< string const * > call_stack;
	name_map::const_pointer definition; // function-like macro awaiting call
protected:
	tokens input; // buffered arguments or scratch storage
	std::size_t paren_depth; // determines when to call macro

public:
	template< typename ... input >
	macro_context( macro_context_info const &common_info, input && ... a )
		: macro_context::stage( std::forward< input >( a ) ... ), common( common_info ),
		definition( nullptr ), paren_depth( 0 ) {}
	
	// flush and operator() accept list of callers as rvalue, return it unchanged
	void flush( call_stack &&callers = call_stack() ) {
		replace_object_completely( callers );
		if ( paren_depth != 0 ) {
			throw error( input[ input.front().type == token_type::ws ], "Unterminated macro invokation." );
		}
		if ( input.size() > 3 ) throw error( input.back(), "ICE: too much input at flush." );
		pass( std::make_move_iterator( input.begin() ), std::make_move_iterator( input.end() ), this->cont );
		input.clear();
		definition = nullptr;
	}
	
	void operator() ( token const &in, call_stack &&callers = call_stack() ) // copy input if not called with std::move
		{ return (*this) ( util::val( in ), std::move( callers ) ); }
	
	/*	operator() trims whitespace, buffers input, and calls macros. It doesn't trim recursion stops.
		replace() sends results back to it by co-recursion, which implements rescanning and further replacement. */
	void operator() ( token &&in, call_stack &&callers = call_stack() ) {
		bool stop_recursion = false; // perform this check before argument gets moved
		if ( in.type == token_type::id ) {
			for ( auto caller : callers ) {
				if ( * caller == in.s ) stop_recursion = true;
			}
		}
		if ( paren_depth == 0 ) {
			if ( definition ) {
				if ( in == pp_constants::recursion_stop ) {
					definition = nullptr; // Abort macro call. Recursion_stop token will be passed through.
				}
				replace_object_completely( callers ); // Call any object-like macro. May set definition to a function-like macro.
				
				if ( definition && is_function_like( * definition ) && in == pp_constants::lparen ) { // function call
					goto got_lparen; // Keep open paren, just for consistency.
				}
			} // Now, if definition is set, it's a function-like macro still awaiting "(".
			
			if ( in.type == token_type::ws ) goto condense_space;
			else {
				name_map::const_iterator def;
				if ( in.type == token_type::id && ( def = common.macros.find( in.s ) ) != common.macros.end() ) {
					if ( definition ) { // Flush uncalled function-like macro. Save trailing space.
						tokens::iterator flush_end = input.end() - ( input.back().type == token_type::ws );
						pass( std::make_move_iterator( input.begin() ), std::make_move_iterator( flush_end ), this->cont );
						input.erase( input.begin(), flush_end );
					}
					definition = &* def;
					input.push_back( std::move( in ) ); // Remember name in case macro isn't called.
				} else {
					flush( std::move( callers ) ); // Clear any uncalled function, or just flush whitespace. No object macro here.
					pass( this->cont, std::move( in ) ); // Common case: not in any part of an invokation, pass through.
				}
			}
		} else {
			if ( in == pp_constants::lparen ) {
		got_lparen:
				++ paren_depth;
				input.push_back( std::move( in ) );
			} else if ( in == pp_constants::rparen ) {
				input.push_back( std::move( in ) );
				-- paren_depth;
				if ( paren_depth == 0 ) replace( callers );
			} else if ( in == pp_constants::recursion_stop && input.back() == pp_constants::recursion_stop ) {
				// trim consecutive recursion stops
			} else
		condense_space:
			if ( in.type != token_type::ws || input.empty() || input.back().type != token_type::ws ) {
				input.push_back( std::move( in ) );
			} else if ( common.token_config.preserve_space ) {
				#if ! CPLUS_USE_STD_STRING
				input.back().s.open();
				#endif
				input.back().s += in.s.c_str();
			}
		}
		if ( stop_recursion ) {
			return (*this)( pp_constants::recursion_stop, std::move( callers ) ); // tail recurse, insert artificial input
		}
	}
	
protected:
	enum class presumption { line, file };
	token get_location( token const &in, presumption kind ) {
		token const *t = & in;
		try {
			while ( typeid( * t->source.get() ) != typeid (inclusion) ) {
				if ( auto subst = dynamic_cast< macro_substitution * >( t->source.get() ) ) {
					t = & subst->arg_begin[ t->location ]; // resolve an argument to its own value, not whole expansion
				} else {
					t = & t->source->source;
				}
			}
		} catch ( std::bad_typeid & ) { throw error( in, "ICE: Failed to evaluate __LINE__ or __FILE__." ); }
		
		if ( kind == presumption::line ) {
			std::int32_t logical_line = std::int32_t( t->location & file_location::line_mask ) + common.presumed.line_displacement;
			return { token_type::num, string{ std::to_string( logical_line ).c_str(), common.token_config.stream_pool } };
		} else {
			if ( common.presumed.filename == token() ) {
				return { token_type::string_lit, static_cast< inclusion const & >( * t->source ).file_name,
							t->source->source.source, t->source->source.location };
			} else {
				return common.presumed.filename;
			}
		}
	}
private:
	template< typename i > // either tokens::iterator or tokens::const_iterator
	static typename std::iterator_traits< i >::pointer skip_space( i &it )
		{ return it->type == token_type::ws? &* it ++ : nullptr; }
	
	static bool is_function_like( name_map::const_reference definition )
		{ return definition.second[0].type != token_type::ws; }
	
	void replace_object_completely( call_stack &callers )
		{ while ( definition && ! is_function_like( * definition ) ) replace( callers ); }
	
	void replace( call_stack &callers ) {
		struct arg_info {
			tokens::const_iterator begin, end;
			
			void trim_begin() // erase whitespace at start
				{ macro_context::skip_space( begin ); }
			void trim_end() // erase whitespace at end
				{ if ( end != begin && end[ -1 ].type == token_type::ws ) -- end; }
		};
		
		bool function_like = is_function_like( * definition );
		std::vector< arg_info > args_info; // size = param count
		tokens::iterator input_pen = input.begin();
		bool leading_space = skip_space( input_pen );
		token name( std::move( * input_pen ) );
		if ( function_like ) skip_space( ++ input_pen ); // ignore space between name and paren
		
		// save macro name and arguments, or special replacement list, in persistent instantiation object
		auto inst = definition->first == pp_constants::line_macro.s || definition->first == pp_constants::file_macro.s?
			std::make_shared< macro_replacement >( std::move( name ), tokens{ pp_constants::space,
				get_location( name, definition->first == pp_constants::line_macro.s? presumption::line : presumption::file ) } )
			: std::make_shared< macro_replacement >( std::move( name ), definition->second,
				tokens{ std::move_iterator< tokens::iterator >{ input_pen }, std::move_iterator< tokens::iterator >{ input.end() } } );
		input.erase( input.begin() + leading_space, input.end() );
		tokens const &def = inst->replacement;
		
		if ( function_like ) { //									======================== IDENTIFY ARGUMENTS ===
			auto arg_pen = inst->args.begin();
			args_info.resize( std::find( def.begin(), def.end(), pp_constants::rparen ) - def.begin() );
			auto info = args_info.begin();
			for ( ++ arg_pen; info != args_info.end() && arg_pen != inst->args.end(); ++ info, ++ arg_pen ) {
				info->begin = arg_pen;
				for ( ; arg_pen != inst->args.end() - 1; ++ arg_pen ) {
					if ( * arg_pen == pp_constants::comma && paren_depth == 0 ) break; // usual exit
					else if ( * arg_pen == pp_constants::lparen ) ++ paren_depth;
					else if ( * arg_pen == pp_constants::rparen ) -- paren_depth;
		extend_variadic: ;
				} // arg_pen points to comma or final close paren (final paren balance is guaranteed)
				info->end = arg_pen;
			}
			
			-- arg_pen;
			if ( info != args_info.end() ) throw error( * arg_pen, "Too few arguments to macro." );
			if ( arg_pen != inst->args.end() - 1 ) {
				if ( args_info.size() == 0 ) {
					skip_space( ++ arg_pen );
					if ( arg_pen != inst->args.end() - 1 ) throw error( * arg_pen, "Macro does not accept any argument." );
				} else if ( def[ args_info.size() - 1 ] == pp_constants::variadic ) {
					-- info;
					goto extend_variadic; // continue from where the comma broke the loop
				} else throw error( * arg_pen, "Too many arguments to macro." );
			}
		}
		callers.push_back( & definition->first ); // Register in call stack before rescanning.
		definition = nullptr; // Clear the "registers" to avoid confusing rescanning.
		
		// obtain iterator access to self
		auto local( std::bind( ref( * this ), std::placeholders::_1, util::rref( callers ) ) );
		
		// Use a local copy of phase 3 to generate tokens one at a time as intermediate results.
		token acc[ 2 ]; // [0] is LHS, [1] is RHS from stringize or LHS recursion stop
		auto acc_limiter( util::limit_range( &* acc ) ); // actual range specified at each use
		auto acc_it = [&]( token &&t )
			{ if ( t.type != token_type::ws ) acc_limiter( std::move( t ) ); }; // filter out whitespace
		
		for ( auto pen = def.begin() + args_info.size() + 1 /* skip ")" or " " */; pen != def.end(); ) {
			/* Each iteration handles some subset of the sequence <token> ## # <token>:
							leading_token			! leading_token
				neither		<lhs>					-
				stringize	-						# <lhs>
				concat		<lhs> ## <rhs>			## <rhs>
				both		<lhs> ## # <rhs>		## # <rhs>
				This is handled as: Perhaps stringize, perhaps catenate, else macro-replace. */
			
			bool stringize = false, concat = false, leading_token = false, trailing_space = false;
			token const *leading_space;
			
			leading_space = skip_space( pen ); // replacement list doesn't end with whitespace
			if ( pp_constants::is_concat( * pen ) ) {
				leading_space = nullptr;
			} else if ( ! pp_constants::is_stringize( * pen ) || ! function_like ) { // lead is _neither_ ## _nor_ #
				pass( std::make_move_iterator( acc ), std::make_move_iterator( acc_limiter.base ), local );
				acc[ 1 ] = token(); // be sure to invalidate recursion stop in acc[1]
				acc_limiter.base = acc;
				* acc_limiter.base ++ = instantiate( inst, pen ++ ); // only consume this non-operator token
				if ( pen != def.end() ) trailing_space = skip_space( pen ); // and this ws
				leading_token = true;
			}
			
			if ( pen != def.end() && pp_constants::is_concat( * pen ) ) {
				skip_space( ++ pen ); // replacement list doesn't end with ##
				concat = true;
			}
			
			if ( function_like // disable # operator for object-like macros
				&& ( ! leading_token || concat ) // exclude case not in above table
				&& pen != def.end() && pp_constants::is_stringize( * pen ) ) { // =================== STRINGIZE ===
				
				skip_space( ++ pen ); // replacement list doesn't end with #
				stringize = true;
				if ( ! concat ) { // flush buffered input if this isn't RHS of ##
					pass( std::make_move_iterator( acc ), std::make_move_iterator( acc_limiter.base ), local );
					acc[ 1 ] = token(); // be sure to invalidate recursion stop in acc[1]
					if ( leading_space ) local( * leading_space );
				}
				acc_limiter.base = acc + concat; // discard and overwrite possible recursion stop
				
				auto arg = args_info[ std::find( def.begin(), def.end(), * pen ) - def.begin() ];
				arg.trim_begin();
				arg.trim_end();
				
				try {
					phase3< decltype( acc_it ), std::false_type > lexer( common.token_config,
						std::make_shared< macro_substitution >( instantiate( inst, pen ++ ), arg.begin ), acc_it );

					acc_limiter.reset( 1 );
					lexer( { '"', 0 } );
					for ( auto pen = arg.begin; pen != arg.end; ++ pen ) {
						if ( pen->type == token_type::ws && ! pen->s.empty() ) {
							lexer( { ' ' } );
							continue;
						}
						for ( std::uint8_t c : pen->s ) {
							if ( ( pen->type == token_type::string_lit || pen->type == token_type::char_lit )
								&& ( c == '"' || c == '\\' // escape quotes and backslashes in strings, incl. hidden in UCNs
									|| c >= 0xC0 || ( c < 0x80 && ! char_in_set( char_set::basic_source, c ) ) ) ) {
								lexer( { '\\', static_cast< size_t >( pen - arg.begin ) } );
							}
							lexer( { c, static_cast< size_t >( pen - arg.begin ) } );
						}
					}
					lexer( { '"', static_cast< size_t >( arg.end - arg.begin ) } );
					finalize( lexer );
				} catch ( std::range_error & ) {
					goto stringize_wrong_count;
				}
				if ( acc_limiter.reset() != 1 || acc_limiter.base[-1].type != token_type::string_lit )
				stringize_wrong_count:
					throw error( arg.begin[ 0 ], "Stringize (#) did not produce one (1) result string." );
			}
			
			if ( concat ) { //													=================== CONCATENATE ===
				if ( ! stringize ) acc[ 1 ] = instantiate( inst, pen ++ ); // if value of pen is used for RHS, increment to next token
				
				enum { lhs, rhs }; // [0] is LHS, [1] is RHS:
				token const *begins[ 2 ] = { acc + 0, acc + 1 }, *ends[ 2 ] = { acc + 1, acc + 2 };
				tokens copies[ 2 ];
				bool stripped_stops[ 2 ] = { acc[1] == pp_constants::recursion_stop, false };
				
				for ( int side = 0; side < 2; ++ side ) { // expand arguments as necessary
					if ( side? stringize : ! leading_token ) continue; // not if using intermediate result
					size_t param_index = std::find( def.begin(), def.begin() + args_info.size(), acc[ side ] ) - def.begin();
					if ( param_index == args_info.size() ) continue; // not if replacement list doesn't indicate a parameter
					
					arg_info arg = args_info[ param_index ];
					side? arg.trim_begin() : arg.trim_end(); // strip ws from end of lhs and start of rhs
					
					auto subst = std::make_shared< macro_substitution >( std::move( acc[ side ] ), arg.begin ); // use new instantiation obj
					for ( auto arg_pen = arg.begin; arg_pen != arg.end; ++ arg_pen ) { // instantiate argument for use of parameter
						copies[ side ].push_back( { arg_pen->type, arg_pen->s, subst, static_cast< size_t >( arg_pen - arg.begin ) } );
					}
					begins[ side ] = &* copies[ side ].begin(); // Tentatively strip recursion stops; restore if no catenation is done:
					stripped_stops[ side ] = ! copies[ side ].empty() && copies[ side ].back() == pp_constants::recursion_stop;
					ends[ side ] = &* copies[ side ].end() - stripped_stops[ side ];
				}
				
				if ( leading_space ) local( * leading_space ); // preserve leading space
				
				acc_limiter.base = acc; // perform the paste into the intermediate result register
				if ( begins[lhs] == ends[lhs] ) {
					if ( begins[rhs] == ends[rhs] ) { // both sides are empty; produce a placemarker token
						* acc_limiter.base ++ = pp_constants::placemarker;
						continue;
					}
					pass( begins[rhs], ends[rhs] - 1, local ); // LHS is empty; output the RHS
					* acc_limiter.base ++ = std::move( ends[rhs][ -1 ] ); // just save the last token as intermediate result
					if ( ! stringize && stripped_stops[rhs] ) {
						* acc_limiter.base ++ = pp_constants::recursion_stop; // preserve stopper
					}
					continue;
				}
				pass( begins[lhs], ends[lhs] - 1, local ); // flush the tokens preceding the result
				
				if ( begins[rhs] == ends[rhs] ) { // RHS is empty; already sent the LHS
					* acc_limiter.base ++ = std::move( ends[lhs][ -1 ] );
					if ( stripped_stops[lhs] ) {
						* acc_limiter.base ++ = pp_constants::recursion_stop; // preserve stopper
					}
					continue;
				}
				
				try {
					phase3< decltype( acc_it ), std::false_type > lexer( common.token_config, ends[0][ -1 ].source, acc_it );
					auto pos = ends[lhs][ -1 ].location; // use LHS for position of pasted token
					
					acc_limiter.reset( 1 );
					for ( std::uint8_t c : std::move( ends[lhs][ -1 ].s ) ) lexer( { c, pos } ); // move to temporaries to be
					for ( std::uint8_t c : std::move( begins[rhs][ 0 ].s ) ) lexer( { c, pos } ); // sure overwriting acc is safe
					finalize( lexer );
				} catch ( std::range_error & ) {
					goto catenate_wrong_count;
				}
				if ( acc_limiter.reset() != 1 ) catenate_wrong_count:
					throw error( begins[rhs][ 0 ], "Catenating tokens did not produce a single result token." );
				
				while ( ++ begins[rhs] != ends[rhs] && * begins[rhs] == pp_constants::recursion_stop ) ; // re-evaluate recursion
				
				if ( begins[rhs] != ends[rhs] ) {
					local( std::move( acc[ 0 ] ) ); // flush if not an intermediate result
					pass( begins[rhs], ends[rhs] - 1, local );
					acc[ 0 ] = ends[rhs][ -1 ]; // get a new intermediate result from end of RHS
					if ( stripped_stops[rhs] ) {
						* acc_limiter.base ++ = pp_constants::recursion_stop; // preserve stopper
					}
				}
			
			} else if ( leading_token ) { //						================================ SUBSTITUTE ===
				if ( leading_space ) local( * leading_space );
				-- acc_limiter.base; // don't leave intermediate result
				if ( trailing_space ) -- pen; // will be next iteration's leading_space
				
				auto param = std::find( def.begin(), def.begin() + args_info.size(), acc[ 0 ] );
				if ( param == def.begin() + args_info.size() ) {
					local( std::move( acc[ 0 ] ) ); // common case - copy from replacement list to output
				} else {
					auto caller_self = callers.back();
					callers.pop_back(); // evaluate argument in caller's context, i.e. current name is not recursion-stopped
					
					macro_context< std::function< void( token && ) > > nested( common, [&]( token &&in ) {
						callers.push_back( caller_self ); // restore current context for rescanning
						local( std::move( in ) ); // local references callers
						callers.pop_back();
					} );
					
					arg_info arg = args_info[ param - def.begin() ];
					auto subst = std::make_shared< macro_substitution >( std::move( acc[ 0 ] ), arg.begin );
					for ( auto arg_pen = arg.begin; arg_pen != arg.end; ++ arg_pen ) {
						nested( token{ arg_pen->type, arg_pen->s,
								subst, std::size_t( arg_pen - arg.begin ) }, std::move( callers ) );
					}
					nested.flush( std::move( callers ) );
					
					callers.push_back( caller_self );
				}
			}
		}
		pass( std::make_move_iterator( acc ), std::make_move_iterator( acc_limiter.base ), local );
		callers.pop_back();
	}
};

macro_context_info::name_map::value_type normalize_macro_definition( tokens &&input, string_pool &macro_pool ) {
	using namespace pp_constants;
	
	tokens::iterator pen = input.begin() + 1; // skip leading space in input buffer
	skip_space( pen, input.end() ); // skip directive "define"
	
	if ( skip_space( ++ pen, input.end() ) == input.end() || pen->type != token_type::id ) {
		if ( pen == input.end() ) -- pen;
		throw error( * pen, "Expected macro definition." );
	}
	string name = repool( pen ++ ->s, macro_pool );
	
	tokens replacement;
	size_t nargs = 0;
	if ( pen != input.end() && pen->type != token_type::ws ) {
		if ( * pen != lparen )
			throw error( * pen, "An object-like macro definition must begin with whitespace (§16.3/3)." );
		while ( * pen != rparen ) {
			if ( skip_space( ++ pen, input.end() ) == input.end() ) parameters_unterminated:
				throw error( pen[ -1 ], "Unterminated parameter list." );
			if ( replacement.empty() && * pen == rparen ) break; // empty list
			
			if ( * pen == variadic )
				throw error( * pen, "The identifier __VA_ARGS__ is reserved (§16.3/5)." );
			if ( * pen == variadic_decl ) { // convert punctuator "..." to identifier "__VA_ARGS__"
				replacement.push_back( { token_type::id, variadic.s, pen->source, pen->location } );
			} else {
				if ( pen->type != token_type::id )
					throw error( * pen, "Parameter name must be an identifier." );
				if ( std::find( replacement.begin(), replacement.end(), * pen ) != replacement.end() )
					throw error( * pen, "Parameter name must be unique (§16.3/6)." );
				//replacement.push_back( std::move( * pen ) );
				replacement.push_back( pen->reallocate( macro_pool ) );
			}
			if ( skip_space( ++ pen, input.end() ) == input.end() ) goto parameters_unterminated;
			if ( * pen != rparen && replacement.back() == variadic )
				throw error( replacement.back(),
					"Variadic arguments must be declared as \"...\" at the end of the parameter "
					"list, and then referred to as \"__VA_ARGS__\" (§16.3/12, §16.3.1/2)." );
			if ( * pen != rparen && * pen != comma )
				throw error( * pen, "Expected \",\" to separate parameter declarations." );
		}
		replacement.push_back( std::move( pen ++ ->reallocate( macro_pool ) ) ); // save ")"
		nargs = replacement.size(); // includes ")" so nonzero if function-like
		
	} else replacement.push_back( space ); // synthesize a space if list is truly empty
	
	if ( skip_space( pen, input.end() ) != input.end() && is_concat( * pen ) ) bad_cat:
		throw error( * pen, "Concatenation (##) operator may not appear at beginning or end of a macro (§16.3.3/1)." );
	
	bool got_stringize = false;
	while ( skip_space( pen, input.end() ) != input.end() ) {
		if ( got_stringize && nargs // validate stringize operator
			&& std::find( replacement.begin(), replacement.begin() + nargs - 1, * pen )
				== replacement.begin() + nargs - 1 ) throw error( * pen,
				"Stringize (#) operator in a function-like macro may only apply to an argument (§16.3.2/1)." );
		got_stringize = is_stringize( * pen );
		
		if ( * pen == variadic // validate variadic usage
			&& ( nargs < 2 || replacement[ nargs - 2 ] != variadic ) )
			throw error( * pen, "The identifier __VA_ARGS__ is reserved (§16.3/5)." );
		
		replacement.push_back( pen ++ ->reallocate( macro_pool ) );
		if ( pen != input.end() && pen->type == token_type::ws ) {
			replacement.push_back( std::move( pen ++ ->reallocate( macro_pool ) ) ); // condense whitespace
		}
	}
	if ( replacement.size() > 1 && replacement.back().type == token_type::ws ) replacement.pop_back();
	
	if ( is_concat( replacement.back() ) )
		goto bad_cat;
	
	return { name, replacement };
}

template< typename output_iterator >
class macro_filter : public stage< output_iterator > {
	friend typename macro_filter::stage;
	
public:
	template< typename ... args >
	macro_filter( args && ... a )
		: macro_filter::stage( std::forward< args >( a ) ... ) {}
	
	void operator() ( token &&in )
		{ if ( ! in.s.empty() ) pass( this->cont, std::move( in ) ); } // Filter out placemarkers and recursion stops.
};

template< typename output_iterator >
class substitution_phase
	: public derived_stage< macro_context< configured_stage_from_functor< macro_filter< output_iterator > > >, phase3_config >,
	protected macro_context_info {
public:
	template< typename in_config_type, typename ... args >
	substitution_phase( in_config_type &&token_config, macro_context_info::name_map &&init_macros, args && ... a )
		: substitution_phase::derived_stage( static_cast< macro_context_info & >( * this ), std::forward< args >( a ) ... ),
		macro_context_info{ token_config, std::move( init_macros ) } {}
};

}
