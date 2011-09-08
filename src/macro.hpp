// Cplus project, preprocessor macro engine.
// copyright David Krauss, created 9/2/11

#include "util.h"
#include "formats.h"
#include "constants.h"
#include "phase_1_2.hpp"
#include "phase_3.hpp"

#include <map>
#include <iterator>

namespace cplus {

template< typename output_iterator >
class macro_context {
	output_iterator cont; // where to send results of substitution
	
public:
	typedef std::map< std::string, pp_tokens > name_map;
	typedef std::vector< std::string const * > call_stack; // used only to prevent recursion
	
	template< typename ... input >
	macro_context( name_map const &in_macros, call_stack &in_callers, input && ... a )
		: cont( std::forward< input >( a ) ... ),
		definition( nullptr ), paren_depth( 0 ),
		macros( in_macros ), callers( in_callers ) {}
	
	macro_context( macro_context && ) = delete;
	macro_context( macro_context const & ) = delete;
	
	void flush() {
		if ( paren_depth != 0 ) throw error( input.front().p, "Unterminated macro invokation." );
		if ( ! input.empty() ) {
			if ( definition && ! is_function_like( * definition ) ) {
				replace(); // side effect: ignores, clears input
			} else {
				std::move( input.begin(), input.end(), util::ref( cont ) );
			}
			input.clear();
		}
		definition = nullptr;
	}
	friend void finalize( macro_context &o ) {
		o.flush();
		finalize( o.cont );
	}
	~macro_context()
		{ if ( ! input.empty() ) throw error( input.front().p, "ICE: Flush macro_context before destroying." ); }
	
	void operator() ( pp_token const &in )
		{ return (*this)( util::copy( in ) ); } // copy input if not called with std::move
	
	/*	operator() trims whitespace, buffers input, and calls macros. It doesn't trim placemarkers and recursion stops.
		replace() sends results back to it by co-recursion, which implements rescanning and further replacement. */
	void operator() ( pp_token &&in ) {
		bool stop_recursion = false;
		if ( in.type == pp_token_type::id ) { // perform this check before argument gets moved
			for ( auto caller : callers ) if ( * caller == in.s ) {
				stop_recursion = true;
				break;
			}
		}

		if ( paren_depth == 0 ) {
			if ( definition ) {
				if ( is_function_like( * definition ) ) {
					if ( in == pp_constants::lparen ) {
						input = { in }; // forget non-call token now that we have a function call
						paren_depth = 1;
						return;
					} else if ( in.type == pp_token_type::ws ) {
						goto queue_space;
					}
				} else if ( in == pp_constants::recursion_stop ) {
					definition = nullptr; // Abort macro call. Recursion_stop token will be passed through.
				}
			} else if ( in.type == pp_token_type::ws ) queue_space: {
				if ( input.empty() || input.back() != pp_constants::space ) {
					input.push_back( std::move( in ) ); // preserve whitespace after non-called function
				}
				return;
			}
			flush(); // call the queued object-like macro, if any, else flush the input buffer
			name_map::const_iterator def;
			if ( in.type == pp_token_type::id && ( def = macros.find( in.s ) ) != macros.end() ) {
				definition = &* def;
				input.push_back( std::move( in ) ); // remember name in case macro isn't called
			
			} else * cont ++ = std::move( in ); // common case: not in any part of an invokation.
		} else {
			if ( in == pp_constants::lparen ) {
				++ paren_depth;
			} if ( in == pp_constants::rparen ) {
				input.push_back( std::move( in ) );
				-- paren_depth;
				if ( paren_depth == 0 ) replace();
				return;
			}
			input.push_back( std::move( in ) );
		}
		if ( stop_recursion ) {
			return (*this)( pp_constants::recursion_stop ); // tail recurse, append artificial input
		}
	}
	
protected:
	name_map::const_pointer definition; // function-like macro awaiting call
	pp_tokens input; // buffered arguments or scratch storage
	std::size_t paren_depth; // determines when to call macro
	
private:
	name_map const &macros; // reference to the calling phase4 object
	call_stack &callers; // nested evaluation stack, always empty except during evaluation
	
	static pp_tokens::const_pointer skip_space( pp_tokens::const_iterator &it )
		{ return it->type == pp_token_type::ws? &* it ++ : nullptr; }
	
	static bool is_function_like( name_map::const_reference definition )
		{ return definition.second[0].type != pp_token_type::ws; }
	
	void replace() {
		struct arg_info {
			pp_tokens::const_iterator begin, end;
			std::size_t evaluated_begin, evaluated_end;
		
			void trim_begin() // erase whitespace at start
				{ macro_context::skip_space( begin ); }
			void trim_end() // erase whitespace at end
				{ if ( end != begin && end[ -1 ].type == pp_token_type::ws ) -- end; }
		};
		
		pp_tokens const args( std::move( input ) ), &def = definition->second;
		pp_tokens args_evaluated;
		bool function_like = is_function_like( * definition );
		callers.push_back( & definition->first );
		input.clear();
		definition = nullptr; // clear the "registers" to avoid confusing re-scanning
		
		std::vector< arg_info > args_info; // size = param count
		if ( function_like ) { // identify arguments
			args_info.resize( std::find( def.begin(), def.end(), pp_constants::rparen ) - def.begin() );
			pp_tokens::const_iterator arg_pen = args.begin() + 1; // skip open paren
			
			macro_context< std::back_insert_iterator< pp_tokens > >
				nested( macros, callers, args_evaluated );
			
			auto info = args_info.begin();
			for ( ; info != args_info.end() && arg_pen != args.end(); ++ info, ++ arg_pen ) {
				info->begin = arg_pen;
				info->evaluated_begin = args_evaluated.size();
				
				for ( ; arg_pen != args.end() - 1; ++ arg_pen ) {
					if ( * arg_pen == pp_constants::comma && paren_depth == 0 ) break; // usual exit
					else if ( * arg_pen == pp_constants::lparen ) ++ paren_depth;
					else if ( * arg_pen == pp_constants::rparen ) -- paren_depth;
		redo_variadic:
					nested( * arg_pen );
				} // arg_pen points to comma or final close paren (final paren balance is guaranteed)
				
				nested.flush();
				
				info->end = arg_pen;
				info->evaluated_end = args_evaluated.size();
			}
			
			-- arg_pen;
			if ( info != args_info.end() ) throw error( arg_pen->p, "Too few arguments to macro." );
			if ( arg_pen != args.end() - 1 ) {
				if ( args_info.size() == 0 ) {
					skip_space( ++ arg_pen );
					if ( arg_pen != args.end() - 1 ) throw error( arg_pen->p, "Macro does not accept any argument." );
				} else if ( def[ args_info.size() - 1 ] != pp_constants::variadic ) {
					throw error( arg_pen->p, "Too many arguments to macro." );
				} else {
					-- info;
					goto redo_variadic;
				}
			}
		}
		
		// obtain iterator access to self
		auto local( util::make_output_iterator( ref( * this ) ) );
		
		// Use a local copy of phases 1-3 to generate tokens one at a time as intermediate results.
		pp_token acc[ 2 ];
		auto acc_limiter( util::limit_range( &* acc ) ); // actual range specified at each use
		auto acc_it = util::make_output_iterator( [&]( pp_token &&t )
			{ if ( t.type != pp_token_type::ws ) * acc_limiter ++ = std::move( t ); } );
		phase1_2< util::output_iterator_from_functor< phase3< decltype( acc_it ) > > > lexer( acc_it );
		
		for ( auto pen = def.begin() + args_info.size() + 1 /* skip ")" or " " */; pen != def.end(); ) {
			/* Each iteration handles some subset of the sequence <token> ## # <token>:
							leading_token			! leading_token
				neither		<lhs>					-
				stringize	-						# <arg>
				concat		<lhs> ## <rhs>			## <rhs>
				both		<lhs> ## # <arg/rhs>	## # <arg/rhs>
				This is handled as: Perhaps stringize, perhaps catenate, else macro-replace. */
			
			bool stringize = false, concat = false, leading_token = false, trailing_space = false;
			pp_token const *leading_space;
			
			leading_space = skip_space( pen ); // replacement list doesn't end with whitespace
			if ( * pen == pp_constants::concat || * pen == pp_constants::concat_alt ) {
				leading_space = nullptr;
			} else if ( ( * pen != pp_constants::stringize && * pen != pp_constants::stringize_alt )
						|| ! function_like ) { // lead is _neither_ ## _nor_ #
				if ( acc_limiter.base != acc ) * local ++ = std::move( * -- acc_limiter.base );
				* acc_limiter.base ++ = * pen ++; // entire if-else block only consumes this non-operator token
				if ( pen != def.end() ) trailing_space = skip_space( pen ); // and this ws
				leading_token = true;
			}
			
			if ( pen != def.end() && ( * pen == pp_constants::concat || * pen == pp_constants::concat_alt ) ) {
				skip_space( ++ pen ); // replacement list doesn't end with ##
				concat = true;
			}
			
			if ( function_like // disable # operator for object-like macros
				&& ( ! leading_token || concat ) // exclude case not in above table
				&& pen != def.end() && ( * pen == pp_constants::stringize
									|| * pen == pp_constants::stringize_alt ) ) { //	============== STRINGIZE ===
				
				skip_space( ++ pen ); // replacement list doesn't end with #
				stringize = true;
				if ( ! concat ) { // flush buffered input if this isn't RHS of ##
					if ( acc_limiter.base != acc ) * local ++ = std::move( * -- acc_limiter.base );
					if ( leading_space ) * local ++ = * leading_space;
				}
				auto arg = args_info[ std::find( def.begin(), def.end(), * pen ++ ) - def.begin() ];
				arg.trim_begin();
				arg.trim_end();
				
				try {
					acc_limiter.reset( 1 );
					lexer( '"' );
					for ( auto pen = arg.begin; pen != arg.end; ++ pen ) {
						for ( uint8_t c : pen->s ) {
							if ( ( pen->type == pp_token_type::string || pen->type == pp_token_type::char_lit )
								&& ( c == '"' || c == '\\'
									|| c >= 0xC0 || ( c < 0x80 && ! char_in_set( char_set::basic_source, c ) ) ) ) {
								lexer( '\\' ); // escape quotes and backslashes in strings, including hidden in UCNs
							}
							lexer( c );
						}
					}
					lexer( '"' );
					finalize( lexer );
				} catch ( std::range_error & ) {
					goto stringize_wrong_count;
				}
				if ( acc_limiter.reset() != 1 || acc_limiter.base[-1].type != pp_token_type::string )
				stringize_wrong_count:
					throw error( arg.begin[ 0 ].p, "Stringize (#) did not produce one (1) result string." );
			}
			
			if ( concat ) { //					=================================================== CONCATENATE ===
				pp_token const *lhs = & acc[0], *lhs_end, *rhs = &* pen, *rhs_end;
				
				for ( int side = 0; side < 2; ++ side ) { // identify arguments:
					pp_token const *&arg_begin = side? rhs : lhs, *&arg_end = side? rhs_end : lhs_end;
					
					if ( side? stringize : ! leading_token ) { // intermediate result,
						arg_begin = & acc[ side ];
						arg_end = arg_begin + 1;
						if ( arg_begin->s.empty() ) arg_end = arg_begin; // handle placemarkers as empty args
					} else {
						auto param = std::find( def.begin(), def.begin() + args_info.size(), * arg_begin );
						if ( param == def.begin() + args_info.size() ) { // literal,
							arg_end = arg_begin + 1;
						} else { // or parameter
							arg_info arg = args_info[ param - def.begin() ];
							side? arg.trim_begin() : arg.trim_end(); // strip ws from end of lhs and start of rhs
							arg_begin = &* arg.begin;
							arg_end = &* arg.end;
						}
						if ( side == 1 ) {
							++ pen; // if value of pen was used for RHS, increment to next token
						}
					}
				}
				if ( leading_space ) * local ++ = * leading_space;
				
				acc_limiter.base = & acc[ 1 ]; // tentatively set the result stack for these early exits:
				if ( lhs == lhs_end ) {
					if ( rhs == rhs_end ) { // both sides are empty; produce a placemarker token
						acc[ 0 ] = pp_constants::placemarker;
						continue;
					}
					std::copy( rhs, rhs_end - 1, local ); // LHS is empty; output the RHS
					acc[ 0 ] = rhs_end[ -1 ];
					continue;
				}
				std::copy( lhs, lhs_end - 1, local ); // flush the tokens before the result
				
				if ( rhs == rhs_end ) { // RHS is empty; already sent the LHS
					acc[ 0 ] = lhs_end[ -1 ]; // just save the last token as intermediate result
					continue;
				}
				
				try {
					acc_limiter.reset( 1 );
					acc_limiter.base = acc; // perform the paste into the intermediate result register
					for ( uint8_t c : util::copy( lhs_end[ -1 ].s ) ) lexer( c ); // work with copies to be
					for ( uint8_t c : util::copy( rhs[ 0 ].s ) ) lexer( c ); // sure overwriting is safe
					finalize( lexer );
				} catch ( std::range_error & ) {
					goto catenate_wrong_count;
				}
				if ( acc_limiter.reset() != 1 ) catenate_wrong_count:
					throw error( rhs[ 0 ].p, "Catenating tokens did not produce a single result token." );
				
				acc[ 0 ].p = lhs_end[ -1 ].p; // use LHS for position of pasted token
				
				if ( rhs + 1 != rhs_end ) {
					* local ++ = std::move( acc[ 0 ] ); // flush if not an intermediate result
					std::copy( rhs + 1, rhs_end - 1, local );
					acc[ 0 ] = rhs_end[ -1 ]; // get a new intermediate result from RHS
				}
			
			} else if ( leading_token ) { //	=================================================== SUBSTITUTE ===
				if ( leading_space ) * local ++ = * leading_space;
				-- acc_limiter.base; // don't leave intermediate result
				if ( trailing_space ) -- pen; // will be next iteration's leading_space
				
				auto param = std::find( def.begin(), def.begin() + args_info.size(), acc[ 0 ] );
				if ( param == def.begin() + args_info.size() ) {
					* local ++ = std::move( acc[ 0 ] ); // common case - copy from replacement list to output
				} else {
					arg_info arg = args_info[ param - def.begin() ];
					
					std::copy( args_evaluated.begin() + arg.evaluated_begin,
								args_evaluated.begin() + arg.evaluated_end, local );
				}
			}
		}
		if ( acc_limiter.base != acc ) * local ++ = std::move( acc[0] );
		callers.pop_back();
	}
};

std::pair< std::string const, pp_tokens > normalize_macro_definition( pp_tokens &&input ) {
	using pp_constants::skip_space;
	
	pp_tokens::iterator pen = input.begin();
	skip_space( pen, input.end() ); // skip directive "define"
	
	if ( skip_space( ++ pen, input.end() ) == input.end() || pen->type != pp_token_type::id ) {
		if ( pen == input.end() ) -- pen;
		throw error( pen->p, "Expected macro definition." );
	}
	std::string name = std::move( pen ++ ->s );
	if ( std::binary_search( pp_constants::reserved_macro_names, std::end( pp_constants::reserved_macro_names ),
		name ) ) throw error( pen[-1].p, "Cannot define a reserved identifier (§17.6.4.3.1/2, 17.6.4.3.2/1)" );
	
	pp_tokens replacement;
	size_t nargs = 0;
	if ( pen != input.end() && pen->type != pp_token_type::ws ) {
		if ( * pen != pp_constants::lparen )
			throw error( pen->p, "An object-like macro definition must begin with whitespace." );
		if ( skip_space( ++ pen, input.end() ) == input.end() ) goto parameters_unterminated;
		if ( * pen == pp_constants::rparen ) goto parameters_done;
		for ( ; skip_space( pen, input.end() ) != input.end(); ++ pen ) {
			if ( * pen == pp_constants::variadic_decl ) {
				replacement.push_back( pp_constants::variadic );
				replacement.back().p = pen->p;
				
				if ( skip_space( ++ pen, input.end() ) == input.end()
					|| * pen != pp_constants::rparen ) {
					if ( pen == input.end() ) -- pen;
					throw error( replacement.back().p,
						"Variadic arguments must be declared as \"...\" at the end "
						"of the parameter list, and then referred to as \"__VA_ARGS__\"." );
				}
				goto parameters_done;
			}
			if ( pen->type != pp_token_type::id )
				throw error( pen->p, "Parameter name must be an identifier." );
			if ( * pen == pp_constants::variadic )
				throw error( pen->p, "The identifier __VA_ARGS__ is reserved (§16.3/5)." );
			replacement.push_back( std::move( * pen ) );
			if ( skip_space( ++ pen, input.end() ) == input.end() ) parameters_unterminated:
				throw error( pen[ -1 ].p, "Unterminated parameter list." );
			if ( * pen == pp_constants::rparen ) goto parameters_done; // NORMAL EXIT POINT
			if ( * pen != pp_constants::comma )
				throw error( pen->p, "Expected \",\" to separate argument declarations" );
		} goto parameters_unterminated;
	parameters_done:
		replacement.push_back( std::move( * pen ++ ) ); // save ")"
		nargs = replacement.size(); // includes ")" so nonzero if function-like
		
	} else replacement.push_back( pp_constants::space );
	
	bool got_stringize = false;
	while ( skip_space( pen, input.end() ) != input.end() ) {
		if ( got_stringize && nargs // validate stringize operator
			&& std::find( replacement.begin(), replacement.begin() + nargs - 1, * pen )
				== replacement.begin() + nargs - 1 ) throw error( pen->p,
				"Stringize (#) operator in a function-like macro may only apply to an argument." );
		got_stringize = * pen == pp_constants::stringize || * pen == pp_constants::stringize_alt;
		
		if ( * pen == pp_constants::variadic // validate variadic usage
			&& ( nargs < 2 || replacement[ nargs - 2 ] != pp_constants::variadic ) )
			throw error( pen->p, "The identifier __VA_ARGS__ is reserved (§16.3/5)." );
		
		replacement.push_back( std::move( * pen ++ ) );
		if ( pen != input.end() && pen->type == pp_token_type::ws ) {
			replacement.push_back( std::move( * pen ++ ) ); // condense whitespace
		}
	}
	
	if ( replacement[ nargs? nargs : 1 ] == pp_constants::concat // handle object- or function-like
		|| replacement[ nargs? nargs : 1 ] == pp_constants::concat_alt
		|| replacement.back() == pp_constants::concat || replacement.back() == pp_constants::concat_alt )
		throw error( replacement.back().p, "Concatenation operator may not appear at beginning or end of a macro." );
	
	return { name, replacement };
}

template< typename output_iterator >
class macro_filter {
	output_iterator cont;
public:
	template< typename ... args >
	macro_filter( args && ... a )
		: cont( std::forward< args >( a ) ... ) {}
	
	void operator() ( pp_token &&in )
		{ if ( ! in.s.empty() ) * cont ++ = std::move( in ); } // Filter out placemarkers and recursion stops.
	
	friend void finalize( macro_filter &o ) { finalize( o.cont ); }
};

template< typename output_iterator >
class substitution_phase
	: public macro_context< util::output_iterator_from_functor< macro_filter< output_iterator > > > {
	typedef macro_context< util::output_iterator_from_functor< macro_filter< output_iterator > > > engine_type;
protected:
	typename engine_type::name_map macros;
	typename engine_type::call_stack callers; // somewhat hackish, but useful when phase 4 instantiates a local engine
	
public:
	template< typename ... args >
	substitution_phase( typename engine_type::name_map &&init_macros, args && ... a )
		: engine_type( macros, callers, std::forward< args >( a ) ... ),
		macros( std::move( init_macros ) ) {}
	
	friend void finalize( substitution_phase &o ) { finalize( static_cast< engine_type & >( o ) ); }
};

}
