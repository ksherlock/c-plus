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
	struct presumptions { // Specific to current file. Caller's info erroneously used if macro invocation spans EOF.
		token filename; // may be set by #line
		std::int32_t line_displacement; // controlled by #line directive
	} presumed;
	
	mutable std::vector< token /*const - GCC workaround */ > callers;
};

template< typename output_iterator > // either another macro_context (via std::function) or a macro_filter
class macro_context : public stage< output_iterator > {
	typedef macro_context_info::name_map name_map;
	macro_context_info const &common; // points to sibling subobject of phase4 next to root context

	name_map::const_pointer definition; // function-like macro awaiting call
protected:
	tokens input; // buffered arguments or scratch storage
	std::size_t paren_depth; // determines when to call macro

public:
	template< typename ... args >
	macro_context( macro_context_info const &common_info, args && ... a )
		: macro_context::stage( std::forward< args >( a ) ... ), common( common_info ),
		definition( nullptr ), input{ pp_constants::placemarker }, paren_depth( 0 ) {}
	
	void flush() {
		replace_object_completely();
		if ( ! this->template diagnose< diagnose_policy::pass, error >( paren_depth != 0,
			util::make_implicit_thunk( [&]{ return input[ input.front().type == token_type::ws ]; } ), "Unterminated macro invokation." ) ) {
			this->template diagnose< diagnose_policy::pass, error >( input.size() > 3 /* space, name, space */, input.back(), "ICE: too much input at flush." );
		}
		this->pass( std::make_move_iterator( input.begin() ), std::make_move_iterator( input.end() ) );
		input.clear();
		definition = nullptr;
	}
	
	void operator() ( token const &in ) // copy input if not called with std::move
		{ return (*this) ( util::val( in ) ); }
	
	/*	operator() trims whitespace, buffers input, and calls macros. It doesn't trim recursion stops.
		replace() sends results back to it by co-recursion, which implements rescanning and further replacement. */
	void operator() ( token &&in ) {
		bool stop_recursion = false; // perform this check before argument gets moved
		if ( in.type == token_type::id ) {
			stop_recursion = std::find( common.callers.begin(), common.callers.end(), in ) != common.callers.end();
		}
		if ( paren_depth == 0 ) {
			if ( definition ) {
				if ( in == pp_constants::recursion_stop ) {
					definition = nullptr; // Abort macro call. Recursion_stop token will be passed through.
				}
				replace_object_completely(); // Call any object-like macro. May set definition to a function-like macro.
				
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
						this->pass( std::make_move_iterator( input.begin() ), std::make_move_iterator( flush_end ) );
						input.erase( input.begin(), flush_end );
					}
					definition = &* def;
					input.push_back( std::move( in ) ); // Remember name in case macro isn't called.
				} else {
					flush(); // Clear any uncalled function, or just flush whitespace. No object macro here.
					this->pass( std::move( in ) ); // Common case: not in any part of an invokation, pass through.
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
				if ( paren_depth == 0 ) replace();
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
			return (*this)( pp_constants::recursion_stop ); // tail recurse, insert artificial input
		}
	}
	
protected:
	static std::size_t line_number( token const &t ) {
		std::size_t line = 1;
		auto location = t.get_location< input_source >();
		std::get< 0 >( location ).filter( [ &line, &location ]( std::uint8_t c ) {
			if ( std::get< 1 >( location ) == 0 ) return;
			-- std::get< 1 >( location );
			line += c == '\n';
		} );
		return line;
	}

private:
	template< typename i > // either tokens::iterator or tokens::const_iterator
	static typename std::iterator_traits< i >::pointer skip_space( i &it )
		{ return it->type == token_type::ws? &* it ++ : nullptr; }
	
	static bool is_function_like( name_map::const_reference definition )
		{ return definition.second[0].type != token_type::ws; }
	
	void replace_object_completely()
		{ while ( definition && ! is_function_like( * definition ) ) replace(); }
	
	void replace() {
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
		bool invocation_leading_space = skip_space( input_pen );
		common.callers.push_back( std::move( * input_pen ) ); // Register in call stack before rescanning.
		CPLUS_FINALLY( common.callers.pop_back(); )
		if ( function_like ) skip_space( ++ input_pen ); // ignore space between name and paren
		
		// save macro name and arguments, or special replacement list, in persistent instantiation object
		auto inst = definition->first == pp_constants::line_macro.s || definition->first == pp_constants::file_macro.s?
			std::make_shared< macro_replacement >( common.callers.front(), tokens{ pp_constants::space,
				definition->first == pp_constants::line_macro.s? // line number has no location as it is generated ex nihilo
				token{ token_type::num, string{ std::to_string( line_number( common.callers.front() ) ).c_str(), common.token_config.stream_pool } }
				: common.presumed.filename, pp_constants::placemarker
			} )
		: std::make_shared< macro_replacement >( common.callers.front(), definition->second,
			tokens{ std::move_iterator< tokens::iterator >{ input_pen }, std::move_iterator< tokens::iterator >{ input.end() } } );
		
		input.erase( input.begin() + invocation_leading_space, input.end() ); // All input has been moved from except space acc.
		definition = nullptr; // Clear the "registers" to avoid confusing rescanning.
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
			if ( this->template diagnose< diagnose_policy::pass, error >( info != args_info.end(), * arg_pen, "Too few arguments to macro." ) ) {
				std::fill( info, args_info.end(), arg_info{ arg_pen, arg_pen } );
			}
			if ( arg_pen != inst->args.end() - 1 ) {
				if ( args_info.size() == 0 ) {
					skip_space( ++ arg_pen );
					this->template diagnose< diagnose_policy::pass, error >( arg_pen != inst->args.end() - 1, * arg_pen, "Macro does not accept any argument." );
				} else if ( ! this->template diagnose< diagnose_policy::pass, error >( // If there isn't an error due to not being a variadic macro, handle variadic list.
					def[ args_info.size() - 1 ] != pp_constants::variadic, * arg_pen, "Too many arguments to macro." ) ) {
					-- info;
					goto extend_variadic; // continue from where the comma broke the loop
				}
			}
		}
		
		// Use a local copy of phase 3 to generate tokens one at a time as intermediate results.
		token acc[ 2 ], *acc_pen = acc; // [0] is LHS, [1] is RHS from stringize or LHS recursion stop
		bool acc_full = false;
		auto acc_pass = std::function< void( token && ) >( [&]( token && t ) {
			if ( t.type == token_type::ws ) return; // Discard trailing newline from tokenizer flush. Would be nice to trap other whitespace.
			
			CPLUS_FINALLY ( acc_full = true; )
			if ( ! acc_full ) ++ acc_pen;
			else this->pass( std::move( acc_pen[ -1 ] ) ); // Passing ahead of the overflow diagnostic preserves consistency between throwing and not.
			acc_pen[ -1 ] = std::move( t );
			this->template diagnose< diagnose_policy::pass, error >( acc_full, t, "Token catenation (##) did not produce only one result token." );
			CPLUS_DO_FINALLY
		} );
		
		token const *leading_space = nullptr;
		for ( auto pen = def.begin() + args_info.size() + 1 /* skip ")" or " " */; pen != def.end(); leading_space = &* pen - 1  ) {
			/* Each iteration handles some subset of the sequence <token> ## # <token>:
							leading_token			! leading_token
				neither		<lhs>					-
				stringize	-						# <lhs>
				concat		<lhs> ## <rhs>			## <rhs>
				both		<lhs> ## # <rhs>		## # <rhs>
				This is handled as: Perhaps stringize, perhaps catenate, else macro-replace. */
			
			bool stringize = false, concat = false, leading_token = false;
			
			if ( pp_constants::is_concat( * pen ) ) {
				leading_space = nullptr; // ## operator consumes space
			} else if ( ! pp_constants::is_stringize( * pen ) || ! function_like ) { // lead is _neither_ ## _nor_ #
				pass( std::make_move_iterator( acc ), std::make_move_iterator( acc_pen ), (*this) );
				acc[ 1 ] = token(); // be sure to invalidate recursion stop in acc[1]
				acc_pen = acc;
				* acc_pen ++ = instantiate_component( inst, pen - def.begin() );
				pen += 2; // Consume this non-operator token and advance past ws (may return to it later).
				leading_token = true;
			}
			
			if ( pen != def.end() && pp_constants::is_concat( * pen ) ) {
				pen += 2; // Replacement list doesn't end with ## or the space following ##.
				concat = true;
			}
			
			if ( function_like // disable # operator for object-like macros
				&& ( ! leading_token || concat ) // exclude case not in above table
				&& pen != def.end() && pp_constants::is_stringize( * pen ) ) { // =================== STRINGIZE ===
				
				pen += 2; // replacement list doesn't end with # or the space following #
				stringize = true;
				if ( ! concat ) { // flush buffered input if this isn't RHS of ##
					pass( std::make_move_iterator( acc ), std::make_move_iterator( acc_pen ), (*this) );
					acc[ 1 ] = token(); // be sure to invalidate recursion stop in acc[1]
					if ( leading_space ) (*this)( * leading_space );
				}
				acc_pen = acc + concat; // discard and overwrite possible recursion stop
				
				auto arg = args_info[ std::find( def.begin(), def.end(), * pen ) - def.begin() ];
				arg.trim_begin();
				arg.trim_end();
				
				string s( 1, '\"' );
				for ( auto pen = arg.begin; pen != arg.end; ++ pen ) {
					if ( pen->type == token_type::ws ) {
						if ( token_semantic_equal( * pen, pp_constants::space ) ) s += ' ';
						continue;
					}
					for ( std::uint8_t c : pen->s ) {
						if ( pen->type == token_type::string_lit || pen->type == token_type::char_lit ) {
							if ( c == '"' || c == '\\' // escape quotes and backslashes in strings, incl. hidden in UCNs
								|| c >= 0xC0 || ( c < 0x80 && ! char_in_set( char_set::basic_source, c ) ) ) {
								s += '\\';
							}
							if ( this->template diagnose< diagnose_policy::pass, error >( c == '\n', * pen,
								"Raw string contains a newline, which is invalid in the non-raw result of the # operator." ) ) {
								s += "\\\\n";
								continue;
							}
						}
						s += c;
					}
				}
				s += '\"';
				
				acc_full = false;
				instantiate( std::make_shared< raw_text >( s,
					instantiate_component( std::make_shared< macro_substitution >( std::move( * pen ), arg.begin, arg.end ), 0 )
				), pile< phase3 >( common.token_config, acc_pass ) );
				pen += 2; // consume argument of #
				
				if ( this->template diagnose< diagnose_policy::pass, error >( ! acc_full || acc_pen[-1].type != token_type::string_lit,
					arg.begin[ 0 ], "Stringize (#) did not produce a valid result string (§16.3.2/2)." ) ) { // How to test this? http://stackoverflow.com/q/17416256/153285
					if ( ! acc_full ) * acc_pen ++ = pp_constants::placemarker;
				}
			}
			
			if ( concat ) { //													=================== CONCATENATE ===
				if ( ! stringize ) {
					acc[ 1 ] = instantiate_component( inst, pen - def.begin() ); // if value of pen is used for RHS, increment to next token
					pen += 2;
				}
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
					
					instantiate( std::make_shared< macro_substitution >( std::move( acc[ side ] ), arg.begin, arg.end ), // use new instantiation obj
						[&]( token t ) { copies[ side ].push_back( std::move( t ) ); } ); // instantiate argument for use of parameter
					begins[ side ] = &* copies[ side ].begin(); // Tentatively strip recursion stops; restore if no catenation is done:
					stripped_stops[ side ] = ! copies[ side ].empty() && copies[ side ].back() == pp_constants::recursion_stop;
					ends[ side ] = &* copies[ side ].end() - stripped_stops[ side ];
				}
				
				if ( leading_space ) (*this)( * leading_space ); // preserve leading space
				
				acc_pen = acc; // perform the paste into the intermediate result register
				if ( begins[lhs] == ends[lhs] ) {
					if ( begins[rhs] == ends[rhs] ) { // both sides are empty; produce a placemarker token
						* acc_pen ++ = pp_constants::placemarker;
						continue;
					}
					pass( begins[rhs], ends[rhs] - 1, (*this) ); // LHS is empty; output the RHS
					* acc_pen ++ = std::move( ends[rhs][ -1 ] ); // just save the last token as intermediate result
					if ( ! stringize && stripped_stops[rhs] ) {
						* acc_pen ++ = pp_constants::recursion_stop; // preserve stopper
					}
					continue;
				}
				pass( begins[lhs], ends[lhs] - 1, (*this) ); // flush the tokens preceding the result
				
				if ( begins[rhs] == ends[rhs] ) { // RHS is empty; already sent the LHS
					* acc_pen ++ = std::move( ends[lhs][ -1 ] );
					if ( stripped_stops[lhs] ) {
						* acc_pen ++ = pp_constants::recursion_stop; // preserve stopper
					}
					continue;
				}
				
				acc_full = false;
				instantiate( std::make_shared< raw_text >( ends[lhs][ -1 ].s + begins[rhs][ 0 ].s, ends[lhs][ -1 ] ), 
					pile< phase3 >( common.token_config, acc_pass ) );
				if ( this->template diagnose< diagnose_policy::pass, error >( ! acc_full, begins[rhs][ 0 ], "Token catenation (##) did not produce any result token." ) ) {
					* acc_pen ++ = pp_constants::placemarker;
				}
				while ( ++ begins[rhs] != ends[rhs] && * begins[rhs] == pp_constants::recursion_stop ) ; // re-evaluate recursion
				
				if ( begins[rhs] != ends[rhs] ) {
					(*this)( std::move( acc[ 0 ] ) ); // flush if not an intermediate result
					pass( begins[rhs], ends[rhs] - 1, (*this) );
					acc[ 0 ] = ends[rhs][ -1 ]; // get a new intermediate result from end of RHS
					if ( stripped_stops[rhs] ) {
						* acc_pen ++ = pp_constants::recursion_stop; // preserve stopper
					}
				}
			
			} else if ( leading_token ) { //						================================ SUBSTITUTE ===
				if ( leading_space ) (*this)( * leading_space );
				-- acc_pen; // don't leave intermediate result
				
				auto param = std::find( def.begin(), def.begin() + args_info.size(), acc[ 0 ] );
				if ( param == def.begin() + args_info.size() ) {
					(*this)( std::move( acc[ 0 ] ) ); // common case - copy from replacement list to output
				} else {
					auto caller_self = common.callers.back();
					common.callers.pop_back(); // evaluate argument in caller's context, i.e. current name is not recursion-
					CPLUS_FINALLY( common.callers.push_back( caller_self ); )
					
					arg_info arg = args_info[ param - def.begin() ];
					instantiate( std::make_shared< macro_substitution >( std::move( acc[ 0 ] ), arg.begin, arg.end ),
						pile< macro_context >( common, std::function< void( token && ) >( [&]( token &&in ) {
							common.callers.push_back( caller_self ); // restore current context for rescanning
							CPLUS_FINALLY( common.callers.pop_back(); )
							(*this)( std::move( in ) );
							CPLUS_DO_FINALLY
						} ) )
					 );
					
					CPLUS_DO_FINALLY
				}
			}
		}
		pass( std::make_move_iterator( acc ), std::make_move_iterator( acc_pen ), (*this) );
		replace_object_completely();
		CPLUS_DO_FINALLY
	}
};

macro_context_info::name_map::value_type normalize_macro_definition( tokens input, string_pool &macro_pool ) {
	using namespace pp_constants;
	
	tokens::iterator pen = input.begin(); // skip leading space in input buffer and directive "define"
	skip_space( pen, input.end() );
	
	if ( skip_space( ++ pen, input.end() ) == input.end() || pen->type != token_type::id || * pen == variadic ) {
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
				replacement.push_back( pen->assign_content( variadic ) );
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
		
	} else {
		replacement.push_back( space ); // synthesize a space if list is truly empty
		replacement.back().construct::operator = ( pen == input.end()? pen[ -1 ] : * pen );
	}
	
	if ( skip_space( pen, input.end() ) != input.end() && is_concat( * pen ) ) bad_cat:
		throw error( * pen, "Concatenation (##) operator may not appear at beginning or end of a macro (§16.3.3/1)." );
	
	skip_space( pen, input.end() ); // Discard whitespace before replacement list, unconditionally.
	
	bool got_stringize = false;
	while ( pen != input.end() ) {
		if ( got_stringize && nargs // validate stringize operator
			&& std::find( replacement.begin(), replacement.begin() + nargs - 1, * pen )
				== replacement.begin() + nargs - 1 ) throw error( * pen,
				"Stringize (#) operator in a function-like macro may only apply to an argument (§16.3.2/1)." );
		got_stringize = is_stringize( * pen );
		
		if ( * pen == variadic // validate variadic usage
			&& ( nargs < 2 || replacement[ nargs - 2 ] != variadic ) )
			throw error( * pen, "The identifier __VA_ARGS__ is reserved (§16.3/5)." );
		
		replacement.push_back( pen ++ ->reallocate( macro_pool ) );
		
		if ( pen == input.end() || pen->type != token_type::ws ) {
			replacement.push_back( placemarker );
			replacement.back().construct::operator = ( replacement.end()[ -2 ] );
			continue;
		}
		replacement.push_back( pen ++ ->reallocate( macro_pool ) );
		while ( pen != input.end() && pen->type == token_type::ws ) {
			replacement.back().s += pen ++ ->s; // condense whitespace
		}
		if ( pen == input.end() ) replacement.back().assign_content( placemarker ); // discard whitespace after replacement list
	}
	
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
		{ if ( ! in.s.empty() ) this->pass( std::move( in ) ); } // Filter out placemarkers and recursion stops.
};

template< typename output_iterator >
class substitution_phase
	: public derived_stage< macro_context< macro_filter< output_iterator > >, phase3_config >,
	protected macro_context_info {
public:
	template< typename ... args >
	substitution_phase( macro_context_info::name_map &&init_macros, args && ... a )
		: substitution_phase::derived_stage( static_cast< macro_context_info & >( * this ), std::forward< args >( a ) ... ),
		macro_context_info{ this->get_config(), std::move( init_macros ), { pp_constants::empty_string } } {}
};

}
