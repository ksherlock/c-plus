// Cplus project, templates relating to dataflow, configuration, and compiler construction in general.
// copyright David Krauss, created 9/17/11

#ifndef CPLUS_FRAMEWORK
#define CPLUS_FRAMEWORK

#include "util.h"

#include <initializer_list>
#include <functional>
#include <stdexcept>
#include <typeindex>
#include <cstdint>
#include <utility>
#include <vector>
#include <memory>
#include <tuple>
#include <map>

#include <cassert>

#ifndef CPLUS_USE_STD_STRING
#include "string.h"

#else
#include <string>

namespace cplus {
using std::string;

struct string_pool : std::allocator< char >
	{ string_pool( char const * ) {} };
string_pool literal_pool( "" );
string repool( string s, string_pool p ) { return { s.c_str(), p }; }
}

#endif

namespace cplus {

struct stage_base {};

// Adaptor for deriving one stage from another and using base's output iterator.
template< typename base_type, typename ... config_types >
struct derived_stage : base_type {
	typedef base_type base;
	typedef std::tuple< config_types ... > used_configs; // not to be instantiated, just a pack container

	void flush() {}
	
protected:
	using base::base;
};

template< typename exception_type, typename output_iterator, typename ... args >
auto pass_or_throw( output_iterator & o, args && ... a )
	-> typename std::conditional< true, void, decltype( * o ++ = std::declval< exception_type >() ) >::type
	{ * o ++ = exception_type{ std::forward< args >( a ) ... }; }

template< typename exception_type, typename ... args >
void pass_or_throw( util::poor_conversion, args && ... a )
	{ throw exception_type{ std::forward< args >( a ) ... }; }

// Non-virtual abstract base class.
template< typename output_iterator, typename ... config_types >
struct stage : derived_stage< stage_base, config_types ... > {
	typename std::conditional< util::is_output_iterator< output_iterator >::value, output_iterator,
								util::output_iterator_from_functor< output_iterator > >::type
		cont;
	
protected:
	template< typename ... args >
	stage( args && ... a )
		: cont( std::forward< args >( a ) ... ) {}
	stage( stage && ) = delete;
	stage( stage const & ) = delete;
	
	template< typename exception_type, typename ... args >
	void pass_or_throw( args && ... a )
		{ cplus::pass_or_throw< exception_type >( cont, std::forward< args >( a ) ... ); }
};

void finalize( util::poor_conversion const & ) {} // fallback overload, worse than derived-to-base conversion

template< typename s >
typename std::enable_if< ! std::is_same< typename s::base, stage_base >::value >::type
finalize( s &o ) {
	o.flush();
	finalize( static_cast< typename s::base & >( o ) );
}

/*	Each pipeline step chains to finalize for the next one. When an iterator without
	finalize is reached, synchronization stops. That iterator should be the last. */
template< typename s >
typename std::enable_if< std::is_same< typename s::base, stage_base >::value >::type
finalize( s &o ) {
	o.flush();
	finalize( o.cont );
}

template< typename t >
void finalize( std::reference_wrapper< t > const & ) {} // Do not propagate termination through references.

typedef std::uint64_t location_t;
struct token {
	int type;
	string s;
	std::shared_ptr< struct instantiation > source; // need to convert to GC
	location_t location; // position in instantiation, meaning depends on type of instantiation
	
	void assign_content( token const &rhs )
		{ type = rhs.type; s = rhs.s; }
	token reallocate( string_pool &pool ) const
		{ return { type, repool( s, pool ), source, location }; }
	
	friend bool operator== ( token const &l, token const &r )
		{ return l.type == r.type && l.s == r.s; }
	friend bool operator!= ( token const &l, token const &r )
		{ return ! ( l == r ); }
};
typedef std::vector< token > tokens;

// describes a source file inclusion, macro expansion, or template instantiation
struct instantiation : util::abc {
	typedef std::shared_ptr< instantiation > pointer;
	token const source; // target location for output messages
	explicit instantiation( token const &in_source ) : source( in_source ) {}
};

// Error reporting format.
struct error : std::runtime_error {
	token p;
	error( token const &pos, char const *what )
		: std::runtime_error( what ), p( pos ) {}
};

template< template< typename ... > class stage, typename ... args >
struct bind_stage {
	template< typename cont >
	using temp = stage< cont, args ... >;
};

template< typename output_iterator, typename functor_type >
struct terminal_stage : functor_type, stage< output_iterator > {
	template< typename in_functor_type, typename ... args >
	terminal_stage( in_functor_type && in_fn, args && ... a )
		: functor_type( std::forward< in_functor_type >( in_fn ) ), terminal_stage::stage( std::forward< args >( a ) ... ) {}
	
	terminal_stage( terminal_stage && o )
		: functor_type( std::move( o ) ), terminal_stage::stage( std::move( o.cont ) ) {}
};

template< typename output_iterator, typename functor_type >
terminal_stage< output_iterator, functor_type >
make_terminal_stage( functor_type && in_fn, output_iterator && in_cont )
	{ return { std::move( in_fn ), std::move( in_cont ) }; }

#if 0 // use if needed
template< typename output_iterator, typename functor_type, typename ... args >
terminal_stage< output_iterator, functor_type >
make_terminal_stage( functor_type && in_fn, args && ... a )
	{ return { std::move( in_fn ), std::forward< args >( a ) ... }; }
#endif

// Stage parameterization and pragma distribution.
struct config_base : util::abc {
	config_base() = default;
	config_base( config_base const & ) = delete;
};
struct config_manager_base {};

template< typename output_iterator >
class config_manager : public stage< output_iterator >, config_manager_base {
protected:
	std::map< std::type_index, std::unique_ptr< config_base > > registry;
public:
	template< typename config_type >
	config_type &get_config() {
		auto entry = registry.find( typeid( config_type ) );
		if ( entry == registry.end() ) {
			entry = registry.insert( std::make_pair( std::type_index( typeid( config_type ) ), std::unique_ptr< config_base >( new config_type() ) ) ).first; // config struct gets value initialized
		}
		return static_cast< config_type & >( * entry->second );
	}
	
	template< typename ... args >
	config_manager( args && ... a )
		: config_manager::stage( std::forward< args >( a ) ... ) {}
	
	template< typename t >
	typename std::enable_if< util::cont_is_assignable< config_manager, t >::value >::type
	operator() ( t &&in ) { * this->cont ++ = std::forward< t >( in ); }
};

template< typename ftor, typename = void >
struct config_traits
	{ typedef std::tuple<> used_configs; };

template< typename ftor >
struct config_traits< ftor, typename std::conditional< true, void, typename ftor::used_configs >::type >
	{ typedef typename ftor::used_configs used_configs; };

template< typename ftor, typename used_configs = typename config_traits< ftor >::used_configs >
struct configured_stage_from_functor;

template< typename ftor, typename ... used_configs >
struct configured_stage_from_functor< ftor, std::tuple< used_configs ... > >
	: util::output_iterator_from_functor< ftor > {
	template< typename ... args >
	configured_stage_from_functor( args && ... a )
		: util::output_iterator_from_functor< ftor >(
			/*	Since the config_manager is likely in a base subobject, use lazy evaluation so it's called
				inside the ftor's constructor, after base initialization. */
			util::make_implicit_thunk( std::bind(
				static_cast< used_configs & (configured_stage_from_functor::*) () >
					( &configured_stage_from_functor::get_config< used_configs > ), this ) ) ...,
			std::forward< args >( a ) ... ) {}
	
	template< typename client >
	typename std::enable_if< ! std::is_base_of< config_manager_base, ftor >::value, client & >::type
	get_config() { return this->cont.template get_config< client >(); }
	
	template< typename client >
	typename std::enable_if< std::is_base_of< config_manager_base, ftor >::value, client & >::type
	get_config() { return ftor::template get_config< client >(); }
};

template< typename ftor >
struct configured_stage_from_functor< std::reference_wrapper< ftor >, std::tuple<> >
	: util::output_iterator_from_functor< std::reference_wrapper< ftor > > {
	configured_stage_from_functor( ftor &a )
		: util::output_iterator_from_functor< std::reference_wrapper< ftor > >( a ) {}
	
	template< typename client >
	typename std::enable_if< ! std::is_base_of< config_manager_base, ftor >::value, client & >::type
	get_config() { return this->get().cont.template get_config< client >(); }
	
	template< typename client > // corner case: taking a reference directly to a config manager
	typename std::enable_if< std::is_base_of< config_manager_base, ftor >::value, client & >::type
	get_config() { return this->get().template get_config< client >(); }
};

template< typename cont, template< typename ... > class ... stages >
struct stack_stages;

template< typename cont >
struct stack_stages< cont >
	{ typedef cont type; };

template< typename cont, template< typename ... > class stage, template< typename ... > class ... rem >
struct stack_stages< cont, stage, rem ... >
	{ typedef configured_stage_from_functor< stage< typename stack_stages< cont, rem ... >::type > > type; };

template< template< typename ... > class ... stages, typename cont, typename ... aux >
typename stack_stages< cont, stages ... >::type
autoconfigured_pile( cont && c, aux && ... a )
	{ return { std::forward< aux >( a ) ..., std::forward< cont >( c ) }; }

/*template< typename cont >
cont autoconfigured_pile( cont &&c )
	{ return std::forward< cont >( c ); }

template< template< typename ... > class stage, template< typename ... > class ... rem, typename cont, typename ... aux >
configured_stage_from_functor< stage< decltype( autoconfigured_pile< rem ... >( std::declval< cont >() ) ) > >
autoconfigured_pile( cont && c, aux && ... a )
	{ return { std::forward< aux >( a ) ..., std::forward< cont >( c ) }; }*/

}

#endif
