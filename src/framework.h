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
#include <map>

#ifndef CPLUS_USE_STD_STRING
#include "string.h"

#elif defined( __GNUC__ )
#include <string>
#include <ext/mt_allocator.h>

namespace cplus {
typedef std::basic_string< char, std::char_traits< char >, __gnu_cxx::__mt_alloc< char > > string;

struct string_pool : __gnu_cxx::__mt_alloc< char >
	{ string_pool( char const * ) {} };
string_pool literal_pool( "" );

int stoi( string const &in ) { return stoi( std::string( in ) ); }
}
#else
#include <string>

namespace cplus {
using std::string;

struct string_pool : std::allocator< char >
	{ string_pool( char const * ) {} };
string_pool literal_pool( "" );
}
#endif

namespace cplus {

struct stage_base {};

// Adaptor for deriving one stage from another and using base's output iterator.
template< typename base_type, typename ... config_types >
struct derived_stage : base_type {
	typedef base_type base;
	typedef typename std::is_same< base_type, stage_base >::type base_stage_tag; // whether NOT derived from another stage
	typedef std::tuple< config_types ... > used_configs; // not to be instantiated, just a pack container

protected:
	template< typename ... args >
	derived_stage( args && ... a )
		: base( std::forward< args >( a ) ... ) {}
	derived_stage( derived_stage && ) = delete;
	derived_stage( derived_stage const & ) = delete;
};

template< typename s >
typename std::enable_if< ! s::base_stage_tag::value, void >::type
finalize( s &o ) {
	o.flush();
	finalize( static_cast< typename s::derived_stage::base & >( o ) );
}

// Non-virtual abstract base class.
template< typename output_iterator, typename ... config_types >
struct stage : derived_stage< stage_base, config_types ... > {
	void flush() {}
	
	output_iterator cont;
	
protected:
	template< typename ... args >
	stage( args && ... a )
		: cont( std::forward< args >( a ) ... ) {}
};

/*	Each pipeline step chains to finalize for the next one. When an iterator without
	finalize is reached, synchronization stops. That iterator should be the last. */
template< typename s >
typename std::enable_if< s::base_stage_tag::value, void >::type
finalize( s &o ) {
	o.flush();
	finalize( o.cont );
}

void finalize( util::poor_conversion const & ) {} // fallback overload, worse than derived-to-base conversion

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
		{ return { type, { s, pool }, source, location }; }
	
	friend bool operator== ( token const &l, token const &r )
		{ return l.type == r.type && l.s == r.s; }
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
		auto entry = registry.insert( std::make_pair( std::type_index( typeid( config_type ) ), nullptr ) );
		if ( entry.second ) {
			entry.first->second.reset( new config_type() ); // config struct gets value initialized
		}
		return static_cast< config_type & >( * entry.first->second );
	}
	
	template< typename ... args >
	config_manager( args && ... a )
		: config_manager::stage( std::forward< args >( a ) ... ) {}
	
	template< typename t >
	void operator() ( t &&in ) { * this->cont ++ = std::forward< t >( in ); }
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

}

#endif
