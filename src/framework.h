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

template< typename, typename = void, typename = void * > // by default, a stage does not handle exceptions
struct parameter_exceptions
	{ typedef std::tuple<> type; };

// Specializations for composite types are more specialized, to be selected first and delegate to specializations for constituent types.
template< typename ama > // First handle amalgamations, which are derived from util::amalgam.
struct parameter_exceptions< ama, typename std::enable_if< ! std::is_same< ama, typename ama::amalgam >::value >::type >
	{ typedef typename parameter_exceptions< typename ama::amalgam >::type type; };

template< typename ... base > // A stage which is an amalgamation acts as the union of its bases.
struct parameter_exceptions< util::amalgam< base ... > >
	{ typedef typename util::tuple_cat< typename parameter_exceptions< base >::type ... >::type type; };

template< typename ftor, typename unspecial > // An intermediate stage may handle some exceptions by itself, and delegate others to later stages.
struct parameter_exceptions< ftor, typename util::mention< decltype( & ftor::cont ) >::type, unspecial * > {
	typedef typename util::tuple_cat<
		typename parameter_exceptions< ftor, void, void >::type, // avoid recursion and call more general specialization
		typename parameter_exceptions< decltype( std::declval< ftor >().cont ) >::type >::type type;
};

template< typename ftor, typename unspecial > // A functor with one nonstatic operator() overload taking a derivative of std::exception handles that type.
struct parameter_exceptions< ftor,
	typename std::enable_if< std::is_base_of< std::exception,
		typename std::decay< typename std::reference_wrapper< decltype( & ftor::operator() ) >::second_argument_type >::type
	>::value >::type, unspecial >
	{ typedef std::tuple< typename std::reference_wrapper< decltype( & ftor::operator() ) >::second_argument_type > type; };

template< typename ftor, typename unspecial > // An object with a parameter_exceptions member uses it as an explicit spec. It cannot satisfy the previous specialization.
struct parameter_exceptions< ftor, typename util::mention< typename ftor::parameter_exceptions >::type, unspecial >
	{ typedef typename ftor::parameter_exceptions type; };

// pass() puts input into a stage (functor or iterator) or its succeeding stages, catches thrown exceptions and feeds them back in.
template< typename tag = struct tag, typename oit, typename v >
auto pass( oit it, v && val )
	-> typename util::mention< decltype( * it ++ = std::forward< v >( val ) ) >::type
	{ * it ++ = std::forward< v >( val ); }

template< typename tag = struct tag, typename fn, typename v >
typename util::mention< typename std::result_of< fn( v ) >::type >::type
pass( fn && obj, v && val );

template< bool en = false > // Dummy overload, may be disabled by using pass< tag >() call.
struct bad_pass *pass( util::poor_conversion, util::poor_conversion ) { static_assert( en, "invalid pass argument" ); return nullptr; }

template< typename tag = struct tag, typename fn, typename v > // Qualified-id avoids ADL and prevents recursion, tag enables ADL and recursion.
typename std::enable_if< std::is_same< bad_pass *, decltype( cplus::pass( std::declval< fn >(), std::declval< v >() ) ) >::value,
	 typename util::mention< decltype( pass< tag >( std::declval< fn >().cont, std::declval< v >() ) ) >::type >::type
pass( fn && obj, v && val )
	{ pass( obj.cont, std::forward< v >( val ) ); }

template< typename fn, typename v >
void pass( fn && obj, v && val, util::mention< std::tuple<> > )
	{ obj( std::forward< v >( val ) ); }

template< typename e, typename ... er, typename fn, typename v >
void pass( fn && obj, v && val, util::mention< std::tuple< e, er ... > > ) {
	try {
		pass( std::forward< fn >( obj ), std::forward< v >( val ), util::mention< std::tuple< er ... > >() );
	} catch ( e & exc ) {
		pass( std::forward< fn >( obj ), std::forward< e >( exc ) ); // May move e. Double exceptions may cause infinite recursion.
	}
}

template< typename, typename fn, typename v >
typename util::mention< typename std::result_of< fn( v ) >::type >::type
pass( fn && obj, v && val )
	{ pass( std::forward< fn >( obj ), std::forward< v >( val ), util::mention< typename parameter_exceptions< typename std::decay< fn >::type >::type >() ); }

template< typename obj, typename iit >
void pass( iit first, iit last, obj && o ) {
	for ( ; first != last; ++ first ) {
		pass( std::forward< obj >( o ), * first );
	}
}

template< typename exception_type, typename output_iterator, typename ... args >
auto pass_or_throw( output_iterator & o, args && ... a )
	-> typename util::mention< decltype( pass< tag >( std::declval< output_iterator >(), std::declval< exception_type >() ) ) >::type
	{ pass( std::forward< output_iterator >( o ), exception_type{ std::forward< args >( a ) ... } ); }

template< typename exception_type, typename ... args >
void pass_or_throw( util::poor_conversion, args && ... a )
	{ throw exception_type{ std::forward< args >( a ) ... }; }

// Adaptor for deriving one stage from another and using base's output iterator.
template< typename base_type, typename ... config_types >
class derived_stage : public base_type {
	std::tuple< config_types const & ... > configs;
	struct tag {};
public:
	typedef base_type base;
	typedef derived_stage stage;

	void flush() {}
	
protected:
	template< typename ... args >
	derived_stage( args && ... a ) : derived_stage( tag(), std::tuple<>(), std::forward< args >( a ) ... ) {}
	
	template< typename ... acc_config, typename in_config, typename ... args >
	derived_stage( typename std::enable_if< sizeof ... ( acc_config ) != sizeof ... ( config_types )
			&& std::is_convertible< in_config, typename util::tuple_element< sizeof ... ( acc_config ), decltype( configs ) >::type >::value, tag >::type,
		std::tuple< acc_config ... > && cacc, in_config && c, args && ... a )
		: derived_stage( tag(), std::tuple_cat( std::move( cacc ), std::forward_as_tuple( std::forward< in_config >( c ) ) ), std::forward< args >( a ) ... ) {}
	
	template< typename ... acc_config, typename in_config, typename ... args >
	derived_stage( typename std::enable_if< sizeof ... ( acc_config ) != sizeof ... ( config_types )
			&& ! std::is_convertible< in_config, typename util::tuple_element< sizeof ... ( acc_config ), decltype( configs ) >::type >::value, tag >::type,
		std::tuple< acc_config ... > && cacc, in_config && c, args && ... a )
		: derived_stage( tag(), std::tuple_cat( std::move( cacc ), std::make_tuple( util::make_implicit_thunk(
			std::bind( & base::template get_config< typename std::tuple_element< sizeof ... ( acc_config ), std::tuple< config_types ... > >::type const >, this ) ) ) ),
			std::forward< in_config >( c ), std::forward< args >( a ) ... ) {}

	template< typename ... acc_config, typename ... args >
	derived_stage( typename std::enable_if< sizeof ... ( acc_config ) == sizeof ... ( config_types ), tag >::type,
		std::tuple< acc_config ... > && cacc, args && ... a )
		: base( std::forward< args >( a ) ... ), configs( std::move( cacc ) ) {}
	
public:
	template< typename client = typename std::tuple_element< 0, typename std::conditional< sizeof ... ( config_types ), std::tuple< config_types const ... >, std::tuple< void > >::type >::type >
	typename std::enable_if< std::is_const< client >::value && util::tuple_index< client &, decltype( configs ) >::value != std::tuple_size< decltype( configs ) >::value, client & >::type
	get_config() { return std::get< util::tuple_index< client &, decltype( configs ) >::value >( configs ); }
	
	template< typename client >
	typename std::enable_if< ( typename util::mention< decltype( std::declval< base >().template get_config< client >() ) >::type(),
		! std::is_const< client >::value || util::tuple_index< client &, decltype( configs ) >::value == std::tuple_size< decltype( configs ) >::value ), client & >::type
	get_config() { return base::template get_config< client >(); }

	template< typename v >
	void pass( v && val ) { cplus::pass( * this, std::forward< v >( val ) ); }

	template< typename iit >
	void pass( iit first, iit last ) { cplus::pass( first, last, * this ); }

	template< typename exception_type, typename ... args >
	void pass_or_throw( args && ... a )
		{ cplus::pass_or_throw< exception_type >( * this, std::forward< args >( a ) ... ); }
};

// Non-virtual abstract base class.
template< typename output >
struct stage_base {
	output cont;
	
	template< typename ... args >
	stage_base( args && ... a ) : cont( std::forward< args >( a ) ... ) {}

	template< typename client >
	typename std::enable_if< ( typename util::mention< decltype( cont.template get_config< client >() ) >::type(), true ), client & >::type
	get_config() { return cont.template get_config< client >(); }
};

template< typename output >
struct stage_base< std::reference_wrapper< output > > {
	std::reference_wrapper< output > cont;
	
	template< typename ... args >
	stage_base( args && ... a ) : cont( std::forward< args >( a ) ... ) {}

	template< typename client >
	typename std::enable_if< ( typename util::mention< decltype( cont.get().template get_config< client >() ) >::type(), true ), client & >::type
	get_config() { return cont.get().template get_config< client >(); }
};

template< typename output_iterator, typename ... config_types >
using stage = derived_stage< stage_base< output_iterator >, config_types ... >;

void finalize( util::poor_conversion const & ) {} // fallback overload, worse than derived-to-base conversion

template< typename s >
typename std::enable_if< ! std::is_same< typename s::base, typename s::stage_base >::value >::type
finalize( s &o ) {
	o.flush();
	finalize( static_cast< typename s::base & >( o ) );
}

/*	Each pipeline step chains to finalize for the next one. When an iterator without
	finalize is reached, synchronization stops. That iterator should be the last. */
template< typename s >
typename std::enable_if< std::is_same< typename s::base, typename s::stage_base >::value >::type
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
};

template< typename cont, template< typename ... > class ... stages >
struct stack_stages;

template< typename cont >
struct stack_stages< cont >
	{ typedef cont type; };

template< typename cont, template< typename ... > class stage, template< typename ... > class ... rem >
struct stack_stages< cont, stage, rem ... >
	{ typedef stage< typename stack_stages< cont, rem ... >::type > type; };

template< template< typename ... > class ... stages, typename cont, typename ... aux >
typename stack_stages< cont, stages ... >::type
autoconfigured_pile( cont && c, aux && ... a )
	{ return { std::forward< aux >( a ) ..., std::forward< cont >( c ) }; }

}

#endif
