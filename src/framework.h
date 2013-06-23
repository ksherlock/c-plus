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
#include <fstream>
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
enum class pass_policy { mandatory, optional, enable_if };

template< pass_policy = {}, typename oit, typename v >
auto pass( oit it, v && val )
	-> typename util::mention< decltype( * it ++ = std::forward< v >( val ) ) >::type
	{ * it ++ = std::forward< v >( val ); }

template< pass_policy = {}, typename fn, typename v >
typename util::mention< typename std::result_of< fn &( v ) >::type >::type
pass( fn && obj, v && val );

template< pass_policy policy = pass_policy::mandatory >
typename std::enable_if< policy != pass_policy::enable_if, pass_policy >::type // to see overload resolution errors, specify enable_if.
pass( util::poor_conversion, util::poor_conversion ) { static_assert( policy == pass_policy::optional, "invalid pass argument" ); return pass_policy::optional; }

template< pass_policy policy = pass_policy::mandatory, typename fn, typename v > // Qualified-id avoids ADL and prevents recursion, tag enables ADL and recursion.
typename std::enable_if< std::is_same< decltype( cplus::pass< pass_policy::optional >( std::declval< fn >(), std::declval< v >() ) ), pass_policy >::value,
	typename util::mention< decltype( pass< pass_policy::enable_if >( std::declval< fn >().cont, std::declval< v >() ) ) >::type >::type
pass( fn && obj, v && val )
	{ pass< policy >( std::forward< fn >( obj ).cont, std::forward< v >( val ) ); }

template< pass_policy policy = pass_policy::mandatory, typename w, typename v >
typename util::mention< decltype( pass( std::declval< w & >(), std::declval< v >() ) ) >::type
pass( std::reference_wrapper< w > wrap, v && val )
	{ pass< policy >( wrap.get(), std::forward< v >( val ) ); }

template< typename fn, typename v >
void pass( fn & obj, v && val, util::mention< std::tuple<> > )
	{ obj( std::forward< v >( val ) ); }

template< typename e, typename ... er, typename fn, typename v >
void pass( fn & obj, v && val, util::mention< std::tuple< e, er ... > > ) {
	try {
		pass( obj, std::forward< v >( val ), util::mention< std::tuple< er ... > >() );
	} catch ( e & exc ) {
		pass( obj, std::forward< e >( exc ) ); // May move e. Double exceptions may cause infinite recursion.
	}
}

void finalize( util::poor_conversion const & ) {} // fallback overload, worse than derived-to-base conversion

template< pass_policy, typename fn, typename v >
typename util::mention< typename std::result_of< fn &( v ) >::type >::type
pass( fn && obj, v && val ) {
	pass( obj, std::forward< v >( val ), util::mention< typename parameter_exceptions< typename std::decay< fn >::type >::type >() );
	if ( ! std::is_reference< fn >::value ) finalize( obj );
}

template< pass_policy policy = pass_policy::mandatory, typename obj, typename iit >
auto
#if __GNUC__
	pass_some
#else
	pass
#endif
			( iit first, iit last, obj && o )
	-> typename util::mention< decltype( pass< policy >( o, * first ) ) >::type {
	for ( ; first != last; ++ first ) {
		pass< policy >( o, * first );
	}
	if ( ! std::is_reference< obj >::value ) finalize( o );
}

#if __GNUC__
template< pass_policy policy = pass_policy::mandatory, typename obj, typename iit >
typename util::mention< decltype( pass_some< policy >( std::declval< iit >(), std::declval< iit >(), std::declval< obj >() ) ) >::type
pass( iit first, iit last, obj && o )
	{ return pass_some( first, last, std::forward< obj >( o ) ); }
#endif

template< typename exception_type, typename obj, typename ... args >
typename util::mention< decltype( pass< pass_policy::enable_if >( std::declval< obj >(), std::declval< exception_type >() ) ) >::type
pass_or_throw( obj && o, args && ... a )
	{ pass( std::forward< obj >( o ), exception_type{ std::forward< args >( a ) ... } ); }

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
	
	template< typename ... acc_config, typename in_config, typename ... args > // if next argument is the next config object
	derived_stage( typename std::enable_if< sizeof ... ( acc_config ) != sizeof ... ( config_types )
			&& std::is_convertible< in_config, typename util::tuple_element< sizeof ... ( acc_config ), decltype( configs ) >::type >::value, tag >::type,
		std::tuple< acc_config ... > && cacc, in_config && c, args && ... a )
		: derived_stage( tag(), std::tuple_cat( std::move( cacc ), std::forward_as_tuple( std::forward< in_config >( c ) ) ), std::forward< args >( a ) ... ) {}
	
	template< typename ... acc_config, typename in_config, typename ... args > // if next argument isn't the next config object
	derived_stage( typename std::enable_if< sizeof ... ( acc_config ) != sizeof ... ( config_types )
			&& ! std::is_convertible< in_config, typename util::tuple_element< sizeof ... ( acc_config ), decltype( configs ) >::type >::value, tag >::type,
		std::tuple< acc_config ... > && cacc, in_config && c, args && ... a )
		: derived_stage( tag(), std::tuple_cat( std::move( cacc ), std::make_tuple( util::make_implicit_thunk(
			std::bind( & base::template get_config< typename std::tuple_element< sizeof ... ( acc_config ), std::tuple< config_types const ... > >::type >, this ) ) ) ),
			std::forward< in_config >( c ), std::forward< args >( a ) ... ) {}

	template< typename ... acc_config, typename ... args > // if configuration is complete
	derived_stage( typename std::enable_if< sizeof ... ( acc_config ) == sizeof ... ( config_types ), tag >::type,
		std::tuple< acc_config ... > && cacc, args && ... a )
		: base( std::forward< args >( a ) ... ), configs( std::move( cacc ) ) {}
	
public:
	template< typename client = typename std::tuple_element< 0, std::tuple< config_types const ..., void > >::type >
	typename std::enable_if< util::tuple_index< client &, decltype( configs ) >::value != ( sizeof ... ( config_types ) ), client & >::type
	get_config() { return std::get< util::tuple_index< client &, decltype( configs ) >::value >( configs ); }
	
	template< typename client >
	typename std::enable_if< ( typename util::mention< decltype( std::declval< base >().template get_config< client >() ) >::type(),
		util::tuple_index< client &, decltype( configs ) >::value == ( sizeof ... ( config_types ) ) ), client & >::type
	get_config() { return base::template get_config< client >(); }

	template< pass_policy policy = pass_policy::mandatory, typename v >
	void pass( v && val ) { cplus::pass< policy >( * this, std::forward< v >( val ) ); }

	template< pass_policy policy = pass_policy::mandatory, typename iit >
	void pass( iit first, iit last ) { cplus::pass< policy >( first, last, * this ); }

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

typedef std::uint64_t location_t;
class construct { // "Construct" as a general noun, not the verb.
	friend struct instantiation;
	
	std::shared_ptr< struct instantiation const > source; // Should be optional. Diagnostic use only, for retracing the translation process.
	location_t location; // position in instantiation, meaning depends on type of instantiation

	template< typename instantiation_derived = instantiation >
	construct const *advance_to_source() const;

	construct( std::shared_ptr< instantiation const > in_s, location_t in_l ) : source( in_s ), location( in_l ) {}

public:
	construct() = default;

	template< typename instantiation_derived = instantiation >
	instantiation_derived const *get_parent() const {
		auto child = advance_to_source< instantiation_derived >();
		return child? static_cast< instantiation_derived const * >( child->source.get() ) : nullptr;
	}
	template< typename instantiation_derived = instantiation >
	std::tuple< instantiation_derived const &, location_t > get_location() const {
		auto child = advance_to_source< instantiation_derived >();
		if ( child == nullptr ) throw std::logic_error( "ICE: missing backtrack link" );
		return std::tie( static_cast< instantiation_derived const & >( * child->source ), child->location );
	}
	template< typename instantiation_derived >
	decltype( std::declval< instantiation_derived >().component(0) )
	get_source() const {
		auto location = get_location< instantiation_derived >();
		return std::get< 0 >( location ).component( std::get< 1 >( location ) );
	}
};

// describes a source file inclusion, macro expansion, or template instantiation
struct instantiation : util::abc, construct {
	instantiation() = default;
	instantiation( construct const &in ) : construct( in ) {} // slice constructor
	
	virtual std::size_t size() const = 0;
	virtual construct const &component( location_t ) const = 0; // covariant

protected:
	static construct source_link( std::shared_ptr< instantiation const > self, std::size_t index )
		{ return { std::move( self ), index }; } // expose private constructor, but not as a mem-initializer
	
	static void advance( construct &c )
		{ ++ c.location; }

private:
	template< typename instantiation_derived >
	static typename std::decay< decltype( std::declval< instantiation_derived >().component(0) ) >::type
	instantiate_component( std::shared_ptr< instantiation_derived > inst, std::size_t index ) {
		auto sub = inst->component( index );
		static_cast< construct & >( sub ) = source_link( std::move( inst ), index );
		return sub;
	}
	template< typename instantiation_derived, typename pile >
	static typename std::enable_if< ! std::is_same< construct const &, decltype( std::declval< instantiation_derived >().component(0) ) >::value >::type
	instantiate( std::shared_ptr< instantiation_derived > const &inst, pile & p ) {
		for ( std::size_t i = 0; i != inst->size(); ++ i ) {
			p( instantiate_component( inst, i ) );
		}
	}

	template< typename instantiation_derived >
	friend typename std::decay< decltype( std::declval< instantiation_derived >().component(0) ) >::type
	instantiate_component( std::shared_ptr< instantiation_derived > const &inst, std::size_t index )
		{ return instantiation_derived::instantiate_component( inst, index ); }
	
	template< typename instantiation_derived, typename pile >
	friend void instantiate( std::shared_ptr< instantiation_derived > const &inst, pile && p ) {
		instantiation_derived::instantiate( inst, p ); // do not forward; finalize usurps move semantics
		if ( ! std::is_reference< pile >::value ) finalize( p );
	}
};

template< typename instantiation_derived >
construct const *construct::advance_to_source() const {
	if ( ! source ) return nullptr;
	auto ret = dynamic_cast< instantiation_derived const * >( source.get() );
	return ret? this : source->get_parent< instantiation_derived >();
}

// Error reporting format.
struct error : std::runtime_error {
	std::unique_ptr< construct const, void (*)( construct const * ) > p;
	
	template< typename c >
	error( c &&pos, char const *what )
		: std::runtime_error( what ),
		p( new typename std::remove_reference< c >::type( std::forward< c >( pos ) ), []( construct const *p )
			{ delete static_cast< typename std::decay< c >::type const * >( p ); } ) {}
};

struct raw_char : construct {
	std::uint8_t c;
	
	raw_char( std::uint8_t in_c = {}, construct in_p = {} ) : construct( std::move( in_p ) ), c( in_c ) {}
};

// Abstract base for user input.
struct input_source : instantiation {
	using instantiation::instantiation;
	
	virtual std::size_t size() const { return 0; }
	virtual construct const &component( location_t ) const override
		{ throw error( static_cast< construct const & >( * this ), "An individual character cannot be retrieved from this construct." ); }
	virtual void filter( std::function< void( std::uint8_t ) > ) const = 0; // Virtual-dispatches each char. Not for use in primary processing.
	
	template< typename input_source_derived, typename pile >
	static void instantiate( std::shared_ptr< input_source_derived > inst, pile & p ) {
		raw_char rc;
		auto inst_p = inst.get();
		rc.construct::operator = ( source_link( std::move( inst ), 0 ) );
		inst_p->filter( [&p, &rc]( uint8_t c ) {
			rc.c = c;
			p( rc );
			advance( rc );
		} );
	}
};
struct raw_file : input_source {
	std::string path;
	
	raw_file( std::string const &p, construct const &c = {} ) : input_source( c ), path( p ) {}
	
	virtual void filter( std::function< void( std::uint8_t ) > fn ) const override { filter<>( fn ); }
		
	template< typename ftor >
	void filter( ftor && fn ) const {
		std::filebuf fb;
		if ( ! fb.open( path, std::ios::in | std::ios::binary ) ) throw error( * this, "Unable to open file." ); // Copies this. Unsafe if cache were added.
		char buf[ 16384 ];
		std::streamsize count;
		do {
			count = fb.sgetn( buf, sizeof buf );
			std::for_each( buf, buf + count, std::ref( fn ) );
		} while ( count == sizeof buf );
	}
};
struct raw_text : input_source {
	string text;
	
	raw_text( char const *p, construct c = {} ) : input_source( std::move( c ) ), text( p ) {}
	raw_text( string s, construct c = {} ) : input_source( std::move( c ) ), text( std::move( s ) ) {}
	
	virtual void filter( std::function< void( std::uint8_t ) > fn ) const override { filter<>( fn ); }
		
	template< typename ftor >
	void filter( ftor && fn ) const
		{ std::for_each( text.begin(), text.end(), std::ref( fn ) ); }
};

// Stage parameterization and pragma distribution.
struct config_base : util::abc {
	config_base() = default;
	config_base( config_base const & ) = delete;
};

template< typename output_iterator >
class config_manager : public stage< output_iterator > {
protected:
	std::map< std::type_index, std::unique_ptr< config_base > > registry;
public:
	template< typename config_ret_type >
	config_ret_type &get_config() {
		typedef typename std::remove_const< config_ret_type >::type config_type;
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
