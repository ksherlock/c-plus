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

struct passed_exception : std::runtime_error { passed_exception() : std::runtime_error( "ICE: Failed to stop exception propagation after normal handling." ) {} };

// pass() puts input into a stage (functor or iterator) or its succeeding stages, catches thrown exceptions and feeds them back in.
enum class pass_policy { mandatory, optional, enable_if };
enum class diagnose_policy { none, pass, fatal }; // Apply to either sender or receiver. Sender cannot specify none.

namespace impl {
	template< typename base, typename v, typename = void >
	struct is_output_iterable : std::false_type {};
	
	template< typename output, typename v >
	struct is_output_iterable< output, v, typename util::mention< decltype( * std::declval< output & >() ++ = std::declval< v >() ) >::type > : std::true_type {};
	
	template< typename base, typename, typename = void >
	struct is_callable : std::false_type {};
	
	template< typename base, typename v >
	struct is_callable< base, v, typename util::mention<decltype( std::declval< base & >() ( std::declval< v >() ) )>::type > : std::true_type {};
	
	template< typename base, typename v > // GCC workaround: usual trait definition style produces strange, inconsistent results.
	constexpr bool is_passable_fn( ... ) { return false; }
	
	template< typename base, typename v >
	constexpr decltype ( std::declval< typename base::stage & >().template pass< pass_policy::enable_if >( std::declval< v >() ), true )
	is_passable_fn( int ) { return true; }
	
	template< typename base, typename v, bool r = is_passable_fn< base, v >(0) >
	struct is_passable : std::integral_constant< bool, r > {};

	template< typename t > struct remove_reference_wrapper { typedef t type; };
	template< typename t > struct remove_reference_wrapper< std::reference_wrapper< t > > { typedef t type; };
}

void finalize( util::poor_conversion const & ) {} // fallback overload, worse than derived-to-base conversion

// Adaptor for deriving one stage from another and using base's output iterator.
template< typename base_type, typename ... config_types >
class derived_stage : public base_type {
	std::tuple< config_types const & ... > configs;
	
	void operator () () = delete; // pass() is a requirement for communication up the stack.
	
	template< typename t >
	typename std::enable_if< ! std::is_base_of< std::exception, typename std::decay< t >::type >::value >::type
	pass_or_diagnose( t && o ) { pass( std::forward< t >( o ) ); }
	
	template< typename t > // Virtual-dispatch to a surrogate error receiver when none is implemented.
	typename std::enable_if< std::is_base_of< std::exception, typename std::decay< t >::type >::value >::type
	pass_or_diagnose( t && o ) { diagnose< diagnose_policy::pass, t >( true, std::forward< t >( o ) ); }
	
	struct tag {};
public:
	typedef base_type base;
	typedef derived_stage stage;

	void flush() {}
	
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
	
	template< typename client = typename std::tuple_element< 0, std::tuple< config_types const ..., void > >::type >
	typename std::enable_if< util::tuple_index< client &, decltype( configs ) >::value != ( sizeof ... ( config_types ) ), client & >::type
	get_config() { return std::get< util::tuple_index< client &, decltype( configs ) >::value >( configs ); }
	
	template< typename client >
	typename std::enable_if< ( typename util::mention< decltype( std::declval< base >().template get_config< client >() ) >::type(),
		util::tuple_index< client &, decltype( configs ) >::value == ( sizeof ... ( config_types ) ) ), client & >::type
	get_config() { return base::template get_config< client >(); }
	
	template< pass_policy policy = pass_policy::mandatory >
	static typename std::enable_if< policy != pass_policy::enable_if >::type // to see overload resolution errors, specify enable_if.
	pass( util::poor_conversion ) { static_assert( policy == pass_policy::optional, "invalid pass argument" ); }
	
	template< pass_policy = pass_policy::mandatory, typename arg >
	typename std::enable_if< impl::is_callable< base_type, arg >::value >::type
	pass( arg && a ) & // Pass along the chain or to immediate superclass.
		try { this->base_type::operator () ( std::forward< arg >( a ) ); }
		catch ( passed_exception & ) {}
	
	template< pass_policy policy = pass_policy::mandatory, typename arg >
	typename std::enable_if< ! impl::is_callable< base_type, arg >::value && impl::is_passable< base_type, arg >::value >::type
	pass( arg && a ) & // Pass to derived_stage recursive base.
		{ this->base_type::stage::template pass< policy >( std::forward< arg >( a ) ); }
	
	template< pass_policy policy = pass_policy::mandatory, typename arg >
	typename std::enable_if< policy != pass_policy::enable_if || impl::is_callable< base_type, arg >::value || impl::is_passable< base_type, arg >::value >::type
	pass( arg && a ) && {
		pass< policy >( std::forward< arg >( a ) );
		finalize( * this );
	}
	
	template< pass_policy policy = pass_policy::mandatory, typename iter >
	typename std::enable_if< policy != pass_policy::enable_if || impl::is_callable< base_type, typename std::iterator_traits< iter >::reference >::value
							|| impl::is_passable< base_type, typename std::iterator_traits< iter >::reference >::value >::type
	pass( iter first, iter last ) &
		{ for ( ; first != last; ++ first ) pass< policy >( * first ); }
	
	template< pass_policy policy = pass_policy::mandatory, typename iter >
	typename std::enable_if< policy != pass_policy::enable_if || impl::is_callable< base_type, typename std::iterator_traits< iter >::reference >::value
							|| impl::is_passable< base_type, typename std::iterator_traits< iter >::reference >::value >::type
	pass( iter first, iter last ) && {
		pass< policy >( first, last );
		finalize( * this );
	}
	
	template< typename ... t >
	util::function< void( t ) ... > pass_function()
		{ return { std::bind( static_cast< void (derived_stage::*) ( t ) >( & derived_stage::pass_or_diagnose ), this, std::placeholders::_1 ) ... }; }
	
	template< diagnose_policy policy, typename exception_type, typename ... args >
	typename std::enable_if< impl::is_passable< stage, exception_type >::value, bool >::type
	diagnose( bool condition, args && ... a ) {
		if ( condition ) {
			pass< pass_policy::mandatory >( exception_type( std::forward< args >( a ) ... ) );
			if ( policy == diagnose_policy::fatal ) throw passed_exception();
		}
		return condition;
	}
	
	template< diagnose_policy policy, typename exception_type, typename ... args >
	typename std::enable_if< ! impl::is_passable< stage, exception_type >::value, bool >::type
	diagnose( bool condition, args && ... a ) {
		if ( condition ) throw exception_type( std::forward< args >( a ) ... );
		return false;
	}
};

// Non-virtual abstract base class.
template< typename output_mem >
struct stage_base {
	typedef typename impl::remove_reference_wrapper< output_mem >::type output_type;
	output_mem cont;
	
	template< typename ... args >
	stage_base( args && ... a ) : cont( std::forward< args >( a ) ... ) {}
	
	template< typename v >
	typename std::enable_if< impl::is_output_iterable< output_type, v >::value >::type
	operator () ( v && val )
		{ * cont ++ = std::forward< v >( val ); }
	
	template< typename v >
	typename std::enable_if< impl::is_callable< output_type, v >::value >::type
	operator () ( v && val )
		try { cont( std::forward< v >( val ) ); }
		catch ( passed_exception & ) {}
	
	template< typename v >
	typename std::enable_if< ! impl::is_callable< output_type, v >::value && impl::is_passable< output_type, v >::value >::type
	operator () ( v && val ) { static_cast< output_type & >( cont ).output_type::stage::pass( std::forward< v >( val ) ); }
	
	template< typename client >
	typename std::enable_if< ( typename util::mention< decltype( std::declval< output_type & >().template get_config< client >() ) >::type(), true ), client & >::type
	get_config() { return static_cast< output_type & >( cont ).template get_config< client >(); }
};

template< typename output_iterator, typename ... config_types >
using stage = derived_stage< stage_base< output_iterator >, config_types ... >;

template< typename s >
typename std::enable_if< ! std::is_same< typename s::base, typename s::stage_base >::value >::type
finalize( s &o ) {
	try {
		o.flush();
	} catch ( passed_exception & ) {}
	finalize( static_cast< typename s::base & >( o ) );
}

/*	Each pipeline step chains to finalize for the next one. When an iterator without
	finalize is reached, synchronization stops. That iterator should be the last. */
template< typename s >
typename std::enable_if< std::is_same< typename s::base, typename s::stage_base >::value >::type
finalize( s &o ) {
	try {
		o.flush();
	} catch ( passed_exception & ) {}
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
			p.pass( instantiate_component( inst, i ) );
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
	return ret? this : source->advance_to_source< instantiation_derived >();
}

// Error reporting format.
class error_base : public std::runtime_error {
	std::unique_ptr< construct const, void (*)( construct const * ) > p;
protected:
	template< typename c >
	error_base( c pos, char const *what )
		: std::runtime_error( what ),
		p( new c( std::move( pos ) ), []( construct const *p ){ delete static_cast< c const * >( p ); } ) {}
public:
	virtual construct const &offender() const { return * p; } // Virtual dispatch is somewhat useless. Derived classes should call this and static_cast the result.
};
struct error : public error_base
	{ error( construct pos, char const * what ) : error_base( std::move( pos ), what ) {} };

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
			p.pass( rc );
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
		if ( ! fb.open( path, std::ios::in | std::ios::binary ) ) throw error( * this, "Unable to open file." );
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

template< template< typename ... > class ... stages, typename ... args >
derived_stage< typename stack_stages< typename std::tuple_element< sizeof ... ( args ) - 1, std::tuple< args ... > >::type, stages ... >::type >
pile( args && ... a )
	{ return { std::forward< args >( a ) ... }; }

}

#endif
