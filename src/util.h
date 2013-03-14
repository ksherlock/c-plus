// Cplus project, general-purpose utilities.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_UTIL
#define CPLUS_UTIL

#include <memory>
#include <utility>
#include <iterator>
#include <stdexcept>
#include <functional>
#include <type_traits>
#include <cstring>
#include <ctime>

// This kind of enumeration is scoped by default but unscoped for inheriting classes.
// It implicitly converts to int, since it's really just a C++03 hack.
// "Importable" means you can import it, not the antonym of "portable."
#define CPLUS_IMPORTABLE_ENUM( TYPENAME, ... ) \
struct TYPENAME ## _import_base \
	{ enum TYPENAME { __VA_ARGS__, TYPENAME ## _last }; }; \
typedef TYPENAME ## _import_base :: TYPENAME TYPENAME;

namespace cplus {

namespace util {

struct abc { inline virtual ~abc() = 0; };
abc::~abc() = default;

template< typename t > // provide symmetry with std::ref
typename std::decay< t >::type val( t &&o )
	{ return std::forward< t >( o ); }

void *align( std::size_t alignment, std::size_t size, void *&ptr, std::size_t &space ) {
	auto pn = reinterpret_cast< std::size_t >( ptr );
	auto aligned = ( pn + alignment - 1 ) & - alignment;
	auto new_space = space - ( aligned - pn );
	if ( new_space < size ) return nullptr;
	space = new_space;
	return ptr = reinterpret_cast< void * >( aligned );
}

template< typename tup, typename elem, std::size_t offset = 0 >
struct tuple_index
	: std::integral_constant< std::size_t, offset > {};

template< typename head, typename ... tail, typename elem, std::size_t offset >
struct tuple_index< std::tuple< head, tail ... >, elem, offset >
	: std::integral_constant< std::size_t, tuple_index< std::tuple< tail ... >, elem, offset + 1 >::value > {};

template< typename ... tail, typename elem, std::size_t offset >
struct tuple_index< std::tuple< elem, tail ... >, elem, offset >
	: std::integral_constant< std::size_t, offset > {};

template< typename t, typename = void >
struct has_member_type : std::false_type {};

template< typename t >
struct has_member_type< t, typename std::conditional< true, void, typename t::type >::type >
	: std::true_type {};

template< typename t, typename v = void >
struct is_output_iterator
	: std::false_type {};

template< typename t >
struct is_output_iterator< t, typename std::conditional< true, void, decltype( * std::declval< t & >() ++ ) >::type >
	: std::true_type {};

template< typename t, typename u, typename = void >
struct cont_is_assignable : std::false_type {};

template< typename t, typename u >
struct cont_is_assignable< t, u, typename std::conditional< true, void, decltype( * std::declval< t >().cont ++ = std::declval< u >() ) >::type >
	: std::true_type {};

template< typename ftor, typename = void >
struct output_iterator_from_functor;

template< typename ftor >
struct output_iterator_from_functor< ftor, void >
	: ftor,
	std::iterator< std::output_iterator_tag, void, void, void, void > {
	typedef std::true_type overrides_ref;
	
	template< typename ... args >
	explicit output_iterator_from_functor( args && ... a )
		: ftor( std::forward< args >( a ) ... ) {}

	template< typename value_type > // performs insertion, is not the move assignment operator
	typename std::enable_if< has_member_type< std::result_of< ftor( value_type ) > >::value
		&& cont_is_assignable< typename std::conditional< true, ftor, value_type >::type, std::exception >::value,
		output_iterator_from_functor & >::type
	operator = ( value_type && o ) {
		try {
			(*this)( std::forward< value_type >( o ) );
		} catch ( std::exception & e ) {
			* this->cont ++ = std::move( e );
		}
		return *this;
	}

	template< typename value_type > // performs insertion, is not the move assignment operator
	typename std::enable_if< has_member_type< std::result_of< ftor( value_type ) > >::value
		&& ! cont_is_assignable< typename std::conditional< true, ftor, value_type >::type, std::exception >::value,
		output_iterator_from_functor & >::type
	operator = ( value_type && o ) {
		(*this)( std::forward< value_type >( o ) );
		return * this;
	}

	template< typename value_type > // if object is the wrong type, try bypassing it down the chain
	auto operator = ( value_type &&o ) ->
	typename std::enable_if< ! has_member_type< std::result_of< ftor( value_type ) > >::value
		&& cont_is_assignable< ftor, value_type >::value,
		output_iterator_from_functor & >::type {
		* this->cont ++ = std::forward< value_type >( o );
		return * this;
	}

	template< typename value_type > // catch-all to print a message when wrong type is passed
	auto operator = ( value_type &&o ) ->
	typename std::enable_if< ! has_member_type< std::result_of< ftor( value_type ) > >::value
		&& ! cont_is_assignable< ftor, value_type >::value
		&& ! std::is_convertible< value_type, std::exception >::value,
		output_iterator_from_functor & >::type {
		static_assert( ! std::is_same< value_type, value_type >::value, "Illegal pass type." );
		return * this;
	}

	friend output_iterator_from_functor &operator*( output_iterator_from_functor &o ) { return o; } // is own referent
	friend output_iterator_from_functor &operator++( output_iterator_from_functor &o ) { return o; } // just no-ops:
	friend output_iterator_from_functor &operator++( output_iterator_from_functor &o, int ) { return o; }
};

template< typename ftor >
output_iterator_from_functor< ftor >
make_output_iterator( ftor && f )
	{ return output_iterator_from_functor< ftor >{ std::forward< ftor >( f ) }; }

template< typename output_iterator >
struct output_iterator_ref
	: std::reference_wrapper< output_iterator >,
	std::iterator< std::output_iterator_tag, void, void, void, void > {
	
	output_iterator_ref( output_iterator &in )
		: std::reference_wrapper< output_iterator >( in ) {}
};

template< typename ftor >
output_iterator_ref< output_iterator_from_functor< ftor > >
ref( output_iterator_from_functor< ftor > &i )
	{ return { i }; }

template< typename t >
struct rvalue_reference_wrapper : std::reference_wrapper< t > {
	rvalue_reference_wrapper( t &o ) : std::reference_wrapper< t >( o ) {}
	t &&get() const { return static_cast< t && >( std::reference_wrapper< t >::get() ); }
#if __GNUC__ // Workaround to enable implicit user-defined conversion in bind call.
	operator t&& () const volatile { return const_cast< rvalue_reference_wrapper const * >( this )->get(); }
#else
	operator t&& () const { return get(); }
#endif
	operator t& () const = delete;
};

template< typename t >
rvalue_reference_wrapper< t > rref( t &o ) { return { o }; }

template< typename ... t >
struct amalgam : t ... {
	amalgam( t && ... in ) : t( std::move( in ) ) ... {} // not perfect forwarding
private:
	struct tag;
protected:
	void operator () ( tag ) = delete;
};

template< typename ftor >
struct add_ftor;

}} namespace std {
template< typename base, typename ftor >
struct common_type< base, cplus::util::add_ftor< ftor > > {
	typedef struct : base {
		using base::operator ();
		using ftor::operator ();
		
		using base::base;
	} type;
};
} namespace cplus { namespace util {

template< typename ... t >
struct amalgam_ftor
	: std::common_type< amalgam< t ... >, add_ftor< t > ... >::type {
	using std::common_type< amalgam< t ... >, add_ftor< t > ... >::type::type;
};

template< typename ... t >
amalgam_ftor< t ... > amalgamate( t && ... in )
	{ return { std::forward< t >( in ) ... }; }

template< typename bound >
struct implicit_thunk {
	bound f;
	operator typename std::result_of< bound() >::type () { return f(); }
};

template< typename bound >
implicit_thunk< typename std::decay< bound >::type > make_implicit_thunk( bound &&f )
	{ return { std::forward< bound >( f ) }; }

struct poor_conversion {
	template< typename t >
	poor_conversion( t const & ) {}
};

template< typename output_iterator >
class limit_range_ftor {
	std::size_t count, max;
public:
	output_iterator base;
	
	template< typename ... args >
	limit_range_ftor( args && ... a )
		: count( 0 ), max( 0 ), base( std::forward< args >( a ) ... ) {}
	
	std::size_t reset( std::size_t new_max = 0 ) {
		std::size_t old_count = count;
		count = 0;
		max = new_max;
		return old_count;
	}
	
	template< typename value_type >
	auto operator() ( value_type &&o ) ->
	typename std::conditional< true, void, decltype( * base ++ = std::forward< value_type >( o ) ) >::type {
		if ( max != 0 && count == max ) throw std::range_error( "sequence too long" );
		++ count;
		* base ++ = std::forward< value_type >( o );
	}
};

template< typename output_iterator >
output_iterator_from_functor< limit_range_ftor< output_iterator > >
limit_range( output_iterator &&iter ) { return output_iterator_from_functor< limit_range_ftor< output_iterator > >{ std::move( iter ) }; }

class utf8_convert {
	char32_t min;
	int len;
public:
	char32_t result;
	
	utf8_convert() : len( 0 ) {}
	bool in( std::uint8_t c ) {
		if ( len != 0 || c >= 0x80 ) return non_ascii( c );
		result = c;
		return true;
	}
	bool non_ascii( std::uint8_t c ) {
		if ( len == 0 ) {
			if ( c >= 0xC0 ) {
				if ( c == 0xFF ) malformed: {
					throw std::range_error( "malformed UTF-8" );
				}
				len = 1;
				for ( std::uint8_t cback = c; cback & 0x20; cback <<= 1 ) {
					++ len;
				}
				result = ( c + ( 0x80 >> len ) ) & 0xFF;
				min = 1 << ( len > 1? len * 5 + 1 : 7 ); // lowest acceptible end result
				return false;
			} else goto malformed;
		} else {
			if ( c < 0x80 || c >= 0xC0 ) goto malformed;
			result = result << 6 | ( c & 0x3F );
			if ( -- len != 0 ) return false;
			if ( result < min ) goto malformed;
			return true;
		}
	}
};

namespace query {
	struct poor_converter // poor as poor_conversion, but less poor than "..."
		{ template< typename t > operator t & () {} };
	
	void ctime_r( ... ); // If ::ctime_r is to be found by unqualified lookup, the fallback must be found by ADL.
}

template< bool en = std::is_same< char *, decltype( ctime_r( query::poor_converter(), query::poor_converter() ) ) >::value >
typename std::enable_if< en, char * >::type ctime( std::time_t const *t, char *&r )
	{ return ctime_r( typename std::enable_if< en, std::time_t const * >::type( t ), r ); } // bogus dependency guards potentially undefined name

template< bool en = std::is_same< char *, decltype( ctime_r( query::poor_converter(), query::poor_converter() ) ) >::value >
typename std::enable_if< ! en, char * >::type ctime( std::time_t const *t, char *&r )
	{ return r = std::ctime( t ); }
	
} } // end namespace cplus, end namespace util

#endif
