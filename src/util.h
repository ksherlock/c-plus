// Cplus project, general-purpose utilities.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_UTIL
#define CPLUS_UTIL

#include <memory>
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

using std::rel_ops::operator!=;

namespace util {

struct abc { inline virtual ~abc() = 0; };
abc::~abc() = default;

template< typename t > // provide symmetry with std::ref
typename std::decay< t >::type val( t &&o )
	{ return std::forward< t >( o ); }

void *align( std::size_t alignment, std::size_t size, void *&ptr, std::size_t &space ) {
	auto pn = reinterpret_cast< std::size_t >( ptr );
	auto aligned = pn + alignment - 1 & - alignment;
	auto new_space = space - ( aligned - pn );
	if ( new_space < size ) return nullptr;
	space = new_space;
	return ptr = reinterpret_cast< void * >( aligned );
}

template< typename ftor >
struct output_iterator_from_functor
	: ftor,
	std::iterator< std::output_iterator_tag, void, void, void, void > {
	typedef std::true_type overrides_ref;
	
	template< typename ... args >
	output_iterator_from_functor( args && ... a )
		: ftor( std::forward< args >( a ) ... ) {}

	template< typename value_type > // performs insertion, is not the move assignment operator
	output_iterator_from_functor &operator=( value_type &&o )
		{ (*this)( std::forward< value_type >( o ) ); return *this; }
	
	friend output_iterator_from_functor &operator*( output_iterator_from_functor &o ) { return o; } // is own object
	friend output_iterator_from_functor &operator++( output_iterator_from_functor &o ) { return o; } // just no-ops:
	friend output_iterator_from_functor &operator++( output_iterator_from_functor &o, int ) { return o; }
};

template< typename ftor >
output_iterator_from_functor< ftor > make_output_iterator( ftor &&f )
	{ return { std::forward< ftor >( f ) }; }

template< typename output_iterator >
struct output_iterator_ref
	: std::reference_wrapper< output_iterator >,
	std::iterator< std::output_iterator_tag, void, void, void, void > {
	
	output_iterator_ref( output_iterator &in )
		: std::reference_wrapper< output_iterator >( in ) {}
	
	operator output_iterator & () { return this->get(); } // provides ++ and *; incompatible with member operator overloads
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
	void operator() ( value_type &&o ) {
		if ( max != 0 && count == max ) throw std::range_error( "sequence too long" );
		++ count;
		* base ++ = std::forward< value_type >( o );
	}
};

template< typename output_iterator >
output_iterator_from_functor< limit_range_ftor< output_iterator > >
limit_range( output_iterator &&iter ) { return { std::move( iter ) }; }

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
			if ( -- len == 0 ) {
				if ( result < min ) goto malformed;
				return true;
			
			} else return false;
		}
	}
};

namespace query {
	char ctime_r( ... );
	
	struct has_ctime_r {
		enum { value = sizeof ctime_r( std::declval< std::time_t * >(), std::declval< char * >() )
						== sizeof( char * ) };
	};

	template< bool available > struct safest_ctime {
		static char *call( std::time_t const *t, char *&r )
			{ return ctime_r( t, r ); }
	};

	template<> struct safest_ctime< false > {
		static char *call( std::time_t const *t, char *&r )
			{ return r = std::ctime( t ); }
	};
}

char *ctime( std::time_t const *t, char *&r )
	{ return query::safest_ctime< query::has_ctime_r::value >().call( t, r ); }

} } // end namespace cplus, end namespace util

#endif
