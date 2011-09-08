// Cplus project, general-purpose utilities.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_UTIL
#define CPLUS_UTIL

#include <iterator>
#include <stdexcept>
#include <functional>
#include <type_traits>
#include <ctime>

#include <typeinfo>

#include "formats.h"

namespace cplus {
namespace util {

template< typename t > // provide symmetry with std::move
typename std::remove_cv< typename std::remove_reference< t >::type >::type copy( t &&o )
	{ return o; }

template< typename ftor >
struct output_iterator_from_functor
	: std::iterator< std::output_iterator_tag, void, void, void, void >,
	ftor {
	template< typename ... args >
	output_iterator_from_functor( args && ... a )
		: ftor( std::forward< args >( a )... ) {}

	template< typename value_type > // performs insertion, is not the move assignment operator
	output_iterator_from_functor &operator=( value_type &&o )
		{ (*this)( std::forward< value_type >( o ) ); return *this; }
	output_iterator_from_functor &operator*() { return *this; }
	output_iterator_from_functor &operator++() { return *this; }
	output_iterator_from_functor &operator++(int) { return *this; }

	friend void finalize( output_iterator_from_functor< ftor > &o )
		{ finalize( static_cast< ftor & >( o ) ); }
};

template< typename ftor >
output_iterator_from_functor< ftor > make_output_iterator( ftor &&f )
	{ return { std::move( f ) }; }

template< typename output_iterator >
struct output_iterator_ref
	: std::iterator< std::output_iterator_tag, void, void, void, void >,
	std::reference_wrapper< output_iterator > {
	
	output_iterator_ref( output_iterator &in )
		: std::reference_wrapper< output_iterator >( in ) {}
	decltype( * std::declval<output_iterator_ref>().get() ) operator*() { return * this->get(); }
	output_iterator_ref &operator++() { ++ this->get(); return *this; }
};

/*	Since no copy of i can exist, postincrement can at best work like preincrement.
	This is still good though, since operator= performs increment on such iterators. */
template< typename ftor >
output_iterator_ref< output_iterator_from_functor< ftor > > &
operator++( output_iterator_ref< output_iterator_from_functor< ftor > > &r, int ) { r.get() ++; return r; }

template< typename output_iterator > // this seems very broken, and accepts perfect forwarding ctors as copy ctors
typename std::enable_if< std::is_copy_constructible< output_iterator >::value,
	output_iterator >::type
operator++( output_iterator_ref< output_iterator > &r, int ) { return r.get() ++; }

template< typename ftor >
output_iterator_ref< output_iterator_from_functor< ftor > >
ref( output_iterator_from_functor< ftor > &i )
	{ return { i }; }

/*	There is no reason to take a reference of a copyable iterator. Presume that all iterators besides
	output_iterator_from_functor are copyable, and return a copy, not a reference. The other way should work, too. */
template< typename output_iterator >
typename std::enable_if< std::is_base_of< std::output_iterator_tag,
						typename std::iterator_traits< output_iterator >::iterator_category >::value, 
	output_iterator >::type
ref( output_iterator &i )
	{ return { i }; }

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

class wstring_convert { // emulate §22.3.3.2.2, not yet implemented in G++
	char32_t min, result;
	int len;
public:
	wstring_convert() : len( 0 ) {}
	std::u32string from_bytes( uint8_t c ) {
		if ( len == 0 ) {
			if ( c >= 0xC0 ) {
				if ( c == 0xFF ) malformed: {
					throw std::range_error( "malformed UTF-8" );
				}
				len = 1;
				for ( uint8_t cback = c; cback & 0x20; cback <<= 1 ) {
					++ len;
				}
				result = ( c + ( 0x80 >> len ) ) & 0xFF;
				min = 1 << ( len > 1? len * 5 + 1 : 7 ); // lowest acceptible end result
				return {};
			
			} else if ( c >= 0x80 ) {
				goto malformed;
			} else {
				return { c }; // common case - it's ASCII
			}
		} else {
			if ( c < 0x80 || c >= 0xC0 ) goto malformed;
			result = result << 6 | ( c & 0x3F );
			if ( -- len == 0 ) {
				if ( result < min ) goto malformed;
				return { result };
			
			} else return {};
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
