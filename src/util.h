// Cplus project, general-purpose utilities.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_UTIL
#define CPLUS_UTIL

#include <memory>
#include <utility>
#include <iterator>
#include <algorithm>
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

namespace cplus { namespace util {

template< typename ... >
struct mention
	{ typedef void type; };

struct poor_conversion {
	template< typename t >
	poor_conversion( t const & ) {}
};

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

template< typename elem, typename tup, std::size_t offset = 0 >
struct tuple_index
	: std::integral_constant< std::size_t, offset > {};

template< typename elem, typename head, typename ... tail, std::size_t offset >
struct tuple_index< elem, std::tuple< head, tail ... >, offset >
	: std::integral_constant< std::size_t, tuple_index< elem, std::tuple< tail ... >, offset + 1 >::value > {};

template< typename elem, typename ... tail, std::size_t offset >
struct tuple_index< elem, std::tuple< elem, tail ... >, offset >
	: std::integral_constant< std::size_t, offset > {};

template< std::size_t i, typename t, typename = void >
struct tuple_element : std::tuple_element< i, t > {};

template< std::size_t i, typename t >
struct tuple_element< i, t, typename std::enable_if< i == std::tuple_size< t >::value >::type > {};

template< typename ... t >
struct tuple_cat
	{ typedef decltype( std::tuple_cat( std::declval< t >() ... ) ) type; };

template< typename t >
struct rvalue_reference_wrapper : std::reference_wrapper< t > {
	rvalue_reference_wrapper( t &o ) : std::reference_wrapper< t >( o ) {}
	t &&get() const { return static_cast< t && >( std::reference_wrapper< t >::get() ); }
	operator t&& () const { return get(); }
	operator t& () const = delete;
};

template< typename t >
rvalue_reference_wrapper< t > rref( t &o ) { return { o }; }

template< typename ... t >
struct amalgam : t ... {
	amalgam( t && ... in ) : t( std::move( in ) ) ... {} // not perfect forwarding
protected:
	void operator () ( struct amalgam_tag ) = delete;
	void operator () ( struct amalgam_tag * ) = delete; // GCC workaround: invalidate expression &fn::operator()
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
	typename mention< decltype( * base ++ = std::forward< value_type >( o ) ) >::type {
		if ( max != 0 && count == max ) throw std::range_error( "sequence too long" );
		++ count;
		* base ++ = std::forward< value_type >( o );
	}
};

template< typename output_iterator >
limit_range_ftor< output_iterator >
limit_range( output_iterator &&iter ) { return limit_range_ftor< output_iterator >{ std::move( iter ) }; }

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
	
	struct error : std::range_error {
		bool incomplete; // true if operator() returned false for this character.
		error( bool in_incomplete ) : std::range_error( "malformed UTF-8" ), incomplete( in_incomplete ) {}
	};
private:
	bool non_ascii( std::uint8_t c ) {
		if ( len == 0 ) {
			if ( c >= 0xC0 ) {
				if ( c == 0xFF ) throw error( false );
				len = 1;
				while ( c & ( 0x40 >> len ) ) ++ len;
				result = ( c + ( 0x80 >> len ) ) & 0xFF;
				min = 1 << ( len > 1? len * 5 + 1 : 7 ); // lowest acceptible end result
				return false;
			} else throw error( false );
		} else {
			if ( c < 0x80 || c >= 0xC0 ) throw error( true );
			result = result << 6 | ( c & 0x3F );
			if ( -- len != 0 ) return false;
			if ( result < min ) throw error( true );
			return true;
		}
	}
};

namespace query {
void ctime_r( ... ); // If ::ctime_r is to be found by unqualified lookup, the fallback must be found by ADL.

struct poor_converter // poor as poor_conversion, but better than "..."
	{ template< typename t > operator t & () {} };
}

template< bool en = std::is_same< char *, decltype( ctime_r( query::poor_converter(), query::poor_converter() ) ) >::value >
typename std::enable_if< en, char * >::type ctime( std::time_t const *t, char *&r )
	{ return ctime_r( typename std::enable_if< en, std::time_t const * >::type( t ), r ); } // bogus dependency guards potentially undefined name

template< bool en = std::is_same< char *, decltype( ctime_r( query::poor_converter(), query::poor_converter() ) ) >::value >
typename std::enable_if< ! en, char * >::type ctime( std::time_t const *t, char *&r )
	{ return r = std::ctime( t ); }
	
} } // end namespace cplus, end namespace util

#endif
