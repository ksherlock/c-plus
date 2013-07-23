// Cplus project, base classes for grammatical structures.
// copyright David Krauss, created 7/23/13

#ifndef CPLUS_CONSTRUCT_H
#define CPLUS_CONSTRUCT_H

#include "util.h"

#include <algorithm>
#include <memory>
#include <string>

namespace cplus {

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

// Generic construct classes.
enum class delimiter_sense : bool { open, close };
template< typename, delimiter_sense > // Announce downstream the scope of a higher-level construct.
struct delimiter : construct { explicit delimiter( construct c ) : construct( std::move( c ) ) {} };

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
template< typename string >
struct raw_text : input_source {
	string text;
	
	raw_text( char const *p, construct c = {} ) : input_source( std::move( c ) ), text( p ) {}
	raw_text( string s, construct c = {} ) : input_source( std::move( c ) ), text( std::move( s ) ) {}
	
	virtual void filter( std::function< void( std::uint8_t ) > fn ) const override { filter<>( fn ); }
		
	template< typename ftor >
	void filter( ftor && fn ) const
		{ std::for_each( text.begin(), text.end(), std::ref( fn ) ); }
};

}

#endif
