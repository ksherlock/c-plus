// Cplus project, data structures and constants interfacing the stages.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_FORMATS_H
#define CPLUS_FORMATS_H

#include "stage.h"
#include "construct.h"

#include <initializer_list>

#ifndef CPLUS_USE_STD_STRING
#	include "string.h"
#else
namespace cplus {
	using std::string;

	struct string_pool : std::allocator< char >
		{ string_pool( char const * ) {} };
	string_pool literal_pool( "" );
	string repool( string s, string_pool p ) { return { s.c_str(), p }; }
}
#endif

namespace cplus {

struct token : construct {
	int type;
	string s;

	token() : type(), s() {}
	token( int in_type, string const &in_s, construct in_c = {} ) // pseudo-aggregate
		: construct( std::move( in_c ) ), type( in_type ), s( in_s ) {}

	token &assign_content( token const &rhs )
		{ type = rhs.type; s = rhs.s; return * this; }
	token &reallocate( string_pool &pool )
		{ s = repool( s, pool ); return * this; }

	friend bool operator== ( token const &l, token const &r )
		{ return l.type == r.type && l.s == r.s; }
	friend bool operator!= ( token const &l, token const &r )
		{ return ! ( l == r ); }
};
typedef std::vector< token > tokens;

typedef raw_file inclusion;

// Output format for Phases 1-2
struct config_pragma_base : config_base {
	typedef std::function< void( tokens && ) > pragma_function;
	typedef std::map< string, pragma_function > pragma_map;
	typedef std::initializer_list< pragma_map::value_type > pragma_handler_list; // C array would suffice but can't be empty
	pragma_handler_list pragma_handlers() { return {}; } // (non-virtual) override in derived class
};
struct propagate_pragma {}; // exception indicates pragma handler is defaulting

struct trigraph; // Undefined but used for delimiter.
struct ucn : raw_codepoint { using raw_codepoint::raw_codepoint; };
template< typename /* trigraph or ucn */ >
struct mapped_char : raw_char { using raw_char::raw_char; };

struct line_splice : construct
	{ explicit line_splice( construct in_c = {} ) : construct( std::move( in_c ) ) {} };

struct char_decoder_config : config_pragma_base {
	bool disable_trigraphs;
	
	pragma_handler_list pragma_handlers() {
		static pragma_handler_list ret = { { "trigraphs", [this]( tokens &&in ) {
			if ( in.size() != 1 ) bad_bool:
				throw error( in.empty()? token() : in[ 0 ], "Expected a numeric Boolean value." );
			try { disable_trigraphs = ! stoi( in[ 0 ].s ); }
			catch ( ... ) { goto bad_bool; }
		} } };
		return ret;
	}
};

// Output format for Phases 3-4
CPLUS_IMPORTABLE_ENUM( token_type, ws, id, num, punct, string_lit, char_lit, header_name, misc )

bool token_semantic_equal( token const & l, token const & r ) {
	if ( l.type != token_type::ws || r.type != token_type::ws ) return l.type == r.type && l.s == r.s;
	auto all_splices = []( string const & in ) {
		if ( in.size() % 2 ) return false;
		for ( auto pen = in.begin(); pen != in.end(); pen += 2 ) {
			if ( ! std::equal( pen, pen + 2, "\\\n" ) ) return false;
		}
		return true;
	};
	return all_splices( l.s ) == all_splices( r.s );
}

enum class lex_decode_state { normal, raw, escape };

struct lexer_config : config_pragma_base {
	mutable string_pool stream_pool;
	
	bool preserve_space;
	
	pragma_handler_list pragma_handlers() {
		static pragma_handler_list ret = { { "preserve_space", [this]( tokens &&in ) {
			if ( in.size() != 1 ) bad_bool:
				throw error( in.empty()? token() : in[ 0 ], "Expected a numeric Boolean value." );
			try { preserve_space = stoi( in[ 0 ].s ); }
			catch ( ... ) { goto bad_bool; }
		} } };
		return ret;
	}
	lexer_config() : stream_pool( "stream" ), preserve_space( false ) {}
};

struct macro_replacement : instantiation { // source of tokens from replacement list
	tokens const args; // substitutions point into this
	tokens const &replacement; // points to name_map; positions index this
	macro_replacement( construct in_origin, tokens const &in_replacement, tokens &&in_args )
		: instantiation( std::move( in_origin ) ), args( std::move( in_args ) ), replacement( in_replacement ) {}
	macro_replacement( construct in_origin, tokens &&in_replacement ) // for __LINE__ and __FILE__ macros
		: instantiation( std::move( in_origin ) ), args( std::move( in_replacement ) ), replacement( args ) {}
	
	virtual std::size_t size() const final override
		{ return replacement.size(); }
	virtual token const &component( location_t i ) const final override
		{ return replacement[ i ]; }
};
struct macro_substitution : instantiation { // an argument used within a macro
	tokens::const_iterator arg_begin, arg_end; // points into argument list; instantiation base points into replacement list
	macro_substitution( construct in_use, tokens::const_iterator in_beg, tokens::const_iterator in_end )
		: instantiation( std::move( in_use ) ), arg_begin( in_beg ), arg_end( in_end ) {}
	
	virtual std::size_t size() const final override
		{ return arg_end - arg_begin; }
	virtual token const &component( location_t i ) const final override
		{ return arg_begin[ i ]; }
};

std::string destringize( std::string in );

struct preprocessor_config : config_pragma_base {
	mutable string_pool macro_pool; // holds replacement lists (and other persistent data)
	std::vector< string > user_paths, system_paths; // header search set
	
	void push_paths( std::vector< string > &pathlist, tokens &&in )
		{ for ( auto &path : in ) pathlist.push_back( repool( destringize( std::move( path.s ) ).c_str(), macro_pool ) ); }
	pragma_handler_list pragma_handlers() {
		static pragma_handler_list ret = {
			{ "system_path", pragma_function( [this]( tokens &&in ) { push_paths( system_paths, std::move( in ) ); } ) },
			{ "user_path", pragma_function( [this]( tokens &&in ) { push_paths( user_paths, std::move( in ) ); } ) }
		};
		return ret;
	}
	preprocessor_config() : macro_pool( "macro" ) {}
};

struct directive; // Undefined, but used for delimiter< directive >.
struct pragma : construct {
	string s;
	pragma( string in_s, construct in_c ) : construct( std::move( in_c ) ), s( std::move( in_s ) ) {}
};

}

#endif
