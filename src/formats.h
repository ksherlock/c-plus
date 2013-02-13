// Cplus project, data structures and constants interfacing the stages.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_FORMATS
#define CPLUS_FORMATS

#include "framework.h"

namespace cplus {

struct inclusion : instantiation { // source file (e.g. header) inclusion instantiates the file's contents
	string file_name;
	inclusion( string in_file_name, token in_source )
		: instantiation( in_source ), file_name( in_file_name ) {}
};

// Output format for Phases 1-2
CPLUS_IMPORTABLE_ENUM( pp_char_source, normal, ucn, trigraph )

namespace file_location {
	int const column_shift = 32; // columns in MSW so they simply wrap around on overflow
	location_t const column_increment = location_t( 1 ) << column_shift,
					line_mask = column_increment - 1;
}

struct config_pragma_base : config_base {
	typedef std::function< void( tokens && ) > pragma_function;
	typedef std::map< string, pragma_function > pragma_map;
	typedef std::initializer_list< pragma_map::value_type > pragma_handler_list; // C array would suffice but can't be empty
	pragma_handler_list pragma_handlers() { return {}; } // (non-virtual) override in derived class
};
struct propagate_pragma {}; // exception indicates pragma handler is defaulting

struct pp_char {
	std::uint8_t c;
	location_t p; // position in file: column * 2^32 + line.
	pp_char_source s;
};

struct phase1_2_config : config_pragma_base {
	bool disable_trigraphs;
	
	pragma_handler_list pragma_handlers() {
		static pragma_handler_list ret = { { "trigraphs", pragma_function/*G++ workaround*/( [this]( tokens &&in ) {
			if ( in.size() != 1 ) bad_bool:
				throw error( in.empty()? token() : in[ 0 ], "Expected a numeric Boolean value." );
			try { disable_trigraphs = ! stoi( in[ 0 ].s ); }
			catch ( ... ) { goto bad_bool; }
		} ) } };
		return ret; // this is obviously wrong, but G++ seems to want to prematurely destroy the initializer_list
	}
};

// Output format for Phases 3-4
CPLUS_IMPORTABLE_ENUM( token_type, ws, id, num, punct, string_lit, char_lit, directive, header_name, misc )

struct raw_string_notification { bool entering; }; // These exceptions are thrown unless
struct inhibit_ucn_notification {}; // the second template arg to stage3 is set to false.

struct phase3_config : config_pragma_base {
	mutable string_pool stream_pool;
	bool preserve_space;
	
	pragma_handler_list pragma_handlers() {
		static pragma_handler_list ret = { { "preserve_space", pragma_function/*G++ workaround*/( [this]( tokens &&in ) {
			if ( in.size() != 1 ) bad_bool:
				throw error( in.empty()? token() : in[ 0 ], "Expected a numeric Boolean value." );
			try { preserve_space = stoi( in[ 0 ].s ); }
			catch ( ... ) { goto bad_bool; }
		} ) } };
		return ret; // this is obviously wrong, but G++ seems to want to prematurely destroy the initializer_list
	}
	phase3_config() : stream_pool( "stream" ), preserve_space( false ) {}
};

struct macro_replacement : instantiation { // source of tokens from replacement list
	tokens const args; // substitutions point into this
	tokens const &replacement; // points to name_map; positions index this
	macro_replacement( token &&in_origin, tokens const &in_replacement, tokens &&in_args )
		: instantiation( std::move( in_origin ) ), args( std::move( in_args ) ), replacement( in_replacement ) {}
	macro_replacement( token &&in_origin, tokens &&in_replacement ) // for __LINE__ and __FILE__ macros
		: instantiation( std::move( in_origin ) ), args( std::move( in_replacement ) ), replacement( args ) {}
	
	friend token instantiate( std::shared_ptr< macro_replacement > shared, tokens::const_iterator source )
		{ return token{ source->type, source->s, shared, location_t( source - shared->replacement.begin() ) }; }
};
struct macro_substitution : instantiation { // an argument used within a macro
	tokens::const_iterator arg_begin; // points into argument list; instantiation base points into replacement list
	macro_substitution( token &&in_use, tokens::const_iterator in_arg )
		: instantiation( in_use ), arg_begin( in_arg ) {}
};

std::string destringize( std::string in );

struct phase4_config : config_pragma_base {
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
	phase4_config() : macro_pool( "macro" ) {}
};

}

#endif
