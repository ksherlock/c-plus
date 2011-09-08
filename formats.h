// Cplus project, data structures and constants interfacing the stages.
// copyright David Krauss, created 8/26/11

#ifndef CPLUS_FORMATS
#define CPLUS_FORMATS

#include <stdexcept>
#include <cstdint>
#include <utility>
#include <string>
#include <vector>
#include <ios>

namespace cplus {

// Error reporting format (needs work)
struct error : std::runtime_error {
	std::streamoff p;
	error( std::streamoff pos, char const *what )
		: std::runtime_error( what ), p( pos ) {}
};

// Output format for Phases 1-2
#define CPLUS_PP_CHAR_SOURCES		normal, ucn, trigraph
enum class pp_char_source { CPLUS_PP_CHAR_SOURCES };

struct pp_char {
	uint8_t c;
	std::streamoff p;
	pp_char_source s;
};

// Output format for Phases 3-4
#define CPLUS_PP_TOKEN_TYPES		ws, id, num, punct, string, char_lit, directive, header_name, misc	
enum class pp_token_type { CPLUS_PP_TOKEN_TYPES };

struct pp_token {
	pp_token_type type;
	std::string s;
	std::streamoff p;
	
	friend bool operator== ( pp_token const &l, pp_token const &r )
		{ return l.type == r.type && l.s == r.s; }
};
using std::rel_ops::operator!=;

typedef std::vector< pp_token > pp_tokens;

namespace config {
	std::string const search_paths_user = "__CPLUS_PATHS_USER__",
		search_paths_system = "__CPLUS_PATHS_SYSTEM__";
}

struct raw_string_notification { bool entering; };
struct inhibit_ucn_notification {};

/*	Each pipeline step chains to finalize for the next one. When an iterator without
	finalize is reached, synchronization stops. That iterator should be the last. */
template< typename t >
void finalize( t & ) {}

}

#endif
