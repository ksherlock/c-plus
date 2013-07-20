// Cplus project, constant values describing the language (not the implementation in particular)
// copyright David Krauss, created 8/26/11
 
#ifndef CPLUS_CONSTANTS
#define CPLUS_CONSTANTS

#include <type_traits>
#include <algorithm>

namespace cplus {

template< std::size_t n >
constexpr bool char_in_set( std::uint8_t const (&map)[ n ], char32_t c );

namespace char_set {

typedef std::uint8_t char_bitmap[ 0x80 / 8 ];

constexpr char_bitmap basic_source = {
	0x00, 0x74, 0x00, 0x00,	// control chars, four are whitespace
	0xF7, 0xFF, 0xFF, 0xFF,	// punctuation except $; numbers
	0x7F, 0xFF, 0xFF, 0xFF,	// skip @; uppercase letters
	0x7F, 0xFF, 0xFF, 0xFE,	// skip ` and DEL; lowercase letters
};

// Reimplement some things from cctype, but improve uniformity at little effort.
constexpr char_bitmap space = {
	0x00, 0x74, 0x00, 0x00,
	0x80, 0x00, 0x00, 0x00,
};

constexpr char_bitmap digit = {
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xFF, 0xC0,
};

constexpr char_bitmap punct = {
	0x00, 0x00, 0x00, 0x00,
	0x56, 0xFF, 0x00, 0x3F, // Do not include single or double quotes
	0x00, 0x00, 0x00, 0x16,
	0x00, 0x00, 0x00, 0x1E,
};

constexpr char_bitmap multipunct = { // characters appearing in multichar punctuators
	0x00, 0x00, 0x00, 0x00,
	0x56, 0x37, 0x00, 0x2E, // !#%& *+-./ :<=>
	0x00, 0x00, 0x00, 0x02, // ^
	0x00, 0x00, 0x00, 0x08, // |
};

constexpr char_bitmap ascii_alnum = {
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xFF, 0xC0,
	0x7F, 0xFF, 0xFF, 0xE1,
	0x7F, 0xFF, 0xFF, 0xE0,
};

constexpr char_bitmap none = {};

struct range {
	char16_t top, bottom;
	friend constexpr bool operator< ( char32_t c, range r )
		{ return c < r.top; }
};
constexpr range initial[] = { // excludes characters disallowed initially §E.2
	{ 0x300, 0x100 }, { 0x1680, 0x0370 }, { 0x180E, 0x1681 }, { 0x1DC0, 0x180F },
	{ 0x2000, 0x1E00 }, { 0x2080, 0x2000 }, { 0x20D0, 0x2080 }, { 0x2190, 0x2100 }, 
	{ 0x2500, 0x2460 }, { 0x2794, 0x2776 }, { 0x2F00, 0x2C00 }, { 0x3000, 0x2E80 },
	{ 0x3040, 0x3000 }, { 0xD800, 0x3040 }, { 0xFD3E, 0xF900 }, 
	{ 0xFDD0, 0xFD40 }, { 0xFE20, 0xFDF0 }, { 0xFE45, 0xFE30 }, { 0xFFFE, 0xFE47 } };

constexpr range identifier[] = { // represents all of §E.1
	{ 0x1680, 0x0100 }, { 0x180E, 0x1681 }, { 0x2000, 0x180F },
	{ 0x2080, 0x2000 }, { 0x2190, 0x2080 }, { 0x2500, 0x2460 }, 
	{ 0x2794, 0x2776 }, { 0x2F00, 0x2C00 }, { 0x3000, 0x2E80 },
	{ 0x3040, 0x3000 }, { 0xD800, 0x3040 }, { 0xFD3E, 0xF900 }, 
	{ 0xFDD0, 0xFD40 }, { 0xFE45, 0xFDF0 }, { 0xFFFE, 0xFE47 } };

template< std::size_t n >
constexpr std::size_t upper_bound( range const (&ranges)[ n ], char32_t c,
			std::size_t first = 0, std::size_t pivot = n / 2, std::size_t last = n ) {
	return first == last? first :
		c < ranges[ pivot ]?
			upper_bound( ranges, c, first, first + ( pivot - first ) / 2, pivot )
		:	upper_bound( ranges, c, pivot + 1, pivot + 1 + ( last - pivot - 1 ) / 2, last );
}

constexpr std::uint8_t latin_alpha_map[] = {
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x7F, 0xFF, 0xFF, 0xE1,
	0x7F, 0xFF, 0xFF, 0xE0,
	
	0x00, 0x00, 0x00, 0x00,
	0x00, 0xA5, 0x3D, 0xEE, // 00A8, 00AA, 00AD, 00AF, 00B2-00B5, 00B7-00BA, 00BC-00BE,
	0xFF, 0xFF, 0xFE, 0xFF, // 00C0-00D6,
	0xFF, 0xFF, 0xFE, 0xFF, // 00D8-00F6, 00F8-00FF
};

constexpr std::uint8_t latin_alnum_map[] = {
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xFF, 0xC0,
	0x7F, 0xFF, 0xFF, 0xE1,
	0x7F, 0xFF, 0xFF, 0xE0,
	
	0x00, 0x00, 0x00, 0x00,
	0x00, 0xA5, 0x3D, 0xEE, // 00A8, 00AA, 00AD, 00AF, 00B2-00B5, 00B7-00BA, 00BC-00BE,
	0xFF, 0xFF, 0xFE, 0xFF, // 00C0-00D6,
	0xFF, 0xFF, 0xFE, 0xFF, // 00D8-00F6, 00F8-00FF
};

constexpr std::uint8_t high_punct_map[] = {
	0x00, 0x1C, 0x00, 0x00, // 200B-200D,
	0x00, 0x3E, 0x00, 0x01, // 202A-202E, 203F-2040,
	0x80, 0x00, 0x08, 0x00, // 2054,
	0xFF, 0xFF, 0xFF, 0xFF, // 2060-206F, 2070-218F,
};

constexpr std::uint8_t cjk_punct_map[] {
	0x0F, 0x00, 0x00, 0x00, // 3004-3007,
	0x7F, 0xFF, 0x7F, 0xFF, // 3021-302F, 3031-303F
};

constexpr bool check_maps( range const ranges[], char32_t c, std::size_t found )
	{ return true; }

template< size_t n_ranges, size_t n_map, typename ... rem >
constexpr bool check_maps( range const (&ranges)[ n_ranges ], char32_t c, std::size_t found, std::size_t r, 
							std::uint8_t const (&map)[ n_map ], rem const & ... tail ) {
	return found == r?
		char_in_set( map, c - ranges[ r ].bottom )
	:	check_maps( ranges, c, found, tail ... );
}

template< size_t n_ranges >
constexpr bool check_range( range const (&ranges)[ n_ranges ], char32_t c, std::size_t found ) {
	return found == n_ranges?
		c < 0xF0000 && ( c & 0xFFFF ) <= 0xFFFD
	:	c >= ranges[ found ].bottom
		&& check_maps( ranges, c, found,
			upper_bound( ranges, 0x2000 ), high_punct_map,
			upper_bound( ranges, 0x3000 ), cjk_punct_map
		);
}

} // end namespace char_set

template< std::size_t n >
constexpr bool char_in_set( std::uint8_t const (&map)[ n ], char32_t c )
	{ return c < n * 8 && map[ c / 8 ] & ( 0x80 >> (c & 7) ); }

template< std::size_t n >
constexpr bool char_in_set( char_set::range const (&ranges)[ n ], char32_t c ) {
	return c < 0x100?
		ranges == char_set::initial? // kludge
			char_in_set( char_set::latin_alpha_map, c )
		:	char_in_set( char_set::latin_alnum_map, c )
	:	char_set::check_range( ranges, c, upper_bound( ranges, c ) );
}


string const multichar_punctuators[] = { // sorted in ASCII lexi order
	"!=", "##", "%:", "%:%:", "%=", "%>", "&&", "&=", "*=", "++", "+=", "--", 
	"-=", "->", "->*", ".*", "...", "/*", "//", "/=", "::", ":>", "<%", "<:", 
	"<::", "<<", "<<=", "<=", "==", ">=", ">>", ">>=", "^=", "|=", "||" // <:: from §2.5/3
}, *const multichar_punctuators_end = std::end( multichar_punctuators ),
	*const block_comment = std::lower_bound( multichar_punctuators, multichar_punctuators_end, "/*" ),
	*const line_comment = std::lower_bound( multichar_punctuators, multichar_punctuators_end, "//" ),
	*const less_scope = std::lower_bound( multichar_punctuators, multichar_punctuators_end, "<::" ),
	*const hash_alt = std::lower_bound( multichar_punctuators, multichar_punctuators_end, "%:" );

string const id_punctuators[] = {
	"and", "and_eq", "bitand", "bitor", "compl", "delete", "include",
	"new", "not", "not_eq", "or", "or_eq", "xor", "xor_eq"
}, *const include_directive = std::lower_bound( id_punctuators, std::end( id_punctuators ), "include" );

string const string_lit_prefixes[] =
	{ "L", "LR", "R", "U", "UR", "u", "u8", "u8R", "uR" };
string const char_lit_prefixes[] =
	{ "L", "U", "u" };

namespace pp_constants {

token const newline{ token_type::ws, "\n" }, // don't test for equality to this; newline runs are condensed
	space{ token_type::ws, " " },
	placemarker{ token_type::ws, "" }, // also happens to be the result of value-initialization
	recursion_stop{ token_type::punct, "" },
	comma{ token_type::punct, "," },
	dot{ token_type::punct, "." },
	asterisk{ token_type::punct, "*" },
	variadic_decl{ token_type::punct, "..." },
	lparen{ token_type::punct, "(" },
	rparen{ token_type::punct, ")" },
	colon{ token_type::punct, ":" },
	conditional{ token_type::punct, "?" },
	concat{ token_type::punct, "##" },
	concat_alt{ token_type::punct, "%:%:" },
	stringize{ token_type::punct, "#" },
	stringize_alt{ token_type::punct, "%:" },
	variadic{ token_type::id, "__VA_ARGS__" },
	line_macro{ token_type::id, "__LINE__" },
	file_macro{ token_type::id, "__FILE__" },
	include_directive{ token_type::id, "include" },
	define_directive{ token_type::id, "define" },
	undef_directive{ token_type::id, "undef" },
	error_directive{ token_type::id, "error" },
	line_directive{ token_type::id, "line" },
	if_directive{ token_type::id, "if" },
	else_directive{ token_type::id, "else" },
	elif_directive{ token_type::id, "elif" },
	endif_directive{ token_type::id, "endif" },
	ifdef_directive{ token_type::id, "ifdef" },
	ifndef_directive{ token_type::id, "ifndef" },
	pragma_directive{ token_type::id, "pragma" },
	pragma_operator{ token_type::id, "_Pragma" },
	defined_operator{ token_type::id, "defined" },
	false_value{ token_type::id, "false" },
	true_value{ token_type::id, "true" },
	zero{ token_type::num, "0" },
	one{ token_type::num, "1" },
	bang{ token_type::punct, "!" },
	empty_string{ token_type::string_lit, "\"\"" };

inline bool is_stringize( token const &in )
	{ return in == stringize || in == stringize_alt; }
inline bool is_concat( token const &in )
	{ return in == concat || in == concat_alt; }

template< typename forward_iterator >
inline forward_iterator skip_space( forward_iterator &it, forward_iterator end )
	{ while ( it != end && it->type == token_type::ws ) ++ it; return it; }

} // end namespace pp_constants

}

#endif
