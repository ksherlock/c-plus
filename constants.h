// Cplus project, constant values describing the language (not the implementation in particular)
// copyright David Krauss, created 8/26/11
 
#ifndef CPLUS_CONSTANTS
#define CPLUS_CONSTANTS

#include <type_traits>
#include <algorithm>

namespace cplus {

template< std::size_t n >
constexpr bool char_in_set( uint8_t const (&map)[ n ], char32_t c );

namespace char_set {

constexpr uint8_t basic_source[] = {
	0x00, 0x74, 0x00, 0x00,	// control chars, four are whitespace
	0xF7, 0xFF, 0xFF, 0xFF,	// punctuation except $ and numbers
	0x7F, 0xFF, 0xFF, 0xFF,	// skip @; uppercase letters
	0x7F, 0xFF, 0xFF, 0xFE,	// skip ` and DEL; lowercase letters
};

// Reimplement some things from cctype, but improve uniformity at little effort.
constexpr uint8_t space[] = {
	0x00, 0x74, 0x00, 0x00,
	0x80, 0x00, 0x00, 0x00,
};

constexpr uint8_t digit[] = {
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xFF, 0xC0,
};

constexpr uint8_t punct[] = {
	0x00, 0x00, 0x00, 0x00,
	0x56, 0xFF, 0x00, 0x3F, // Do not include single or double quotes
	0x00, 0x00, 0x00, 0x16,
	0x00, 0x00, 0x00, 0x1E,
};

constexpr uint8_t multipunct[] = { // characters appearing in multichar punctuators
	0x00, 0x00, 0x00, 0x00,
	0x56, 0x37, 0x00, 0x2E, // !#%& *+-./ :<=>
	0x00, 0x00, 0x00, 0x02, // ^
	0x00, 0x00, 0x00, 0x08, // |
};

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

constexpr uint8_t latin_alpha_map[] = {
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x7F, 0xFF, 0xFF, 0xE1,
	0x7F, 0xFF, 0xFF, 0xE0,
	
	0x00, 0x00, 0x00, 0x00,
	0x00, 0xA5, 0x3D, 0xEE, // 00A8, 00AA, 00AD, 00AF, 00B2-00B5, 00B7-00BA, 00BC-00BE,
	0xFF, 0xFF, 0xFE, 0xFF, // 00C0-00D6,
	0xFF, 0xFF, 0xFE, 0xFF, // 00D8-00F6, 00F8-00FF
};

constexpr uint8_t latin_alnum_map[] = {
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xFF, 0xC0,
	0x7F, 0xFF, 0xFF, 0xE1,
	0x7F, 0xFF, 0xFF, 0xE0,
	
	0x00, 0x00, 0x00, 0x00,
	0x00, 0xA5, 0x3D, 0xEE, // 00A8, 00AA, 00AD, 00AF, 00B2-00B5, 00B7-00BA, 00BC-00BE,
	0xFF, 0xFF, 0xFE, 0xFF, // 00C0-00D6,
	0xFF, 0xFF, 0xFE, 0xFF, // 00D8-00F6, 00F8-00FF
};

constexpr uint8_t high_punct_map[] = {
	0x00, 0x1C, 0x00, 0x00, // 200B-200D,
	0x00, 0x3E, 0x00, 0x01, // 202A-202E, 203F-2040,
	0x80, 0x00, 0x08, 0x00, // 2054,
	0xFF, 0xFF, 0xFF, 0xFF, // 2060-206F, 2070-218F,
};

constexpr uint8_t cjk_punct_map[] {
	0x0F, 0x00, 0x00, 0x00, // 3004-3007,
	0x7F, 0xFF, 0x7F, 0xFF, // 3021-302F, 3031-303F
};

constexpr bool check_maps( range const ranges[], char32_t c, std::size_t found )
	{ return true; }

template< size_t n_ranges, size_t n_map, typename ... rem >
constexpr bool check_maps( range const (&ranges)[ n_ranges ], char32_t c, std::size_t found, std::size_t r, 
							uint8_t const (&map)[ n_map ], rem const & ... tail ) {
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
constexpr bool char_in_set( uint8_t const (&map)[ n ], char32_t c )
	{ return c < n * 8 && map[ c / 8 ] & ( 0x80 >> (c & 7) ); }

template< std::size_t n >
constexpr bool char_in_set( char_set::range const (&ranges)[ n ], char32_t c ) {
	return c < 0x100?
		ranges == char_set::initial? // kludge
			char_in_set( char_set::latin_alpha_map, c )
		:	char_in_set( char_set::latin_alnum_map, c )
	:	char_set::check_range( ranges, c, upper_bound( ranges, c ) );
}


std::string const multichar_punctuators[] = { // sorted in ASCII lexi order
	"!=", "##", "%:", "%:%:", "%=", "%>", "&&", "&=", "*=", "++", "+=", "--", 
	"-=", "->", "->*", ".*", "...", "/*", "//", "/=", "::", ":>", "<%", "<:", 
	"<::", "<<", "<<=", "<=", "==", ">=", ">>", ">>=", "^=", "|=", "||" // <:: from §2.5/3
}, *const multichar_punctuators_end = std::end( multichar_punctuators ),
	*const block_comment = std::lower_bound( multichar_punctuators, multichar_punctuators_end, "/*" ),
	*const line_comment = std::lower_bound( multichar_punctuators, multichar_punctuators_end, "//" ),
	*const less_scope = std::lower_bound( multichar_punctuators, multichar_punctuators_end, "<::" ),
	*const alt_hash = std::lower_bound( multichar_punctuators, multichar_punctuators_end, "%:" );

std::string const id_punctuators[] = {
	"and", "and_eq", "bitand", "bitor", "compl", "delete", "include",
	"new", "not", "not_eq", "or", "or_eq", "xor", "xor_eq"
}, *const include_directive = std::lower_bound( id_punctuators, std::end( id_punctuators ), "include" );

std::string const string_prefixes[] =
	{ "L", "LR", "R", "U", "UR", "u8", "u8R" };

namespace pp_constants {

pp_token const //newline{ pp_token_type::ws, "\n" }, // don't test for equality to this; newline runs are condensed
	space{ pp_token_type::ws, " " },
	placemarker{ pp_token_type::ws, "" },
	recursion_stop{ pp_token_type::punct, "" },
	comma{ pp_token_type::punct, "," },
	variadic_decl{ pp_token_type::punct, "..." },
	lparen{ pp_token_type::punct, "(" },
	rparen{ pp_token_type::punct, ")" },
	concat{ pp_token_type::punct, "##" },
	concat_alt{ pp_token_type::punct, "%:%:" },
	stringize{ pp_token_type::punct, "#" },
	stringize_alt{ pp_token_type::punct, "%:" },
	variadic{ pp_token_type::id, "__VA_ARGS__" },
	include_directive{ pp_token_type::directive, "include" },
	define_directive{ pp_token_type::directive, "define" },
	undef_directive{ pp_token_type::directive, "undef" },
	error_directive{ pp_token_type::directive, "error" },
	line_directive{ pp_token_type::directive, "line" },
	if_directive{ pp_token_type::directive, "if" },
	else_directive{ pp_token_type::directive, "else" },
	elif_directive{ pp_token_type::directive, "elif" },
	endif_directive{ pp_token_type::directive, "endif" },
	ifdef_directive{ pp_token_type::directive, "ifdef" },
	ifndef_directive{ pp_token_type::directive, "ifndef" },
	pragma_directive{ pp_token_type::directive, "pragma" },
	pragma_operator{ pp_token_type::id, "_Pragma" },
	defined_operator{ pp_token_type::id, "defined" },
	false_value{ pp_token_type::id, "false" },
	true_value{ pp_token_type::id, "true" },
	empty_string{ pp_token_type::string, "\"\"" };

std::string const reserved_macro_names[] = { // keywords + defined override final noreturn carries_dependency
	"__CPLUS_STRINGIZE__", "__FILE__", "__LINE__", // only forbid redefining macros tied to hard code
	"alignas", "alignof", "asm", "auto", "bool", "break", "carries_dependency", "case", "catch", "char", 
	"char16_t", "char32_t", "class", "const", "constexpr", "const_cast", "continue", "decltype", "default", 
	"defined", "delete", "do", "double", "dynamic_cast", "else", "enum", "explicit", "export", "extern", "false", 
	"final", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable", "namespace", "new", 
	"noexcept", "noreturn", "nullptr", "operator", "override", "private", "protected", "public", "register", 
	"reinterpret_cast", "return", "short", "signed", "sizeof", "static", "static_assert", "static_cast", 
	"struct", "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef", "typeid", 
	"typename", "union", "unsigned", "using", "virtual", "void", "volatile", "wchar_t", "while" //, "yikes!"
};

template< typename forward_iterator >
inline forward_iterator skip_space( forward_iterator &it, forward_iterator end )
	{ while ( it != end && it->type == pp_token_type::ws ) ++ it; return it; }

} // end namespace pp_constants

}

#endif
