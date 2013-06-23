// Cplus project, sample driver
// copyright David Krauss, created 8/22/11

#include <iostream>
#include <typeinfo>

#include "phase_1_2.hpp"
#include "phase_3.hpp"
#include "phase_4.hpp"

#include <fstream>
#include <clocale>
#include <cstdio> // for std::perror

namespace {
using namespace cplus;
void point_at( construct const & );

void point_at_inclusion( construct const &t ) {
	auto source = t.get_parent< inclusion >();
	std::string path = source->path.c_str();
	std::ifstream in( path, std::ios::in | std::ios::binary );
	if ( ! in.is_open() ) {
		std::cerr << "Could not open source file " << path << '\n';
	} else try {
		in.exceptions( std::ios::badbit | std::ios::failbit );
		std::cerr.exceptions( std::ios::badbit | std::ios::failbit );
		
		std::size_t line = 1, column = std::max( std::get< 1 >( t.get_location< inclusion >() ), 1ull );
		std::string line_text;
		while ( getline( in, line_text ) && column > line_text.size() + 1 ) column -= line_text.size() + 1, ++ line;
		
		std::cerr << line_text << '\n';
		std::replace_copy_if( & line_text[0], & line_text[0] + column, // align caret with error
			std::ostream_iterator< char >( std::cerr ),
			std::bind1st( std::not_equal_to< char >(), '\t' ), ' ' ); // preserve tabs for spacing on console
		std::cerr << "^ here\n" << source->path << ": line " << line << ", column " << column << '\n';
	} catch ( std::ios::failure & ) {
		if ( in.eof() ) std::cerr << "at end of input\n";
		else std::perror( "I/O error while printing context" );
	} catch ( std::exception &exc ) {
		std::cerr << "An error also occurred while attempting to print the error's context.\n"
			<< exc.what() << '\n';
	}
}

void point_at_replacement( construct const &t, bool deep = true ) {
	point_at( t.get_source< macro_replacement >() ); // point inside the macro definition
	if ( deep ) { // don't point out how a parameter came to be instantiated, after tracing its argument
		auto && param = t.get_source< macro_substitution >();
		std::cerr << "in replacement of " << param.s << '\n';
		point_at( param );
	}
}

void point_at_substitution( construct const &t ) {
	point_at( t.get_source< macro_substitution >() );
	if ( t.get_source< macro_replacement >().get_parent() ) { // don't point at internal macros such as "defined" or "#"
		std::cerr << "in macro parameter\n";
		point_at_replacement( t.get_source< macro_replacement >(), false );
	}
}

void point_at( construct const &t ) {
	if ( dynamic_cast< inclusion const * >( t.get_parent() ) ) point_at_inclusion( t );
	else if ( dynamic_cast< macro_replacement const * >( t.get_parent() ) ) point_at_replacement( t );
	else if ( dynamic_cast< macro_substitution const * >( t.get_parent() ) ) point_at_substitution( t );
	else if ( t.get_parent() ) std::cerr << "error message dispatch failure ("
		<< typeid( * t.get_parent() ).name() << " @ " << t.get_parent() << ")\n";
}
}

int main( int argc, char *argv[] ) {
	int status = 0;
	std::setlocale( LC_ALL, "" );
	
	int count = 0;
	
	auto && pile = cplus::autoconfigured_pile< cplus::phase1_2, cplus::phase3, cplus::phase4, cplus::pragma_filter > (
		cplus::util::amalgamate(
			[&count]( cplus::token &&token ){ std::fwrite( token.s.c_str(), 1, token.s.size(), stdout ); std::fwrite( "·", 1, std::strlen( "·" ), stdout ); },
			[]( cplus::error && err ) {
				std::clog << err.what() << '\n';
				point_at( static_cast< token const & >( * err.p ) );
			}
		)
	);

	instantiate( std::make_shared< cplus::raw_text >(
		"#define __STDC__ 1\n"
		"#define __cplusplus 199711L //201103L\n"
		"#define __i386__ 1\n"
		"#define __LP64__ 1\n"
		"#define __GNUC__ 4\n"
		"#pragma preserve_space 1\n"
		"#pragma system_path "
			"\"/usr/local/lib/gcc/x86_64-apple-darwin12.4.0/4.9.0/include/\" "
			"\"/usr/include/\" \"/usr/include/sys/\" \"/usr/local/include/\" "
			"\"/usr/local/include/c++/4.9.0/\" "
			"\"/usr/local/include/c++/4.9.0/x86_64-apple-darwin12.4.0/i386/\"\n"
	), pile );
	
	cplus::pass( std::istreambuf_iterator< char >{ std::cin }, std::istreambuf_iterator< char >{}, pile );
	finalize( pile );
	
	/*std::cerr << "fast x " << cplus::fast_dispatch << ", slow x " << cplus::slow_dispatch << '\n';
	std::copy( std::begin( cplus::slow_histo ), std::end( cplus::slow_histo ), std::ostream_iterator< int >( std::cerr, ", " ) );
	std::cerr << '\n' << "total passed " << count << '\n';
	*/
	return status;
}
