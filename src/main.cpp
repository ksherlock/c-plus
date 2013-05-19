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
void point_at( token const & );

void point_at_inclusion( token const &t ) {
	auto source = static_cast< inclusion * >( t.source.get() );
	std::string path = source->file_name.c_str();
	path = path.substr( 1, path.size() - 2 );
	std::ifstream in( path, std::ios::in | std::ios::binary );
	if ( ! in.is_open() ) {
		std::cerr << "Could not open source file " << path << '\n';
	} else try {
		in.exceptions( std::ios::badbit | std::ios::failbit );
		std::cerr.exceptions( std::ios::badbit | std::ios::failbit );
		
		std::uint32_t line = t.location & file_location::line_mask,
					column = t.location >> file_location::column_shift;
		column = std::max( column, std::uint32_t( 1 ) ); // advance newline to beginning of next line - hopefully never happens
		
		for ( std::uint32_t l = 1; l != line; l += in.get() == '\n' ) ; // throws at eof
		std::string line_text;
		getline( in, line_text );
		std::cerr << line_text << '\n';
		std::replace_copy_if( & line_text[0], & line_text[0] + column - 1, // align caret with error
			std::ostream_iterator< char >( std::cerr ),
			std::bind1st( std::not_equal_to< char >(), '\t' ), ' ' ); // preserve tabs for spacing on console
		std::cerr << "^ here\n" << source->file_name << ": line " << line << ", column " << column << '\n';
	} catch ( std::ios::failure & ) {
		if ( in.eof() ) std::cerr << "at end of input\n";
		else std::perror( "I/O error while printing context" );
	} catch ( std::exception &exc ) {
		std::cerr << "An error also occurred while attempting to print the error's context.\n"
			<< exc.what() << '\n';
	}
}

void point_at_replacement( token const &t, bool deep = true ) {
	auto source = static_cast< macro_replacement * >( t.source.get() );
	point_at( source->replacement[ t.location ] );
	if ( deep ) { // don't point out how a parameter came to be instantiated, after tracing its argument
		std::cerr << "in replacement of " << source->source.s << '\n';
		point_at( source->source );
	}
}

void point_at_substitution( token const &t ) {
	auto source = static_cast< macro_substitution * >( t.source.get() );
	point_at( source->arg_begin[ t.location ] );
	if ( static_cast< macro_replacement * >( source->source.source.get() ) // substitution only occurs within replacement
			->replacement[ source->source.location ].source ) { // don't point at internal macros such as "defined" or "#"
		std::cerr << "in macro parameter\n";
		point_at_replacement( source->source, false );
	} else {
		std::cerr << "(not descending into " << source->source.s << ")\n";
	}
}

void point_at( token const &t ) {
	if ( dynamic_cast< inclusion * >( t.source.get() ) ) point_at_inclusion( t );
	else if ( dynamic_cast< macro_replacement * >( t.source.get() ) ) point_at_replacement( t );
	else if ( dynamic_cast< macro_substitution * >( t.source.get() ) ) point_at_substitution( t );
	else if ( t.source ) std::cerr << "error message dispatch failure ("
		<< typeid( * t.source.get() ).name() << " @ " << t.source.get() << ")\n";
	else std::cerr << "(null source link for \"" << t.s << "\"!)\n";
}
}

int main( int argc, char *argv[] ) {
	int status = 0;
	std::setlocale( LC_ALL, "" );
	
	int count = 0;
	
	auto && pile = cplus::autoconfigured_pile< cplus::phase1_2, cplus::phase3, cplus::phase4, cplus::pragma_filter > (
		cplus::util::amalgamate(
			[&count]( cplus::token &&token ){ std::fwrite( token.s.c_str(), 1, token.s.size(), stdout ); },
			[]( cplus::error && err ) {
				std::clog << err.what() << '\n';
				if ( ! err.p.source ) std::cerr << "but I don't know where\n";
				point_at( err.p );
			}
		),
		nullptr
	);

	char initialization[] =
		"#define __STDC__ 1\n"
		"#define __cplusplus 199711L //201103L\n"
		"#define __i386__ 1\n"
		"#define __LP64__ 1\n"
		"#define __GNUC__ 4\n"
		"#pragma system_path "
			"\"/usr/local/lib/gcc/x86_64-apple-darwin12.2.0/4.8.0/include/\" "
			"\"/usr/include/\" \"/usr/include/sys/\" \"/usr/local/include/\" "
			"\"/usr/local/include/c++/4.8.0/\" "
			"\"/usr/local/include/c++/4.8.0/x86_64-apple-darwin12.2.0/i386/\"\n"
		//"#include <iostream>\n"
		;
	
	cplus::pass( initialization, std::end( initialization ) - 1, pile );
	
	cplus::pass( std::istreambuf_iterator< char >{ std::cin }, std::istreambuf_iterator< char >{}, pile );
	finalize( pile );
	
	/*std::cerr << "fast x " << cplus::fast_dispatch << ", slow x " << cplus::slow_dispatch << '\n';
	std::copy( std::begin( cplus::slow_histo ), std::end( cplus::slow_histo ), std::ostream_iterator< int >( std::cerr, ", " ) );
	std::cerr << '\n' << "total passed " << count << '\n';
	*/
	return status;
}
