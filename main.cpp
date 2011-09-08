// Cplus project, sample driver
// copyright David Krauss, created 8/22/11

#include "phase_1_2.hpp"
#include "phase_3.hpp"
#include "phase_4.hpp"

#include <clocale>
#include <iostream>

int main( int argc, char *argv[] ) {
	std::setlocale( LC_ALL, "" );
	
	cplus::util::output_iterator_from_functor< cplus::phase1_2<
	cplus::util::output_iterator_from_functor< cplus::phase3<
	cplus::util::output_iterator_from_functor< cplus::phase4<
	cplus::util::output_iterator_from_functor< std::function< void (cplus::pp_token &&)
		> > > > > > > > pile(
		[]( cplus::pp_token &&token )
			{ std::cout << '`' << token.s << '`' << int(token.type); }
		);
	
	std::copy( std::istreambuf_iterator< char >( std::cin ), std::istreambuf_iterator< char >(),
				ref( pile ) );
	
	finalize( pile );
}
