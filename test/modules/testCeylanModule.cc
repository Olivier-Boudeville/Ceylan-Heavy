#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <iostream>	// for cerr, endl
#include <exception>

#include <string>
using std::string ;



/**
 * Test for Module handling services.
 *
 * @see Module
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;	


	try
	{


		LogPlug::info( "Testing Module's implementation." ) ;

		Ceylan::Version moduleVersion( 0, 1, 2) ;
		
		Ceylan::Module myModule( 
			"TestModule",
			"a module designed for test",
			"http://www.esperide.com",
			"Olivier Boudeville",
			"olivier.boudeville@online.fr", 
			moduleVersion,
			"LGPL" ) ;

		LogPlug::info( "Description of example module : " 
			+  myModule.toString( Ceylan::high ) ) ;


		LogPlug::info( "End of Module test." ) ;
		
	}
	
	catch ( const Ceylan::Exception & e )
	{
		std::cerr << "Ceylan exception caught : "
			<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
		std::cerr << "Standard exception caught : " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
		std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

	}

	return Ceylan::ExitSuccess ;

}
