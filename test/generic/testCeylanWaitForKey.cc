#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <exception>
#include <string>

using namespace std ;



/**
 * Test for portable primitives helping handling key events.
 *
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder myLog( argc, argv ) ;


    try
    {


		LogPlug::info( 
			"Testing portable primitives helping handling key events." ) ;

		LogPlug::info( "Testing : waiting for a key to be pressed." ) ;
		
		if ( argc > 1 && argv[1] == Ceylan::BatchTestOption )
			LogPlug::info( "Non-interactive test mode, "
				"not waiting for any user input." ) ;
		else
			LogPlug::info( "Scan code for hit key is : " 
				+ toString( waitForKey() ) ) ;
		
		LogPlug::info( "End of key handling test." ) ;

    }

    catch ( const Ceylan::Exception & e )
    {
        LogPlug::error( "Ceylan exception caught : "
        	 + e.toString( Ceylan::high ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        LogPlug::error( "Standard exception caught : " 
			 + std::string( e.what() ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        LogPlug::error( "Unknown exception caught" ) ;
       	return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}

