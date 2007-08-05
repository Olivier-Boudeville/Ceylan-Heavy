#include "Ceylan.h"

#include <iostream>

#include <string>
using std::string ;

#include <exception>


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;



/**
 * All-in-one test for the Ceylan library on Nintendo DS.
 *
 * As loading and running executables on this embedded platform cannot be
 * well automated, this test agregates all subtests, that are run in a row:
 * only one load/run sequence is needed then.
 *
 * Test coverage is far less complete than for usual computer platforms though.
 *
 */
int main( int argc, char * argv[] )
{

	
    try
    {
	
		Console MyConsole ;
		
		MyConsole.addInBuffer( "Hello console !\n" ) ;
		
		MyConsole.render() ;
		
		waitForKey( "" ) ;

		MyConsole.addInBuffer( "Bye !\n" ) ;
		
		MyConsole.render() ;
			
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
