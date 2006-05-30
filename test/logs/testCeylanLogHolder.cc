#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;

#include <exception>



/**
 * Test of LogHolder.
 *
 * @see LogHolder, LogPlug.
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;
		
    try
    {

        cout << endl << "Testing LogHolder implementation."
        	<< endl << endl ;

        LogPlug::info(    "This is a info message"        ) ;
        LogPlug::trace(   "This is a trace message"       ) ;
        LogPlug::debug(   "This is a debug message"       ) ;
        LogPlug::warning( "This is a warning message"     ) ;
        LogPlug::error(   "This is an error message"      ) ;
        LogPlug::fatal(   "This is a fatal error message" ) ;

        LogPlug::info( "This is another info message" ) ;

        cout << endl << "End of LogHolder test." << endl ;


		/*
		 * One can test by uncommenting the next line that even if an 
		 * exception is raised,
		 * Log system is correctly shut down.
		 *

		throw Ceylan::Exception( "This is a test to show that "
			"Log system is well managed" ) ;
		 
		 * @see testCeylanFullLogSystem.cc
		 *
		 */
		
    }

    catch ( const Ceylan::Exception & e )
    {
        cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        cerr << "Standard exception caught : " 
			 << e.what() << endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        cerr << "Unknown exception caught" << endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
