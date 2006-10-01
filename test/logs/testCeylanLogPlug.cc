#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;

#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;

#include <exception>


/**
 * Test of LogPlug implementation of the Log system.
 *
 * @note for the moment, this results in testing directly the 
 * LogPlugClassical.
 *
 * @see Log.
 *
 */
int main( int argc, char * argv[] )
{


    try
    {

        cout << endl << "Testing LogPlug implementation "
			"of the Log system." << endl << endl ;

		// To avoid writing logs alongside the test executable :
		std::string speakerName ;

		Ceylan::System::Directory::StripFilename( argv[0], 
			/* base path */ 0, & speakerName ) ;

        LogPlugClassical::StartService( speakerName ) ;

        LogPlug::info(    "This is an info message"       ) ;
        LogPlug::trace(   "This is a trace message"       ) ;
        LogPlug::debug(   "This is a debug message"       ) ;
        LogPlug::warning( "This is a warning message"     ) ;
        LogPlug::error(   "This is an error message"      ) ;
        LogPlug::fatal(   "This is a fatal error message" ) ;

        LogPlug::info( "This is another info message" ) ;

		{
		
			Object aTry ;
		
			aTry.send( "This is my first message." ) ;
		
			// Force automatic variable aTry to be deallocated now.
		}
		
		cout << "Shutting down LogPlug Service." << endl ;
		
    	LogPlugClassical::StopService() ;

        cout << endl << "End of LogPlug test." << endl ;


    }

    catch ( const Ceylan::Exception & e )
    {
        cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << endl ;
		LogPlugClassical::StopService() ;	
        return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        cerr << "Standard exception caught : " 
			 << e.what() << endl ;
		LogPlugClassical::StopService() ;	 
        return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        cerr << "Unknown exception caught" << endl ;
		LogPlugClassical::StopService() ;
        return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
