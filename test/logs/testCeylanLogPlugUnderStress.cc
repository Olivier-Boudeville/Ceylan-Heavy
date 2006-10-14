#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <exception>

#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;





/**
 * Test of LogClassical implementation of the Log system under heavy load.
 *
 * @see LogClassical, Log.
 *
 */
int main( int argc, char * argv[] )
{

    try
    {
	
		Ceylan::Uint32 messageCount = 10000 ;
		bool beFast = false ; 
		
		
		/*
		 * Any argument is supposed to mean that the user does not want
		 * to wait for too long, and does not want this test to
		 * be interactive.
		 *
		 */
		
		if ( argc > 1 )
		{
			beFast = true ;
			messageCount = 15 ;
			cout << "Test run in fast non-interactive mode, "
				"message count will be shorten to "
				<< messageCount << "." << endl ;
		}	
		
		if ( ! beFast )
		{
		
			cout << "Warning, this test will generate " << messageCount 
				 << " log messages, it might be quite long."<< endl ;
				
			cout << "Waiting to let the user the opportunity of "
				"issuing a ps command (example : ps -a -C -o v) to record"
				" how fat (in memory) this process was, initially." 
				" The problem is that with some OS, even if "
				"the process released memory, the virtual pages "
				"remain asigned to it, falsifying the test." << endl ;
		
			waitForKey() ;
		}
		

        cout << endl << "Testing LogPlugClassical implementation "
			"of the Log system"
			<< "under heavy stress." << endl
			<< "Hint : use ps command and/or valgrind to be sure "
			"there are not too many"
			<< " memory leaks (most if not all of them "
			"should be created externally,"
			<< " by libraries and so on (starring : the STL)."
        	<< endl << endl ;

		// To avoid writing logs alongside the test executable :
		std::string speakerName ;
		Ceylan::System::Directory::StripFilename( argv[0], 
			/* base path */ 0, & speakerName ) ;
		
        LogPlugClassical::StartService( speakerName ) ;

        LogPlug::info(    "This is a info message"        ) ;
        LogPlug::trace(   "This is a trace message"       ) ;
        LogPlug::debug(   "This is a debug message"       ) ;
        LogPlug::warning( "This is a warning message"     ) ;
        LogPlug::error(   "This is an error message"      ) ;
        LogPlug::fatal(   "This is a fatal error message" ) ;

        LogPlug::info( "This is another info message" ) ;

		{
		
			Object aTry ;
		
			aTry.send( "This is my first message." ) ;
		
			for ( Ceylan::Uint32 i = 0 ; i < messageCount ; i++ )
			{
				aTry.send( "This is my message #" + Ceylan::toString( i ) ) ;
					
			}
			
			if ( ! beFast )
			{

				cout << "Waiting to let the user the opportunity "
				"of issuing a ps to check"
				" how fat the process get (peak memory usage)." << endl ;
		
				waitForKey() ;
			}
			
			
			// Force automatic variable aTry to be deallocated now.
		}
		
		
		cout << "Shutting down LogPlugClassical Service." << endl ;
		
    	LogPlugClassical::StopService() ;


		if ( ! beFast )
		{

			cout << "Waiting to let the user the opportunity "
				"of issuing a ps to check"
				" that process did not remain too fat after shutdown." 
				<< endl ;
		
			waitForKey() ;
		}
		
        cout << endl << "End of LogPlugClassical test." << endl ;


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
