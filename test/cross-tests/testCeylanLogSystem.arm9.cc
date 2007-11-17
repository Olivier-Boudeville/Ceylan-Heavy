#include "Ceylan.h"

#include <iostream>

#include <string>
using std::string ;

#include <list>
using std::list ;

#include <exception>


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;



#define CEYLAN_DEBUG_TEST 1

#if CEYLAN_DEBUG_TEST

#define CEYLAN_DEBUG_LOG 1
#include "CeylanLogLight.h"
#define CEYLAN_TEST_LOG(message) CEYLAN_LOG((string(message)))

#else // CEYLAN_DEBUG_TEST

#define CEYLAN_TEST_LOG(message)

#endif // CEYLAN_DEBUG_TEST




/**
 * Test for the log support offered by the Ceylan library on Nintendo DS.
 *
 * Test coverage is far less complete than for usual computer platforms though.
 *
 */
int main( int argc, char * argv[] )
{

	 

	LogHolder myLog( argc, argv, /* forceImmediateWrite */ true ) ;

	
    try
    {
				
		
		LogPlug::info( "This is an info message" ) ;		
		LogPlug::warning( "This is a warning message" ) ;
		LogPlug::trace( "This is a trace message" ) ;
		LogPlug::debug( "This is a debug message" ) ;
		LogPlug::error( "This is an error message" ) ;
		LogPlug::fatal( "This is a fatal message" ) ;

		LogPlug::info( "This is a kind of message that used to be incorrectly displayed in other layouts than the raw one:" ) ;
		
		LogPlug::info( "OSDL state:\n   + Common root module, with currently video module disabled, with event module disabled, with audio module disabled, using no CD-ROM handler." ) ;
		
		LogPlug::info( "This is a test for very long words: Yourobjectistosave theworld,whilestillleadingapleasantlife." ) ;
		
		
		/**
		 * Change to 10 000 to check whether it would fit in memory:
		 * (it does)
		 *
		 */
		const Ceylan::Uint16 logCount = 10 ;
		
		for ( Ceylan::Uint16 i = 0 ; i < logCount; i++ )
			LogPlug::info( "Testing log scrolling, line #" 
				+ Ceylan::toString( i ) ) ;
						
		LogPlug::info( "This is a test for very long words: Yourobjectistosave theworld,whilestillleadingapleasantlife." ) ;
				
		LogPlug::debug( "Logs are collected during program execution, and at the end of the program, whether on error (uncaught exception) or not, the logs should be displayed in the DS console, thanks to a small log browser allowing to navigate through all past logs." ) ;
		
		LogPlug::info( 
			"Press any key to end the program and look at the logs" ) ;
				
		LogPlug::info( "(note that immediate write behaves correctly "
			"if you see these messages before ending the program "
			"by pressing a key)" ) ;
			
		waitForKey() ;
					
    }
   
    catch ( const Ceylan::Exception & e )
    {
	
        displayError( "Ceylan exception caught: " 
			+ e.toString( Ceylan::high ) ) ;
			
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
	
        displayError( string( "Standard exception caught: " ) + e.what() ) ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
	
        displayError( "Unknown exception caught" ) ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
