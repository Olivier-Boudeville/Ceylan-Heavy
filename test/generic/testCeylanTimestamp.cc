#include "Ceylan.h"

#include <exception>
#include <string>


using namespace Ceylan ;
using namespace Ceylan::Log ;


/**
 * Test of timestamp facility.
 *
 * @see Timestamp
 *	
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


    try
    {

 
		LogPlug::info( "Testing Timestamp's implementation." ) ;


        LogPlug::info( "First time-stamp : " ) ;
		Timestamp firstStamp ;
		LogPlug::info( firstStamp.toString() ) ;
		
		if ( Features::areFileDescriptorsSupported() )
		{
			// 1/10 second waiting :
			Ceylan::System::basicSleep( 0 /* second */, 
				/* nanoseconds */ 100000000 ) ;
		}
		else
		{
			
			LogPlug::warning( "Ceylan::System::basicSleep not available, "
				"defaulting to Ceylan::System::sleepForSeconds" ) ;
				
			// Otherwise sleep for one second :
			Ceylan::System::sleepForSeconds( 1 ) ;
		}	
			
        LogPlug::info( "Second time-stamp : " ) ;
		Timestamp secondStamp ;
		LogPlug::info( secondStamp.toString() ) ;
		
		// Sleep for a full second :
		Ceylan::System::sleepForSeconds( 1 ) ;
		
				
        LogPlug::info( "Third time-stamp : " ) ;
		Timestamp thirdStamp ;
		LogPlug::info( thirdStamp.toString() ) ;

        LogPlug::info( "Testing comparison operator "
			"applied to Timestamps : " ) ;

		if ( firstStamp < secondStamp )
			LogPlug::info( "First timestamp is inferior to second." ) ;
		else
		{
			if ( secondStamp < firstStamp )
				throw Ceylan::TestException( "First timestamp ("
					+ firstStamp.toString() 
					+ ") should be at most equal to second ("
					+ secondStamp.toString() + "), if not inferior." ) ;
			else		
				LogPlug::info( "First timestamp is equal to second." ) ;
		}
		
			
		if ( thirdStamp < firstStamp  )
			throw Ceylan::TestException( 
				"Third timestamp (" + thirdStamp.toString() 
					+ ") is inferior to first (" + firstStamp.toString()
					+ "), which is abnormal." ) ;
		else
			LogPlug::info( "Third timestamp is superior "
				"or equal to first." ) ;
		
		
        LogPlug::info( "End of Timestamp test." ) ;

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
