#include "Ceylan.h"


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;



using namespace Ceylan::System ;
using namespace Ceylan::Log ;




/**
 * Test of Ceylan time utilities.
 *
 * @note Beware, this test is very long : it can last up to a few minutes.
 *
 * @see Ceylan::System.
 *
 * Ceylan numerical datatypes (ex : Ceylan::Uint64) could be used instead
 * of long long, etc.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


    try
    {


		std::cout << "(Beware, this test might be very long : "
			"it can last up to a few minutes)" << std::endl ;
		
        LogPlug::info( "Testing Ceylan's time implementation "
			"(warning : this test is long)." ) ;

		Ceylan::Uint32 now = getTime() ;
		
		LogPlug::info( "Testing getTime and timeToString : current time is " 
			+ timeToString( now ) + " (" + Ceylan::toString( now ) 
			+ " seconds)" ) ;
		
		Second currentSecond ;
		Microsecond currentMicrosecond ;
		
		getPreciseTime( currentSecond, currentMicrosecond ) ;
		LogPlug::info( "Testing getPreciseTime : current second is " 
			+ Ceylan::toString( currentSecond ) + ", current microsecond is "
			+ Ceylan::toString( currentMicrosecond ) + "." ) ;
			
		Microsecond min, max, mean ;
			
		mean = getAccuracyOfPreciseTime( & min, & max ) ;
		LogPlug::info( "Testing getAccuracyOfPreciseTime : "
			"in microseconds, measured minimum accuracy is " 
			+ Ceylan::toString( min ) + ", maximum is " 
			+ Ceylan::toString( max )
			+ ", average is " + Ceylan::toString( mean ) + "." ) ; 

		Microsecond preciseTimeDuration = getPreciseTimeCallDuration() ;
		
		LogPlug::info( "A call to getPreciseTime lasts roughly for " 
			+ Ceylan::toString( preciseTimeDuration ) + " microsecond(s)." ) ;


		if ( ! Ceylan::Features::areFileDescriptorsSupported() )
		{
		
			LogPlug::warning( 
				"As the file descriptor feature is not available, the test "
				"stopped before computing the scheduling granularity." ) ;
			
			return Ceylan::ExitSuccess ;
			
		}
		
		if ( argc > 1 && argv[1] == Ceylan::BatchTestOption )
		{
		
			LogPlug::info( "Non-interactive test mode, "
				"no scheduling granularity computed." ) ;
				
			return Ceylan::ExitSuccess ;
			
		}	


		/*
		 * Logs can been interpreted thanks to gnuplot, ex : 
		 *   - set DEBUG_SYSTEM=1 in src/conf/build/configure-template.ac
		 *   - rebuilds all (use autogen.sh)
		 *   - run 'testCeylanTime.exe' from the build tree
		 *   - run 'gnuplot plotSchedulingGranularity.p'
		 *   - look at schedulingGranularity.png
		 *
		 */
		
		Microsecond granularity = getSchedulingGranularity() ;
		LogPlug::info( "Measured scheduling granularity is " 
			+ Ceylan::toString( granularity ) + " microseconds." ) ;
			
					
		// First wait duration, in microsecond :
		Microsecond shortestMicroDuration = 10 ;  	
		
		// Last wait duration, in microsecond (stops immediatly after 20 ms) :
		Microsecond longestMicroDuration = 21000 ;  	
				
		// Number of steps :
		Ceylan::Uint32 durationStepsCount = 50 ;
		
		// Micro duration step :
		Microsecond microDuration = static_cast<Microsecond>( 
			( longestMicroDuration - shortestMicroDuration ) 
				/ durationStepsCount ) ;  	
		
		Microsecond * durations = new Microsecond[ durationStepsCount ] ;
				
		LogPlug::info( "Testing basicSleep with wait ticks incrementing by "
			+ Ceylan::toString( microDuration ) +  " microsecond steps." ) ;
				
		register Nanosecond waitingNanoTime ;
		
		// Avoid to distort measures, by doing only the strict necessary :
		
		Second lastSecond ;
		Microsecond lastMicrosecond ;
		
		for ( Ceylan::Uint32 i = 0 ; i < durationStepsCount; i++ )
		{
			waitingNanoTime = 1000 * 
				( shortestMicroDuration + i * microDuration ) ;
			
			getPreciseTime( lastSecond, lastMicrosecond ) ;
			Ceylan::System::basicSleep( 0 /* second */, 
				waitingNanoTime /* nanoseconds */ ) ;
			getPreciseTime( currentSecond, currentMicrosecond ) ;
			durations[i] =  ( currentSecond - lastSecond ) * 1000000 
				+ currentMicrosecond - lastMicrosecond ;
		}


		register Microsecond waitingMicroTime ;

		
		for ( Ceylan::Uint32 i = 0; i < durationStepsCount; i++ )
		{

			waitingMicroTime = shortestMicroDuration + i * microDuration ;

			LogPlug::debug( "For a requested wait of " 
				+ Ceylan::toString( waitingMicroTime ) 
				+ " microseconds, measured time has been "  
				+ Ceylan::toString( durations[i] ) + " microseconds." ) ;
				
		}
						
		
		
		LogPlug::info( "Testing smartSleep with random sleep durations." ) ;
		
		// Would be preferably 30, but would be very long : 
		Ceylan::Uint32 sampleCount = 5 ;
		
		Second drawnSecond ;
		Microsecond drawnMicrosecond ;
		
		// Wait for 0, 1 or 2 seconds :
		Ceylan::Maths::Random::WhiteNoiseGenerator secondRand( 0, 3 ) ;
		
		// Wait for 0 to 999 999 microseconds :
		Ceylan::Maths::Random::WhiteNoiseGenerator 
			microsecondRand( 0, 1000000 ) ;
		
		/*
		 * Determines what is the maximum error tolerated for smartSleep,
		 * in microseconds :
		 *
		 */
		Ceylan::SignedLongInteger maximumAllowedError = 5 ;
		
		Ceylan::SignedLongInteger currentError ;
		
		/*
		 * smartSleep can be called safely since getSchedulingGranularity
		 * was already called.
		 *
		 */
		
		for ( Ceylan::Uint32 i = 0; i < sampleCount; i++ )
		{
		
			drawnSecond = secondRand.getNewValue() ;
			drawnMicrosecond = microsecondRand.getNewValue() ;
			getPreciseTime( lastSecond, lastMicrosecond ) ;
			Ceylan::System::smartSleep( drawnSecond /* seconds */, 
				drawnMicrosecond /* microsecond */ ) ;
			getPreciseTime( currentSecond, currentMicrosecond ) ;

			/*
			 * preciseTimeDuration is substracted since getPreciseTime 
			 * lasts for a moment too :
			 *
			 */
			
			currentError = static_cast<Ceylan::SignedLongInteger>(
				( currentSecond - lastSecond - drawnSecond ) * 1000000 )
				+ static_cast<Ceylan::SignedLongInteger>( 
					currentMicrosecond - lastMicrosecond - drawnMicrosecond ) 
				- preciseTimeDuration ;	
			
			if ( Ceylan::Maths::Abs( static_cast<int>( currentError ) ) >
				maximumAllowedError )
			{
				
				if ( currentError > 0 ) 
				{
					// Waited too much :
					LogPlug::error( "smartSleep waited for too long : "
						"for a requested sleep of " 
						+ Ceylan::toString( drawnSecond ) 
						+ " second(s) and " 
						+ Ceylan::toString( drawnMicrosecond ) 
						+ " microseconds, actual measured sleep duration "
						"minus requested sleep duration is "
						+  Ceylan::toString( currentError ) 
						+ " microsecond(s)." ) ;	
				}		
				else 
				{
					// Did not wait enough :
					LogPlug::error( "smartSleep did not wait enough : "
						"for a requested sleep of " 
						+ Ceylan::toString( drawnSecond ) 
						+ " second(s) and " 
						+ Ceylan::toString( drawnMicrosecond ) 
						+ " microseconds, actual requested sleep duration "
						"minus measured sleep duration is "
						+  Ceylan::toString( -currentError ) 
						+ " microsecond(s)." ) ;	
				}	
														
			}
			else
			{
				LogPlug::info( "For a requested smart sleep of " 
				+ Ceylan::toString( drawnSecond ) 
				+ " second(s) and " + Ceylan::toString( drawnMicrosecond ) 
				+ " microseconds, actual measured sleep duration "
				"minus requested sleep duration is "
				+  Ceylan::toString( currentError ) 
				+ " microsecond(s)." ) ;				
			}	
						
				
		}
		
		
		LogPlug::info( "Testing smartSleepUntil with random time target." ) ;
		

		for ( Ceylan::Uint32 i = 0; i < sampleCount; i++ )
		{
		
			drawnSecond = secondRand.getNewValue() ;
			drawnMicrosecond = microsecondRand.getNewValue() ;
			getPreciseTime( lastSecond, lastMicrosecond ) ;
			Ceylan::System::smartSleepUntil( 
				/* seconds */ drawnSecond + lastSecond, 
				/* microsecond */ drawnMicrosecond + lastMicrosecond ) ;
			getPreciseTime( currentSecond, currentMicrosecond ) ;

			currentError = static_cast<Ceylan::SignedLongInteger>(
				( currentSecond - lastSecond - drawnSecond ) * 1000000 )
				+ static_cast<Ceylan::SignedLongInteger>( 
					currentMicrosecond - lastMicrosecond - drawnMicrosecond ) ;	
			
			if ( Ceylan::Maths::Abs( static_cast<int>( currentError ) > 
				maximumAllowedError ) )
			{
				
				if ( currentError > 0 ) 
				{
					// Waited too much :
					LogPlug::error( "smartSleepUntil waited for too long : "
						"for a target time of " 
						+ Ceylan::toString( lastSecond + drawnSecond ) 
						+ " second(s) and " 
						+ Ceylan::toString( lastMicrosecond + drawnMicrosecond )
						+ " microseconds, woke up too late of "
						+  Ceylan::toString( currentError ) 
						+ " microsecond(s)." ) ;	
				}		
				else 
				{
					// Did not wait enough :
					LogPlug::error( "smartSleepUntil did not wait enough : "
						"for a target time of " 
						+ Ceylan::toString( lastSecond + drawnSecond ) 
						+ " second(s) and " 
						+ Ceylan::toString( lastMicrosecond + drawnMicrosecond )
						+ " microseconds, woke up too early of "
						+  Ceylan::toString( -currentError ) 
						+ " microsecond(s)." ) ;	
				}		
													
			}
			else
			{
			
				LogPlug::info( "For a requested sleep until " 
				+ Ceylan::toString( lastSecond + drawnSecond ) 
				+ " second(s) and " 
				+ Ceylan::toString( lastMicrosecond + drawnMicrosecond ) 
				+ " microseconds, actual measured wake-up time "
				"minus target time is "
				+  Ceylan::toString( currentError ) 
				+ " microsecond(s)." ) ;			
					
			}				
		
		}
		
		
		// Testing 	durationToString now :
			
		Second startingSecond ;
		Microsecond startingMicrosecond ;
		
		Second stoppingSecond ;
		Microsecond stoppingMicrosecond ;
		
		startingSecond = 5 ;
		startingMicrosecond = 5000 ;
		
		stoppingSecond = 5 ;
		stoppingMicrosecond = 6000 ;
		
		LogPlug::info( "Getting duration from (sec, microsec) = (" 
			+ Ceylan::toString( startingSecond ) + ", "
			+ Ceylan::toString( startingMicrosecond ) + ") to ("
			+ Ceylan::toString( stoppingSecond ) + ", "
			+ Ceylan::toString( stoppingMicrosecond ) + ") : " 
			+ Ceylan::System::durationToString( startingSecond,
				startingMicrosecond, stoppingSecond, stoppingMicrosecond ) ) ;
		
		
		startingSecond = 5 ;
		startingMicrosecond = 5000 ;
		
		stoppingSecond = 6 ;
		stoppingMicrosecond = 1000 ;
		

		LogPlug::info( "Getting duration from (sec, microsec) = (" 
			+ Ceylan::toString( startingSecond ) + ", "
			+ Ceylan::toString( startingMicrosecond ) + ") to ("
			+ Ceylan::toString( stoppingSecond ) + ", "
			+ Ceylan::toString( stoppingMicrosecond ) + ") : " 
			+ Ceylan::System::durationToString( startingSecond,
				startingMicrosecond, stoppingSecond, stoppingMicrosecond ) ) ;

		startingSecond = 5 ;
		startingMicrosecond = 5000 ;
		
		stoppingSecond = 5 ;
		stoppingMicrosecond = 5000 ;
		

		LogPlug::info( "Getting duration from (sec, microsec) = (" 
			+ Ceylan::toString( startingSecond ) + ", "
			+ Ceylan::toString( startingMicrosecond ) + ") to ("
			+ Ceylan::toString( stoppingSecond ) + ", "
			+ Ceylan::toString( stoppingMicrosecond ) + ") : " 
			+ Ceylan::System::durationToString( startingSecond,
				startingMicrosecond, stoppingSecond, stoppingMicrosecond ) ) ;

		startingSecond = 5 ;
		startingMicrosecond = 2000 ;
		
		stoppingSecond = 5 ;
		stoppingMicrosecond = 1000 ;

		bool detected = false ;
				
		try
		{
			Ceylan::System::durationToString( startingSecond,
				startingMicrosecond, stoppingSecond, stoppingMicrosecond ) ;
		}
		catch ( const Ceylan::System::SystemException & e )
		{
			LogPlug::info( "Negative durations are correctly detected "
				"(simple case)." ) ;
			detected = true ;
		}	
			
		if ( detected == false )
			throw Ceylan::TestException( "Ceylan::System::durationToString "
				"failed to detect negative duration (simple case)." ) ;
		
		detected = false ;
				
		startingSecond = 5 ;
		startingMicrosecond = 2000000 ;
		
		stoppingSecond = 6 ;
		stoppingMicrosecond = 1000000 ;
		
		LogPlug::info( "Getting duration from (sec, microsec) = (" 
			+ Ceylan::toString( startingSecond ) + ", "
			+ Ceylan::toString( startingMicrosecond ) + ") to ("
			+ Ceylan::toString( stoppingSecond ) + ", "
			+ Ceylan::toString( stoppingMicrosecond ) + ") : " 
			+ Ceylan::System::durationToString( startingSecond,
				startingMicrosecond, stoppingSecond, stoppingMicrosecond ) ) ;


		startingSecond = 5 ;
		startingMicrosecond = 2000001 ;
		
		stoppingSecond = 6 ;
		stoppingMicrosecond = 1000000 ;

		try
		{		

			Ceylan::System::durationToString( startingSecond,
				startingMicrosecond, stoppingSecond, stoppingMicrosecond ) ;

		}
		catch ( const Ceylan::System::SystemException & e )
		{
			LogPlug::info( "Negative durations are correctly detected." ) ;
			detected = true ;
		}	
		
		if ( ! detected )
			throw Ceylan::TestException( "Ceylan::System::durationToString "
				"failed to detect negative duration." ) ;
		
		delete durations ;
			
			
        LogPlug::info( "End of time test." ) ;
		

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
