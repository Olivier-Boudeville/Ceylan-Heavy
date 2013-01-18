/*
 * Copyright (C) 2003-2013 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option)
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "Ceylan.h"


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;



using namespace Ceylan::System ;
using namespace Ceylan::Log ;



/*
 * Burns thanks to a busy loop some CPU, to create an offset to the beginning of
 * a time slice.
 *
 * On the test laptop, it last for 1,9 ms.
 *
 */
Ceylan::Float32 activeWaiting() throw()
{

  Ceylan::Float32 f = 0 ;

  for ( Ceylan::Uint32 i = 0 ; i < 10000 ; i++ )
	f += 3 * Ceylan::Maths::Sin( static_cast<Ceylan::Float32>( i ) ) ;

  return f ;

}



/**
 * Test of Ceylan time utilities.
 *
 * @note Beware, this test is very long: it can last up to a few minutes.
 *
 * @see Ceylan::System.
 *
 * Ceylan numerical datatypes (ex: Ceylan::Uint64) could be used instead of long
 * long, etc.
 *
 * @note Beware to the classical log plug that may cause wrong timings due to
 * immediate I/O.
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{


	  bool isBatch = false ;
	  bool burnCPUCycles = false ;

	  std::string executableName ;
	  std::list<std::string> options ;

	  Ceylan::parseCommandLineOptions( executableName, options, argc, argv ) ;

	  std::string token ;
	  bool tokenEaten ;


	  while ( ! options.empty() )
	  {

		token = options.front() ;
		options.pop_front() ;

		tokenEaten = false ;

		if ( token == "--batch" )
		{
		  LogPlug::info( "Batch mode selected" ) ;
		  isBatch = true ;
		  tokenEaten = true ;
		}

		if ( token == "--online" )
		{
		  // Ignored:
		  tokenEaten = true ;
		}

		if ( token == "--burnCPUCycles" )
		{
		  burnCPUCycles = true ;
		  tokenEaten = true ;
		}

		if ( LogHolder::IsAKnownPlugOption( token ) )
		{
		  // Ignores log-related (argument-less) options.
		  tokenEaten = true ;
		}


		if ( ! tokenEaten )
		{
		  throw Ceylan::CommandLineParseException(
			"Unexpected command line argument: " + token ) ;
		}

	  }


	  if ( burnCPUCycles )
	  {

		std::cout << "Will burn now as many CPU cycles as possible "
		  "(useful to test other modules on heavy loads). "
		  "Use CTRL-C to stop." << std::endl ;

		Ceylan::Float32 a = 1 ;

		while ( true )
		{
		  a += static_cast<Ceylan::Float32>(
			Ceylan::Maths::Sin( a ) * 5.1 ) ;
		}

		Ceylan::shutdown() ;

		return Ceylan::ExitSuccess ;

	  }

	  std::cout << "(Beware, this test might be very long: "
		"it can last up to a few minutes)" << std::endl ;

	  LogPlug::info( "Testing Ceylan's time implementation "
		"(warning: this test is long)." ) ;

	  Ceylan::Uint32 now = getTime() ;

	  LogPlug::info( "Testing getTime and timeToString: current time is "
		+ timeToString( now ) + " (" + Ceylan::toString( now )
		+ " seconds)" ) ;

	  Second currentSecond ;
	  Microsecond currentMicrosecond ;

	  getPreciseTime( currentSecond, currentMicrosecond ) ;
	  LogPlug::info( "Testing getPreciseTime: current second is "
		+ Ceylan::toString( currentSecond ) + ", current microsecond is "
		+ Ceylan::toString( currentMicrosecond ) + "." ) ;

	  Microsecond min, max, mean ;

	  mean = getAccuracyOfPreciseTime( & min, & max ) ;
	  LogPlug::info( "Testing getAccuracyOfPreciseTime: "
		"in microseconds, measured minimum accuracy is "
		+ Ceylan::toString( min ) + ", maximum is "
		+ Ceylan::toString( max )
		+ ", average is " + Ceylan::toString( mean ) + "." ) ;

	  Microsecond preciseTimeDuration = getPreciseTimeCallDuration() ;

	  LogPlug::info( "A call to getPreciseTime lasts roughly for "
		+ Ceylan::toString( preciseTimeDuration ) + " microsecond(s)." ) ;

	  if ( isBatch )
	  {

		LogPlug::info( "Non-interactive test mode, "
		  "no scheduling granularity computed, test finished." ) ;

		Ceylan::shutdown() ;

		return Ceylan::ExitSuccess ;

	  }


	  if ( ! areSubSecondSleepsAvailable() )
	  {

		LogPlug::info( "No subsecond sleeps available, "
		  "no scheduling granularity computed, test finished." ) ;

		Ceylan::shutdown() ;

		return Ceylan::ExitSuccess ;

	  }


	  /*
	   * Logs can been interpreted thanks to gnuplot, ex:
	   *
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


	  // First wait duration, in microsecond:
	  Microsecond shortestMicroDuration = 0 ;

	  // Last wait duration, in microsecond (stops immediatly after 20 ms):
	  Microsecond longestMicroDuration = /* 5000 */ 21000 ;

	  // Number of steps:
	  Ceylan::Uint32 durationStepsCount = /* 1000 */ 2000 ;

	  // Micro duration step:
	  Microsecond microDuration = static_cast<Microsecond>(
		( longestMicroDuration - shortestMicroDuration )
		/ durationStepsCount ) ;

	  if ( microDuration == 0 )
		microDuration = 1 ;

	  Microsecond * durations = new Microsecond[ durationStepsCount ] ;

	  LogPlug::info( "Testing basicSleep with wait ticks incrementing by "
		+ Ceylan::toString( microDuration ) +  " microsecond steps." ) ;


	  // Avoid to distort measures, by doing only the strict necessary:


	  Second lastSecond ;
	  Microsecond lastMicrosecond ;


	  getPreciseTime( lastSecond, lastMicrosecond ) ;

	  activeWaiting() ;

	  getPreciseTime( currentSecond, currentMicrosecond ) ;


	  LogPlug::debug( "Active waiting last for "
		+ durationToString( lastSecond, lastMicrosecond,
		  currentSecond, currentMicrosecond ) ) ;

	  /*
	   * Perform between 0 to 10 waiting loops to avoid being sync'ed with the
	   * beginning of time slices:
	   *
	   */
	  Ceylan::Maths::Random::WhiteNoiseGenerator activeWaitingRand( 0, 10 ) ;

	  Ceylan::Uint32 waitCount ;

	  for ( Ceylan::Uint32 i = 0 ; i < 10; i ++ )
	  {

		waitCount = activeWaitingRand.getNewValue() ;

		// Do not get aligned with beginning of time slices:
		for ( Ceylan::Uint32 w = 0; w < waitCount; w++ )
		  activeWaiting() ;

		getPreciseTime( lastSecond, lastMicrosecond ) ;

		Ceylan::System::basicSleep( 0 /* second */, 0 /* nanoseconds */ ) ;

		getPreciseTime( currentSecond, currentMicrosecond ) ;

		LogPlug::debug( "For a null requested duration, waited for "
		  + durationToString( lastSecond, lastMicrosecond,
			currentSecond, currentMicrosecond ) ) ;

	  }


	  const string logFilename = "testCeylanTime-granularity.dat" ;
	  Ceylan::Holder<File> logFile( File::Create( logFilename ) ) ;

	  logFile->write(
		"# This file records the requested sleep durations (first column) "
		"and the corresponding actual sleep durations (second column).\n"
		"# The scheduling granularity can be usually guessed from it.\n"
		"# One may use gnuplot to analyze the result.\n\n"   ) ;


	  Microsecond currentRequestedDuration ;

	  for ( Ceylan::Uint32 i = 0 ; i < durationStepsCount; i++ )
	  {

		// Do not get aligned with beginning of time slices:
		for ( Ceylan::Uint32 w = 0; w < waitCount; w++ )

		  waitCount = activeWaitingRand.getNewValue() ;

		// Do not get aligned with beginning of time slices:
		for ( Ceylan::Uint32 w = 0; w < waitCount; w++ )
		  activeWaiting() ;

		currentRequestedDuration =
		  shortestMicroDuration + i * microDuration ;

		getPreciseTime( lastSecond, lastMicrosecond ) ;

		Ceylan::System::basicSleep( 0 /* second */,
		  currentRequestedDuration * 1000 /* nanoseconds */ ) ;

		getPreciseTime( currentSecond, currentMicrosecond ) ;

		// Do not use durationToString, it would create english text:
		logFile->write( Ceylan::toString( currentRequestedDuration )
		  + " \t "
		  + Ceylan::toString( getDurationBetween(
			  lastSecond, lastMicrosecond,
			  currentSecond, currentMicrosecond ) )
		  + " \n" ) ;

		LogPlug::debug( "For a requested wait of "
		  + Ceylan::toString( shortestMicroDuration + i * microDuration )
		  + " microseconds, measured time has been "
		  + durationToString( lastSecond, lastMicrosecond,
			currentSecond, currentMicrosecond )
		  + " microseconds." ) ;

	  }


	  LogPlug::info( "Testing smartSleep with random sleep durations." ) ;

	  // Can easily be very long:
	  Ceylan::Uint32 sampleCount = 50 ;

	  if ( ! isBatch )
		sampleCount = 1 ;

	  Second drawnSecond ;
	  Microsecond drawnMicrosecond ;

	  // Wait for 0, 1 or 2 seconds:
	  Ceylan::Maths::Random::WhiteNoiseGenerator secondRand( 0, 3 ) ;

	  // Wait for 0 to 999 999 microseconds:
	  Ceylan::Maths::Random::WhiteNoiseGenerator
		microsecondRand( 0, 1000000 ) ;

	  /*
	   * Determines what is the maximum error tolerated for smartSleep, in
	   * microseconds:
	   *
	   */
	  Ceylan::SignedLongInteger maximumAllowedError = 5 ;

	  Ceylan::SignedLongInteger currentError ;

	  Ceylan::Uint32 tooEarlyCount = 0 ;
	  Ceylan::Uint32 tooEarlyTotal = 0 ;

	  Ceylan::Uint32 tooLateCount = 0 ;
	  Ceylan::Uint32 tooLateTotal = 0 ;

	  /*
	   * smartSleep can be called safely since getSchedulingGranularity was
	   * already called.
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
		 * preciseTimeDuration is substracted since getPreciseTime lasts for
		 * a moment too:
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

			// Waited too much:
			LogPlug::error( "smartSleep waited for too long: "
			  "for a requested sleep of "
			  + Ceylan::toString( drawnSecond )
			  + " second(s) and "
			  + Ceylan::toString( drawnMicrosecond )
			  + " microseconds, actual measured sleep duration "
			  "minus requested sleep duration is "
			  +  Ceylan::toString( currentError )
			  + " microsecond(s)." ) ;

			tooLateCount++ ;
			tooLateTotal += currentError ;

		  }
		  else
		  {
			// Did not wait enough:
			LogPlug::error( "smartSleep did not wait enough: "
			  "for a requested sleep of "
			  + Ceylan::toString( drawnSecond )
			  + " second(s) and "
			  + Ceylan::toString( drawnMicrosecond )
			  + " microseconds, actual requested sleep duration "
			  "minus measured sleep duration is "
			  +  Ceylan::toString( -currentError )
			  + " microsecond(s)." ) ;

			tooEarlyCount++ ;
			tooEarlyTotal -= currentError ;

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

	  LogPlug::info( "Finally, smartSleep waited too much "
		+ Ceylan::toString( tooLateCount )
		+ " times, for a total duration of "
		+ Ceylan::toString( tooLateTotal )
		+ " microseconds, and waited not enough "
		+ Ceylan::toString( tooEarlyCount )
		+ " times, for a total duration of "
		+ Ceylan::toString( tooEarlyTotal )
		+ " microseconds." ) ;



	  LogPlug::info( "Testing smartSleepUntil with random time target." ) ;

	  tooEarlyCount = 0 ;
	  tooEarlyTotal = 0 ;

	  tooLateCount = 0 ;
	  tooLateTotal = 0 ;

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
			// Waited too much:
			LogPlug::error( "smartSleepUntil waited for too long: "
			  "for a target time of "
			  + Ceylan::toString( lastSecond + drawnSecond )
			  + " second(s) and "
			  + Ceylan::toString( lastMicrosecond + drawnMicrosecond )
			  + " microseconds, woke up too late of "
			  +  Ceylan::toString( currentError )
			  + " microsecond(s)." ) ;

			tooLateCount++ ;
			tooLateTotal += currentError ;
		  }
		  else
		  {
			// Did not wait enough:
			LogPlug::error( "smartSleepUntil did not wait enough: "
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

		  tooEarlyCount++ ;
		  tooEarlyTotal -= currentError ;

		}

	  }

	  LogPlug::info( "Finally, smartSleepUntil waited too much "
		+ Ceylan::toString( tooLateCount )
		+ " times, for a total duration of "
		+ Ceylan::toString( tooLateTotal )
		+ " microseconds, and waited not enough "
		+ Ceylan::toString( tooEarlyCount )
		+ " times, for a total duration of "
		+ Ceylan::toString( tooEarlyTotal )
		+ " microseconds." ) ;


	  // Testing durationToString now:

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
		+ Ceylan::toString( stoppingMicrosecond ) + "): "
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
		+ Ceylan::toString( stoppingMicrosecond ) + "): "
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
		+ Ceylan::toString( stoppingMicrosecond ) + "): "
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
	  catch ( const Ceylan::System::SystemException )
	  {
		LogPlug::info( "Negative durations are correctly detected "
		  "(simple case)." ) ;
		detected = true ;
	  }

	  if ( ! detected )
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
		+ Ceylan::toString( stoppingMicrosecond ) + "): "
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
	  catch ( const Ceylan::System::SystemException )
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
	  std::cerr << "Ceylan exception caught: "
				<< e.toString( Ceylan::high ) << std::endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
	  std::cerr << "Standard exception caught: "
				<< e.what() << std::endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
	  std::cerr << "Unknown exception caught" << std::endl ;
	  return Ceylan::ExitFailure ;

	}

  }

  Ceylan::shutdown() ;

  return Ceylan::ExitSuccess ;

}
