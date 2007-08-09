#include "CeylanSystem.h"


#include "CeylanLogPlug.h"                // for LogPlug
#include "CeylanOperators.h"              // for toString
#include "CeylanMathsBasic.h"             // for Abs
#include "CeylanEnvironmentVariables.h"   // for getEnvironmentVariable
#include "CeylanFile.h"                   // for File


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"                 // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h"    // for irqInit and al (ARM9)
#endif // CEYLAN_ARCH_NINTENDO_DS


#if CEYLAN_ARCH_WINDOWS
#include "CeylanNetwork.h"                // for explainSocketError, etc.
#endif // CEYLAN_ARCH_WINDOWS


// <cstring> is not enough for Sun CC.
extern "C"
{

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>                       // for opening and mode flags, usleep
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_STRING_H
#include <string.h>
#endif // CEYLAN_USES_STRING_H

#ifdef CEYLAN_USES_SYS_TIME_H
#include <sys/time.h>                     // for gettimeofday
#endif // CEYLAN_USES_SYS_TYME_H


#ifdef CEYLAN_USES_SYS_SELECT_H
#include <sys/select.h>                   // for select
#endif // CEYLAN_USES_SYS_SELECT_H

#ifdef CEYLAN_USES_WINDOWS_H
#include <windows.h>                      // for Sleep
#endif // CEYLAN_USES_WINDOWS_H

#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>                    // for _ftime_s
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SYS_TIMEB_H
#include <sys/timeb.h>                    // for _ftime_s
#endif // CEYLAN_USES_SYS_TIMEB_H

#ifdef CEYLAN_USES_WINSOCK2_H
#include <winsock2.h>					  // for Windows read/write operations
#endif // CEYLAN_USES_WINSOCK2_H

}


#define CEYLAN_CHECK_FOR_FREEZE 0

#if CEYLAN_CHECK_FOR_FREEZE
#include <iostream>
#endif // CEYLAN_CHECK_FOR_FREEZE


#include <cstdlib>
#include <cerrno>                         // for errno, EAGAIN
#include <iostream>                       // for sync_with_stdio


// For Windows select: #define FD_SETSIZE 512 (instead of 64)

using std::string ;

using namespace Ceylan::Log ;
using namespace Ceylan::System ;



Ceylan::System::SystemException::SystemException( const string & message )
		throw() :
	Ceylan::Exception( message )
{

}




Ceylan::System::SystemException::~SystemException() throw()
{

}



Ceylan::System::IOException::IOException( const string & message ) throw() :
	SystemException( message )
{

}




Ceylan::System::IOException::~IOException() throw()
{

}



const Second Ceylan::System::MaximumDurationWithMicrosecondAccuracy = 4100 ;


const Ceylan::Uint32 OneMillion = 1000000 ;




ErrorCode Ceylan::System::getError() throw()
{

	return errno ;
	
}




const string Ceylan::System::explainError( ErrorCode errorID ) throw()
{

	return string( ::strerror( errorID ) ) ;
	
}




const string Ceylan::System::explainError() throw()
{

#ifdef CEYLAN_USES_STRERROR

	return string( ::strerror( errno ) ) ;

#else // CEYLAN_USES_STRERROR

	return "Ceylan::System::explainError not available on this platform  "
		"(no ::strerror function found)" ;

#endif // CEYLAN_USES_STRERROR

}




string Ceylan::System::getShellName() throw()
{

	return Ceylan::System::getEnvironmentVariable( "SHELL" ) ;
	
}


void Ceylan::System::InitializeInterrupts( bool force ) throw( SystemException )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw SystemException( 
		"Ceylan::System::InitializeInterrupts : only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	static bool initialized = false ;
	
	if ( ( ! initialized ) || force )
	{
	
		// Initialize the interrupt subsystem:
		irqInit() ;

		/*
		 * VBlank enabled but no specific handler set 
		 * (only wanting ot for swiWaitForVBlank):
		 *
		 */
		irqEnable( IRQ_VBLANK ) ;
		
		// Force a first update, to avoid reading random keys before first VBL:
		swiWaitForVBlank() ;
		
		initialized = true ;
		//CEYLAN_LOG( "Interrupts initialized." ) ;
		
	}	

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS

	LogPlug::warning( "Ceylan::System::InitializeInterrupts "
		"should not be called on this platform." ) ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS

}



bool Ceylan::System::HasAvailableData( FileDescriptor fd ) throw()
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	struct timeval tv ;
	tv.tv_sec  = 0 ;
	tv.tv_usec = 0 ;

	// Creates the set of waiting file descriptors :
	fd_set set ;
	FD_ZERO( & set ) ;
	FD_SET( fd, & set ) ;

	Ceylan::Sint32 n = ::select( fd + 1, & set, 0, 0, & tv ) ;

	if ( n > 0 )
		return FD_ISSET( fd, & set ) ;

	if ( n == -1 )
		LogPlug::error( "Ceylan::System::HasAvailableData failed : "
			+ System::explainError() ) ;

	return false ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	LogPlug::error( "Ceylan::System::HasAvailableData : "
		"file descriptor feature not available" ) ;

	return false ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

}




Size Ceylan::System::FDRead( FileDescriptor fd, char * dataBuffer,
		Size toReadBytesNumber )
	throw( IOException, Features::FeatureNotAvailableException )
{

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
 	LogPlug::trace( "Ceylan::System::FDRead : will try to read "
		+ Ceylan::toString( 
			static_cast<Ceylan::Uint32>( toReadBytesNumber ) ) + " byte(s)." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS


	SignedSize totalReadBytesNumber = 0 ;

#if CEYLAN_ARCH_WINDOWS

	// On Windows, such IO are only used for sockets.

#if CEYLAN_USES_NETWORK

	char * pos = dataBuffer ;

 	SignedSize readBytesNumber ;
	

	while ( toReadBytesNumber &&
		( readBytesNumber = ::recv( fd, pos, 
			static_cast<int>( toReadBytesNumber ), 
			/* flags */ 0 ) ) != 0 )
	{

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
 				LogPlug::trace( "Ceylan::System::FDRead : recv returned "
					+ Ceylan::toString( readBytesNumber ) + "." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS


		if ( readBytesNumber == SOCKET_ERROR )
		{

			// Non-blocking reads return WSAEWOULDBLOCK if there is no data :
			if ( Network::getSocketError() == WSAEWOULDBLOCK )
			{

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
 				LogPlug::trace( "Ceylan::System::FDRead : "
					"operation would block." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS

				readBytesNumber = 0 ;
				break ;
			}
			else
			{

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
 				LogPlug::trace( "Ceylan::System::FDRead : operation failed." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS

				throw IOException( "Ceylan::System::FDRead failed : "
					+ Network::explainSocketError() ) ;
			}

		}

		totalReadBytesNumber += readBytesNumber ;
		toReadBytesNumber    -= readBytesNumber ;
		pos                  += readBytesNumber ;

	}

	// readBytesNumber == 0 means end of file, if blocking.


#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( "Ceylan::System::FDRead : "
		"network feature not available" ) ;

#endif // CEYLAN_USES_NETWORK

#else // CEYLAN_ARCH_WINDOWS

#if CEYLAN_USES_FILE_DESCRIPTORS


	char * pos = dataBuffer ;

 	SignedSize readBytesNumber ;
	
	while ( toReadBytesNumber &&
		( readBytesNumber = ::read( fd, pos, toReadBytesNumber ) ) != 0 )
	{

		if ( readBytesNumber < 0 )
		{

			// Non-blocking reads return EAGAIN if there is no data :
			if ( System::getError() == EAGAIN )
			{
				readBytesNumber = 0 ;
				break ;
			}
			else
			{
				throw IOException( "Ceylan::System::FDRead failed : "
					+ explainError() ) ;
			}

		}

		totalReadBytesNumber += readBytesNumber ;
		toReadBytesNumber    -= readBytesNumber ;
		pos                  += readBytesNumber ;

	}

	// readBytesNumber == 0 means end of file, if blocking.


#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw Features::FeatureNotAvailableException( "Ceylan::System::FDRead : "
		"file descriptor feature not available" ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_WINDOWS


#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
 	LogPlug::trace( "Ceylan::System::FDRead : read "
		+ Ceylan::toString( totalReadBytesNumber ) + " byte(s)." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS

	return static_cast<Size>( totalReadBytesNumber ) ;

}




Size Ceylan::System::FDWrite( FileDescriptor fd,
		const Ceylan::Byte * dataBuffer, Size toWriteBytesNumber )
	throw( IOException, Features::FeatureNotAvailableException )
{

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
 	LogPlug::trace( "Ceylan::System::FDWrite : will try to write "
		+ Ceylan::toString( 
			static_cast<Ceylan::Uint32>( toWriteBytesNumber ) ) 
		+ " byte(s)." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS


	SignedSize totalWroteBytesNumber = 0 ;

#if CEYLAN_ARCH_WINDOWS

	// On Windows, such IO are only used for sockets.

#if CEYLAN_USES_NETWORK

	const char * pos = dataBuffer ;

 	SignedSize wroteBytesNumber ;

	while( toWriteBytesNumber
		&& ( wroteBytesNumber = ::send( fd, pos, 
			static_cast<int>( toWriteBytesNumber ), 
			/* flags */ 0 ) ) != 0 )
	{

		if ( wroteBytesNumber < 0 )
		{

			/*
			 * Non-blocking write return WSAEWOULDBLOCK if writing 
			 * would block :
			 *
			 */
			if ( Network::getSocketError() == WSAEWOULDBLOCK )
			{
				wroteBytesNumber = 0 ;
				break ;

			}
			else
			{

				throw IOException( "Ceylan::System::FDWrite failed : "
					+ Network::explainSocketError() ) ;
			}
		}

		totalWroteBytesNumber += wroteBytesNumber ;
		toWriteBytesNumber    -= wroteBytesNumber ;
		pos                   += wroteBytesNumber ;

	}

#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( "Ceylan::System::FDWrite : "
		"network feature not available" ) ;

#endif // CEYLAN_USES_NETWORK

#else // CEYLAN_ARCH_WINDOWS

#if CEYLAN_USES_FILE_DESCRIPTORS


	const char * pos = dataBuffer ;

 	SignedSize wroteBytesNumber ;

	while( toWriteBytesNumber
		&& ( wroteBytesNumber = ::write( fd, pos, toWriteBytesNumber ) ) != 0 )
	{

		if ( wroteBytesNumber < 0 )
		{

			// Non-blocking write return EAGAIN if writing would block :
			if ( System::getError() == EAGAIN )
			{
				wroteBytesNumber = 0 ;
				break ;

			}
			else
			{

				throw IOException( "Ceylan::System::FDWrite failed : "
					+ explainError() ) ;
			}
		}

		totalWroteBytesNumber += wroteBytesNumber ;
		toWriteBytesNumber    -= wroteBytesNumber ;
		pos                   += wroteBytesNumber ;

	}


#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw Features::FeatureNotAvailableException( "Ceylan::System::FDWrite : "
		"file descriptor feature not available" ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_WINDOWS

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
 	LogPlug::trace( "Ceylan::System::FDWrite : wrote "
		+ Ceylan::toString( totalWroteBytesNumber ) + " byte(s)." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS

	return static_cast<Size>( totalWroteBytesNumber ) ;

}




// Time section.


Second Ceylan::System::getTime() throw( SystemException )
{

#ifdef CEYLAN_USES_TIME

	Ceylan::Sint32 currentTime = static_cast<Ceylan::Sint32>(
			::time( 0 ) ) ;

	if ( currentTime == -1 )
		throw SystemException( explainError( getError() ) ) ;

	/*
	 * Risk of overflow due to signed 32 bit integer :
	 *
	 * 2^31/365/24/3600 = 68, 1970 + 68 = 2038, far in the future.
	 *
	 */
	return static_cast<Second>( currentTime ) ;

#else // CEYLAN_USES_TIME

	throw SystemException( "Ceylan::System::getTime : "
		"not available on this platform (no ::time function found)." ) ;

#endif // CEYLAN_USES_TIME


}




string Ceylan::System::timeToString( const time_t & t )
	throw( SystemException )
{

#ifdef CEYLAN_USES_CTIME

	char * charTime = ::ctime( &t ) ;

	if ( charTime == 0 )
		throw SystemException( "Ceylan::System::timeToString : "
			"unable to convert time to string with ctime." ) ;

	string stringTime = charTime ;
	string eol = "\n" ;
	stringTime.erase( stringTime.size() - eol.size(), eol.size() ) ;
	return stringTime ;

#else // CEYLAN_USES_CTIME

	throw SystemException( "Ceylan::System::timeToString : "
		"not available on this platform (no ::ctime function found)." ) ;

#endif // CEYLAN_USES_CTIME

}




string Ceylan::System::durationToString(
		Second startingSecond, Microsecond startingMicrosecond,
		Second stoppingSecond, Microsecond stoppingMicrosecond )
	throw( SystemException )
{

	// What a shame, I spent hours on this stupid code !


	// These are <b>integer</b> divisions :
	Second s1 = startingSecond + startingMicrosecond / OneMillion ;
	Second s2 = stoppingSecond + stoppingMicrosecond / OneMillion ;

	Microsecond r1 = startingMicrosecond % OneMillion ;
	Microsecond r2 = stoppingMicrosecond % OneMillion ;

	// Microseconds are here in the [0;100000[ range.

	if ( s1 > s2 )
		throw SystemException( "Ceylan::System::durationToString : "
			"specified duration is negative." ) ;

	if ( s1 == s2 )
	{
		if ( r1 > r2 )
			throw SystemException( "Ceylan::System::durationToString : "
				"specified duration should not be negative." ) ;
	}


	// Duration is non-negative here.

	if ( r2 >= r1 )
	{
		return Ceylan::toString( s2 - s1 ) + " second(s) and "
			+ Ceylan::toString( r2 - r1 ) + " microsecond(s)" ;
	}
	else
	{
		return Ceylan::toString( s2 - s1 - 1 ) + " second(s) and "
			+ Ceylan::toString( OneMillion + r2 - r1 )
			+ " microsecond(s)" ;
	}

}




Microsecond Ceylan::System::getDurationBetween(
		Second startingSecond, Microsecond startingMicrosecond,
		Second stoppingSecond, Microsecond stoppingMicrosecond )
	throw( SystemException )
{


	/*
	 * Microsecond is Uint32, hence its maximum value is 4 294 967 295, it 
	 * corresponds to 4294 seconds, i.e. about 1 hour and 11 minutes.
	 *
	 */
	 	 

	// These are <b>integer</b> divisions :
	Second s1 = startingSecond + startingMicrosecond / OneMillion ;
	Second s2 = stoppingSecond + stoppingMicrosecond / OneMillion ;

	Microsecond r1 = startingMicrosecond % OneMillion ;
	Microsecond r2 = stoppingMicrosecond % OneMillion ;

	// Microseconds are here in the [0;100000[ range.

	if ( s1 > s2 )
		throw SystemException( "Ceylan::System::getDurationBetween : "
			"specified duration is negative." ) ;

	if ( s1 == s2 )
	{
		if ( r1 > r2 )
			throw SystemException( "Ceylan::System::getDurationBetween : "
				"specified duration should not be negative." ) ;
	}


	// Duration is non-negative here.

	if ( ( s2 - s1 ) > MaximumDurationWithMicrosecondAccuracy )
		throw SystemException( "Ceylan::System::getDurationBetween : "
				"specified duration should not exceed "
				+ Ceylan::toString( MaximumDurationWithMicrosecondAccuracy ) 
				+ " seconds." ) ;

	return ( ( s2 - s1 ) * OneMillion + r2 -r1 ) ;

}




void Ceylan::System::getPreciseTime( Second & seconds,
		Microsecond & microsec )
	throw( SystemException )
{


#ifdef CEYLAN_USES_GETTIMEOFDAY

	timeval currentTime ;

	if ( ::gettimeofday( & currentTime,
			/* timezone must no be set */ 0 ) != 0 )
		throw SystemException( "System::getPreciseTime : "
			+ Ceylan::System::explainError() ) ;

	seconds  = static_cast<Second>( currentTime.tv_sec ) ;
	microsec = static_cast<Microsecond>( currentTime.tv_usec ) ;

#else // CEYLAN_USES_GETTIMEOFDAY


#ifdef CEYLAN_USES__FTIME_S

    struct _timeb timeBuffer ;

	if( ::_ftime_s( & timeBuffer ) != 0 )
		throw SystemException( "System::getPreciseTime : "
			"_ftime_s failed." ) ;

    seconds  = static_cast<Second>( timeBuffer.time ) ;
	microsec = static_cast<Microsecond>( timeBuffer.millitm * 1000 ) ;


#else // CEYLAN_USES__FTIME_S

	throw SystemException( "System::getPreciseTime : "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__FTIME_S

#endif // CEYLAN_USES_GETTIMEOFDAY

}




Microsecond Ceylan::System::getAccuracyOfPreciseTime( Microsecond * minGap,
	Microsecond * maxGap ) throw( SystemException )
{

	Ceylan::Uint32 numberOfMeasures = 100 ;

	/*
	 * 4 000 000 000 microseconds is more than hour, all accuracies
	 * should be far below.
	 *
	 */
	Microsecond minDuration = 4000000000U ;
	Microsecond maxDuration = 0 ;

	Microsecond currentDuration ;
	Microsecond cumulativeDuration = 0 ;

	Microsecond lastSecond ;
	Microsecond lastMicrosecond ;

	Microsecond currentSecond ;
	Microsecond currentMicrosecond ;

#if CEYLAN_DEBUG_SYSTEM

	/*
	 * Will not be displayed during the measure since it would risk to distort
	 * it.
	 *
	 * ISO C++ forbids variable-size array such as :
	 * Microsecond durations[numberOfMeasures]
	 *
	 */
	Microsecond * durations = new Microsecond[numberOfMeasures] ;

#endif // CEYLAN_DEBUG_SYSTEM

	getPreciseTime( lastSecond, lastMicrosecond ) ;

	for ( Ceylan::Uint32 i = 0; i < numberOfMeasures; i++ )
	{
	
		getPreciseTime( currentSecond, currentMicrosecond ) ;
		
		currentDuration = getDurationBetween( lastSecond, lastMicrosecond,
			currentSecond, currentMicrosecond ) ;
					
		if ( currentDuration < minDuration )
			minDuration = currentDuration ;
		else if ( currentDuration > maxDuration )
			maxDuration = currentDuration ;
			
		cumulativeDuration += currentDuration ;

#if CEYLAN_DEBUG_SYSTEM
		durations[i] = currentDuration ;
#endif // CEYLAN_DEBUG_SYSTEM

		lastSecond = currentSecond ;
		lastMicrosecond = currentMicrosecond ;

	}

	if ( minGap != 0 )
		*minGap = minDuration ;

 	if ( maxGap != 0 )
		*maxGap = maxDuration ;

#if CEYLAN_DEBUG_SYSTEM

	for ( Ceylan::Uint32 i = 0; i < numberOfMeasures; i++ )
		Log::LogPlug::debug( "Duration of getPreciseTime call : "
			+ Ceylan::toString( durations[i] ) + " microseconds." ) ;
			
	Log::LogPlug::debug( "Real average duration : "
		+ Ceylan::toString(
			static_cast<Ceylan::Float32>( 
				cumulativeDuration ) / numberOfMeasures ) ) ;

	delete durations ;

#endif // CEYLAN_DEBUG_SYSTEM

	Microsecond result = static_cast<Microsecond>(
		cumulativeDuration /  numberOfMeasures ) ;

	// Null accuracy would not make sense :
	if ( result == 0 )
		return 1 ;
	else
		return result ;

}




Microsecond Ceylan::System::getPreciseTimeCallDuration() throw()
{

	static Microsecond duration = 0 ;

	// Already computed ? Returned it !
	if ( duration != 0 )
		return duration ;

	// Number of getPreciseTime calls :
	Ceylan::Uint16 calls = 1000 ;

	Second lastSecond ;
	Microsecond lastMicrosecond ;

	Second currentSecond ;
	Microsecond currentMicrosecond ;

	getPreciseTime( lastSecond, lastMicrosecond ) ;

	// Measures the duration of a call to getPreciseTime :
	for ( Ceylan::Uint16 i = 0; i < calls ; i++ )
		getPreciseTime( currentSecond, currentMicrosecond ) ;

	Microsecond totalTime = getDurationBetween( lastSecond, lastMicrosecond,
		currentSecond, currentMicrosecond ) ;

	// Integer division is enough :
	duration = totalTime / calls ;

	if ( duration == 0 )
		duration = 1 ;
		
	return duration ;

}




void Ceylan::System::sleepForSeconds( Second seconds )
	throw( SystemException )
{

#ifdef CEYLAN_USES_SLEEP

	Second stillToBeSlept = seconds ;

	while ( stillToBeSlept != 0 )
	{

		/*
		 * ::sleep returns zero if the requested time has elapsed, or
		 * the number of seconds left to sleep.
		 *
		 */
		stillToBeSlept = ::sleep( stillToBeSlept ) ;

	}

#else // CEYLAN_USES_SLEEP

#ifdef CEYLAN_USES_WINDOWS_H

	// Windows Sleep needs windows.h, hence the PSDK.
	::Sleep( 1000 * seconds /* milliseconds */ ) ;

#else // CEYLAN_USES_WINDOWS_H

	throw SystemException( "Ceylan::sleepForSeconds : "
		"not available on this platform (no ::sleep function found)" ) ;

#endif // CEYLAN_USES_WINDOWS_H

#endif // CEYLAN_USES_SLEEP

}


bool Ceylan::System::areSubSecondSleepsAvailable() throw()
{

#if CEYLAN_ARCH_NINTENDO_DS
		
	// swiWaitForVBlank available for both ARMs:
	return true ; 

#elif CEYLAN_ARCH_WINDOWS == 1

	return true ;
	
#else // CEYLAN_ARCH_WINDOWS


#if CEYLAN_USES_FILE_DESCRIPTORS
	return true ;
#else // CEYLAN_USES_FILE_DESCRIPTORS
	return false ;
#endif // CEYLAN_USES_FILE_DESCRIPTORS
	
	
#endif // CEYLAN_ARCH_NINTENDO_DS
		
}



void Ceylan::System::basicSleep( Second seconds, Nanosecond nanos )
	throw( SystemException )
{

#if CEYLAN_DEBUG_SYSTEM
	LogPlug::debug( "Ceylan::System::basicSleep : requested duration is "
		+ Ceylan::toString( seconds ) + " second(s) and "
		+ Ceylan::toString( nanos ) + " nanosecond(s)." ) ;
#endif // CEYLAN_DEBUG_SYSTEM


#if CEYLAN_ARCH_WINDOWS

	// Expressed in milliseconds :
	::Sleep( seconds * 1000 + nanos / OneMillion ) ;

#else // CEYLAN_ARCH_WINDOWS

// Nanosleep is currently disabled :
#define CEYLAN_USES_NANOSLEEP 0

#if CEYLAN_USES_NANOSLEEP

	/*
	 * nanosleep is POSIX compliant and should be accurate, but on the two
	 * Linux 2.4 kernel we tried, instead of having strict 10 ms scheduling,
	 * the minimum waiting time was two time slices (20 ms).
	 * The step was then 10 ms indeed (so, with longer waiting time, the
	 * actual duration was 30 ms, then 40 ms, and so on, but we were unable to
	 * have an actual sleep duration of 10 ms, even for requested sleep
	 * below 1 ms).
	 *
	 * We therefore disabled the use of nanosleep.
	 *
	 */

	timespec waiter ;

	waiter.tv_sec  = static_cast<Second>( seconds ) ;
	waiter.tv_nsec = static_cast<long>( nanos ) ;

	if ( ::nanosleep( & waiter, /* no remainder information wanted */ 0 ) != 0 )
		throw SystemException( "System::basicSleep with nanosleep : "
			+ Ceylan::System::explainError() ) ;

#else // CEYLAN_USES_NANOSLEEP


#if CEYLAN_USES_FILE_DESCRIPTORS

	/*
	 * Use ::select (chosen by default), which seems to be an
	 * effective way of waiting.
	 *
	 * Should be portable on most UNIX systems.
	 *
	 */

    timeval timeout ;
	timeout.tv_sec = seconds ;

	// Rounded to microseconds :
	timeout.tv_usec = static_cast<long>( nanos / 1000 ) ;


	// Nasty trick : select blocks waiting for the timeout to expire.
	if ( ::select( 0, static_cast<fd_set *>( 0 ),
			static_cast<fd_set *>( 0 ), static_cast<fd_set *>( 0 ),
			& timeout ) < 0 )
		throw SystemException( "System::basicSleep with select : "
			+ Ceylan::System::explainError() ) ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw SystemException( "System::basicSleep : "
		"file descriptor feature not available" ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS


#endif // CEYLAN_USES_NANOSLEEP

#endif // CEYLAN_ARCH_WINDOWS

#if CEYLAN_DEBUG_SYSTEM
	LogPlug::debug( "Ceylan::System::basicSleep : awoken now." ) ;
#endif // CEYLAN_DEBUG_SYSTEM

}




void Ceylan::System::atomicSleep() throw( SystemException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
	/*
	 * Available for both ARMs.
	 * Mean time waiting is 1/(60*2) = 8.3 ms.
	 *
	 */
	swiWaitForVBlank() ;
	
#else // CEYLAN_ARCH_NINTENDO_DS

	/*
	 * Factor of margin so that the requested waiting time will most
	 * probably exactly trigger one time slice of waiting.
	 *
	 * The factor below 1.0 avoids asking for two time slices if
	 * their length was surestimated a bit.
	 *
	 * Being conservative and having a margin is the safe way when the
	 * hosting computer is not idle.
	 *
	 */
	const Ceylan::Float32 marginDecreaseFactor = 0.75f ;

#if CEYLAN_DEBUG_SYSTEM

		LogPlug::debug( "Ceylan::System::atomicSleep triggered." ) ;

#endif // CEYLAN_DEBUG_SYSTEM


	basicSleep( static_cast<Microsecond>(
		marginDecreaseFactor * getSchedulingGranularity() ) ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS		


}




void Ceylan::System::basicSleep( Microsecond micros ) throw( SystemException )
{

	// Split waiting time thanks to integer division :
	basicSleep( /* seconds */ micros / OneMillion,
		/* nanoseconds */ ( micros % OneMillion ) * 1000 ) ;

}




bool Ceylan::System::smartSleep( Second seconds, Microsecond micros )
	throw( SystemException )
{


	/*
	 * Warning : this piece of code is especially sensitive to overflows,
	 * since very high numbers of microseconds are to be handled in
	 * often unsigned variables. Beware !
	 *
	 * Please test it thoroughfully thanks to testCeylanTime utility.
	 *
	 * @see testCeylanTime.cc
	 *
	 */

	// Store initial time :
	Second targetSecond ;
	Microsecond targetMicrosecond ;

	getPreciseTime( targetSecond, targetMicrosecond ) ;

	// Computes the target time to be waited :
	targetSecond += seconds ;

	/*
	 * The substracted microsecond compensate on average for partial
	 * microseconds and overhead (calls) :
	 *
	 */
	targetMicrosecond += micros - getPreciseTimeCallDuration() - 1  ;

	/*
	 * Would risk overflow :
	 * Microsecond targetTime = targetSecond * OneMillion + targetMicrosecond ;
	 *
	 */


	/*
	 * Factor of margin so that time slice is not underestimated, which would
	 * result in surplus waited time slices. Being conservative and having a
	 * margin is the safe way when the hosting computer is not idle.
	 *
	 */
	const Ceylan::Float32 marginIncreaseFactor = 1.2f ;


	/*
	 * Better sleep not enough than too much, since active waiting will
	 * supplement.
	 *
	 * Let's hope that a previous call to getSchedulingGranularity is already
	 * performed, otherwise we might not be on time if we were to compute the
	 * granularity here.
	 *
	 *
	 */
	const Microsecond usedGranularity = static_cast<Microsecond>(
		getSchedulingGranularity() * marginIncreaseFactor ) ;

	/*
	 * Integer divisions to compute the maximum number of time slices fitting
	 * in requested time : here lies the risk of overflow, as specified in the
	 * method doc. Integer division means rounded down, since preferring not
	 * enough sleeping to too much.
	 *
	 * usedGranularity is used so that not too many slices can be selected.
	 *
	 */
	Ceylan::Uint32 fullTimeSliceCount = ( seconds * OneMillion + micros )
		/ usedGranularity ;

#if CEYLAN_DEBUG_SYSTEM
	LogPlug::debug( "Ceylan::System::smartSleep : will first use "
		+ Ceylan::toString( fullTimeSliceCount )
		+ " full time slice(s) for waiting." ) ;
#endif // CEYLAN_DEBUG_SYSTEM

	/*
	 * Sleep for the main part. Use 'basicSleep' rather than
	 * 'getActualDurationForSleep' since the starting time might differ
	 * largely, because of the previous 'getSchedulingGranularity' call.
	 *
	 * Make so that the exact wanted full time slices are waited :
	 *
	 */
	if ( fullTimeSliceCount != 0 )
	{

#if CEYLAN_DEBUG_SYSTEM

		LogPlug::debug( "Ceylan::System::smartSleep : will ask basicSleep "
			+ Ceylan::toString( fullTimeSliceCount
				* getSchedulingGranularity() )
			+ " microseconds." ) ;

#endif // CEYLAN_DEBUG_SYSTEM

		basicSleep( fullTimeSliceCount * getSchedulingGranularity() ) ;

	}

	Second currentSecond ;
	Microsecond currentMicrosecond ;

	getPreciseTime( currentSecond, currentMicrosecond ) ;

	/*
	 * Watch out the overflow on abnormal waiting conditions :
	 * (it is signed hence no getDurationBetween needed) 
	 *
	 */
	Ceylan::SignedLongInteger currentError
		= ( currentSecond - targetSecond ) * OneMillion
			+ currentMicrosecond - targetMicrosecond ;

	LogPlug::debug( "Ceylan::System::smartSleep : "
		"after having waited the big chunk, we are at "
		+ Ceylan::toString( -currentError ) 
		+ " microseconds before deadline." ) ;
		
#if CEYLAN_DEBUG_SYSTEM
	Ceylan::Uint32 atomicCount = 0 ;
#endif // CEYLAN_DEBUG_SYSTEM
			


	while ( -currentError > static_cast<int>( 2 * usedGranularity ) )
	{

#if CEYLAN_DEBUG_SYSTEM
		atomicCount++ ;
#endif // CEYLAN_DEBUG_SYSTEM

		/*
		 * Sleep for one time slice as long as there is another one
		 * remaining.
		 *
		 */
		atomicSleep() ;

		getPreciseTime( currentSecond, currentMicrosecond ) ;
		
		// currentError still signed :
		currentError = ( currentSecond - targetSecond ) * OneMillion
			+ currentMicrosecond - targetMicrosecond ;
			
	}

#if CEYLAN_DEBUG_SYSTEM

	LogPlug::debug( "Ceylan::System::smartSleep : used "
		+ Ceylan::toString( atomicCount )
		+ " atomic waitings." ) ;

#endif // CEYLAN_DEBUG_SYSTEM

	// Check we are still before target time :
	if ( currentError > 0 )
	{
	
		LogPlug::warning( "Ceylan::System::smartSleep : "
			"sleeps waited too much, target time missed of "
			+ Ceylan::toString( currentError )
			+ " microseconds, because of non-precomputed "
			"or underestimated time slices (their evaluated duration was "
			+ Ceylan::toString( getSchedulingGranularity() )
			+ " microseconds)." ) ;
		return false ;
		
	}

	// Yes, still early.


#if CEYLAN_DEBUG_SYSTEM

	Microsecond remaining = static_cast<Microsecond>( -currentError ) ;

	LogPlug::debug( "Ceylan::System::smartSleep : remaining time ("
		+ Ceylan::toString( remaining ) + " microseconds)"
			+ " will be spent in active waiting." ) ;

#endif // CEYLAN_DEBUG_SYSTEM


	bool done = false ;

	/*
	 * After the full time slices have elapsed, perform active waiting for
	 * the remaining time :
	 *
	 */


#if CEYLAN_CHECK_FOR_FREEZE
	std::cerr << std::endl << "active waiting started -> " ;
#endif // CEYLAN_CHECK_FOR_FREEZE

	Ceylan::Sint32 preciseTimeDuration =
		static_cast<Sint32>( getPreciseTimeCallDuration() ) ;

#if CEYLAN_DEBUG_SYSTEM

	Ceylan::Uint32 logCount = 0 ;

#endif // CEYLAN_DEBUG_SYSTEM

	// Should remains small :
	Ceylan::Sint32 remainingTime ;

	while( ! done )
	{

		getPreciseTime( currentSecond, currentMicrosecond ) ;
		remainingTime =
			static_cast<Sint32>( targetSecond - currentSecond ) * OneMillion
			+ targetMicrosecond - currentMicrosecond ;

#if CEYLAN_DEBUG_SYSTEM
		
		logCount++ ;
		
		if ( ( logCount % 50 ) == 0 )
			LogPlug::debug( "Ceylan::System::smartSleep : "
				"remaining time in active waiting is "
				+ Ceylan::toString( remainingTime ) + " microseconds." ) ;
			
		
#endif // CEYLAN_DEBUG_SYSTEM


#if CEYLAN_ARCH_WINDOWS

		/*
		 * On Windows (at least XP), we have a high scheduling granularity 
		 * (we measured up to 16 ms) and, worse, as soon as we perform busy
		 * waiting (because remaining time is too small to take the risk of
		 * requesting a waiting of one time slice), we observed it led to
		 * the OS perfoming a context switch (hence even with busy waiting
		 * we end up with a full time slice penalty, and we would be always
		 * late, of up to one time slice. Bad performance).
		 * Let Ts be the time slice duration (16 ms for example).
		 * Thus at this point if remaining time tr is between 0 and Ts / 2,
		 * we have the choice to be too early of tr, or to be late of at least
		 * Ts - tr > tr.
		 * We prefer the former to the latter (earlier better than later, and
		 * error is smaller).
		 * Hence on average we will be waiting the right duration, even 
		 * though we will be most of the time either too early or too
		 * late. 
		 * On GNU/Linux the busy waiting does not trigger such context changes
		 * and we just have to wait the deadline as expected, with pretty
		 * good results.
		 *
		 */
		
		if ( remaining  < 1.5 * usedGranularity )
		{

#if CEYLAN_DEBUG_SYSTEM
			LogPlug::debug( "Ceylan::System::smartSleep : remaining time ("
				+ Ceylan::toString( remaining ) + " microseconds)"
				+ " smaller than half of a time slice ("
				+ Ceylan::toString( usedGranularity ) 
				+ " microseconds) and we are on Windows, "
				"we prefer to finish earlier than later." ) ;
#endif // CEYLAN_DEBUG_SYSTEM

			return false ;

		}

#endif // CEYLAN_ARCH_WINDOWS

		/*
		 * getPreciseTime would have it own duration, and would take into
		 * account call overhead :
		 *
		 */
		if ( remainingTime < preciseTimeDuration )
			done = true ;

		/*
		 * Nothing is done against the accuracy of getPreciseTime
		 * (see getAccuracyOfPreciseTime), since the error should be balanced
		 * and null on average.
		 *
		 */

	}

#if CEYLAN_CHECK_FOR_FREEZE
	std::cerr << "finished" ;
#endif // CEYLAN_CHECK_FOR_FREEZE

	// We should be almost just-in-time here !

	return true ;
	
}




bool Ceylan::System::smartSleepUntil( Second second, Microsecond micro )
	throw( SystemException )
{

	Second currentSecond ;
	Microsecond currentMicrosecond ;

	getPreciseTime( currentSecond, currentMicrosecond ) ;

	/*
	 * To compensate for specific smartSleepUntil overhead
	 * (ex : getPreciseTime call) :
	 *
	 */
	currentMicrosecond++ ;

	if ( currentSecond > second || currentMicrosecond > micro )
		throw SystemException( "Ceylan::System::smartSleepUntil : "
			"specified date (" + Ceylan::toString( second )
			+ " seconds and " + Ceylan::toString( micro )
			+ " microseconds) is on the past (current date is "
			+ Ceylan::toString( currentSecond ) + " seconds, "
			+ Ceylan::toString( currentMicrosecond ) + " microseconds)." ) ;
	 
	if ( micro < currentMicrosecond )
	{
		second-- ;
		micro += OneMillion ;
	}
	
	return smartSleep( second - currentSecond, micro - currentMicrosecond ) ;

}




Microsecond Ceylan::System::getActualDurationForSleep(
		Microsecond requestedMicroseconds, Second requestedSeconds )
	throw( SystemException )
{


	// (disabled, see below) Be sure it is precomputed :
	//Microsecond preciseTimeDuration = getPreciseTimeCallDuration() ;

	// Beware of overflows !

	Second lastSecond ;
	Microsecond lastMicrosecond ;

	Second currentSecond ;
	Microsecond currentMicrosecond ;

	getPreciseTime( lastSecond, lastMicrosecond ) ;
	
	Ceylan::System::basicSleep( requestedSeconds /* second */,
		requestedMicroseconds * 1000 /* nanoseconds */ ) ;
		
	getPreciseTime( currentSecond, currentMicrosecond ) ;


	/*
	 * Previously, 'preciseTimeDuration' was substracted but it was leading to
	 * underestimation.
	 *
	 */

	return getDurationBetween( lastSecond, lastMicrosecond, 
		currentSecond, currentMicrosecond) ;


}




Microsecond Ceylan::System::getSchedulingGranularity() throw( SystemException )
{


	/*
	 * Requesting too low sleep durations will not trigger the time slice
	 * waiting (ex : on Linux 2.4, for a requested sleep of 310 microseconds,
	 * measured time has been 3166 microseconds whereas the time slice is
	 * 10 000 microseconds).
	 *
	 * Assuming no system will have a granularity below 500 microseconds,
	 * the test of a sleep of 450 microseconds should be relevant.
	 *
	 */

	static Microsecond granularity = 0 ;

	// Already computed ? Returned it !
	if ( granularity != 0 )
		return granularity ;

#if CEYLAN_DEBUG_SYSTEM

	bool logMeasures = true ;
	
	/*
	 * Logs can been interpreted thanks to gnuplot, see comment in 
	 * test/system/testCeylanTime.cc for more detailed explanations.
	 *
	 */
	 
	const string logFilename = "granularity.dat" ;
	
	File * logFile = 0 ;
	
	if ( logMeasures )
	{
	
		logFile = new File( logFilename ) ;
		logFile->write( 
			"# This file records the requested sleep durations (first column) "
			"and the corresponding actual sleep durations (second column).\n"
			"# The scheduling granularity can be usually guessed from it.\n"
			"# One may use gnuplot to analyze the result.\n\n"   ) ;
			
		LogPlug::trace( "Ceylan::System::getSchedulingGranularity : "
			"computing granularity now, and logging the result in the '"
			+ logFilename + "' file." ) ;

	}	
	else
	{
	
		LogPlug::trace( "Ceylan::System::getSchedulingGranularity : "
			"computing granularity now (no file logging requested)." ) ;
			
	}
		
#endif // CEYLAN_DEBUG_SYSTEM


	// Not computed yet, let's evaluate it one time for all.


	/*
	 * The algorithm is simple : as long as two successive small sleep values
	 * do not result in relatively similar actual sleeping times, increase
	 * the requested sleeping time.
	 *
	 */

	Microsecond lastMeasuredDuration = 0 ;
	Microsecond currentMeasuredDuration = 0 ;


	const Microsecond durationStep = 250 ;

	/**
	 * Upper bound to time slice is 110 ms, to be able to catch as high as
	 * a 100 ms granularity.
	 *
	 */
	Microsecond maximumPossibleDuration = 110000 ;


	/*
	 * Relative comparison : x and y are relatively equal iff
	 * |x-y|/(x+y)< epsilon/2
	 *
	 * @see Ceylan::Maths::AreRelativelyEqual
	 *
	 */

	Microsecond currentRequestedDuration = 0 ;

	while ( currentRequestedDuration < maximumPossibleDuration )
	{

		// Increase the duration :
		currentRequestedDuration += durationStep ;
		
		lastMeasuredDuration = currentMeasuredDuration ;
		
		currentMeasuredDuration =
			getActualDurationForSleep( currentRequestedDuration ) ;

#if CEYLAN_DEBUG_SYSTEM
		logFile->write( Ceylan::toString( currentRequestedDuration ) 
			+ " \t " + Ceylan::toString( currentMeasuredDuration ) 
			+ " \n" ) ;
#endif // CEYLAN_DEBUG_SYSTEM


		// Avoid zero division and do not stop at zero :
		if ( currentMeasuredDuration == lastMeasuredDuration )
			if ( currentMeasuredDuration == 0 )
				continue ;
			else
				break ;

		// Loop until a gentle slope is found (1%) :
		if ( Ceylan::Maths::Abs( static_cast<Ceylan::Float32>(
					currentMeasuredDuration - lastMeasuredDuration ) )
				/ ( currentMeasuredDuration + lastMeasuredDuration ) < 0.005f )
			break ;

#if CEYLAN_DEBUG_SYSTEM
		LogPlug::debug( "Previous (" + Ceylan::toString( lastMeasuredDuration )
			+ ") and current (" + Ceylan::toString( currentMeasuredDuration )
			+ ") measured durations are not deemed relatively equal, "
			" continuing." ) ;
#endif // CEYLAN_DEBUG_SYSTEM

	}


	/*
	 * When all measures fail, use this test duration to approximately guess
	 * the time slice.
	 *
	 */
	Microsecond testDuration = 900 ;


	// Hit top, nothing found ?
	if ( currentRequestedDuration == maximumPossibleDuration )
	{
	
		currentMeasuredDuration = getActualDurationForSleep( testDuration ) ;
		LogPlug::warning( "Ceylan::System::getSchedulingGranularity : "
			"failed to guess actual time slice duration, taking "
			+ Ceylan::toString( currentMeasuredDuration )
			+ " microseconds as an experimental basis." ) ;
			
	}
	else
	{

#if CEYLAN_DEBUG_SYSTEM
		// currentMeasuredDuration is the duration of our time slice.
		LogPlug::debug( "Relative equality has been found between previous ("
			+ Ceylan::toString( lastMeasuredDuration )
			+ ") and current (" + Ceylan::toString( currentMeasuredDuration )
			+ ") measured durations." ) ;
#endif // CEYLAN_DEBUG_SYSTEM

	}


	/*
	 * Requesting multiple times the time slice allows to measure it finely,
	 * in order to compute the average value.
	 *
	 */
	const Ceylan::Uint8 sampleCount = 20 ;

	// Useless but careful :
	granularity = 0 ;

	for ( Ceylan::Uint8 i = 0; i < sampleCount; i++ )
	{

		/*
		 * The 0.4 factor is here to ensure we do not request just more than
		 * the time-slice, if we had surestimated it a bit, we could have 
		 * two time slices.
		 *
		 */

		granularity += getActualDurationForSleep(
			static_cast<Microsecond>( 0.4 * currentMeasuredDuration ) ) ;

	}

	granularity /= sampleCount ;

#if CEYLAN_DEBUG_SYSTEM
	LogPlug::debug( "Final returned scheduling granularity is "
		+ Ceylan::toString( granularity ) + " microseconds." ) ;
		
	if ( logFile != 0 )
	{
		logFile->close() ;
		delete logFile ;
		logFile = 0 ;
	}	
#endif // CEYLAN_DEBUG_SYSTEM

	return granularity ;

}



bool Ceylan::System::setLegacyStreamSynchronization( bool synchronized )
	throw()
{

	/*
	 * @note It is unclear whether file I/O can be impacted.
	 *
	 * @see the following two pages :
	 * - http://www.cplusplus.com/ref/iostream/ios_base/sync_with_stdio.html
	 * - http://gcc.gnu.org/onlinedocs/libstdc++/27_io/howto.html#8
	 *
	 */

	// Use only C++ streams, and only after this call :
	return std::ios::sync_with_stdio( synchronized ) ;

}

