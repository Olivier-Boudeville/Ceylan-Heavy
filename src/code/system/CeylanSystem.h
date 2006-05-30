#ifndef CEYLAN_SYSTEM_H_
#define CEYLAN_SYSTEM_H_


#include "CeylanTypes.h"        // for Uint32
#include "CeylanException.h"    // for Exception
#include "CeylanFeatures.h"     // for FeatureNotAvailableException


#include <ctime>
#include <string>
#include <iosfwd>


namespace Ceylan
{


	/**
	 * General system calls and properties.
	 *
	 * Manages in a C++ user-friendly fashion basic system services.
	 *
	 */
	namespace System
	{


		/**
		 * Unsigned size, in bytes, for example for file operations.
		 *
		 */
		typedef size_t Size ;
				
				
		/**
		 * Signed size, in bytes, for example for file operations.
		 *
		 */
		typedef ssize_t SignedSize ;
				
				
		/**
		 * Records seconds, even for long periods of time
		 * (more than a century).
		 *
		 * @note This can be useful to record dates expressed as a number of 
		 * seconds since the Epoch, 00:00:00 on January 1, 1970, 
		 * Coordinated Universal Time (UTC).
		 *
		 */
		typedef Uint32 Second ;			
		
			
		/**
		 * Records milliseconds, which last 10^¯3 second.
		 *
		 */
		typedef Uint32 Millisecond ;			
			
			
		/**
		 * Records microseconds, which last 10^¯6 second.
		 *
		 */
		typedef Uint32 Microsecond ;			
			
			
		/**
		 * Records nanoseconds, which last 10^¯9 second.
		 *
		 */
		typedef Uint32 Nanosecond ;			
			


		/// Exception raised when system operation fails.
		class SystemException : public Ceylan::Exception
		{
			public :
				explicit SystemException( const std::string & message ) 
					throw() ;
				virtual ~SystemException() throw() ;
	
		} ;


		/// Exception raised when basic input/output operation fails.
		class IOException : public SystemException
		{
			public :
				IOException( const std::string & message ) throw() ;
				virtual ~IOException() throw() ;
		} ;



		/**
		 * UNIX File descriptor type, they have to be transformed into
		 * the StreamID datatype to comply with the Stream interface.
		 *
		 */
		typedef int FileDescriptor ;


	
		/// Error number as defined by errno.
		typedef int ErrorCode ;
	
	
		/// Returns the error ID (errno).
		ErrorCode getError() throw() ;
	
	
		/// Returns the diagnosis string corresponding to errorID (errno).
		const std::string explainError( ErrorCode errorID ) throw() ;
	
		/**
		 * Returns the diagnosis string corresponding to current error ID
		 * (errno).
		 *
		 */
		const std::string explainError() throw() ;


		/**
		 * Returns the name of the default shell, if found, otherwise returns
		 * an empty string.
		 *
		 * This is done by reading environment variable 'SHELL'.
		 *
		 * @note Some badly behaving shells, or old ones, do not set SHELL or
		 * use a misleading value.
		 *
		 */
		std::string getShellName() throw() ; 

		
	
		/**
		 * Reads from <b>fd</b> stream to <b>dataBuffer</b> 
		 * <b>toReadBytesNumber</b> bytes.
		 *
		 * Will raise an IOException if read fails.
		 *
		 * @return the number of bytes that have actually been read.
		 *
		 * @throw FeatureNotAvailableException if the file descriptor
		 * feature is not available.
		 *
		 */
		SignedSize FDRead( FileDescriptor fd, char * dataBuffer, 
				Size toReadBytesNumber ) 
			throw( IOException, Features::FeatureNotAvailableException ) ;
	
	
		/**
		 * Writes to <b>fd</b> stream from <b>dataBuffer</b> 
		 * <b>toWriteBytesNumber</b> bytes.
		 *
		 * Will raise an IOException if write fails.
		 *
		 * @return the number of bytes that have actually been read.
		 *
		 * @throw FeatureNotAvailableException if the file descriptor
		 * feature is not available.
		 *
		 */
		SignedSize FDWrite( FileDescriptor fd, const char * dataBuffer, 
				Size toWriteBytesNumber )
			throw( IOException, Features::FeatureNotAvailableException ) ;
			
		
	
		/*
		 * Time section.
		 *
		 * @see Ceylan::Timestamp to easily access the correct local time.
		 *
		 */
	
	
		/**
		 * Returns the current time since the Epoch (00:00:00 UTC, January 1,
		 * 1970), measured in seconds.
		 *
		 * @note Will cause an overflow after year 2038.
		 *
		 * @see Ceylan::Timestamp to easily access the correct local time.
		 *
		 */
		Second getTime() throw( SystemException ) ;
			
	
		/**
		 * Converts the time moment into a human readable string.
		 *
		 * @throw SystemException if the conversion is not available or
		 * failed.
		 *
		 * @see Ceylan::Timestamp to easily output the correct local time.
		 *
		 */
		std::string timeToString( const time_t & t ) throw( SystemException ) ;


		/**
		 * Returns a textual representation for the duration specified thanks 
		 * to its start and stop times. 
		 *
		 * @example : "3 second(s) and 322 microsecond(s)".
		 *
		 * This is not a totally obvious function, since overflow must not
		 * occur, and microseconds are not required to be in the [0;100000[
		 * range.
		 *
		 * @param startingSecond the second corresponding to the starting
		 * moment.
		 *
		 * @param startingMicrosecond the microsecond corresponding to the
		 * starting moment.
		 *
		 * @param stoppingSecond the second corresponding to the stopping
		 * moment.
		 *
		 * @param stoppingMicrosecond the microsecond corresponding to the
		 * stopping moment.
		 *
		 * @return the textual description of the corresponing duration.
		 *
		 * @throw SystemException if the duration is strictly negative.
		 *
		 */
		std::string durationToString( 
				Second startingSecond, Microsecond startingMicrosecond,
				Second stoppingSecond, Microsecond stoppingMicrosecond ) 
			throw( SystemException ) ;
			
			
		/**
		 * Returns the duration, in microseconds, between the two specified
		 * times, i.e. duration = (stopping time) - (starting time).
		 *
		 * @note The duration shall not exceed the capacity of the Microsecond
		 * storage type, i.e. about 4 200 seconds, a little more than one hour. 
		 *  
		 * @throw SystemException if the duration is strictly negative, or 
		 * if duration is above 4 200 seconds, to prevent overflow.
		 *
		 */	
		Microsecond getDurationBetween( 
				Second startingSecond, Microsecond startingMicrosecond,
				Second stoppingSecond, Microsecond stoppingMicrosecond ) 
			throw( SystemException ) ;
			
			
		/**
		 * Returns the time since the Epoch (00:00:00 UTC, January 1, 1970),
		 * with up to a one microsecond accuracy, expressed as a pair 
		 * containing the number of seconds and the number of microseconds
		 * elapsed.
		 *
		 * @param seconds the variable which will be updated with the number 
		 * of seconds elapsed.
		 *
		 * @param microsec the variable which will be updated with the number 
		 * of microseconds elapsed.
		 *
		 * @note Keep in mind that a microsecond lasts for 10^-6 second.
		 *
		 * @note The returned number of seconds should be roughly equal to :
		 * 35 years * 365 days * 24 hours * 3600 seconds = 1 103 760 000 which
		 * does not risk overflow when stored in Uint32, whose maximum value 
		 * is 4 294 967 295.
		 * For Pentiums, the rdtsc code fragment is accurate to one clock cycle.
		 *
		 * @note Expect this call to last for about 3 microseconds on 2.6
		 * kernels.
		 *
		 * @throw SystemException should a problem occur.
		 *
		 */
		void getPreciseTime( Second & seconds, Microsecond & microsec ) 
			throw( SystemException ) ;


		/**
		 * Returns the mean runtime-computed actual accuracy if the precise 
		 * time measurement, expressed in microseconds. Result is of course 
		 * one microsecond or more.
		 *
		 * The accuracy is measured thanks to two successive calls to
		 * getPreciseTime. As most systems are running at 1 GHz or more, they
		 * should be able to execute about one instruction per nanosecond, or
		 * about 1000 instructions per microsecond. 
		 * Assuming the instructions involved by the call to getPreciseTime are
		 * negligible compared to 1000 instructions, the result should be at
		 * least roughly accurate at the millisecond scale.
		 * 		 
		 * @see getPreciseTime
		 *
		 * @param minGap updates, if non-null, the pointed variable with the
		 * smallest measured duration. Should be the actual granularity of the
		 * underlying clock.
		 *
		 * @param maxGap updates, if non-null, the pointed variable with the
		 * maximal measured duration. 
		 *
		 * @throw SystemException should a problem occur.
		 *
		 * @note A typical value for accuracy on a Linux 2.4 kernel is exactly
		 * one microsecond, which is the best possible for this method, which
		 * returns an integer number of microseconds. 
		 * Such a precise accuracy should be overkill for most applications.
		 * However, on 2.6 kernels, accuracy is found to be about 3
		 * microseconds.
		 *
		 */
		Microsecond getAccuracyOfPreciseTime( Microsecond * minGap = 0 , 
			Microsecond * maxGap = 0 ) throw( SystemException ) ;
		
		
		/**
		 * Returns a mean duration of a call to getPreciseTime.
		 *
		 * @note The computation is done one time for all. The first call may
		 * last up to 3 milliseconds. 
		 * Next calls to this method will return almost immediatly this
		 * precomputed value.
		 *
		 * @return the mean duration of a call to getPreciseTime
		 *
		 */
		Microsecond getPreciseTimeCallDuration() throw() ;
		
		
		/**
		 * Sleeps for the specified number of seconds.
		 *
		 * It makes  the  current process sleep until seconds seconds have
		 * elapsed or a signal arrives which is not ignored.
		 *
		 * @throw SystemException if the sleep failed.
		 *
		 * @see basicSleep methods for far better accuracy.
		 *
		 */
		void sleepForSeconds( Second seconds ) throw( SystemException ) ;  
		
		
		/**
		 * Makes the process basically sleep for a small duration, 
		 * which is probably the smallest possible duration on the system,
		 * scheduler-wise, i.e. exactly one time slice.
		 *
		 * This is useful to spare CPU time/battery life by avoiding 
		 * busy loops, without needing fine-grained timing : it is just
		 * a convenient way of adding a delay in a busy loop so that 
		 * it becomes more resource-friendly.
		 *
		 * @note The first call to this method may trigger the computing
		 * of the scheduling granularity, which takes some time.
		 *
		 * @throw SystemException if the operation failed, included if the
		 * necessary file descriptor feature is not available.
		 *
		 * @see Ceylan::Features::areFileDescriptorsSupported to check it 
		 * prior to calling this sleep method.
		 *
		 */
		void basicSleep() throw( SystemException ) ; 
		 
		 
		/**
		 * Makes the process basically sleep for (at least) specified duration.
		 *
		 * Sleeps are far better than busy waiting (waiting loops) insofar as
		 * other processes can be executed during the sleep, and most laptops
		 * should consume less power that way.
		 *
		 * The problem with basic sleeping is that it depends on the 
		 * granularity of the scheduling for the underlying operating system.
		 * For example, no sleep may be shorter than 10 ms on Linux/i386
		 * (prior to kernel 2.6) and 1 ms on Linux/Alpha.
         *
		 * @param second the number of seconds to wait
		 *
		 * @param nanos the remaining part of the time to wait, expressed as 
		 * a number of nanoseconds. As full seconds should be taken into 
		 * account with the parameter <b>second</b>, <b>nanos</b> should be 
		 * less than one second, i.e. should be in the range 0 to 10E9 - 1.
		 *
		 * @throw SystemException if a non-blocked signal interrupted the 
		 * sleep, or if the nanos parameter was out of range, or if the
		 * necessary file descriptor feature is not available.
		 *
		 * @see Ceylan::Features::areFileDescriptorsSupported to check it 
		 * prior to calling this sleep method.
		 *
		 *
		 */
		void basicSleep( Second seconds, Nanosecond nanos ) 
			throw( SystemException ) ; 


		/**
		 * Makes the process basically sleep for (at least) specified duration.
		 *
		 * @see basicSleep( Second seconds, Nanosecond nanos )
		 *
		 */
		void basicSleep( Microsecond micros ) throw( SystemException ) ; 


		/**
		 * Makes the process smartly sleep for the specified duration.
		 *
		 * Smart sleep is the result of as many basic sleeps as needed to wait
		 * for the main part of the specified duration (which is the result 
		 * of the integer division of the requested duration by the guessed 
		 * time slice duration), followed by active waiting to complete the
		 * remaining part of the specified waiting time. 
		 * Finally, the process should wait for the specified time with as 
		 * small as possible resource use, and with a precision of a few 
		 * microseconds, on a not too heavily loaded computer.
		 *
		 * @note As much as possible, no active polling is made to save CPU
		 * cycles for other processes and to save laptop batteries. 
		 * Fine grained waiting is nevertheless performed, with one big 
		 * sleeping duration, supplemented by as many one-slice sleeps as
		 * needed to end up with the last time slice and the remaining 
		 * sub-slice time being spent in active waiting.
		 *
		 * @note A preliminary call to getSchedulingGranularity should be
		 * performed first, so that the time slice evaluation, which is a
		 * lengthy process, is done before actual run.  
		 * Otherwise the first call to smartSleep would return too late.
		 *
		 * @param seconds the number of seconds to wait, should be in the 
		 * range 0 to 4200 to avoid overflow. For longer periods, use multiple
		 * calls to smartSleep.
		 *
		 * @param micros the remaining part of the time to wait, expressed 
		 * as a number of microseconds. As full seconds should be taken into
		 * account with the parameter <b>seconds</b>, <b>micros</b> should be
		 * less than one second, i.e. should be in the range 0 to 10E6 - 1.
		 *
		 * @return whether the deadline was successfully met, i.e. if 
		 * the waiting was on schedule. 
		 *
		 * @throw SystemException if a system call failed or if the necessary
		 * file descriptor feature is not available.
		 *
		 * @see Ceylan::Features::areFileDescriptorsSupported to check it 
		 * prior to calling this sleep method.
		 *
		 */
		bool smartSleep( Second seconds, Microsecond micros ) 
			throw( SystemException ) ; 


		/**
		 * Makes the process smartly sleep until the specified time arrives.
		 * 
		 * @param second the second to wait for.
		 *
		 * @param micro the microsecond to wait for, expressed as a number of
		 * microseconds. 
		 * As full seconds should be taken into account with the parameter
		 * <b>second</b>, <b>micros</b> should be less than one second, i.e.
		 * should be in the range 0 to 10E6 - 1.
		 *
		 * @note Uses smartSleep
		 *
		 * @throw SystemException if a system call failed, or if specified 
		 * time is in the past, or if the necessary file descriptor feature 
		 * is not available.
		 *
		 * @see Ceylan::Features::areFileDescriptorsSupported to check it 
		 * prior to calling this sleep method. 
		 *
		 */
		bool smartSleepUntil( Second second, Microsecond micro ) 
			throw( SystemException ) ; 
		 
		 
		/**
		 * Sleeps, and returns the actual sleeping time corresponding to 
		 * the requested sleeping time, expressed in seconds and microseconds.
	 	 *
		 * @param requestedMicroseconds the time to sleep, expressed in
		 * microseconds, in the [0, 1 000 000 [ range.
		 *
		 * @param requestedSeconds the full seconds to be waited, not more 
		 * than an hour to avoid overflow of returned value.
		 *
		 * @return the actual slept duration, expressed in microseconds
		 *
		 * @throw SystemException if waiting or measuring time went wrong.
		 *
 		 */
		Microsecond getActualDurationForSleep( 
				Microsecond requestedMicroseconds, Second requestedSeconds = 0 )
			throw( SystemException ) ;
		
		
		/**
		 * Returns the run-time computed scheduling granularity of the time
		 * slice.
		 *
		 * Sleeping for smaller durations will result in sleeping the duration
		 * corresponding to the granularity.
		 *
		 * For example, with a scheduling granularity of 10 ms, sleeping for
		 * durations between 0 (excluded) and 10 ms (excluded) will result on 
		 * an idle computer exactly in a 10 ms sleep.
		 *
		 * @note The computation is done one time for all. It may last up to
		 * a few seconds.
		 * Next calls to this method will return almost immediatly this
		 * precomputed value.
		 *
		 * @note If the computer is loaded with other demanding processes, 
		 * then the computed time slice will not be the kernel basic time 
		 * slice (say, 10 ms) but an average time slice availability (say, 14
		 * ms) since other processes use CPU time too. It is however the 
		 * intended behaviour since this user application needs to rely on an
		 * actual availability rather than a theoritical one.
		 * This granularity will be relevant as long as the computer load will
		 * remain relatively constant during the application execution.
		 *
		 * Typically, on Linux 2.4 kernels, the returned value on idle 
		 * computers should be about 10 000 (microseconds), i.e. 10 ms.
		 *
		 * @note One may force a first call to this method to have the
		 * granularity precomputed one time for all.
		 *
		 * @throw SystemException if the operation is not supported (file
		 * descriptor feature is needed) or if the measurement failed.
		 *
		 */
		Microsecond getSchedulingGranularity() throw( SystemException ) ; 



		/**
		 * Sets whether the C++ standard streams (cin, cout, cerr, clog, and
		 * their wide-character counterparts) should be synchronized with 
		 * their C-stream counterparts (this is the default situation). 
		 *
		 * Deactivating synchronization leads to substancial gains of speed 
		 * for I/O operations operating on these streams, at the price of 
		 * using only C++ streams (therefore the C-stream counterparts should
		 * not be used at all).
		 *
		 * @param synchronized if true, ensures C and C++ streams are
		 * synchronized (the default), if false no specific synchonization
		 * is performed.
		 *
		 * @return the previous synchronization status.
		 *
		 * @note Should be called before performing any I/O via the C++ 
		 * stream objects.
		 *
		 * @note It is unclear whether file I/O can be impacted.
		 *
		 */
		bool setLegacyStreamSynchronization( bool synchronized ) throw() ;
		
		
	}
	
}



#endif // CEYLAN_SYSTEM_H_
