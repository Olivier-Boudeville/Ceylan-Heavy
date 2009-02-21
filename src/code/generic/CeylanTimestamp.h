#ifndef CEYLAN_TIME_STAMP_H_
#define CEYLAN_TIME_STAMP_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanUtils.h"            // for UtilsException
#include "CeylanTypes.h"            // for Ceylan::Sint16 etc.
#include "CeylanSystem.h"           // for Second


#include <string>


namespace Ceylan
{



	/**
	 * The purpose of this class is to record specific times, in order to be
	 * able to date events and compare their order, at least on a local
	 * computer.
	 *	
	 * @note On a distributed system, one should check that clocks are, at 
	 * least roughly (ex: thanks to NTP), in synch.
	 *
	 * @note To understand numerical conventions behind the time units, one can
	 * have a look to UNIX 'ctime' manual, for example.
	 *
	 * @note The granularity (accuracy) of this timestamp implementation is 
	 * one second, hence two timestamps may be exactly equal whereas they
	 * were not created at the same time.
	 *
	 * @see CeylanSystem.h for precise time measurement and fine-grained
	 * waiting.
	 *
	 */
	class CEYLAN_DLL Timestamp : public TextDisplayable
	{
	
	
		public:
		
		
			/**
			 * Creates the timestamp corresponding to the moment when it is
			 * called.
			 *
			 * @throw UtilsException if the time could not be obtained.
			 *
			 */
			Timestamp() throw( UtilsException ) ;
			
			
			/// Basic virtual destructor.
			virtual ~Timestamp() throw() ;
			
			
			/**
			 * Returns a user-friendly description of the state of this object.
		     *
			 * The timestamp format is [year/month/day hours:minutes:seconds]
			 * For instance: [2004/05/07 19:07:54] 
			 *
			 * Another format could have been the one given by 
			 * System::timeToString( System::getTime() ) ;
			 *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
		     * @see TextDisplayable, Displayable
		     * @see Ceylan::VerbosityLevels
		     *
		     */
		    virtual const std::string toString( 
				Ceylan::VerbosityLevels level = Ceylan::high ) const throw() ;
	
	
			/**
			 * Returns a string describing litterally the specied duration.
			 *
			 * @param duration the duration, in seconds, to describe.
			 *
			 * @return the textual description of the duration.
			 *
			 * @note For this evaluation, years all have exactly 365 days.
			 *
			 */
			static std::string DescribeDuration( 
				Ceylan::System::Second duration ) throw() ;
			
	
			/**
			 * Tells whether this Timestamp's recorded time is strictly
			 * inferior to the one of second Timestamp.
			 *
			 */
			bool operator < ( Timestamp & second ) throw() ;

			
			/// Years are coded as pure numerical value, starting from 0 AC.
			typedef Ceylan::Uint16 Year ;
			
			
			/// Months of the year range from 1 to 12, from January to December.
			typedef Ceylan::Uint8 MonthOfTheYear ;
			
			
			/// Days of the month range from 1 to 31.
			typedef Ceylan::Uint8 DayOfTheMonth ;
			
			
			/// Hours of the day range from 0 to 23.
			typedef Ceylan::Uint8 HourOfTheDay ;
			
			
			/// Minutes of the hour range from 0 to 59.
			typedef Ceylan::Uint8 MinuteOfTheHour ;
			
			
			/**
			 * Seconds of the minute range from 0 to 61, to allow for leap
			 * seconds.
			 *
			 */
			typedef Ceylan::Uint8 SecondOfTheMinute ;
			
			
			
		protected:		
		
		
			/// Timestamp's year.
			Year _year ;
			
			/// Timestamp's month.
			MonthOfTheYear _month ;

			/// Timestamp's day.
			DayOfTheMonth _day ;
			
			/// Timestamp's hour.
			HourOfTheDay _hour ;
			
			/// Timestamp's minute.
			MinuteOfTheHour _minute ;
						
			/// Timestamp's second.
			SecondOfTheMinute _second ;
		
		

		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Timestamp( const Timestamp & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Timestamp & operator = ( const Timestamp & source ) throw() ;
		
			
	} ;
	
	
}


#endif // CEYLAN_TIME_STAMP_H_

