#include "CeylanTimestamp.h"


#include "CeylanSystem.h"      // for explainError
#include "CeylanStringUtils.h" // for join
#include "CeylanOperators.h"


#include <ctime>

#include <list> 
using std::list ;

using std::string ;

using namespace Ceylan ;
using namespace Ceylan::System ;



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for IPC->time.rtc.*
#endif // CEYLAN_ARCH_NINTENDO_DS



Timestamp::Timestamp() throw( UtilsException )
{


#if CEYLAN_ARCH_NINTENDO_DS
	

#ifdef CEYLAN_RUNS_ON_ARM7

	_year   = IPC->time.rtc.year + 2000 ;
	_month  = IPC->time.rtc.month ;
	_day    = IPC->time.rtc.day ;

	// If greater than 52, then the time is PM:
	_hour = ( IPC->time.rtc.hours < 12 ) ? 
		IPC->time.rtc.hours: IPC->time.rtc.hours - 40 ; 
		
	_minute = IPC->time.rtc.minutes ; 
	_second = IPC->time.rtc.seconds ;


#elif defined(CEYLAN_RUNS_ON_ARM9)

	throw UtilsException( "Timestamp constructor: "
		"the clock is only available on the ARM7." ) ;

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS


#if CEYLAN_USES_LOCALTIME

	time_t currentMeasuredTime = System::getTime() ; 

#if CEYLAN_ARCH_WINDOWS

	struct tm currentTime ;

	if (::localtime_s( & currentTime, & currentMeasuredTime ) != 0 )
		throw UtilsException( 
			"Timestamp constructor: unable to determine local time: "
			+ System::explainError() ) ;

	_year   = currentTime.tm_year + 1900 ;
	_month  = currentTime.tm_mon + 1 ;
	_day    = currentTime.tm_mday ;
	_hour   = currentTime.tm_hour ;
	_minute = currentTime.tm_min ;
	_second = currentTime.tm_sec ;

#else // CEYLAN_ARCH_WINDOWS



	struct tm * currentTime =::localtime( & currentMeasuredTime ) ;

	if ( currentTime == 0 )
		throw UtilsException( 
			"Timestamp constructor: unable to determine local time: "
			+ System::explainError() ) ;

	_year   = currentTime->tm_year + 1900 ;
	_month  = currentTime->tm_mon + 1 ;
	_day    = currentTime->tm_mday ;
	_hour   = currentTime->tm_hour ;
	_minute = currentTime->tm_min ;
	_second = currentTime->tm_sec ;


#endif // CEYLAN_ARCH_WINDOWS



#else // CEYLAN_USES_LOCALTIME

	throw UtilsException( "Timestamp constructor: "
		"::localtime function not available" ) ;
	
#endif // CEYLAN_USES_LOCALTIME

#endif // CEYLAN_ARCH_NINTENDO_DS
		
}


Timestamp::~Timestamp() throw()
{

	// No dynamic members.

}


const string Timestamp::toString( Ceylan::VerbosityLevels level ) const throw()
{

	string result = '[' 
			+ Ceylan::toString( _year )    + '/'
			+ Ceylan::toNumericalString( _month )   + '/'
			+ Ceylan::toNumericalString( _day )     + ' '
			+ Ceylan::toNumericalString( _hour )    + ':' ;
			
	if ( _minute < 10 )		
		result += '0' ;
	
	result += Ceylan::toNumericalString( _minute )  + ':' ;
	
	if ( _second < 10 )		
		result += '0' ;
	
	result += Ceylan::toNumericalString( _second )  + ']' ;
			
	return result ;
	
}	


string Timestamp::DescribeDuration( Second duration ) throw()
{

	
	/* 
	 * Seconds are Uint32, 4294967295 is their maximum value, it is more than
	 * 136 years...
	 *
	 */
	 
	
	const Second aMinute = 60 ;
	const Second anHour  = 60  * aMinute ;
	const Second aDay    = 24  * anHour  ;
	const Second aYear   = 365 * aDay    ;
	
	list<string> res ;
	
	if ( duration >= aYear )
	{
	
		Ceylan::Uint32 years = duration / aYear ;
		
		if ( years == 1 )
			res.push_back( "1 year" ) ;
		else	
			res.push_back( Ceylan::toString( duration / aYear ) + " years" ) ;
					
	}
	
	duration = duration % aYear ;
	
	
	if ( duration >= aDay )
	{
	
		Ceylan::Uint32 days = duration / aDay ;
		
		if ( days == 1 )
			res.push_back( "1 day" ) ;
		else	
			res.push_back( Ceylan::toString( duration / aDay ) + " days" ) ;
					
	}

	duration = duration % aDay ;
	
	
	if ( duration >= anHour )
	{
	
		Ceylan::Uint32 hours = duration / anHour ;
		
		if ( hours == 1 )
			res.push_back( "1 hour" ) ;
		else	
			res.push_back( Ceylan::toString( duration / anHour ) + " hours" ) ;
					
	}

	duration = duration % anHour ;
	
	
	if ( duration >= aMinute )
	{
	
		Ceylan::Uint32 minutes = duration / aMinute ;
		
		if ( minutes == 1 )
			res.push_back( "1 minute" ) ;
		else	
			res.push_back( 
				Ceylan::toString( duration / aMinute ) + " minutes" ) ;
					
	}

	duration = duration % aMinute ;
	
	
	if ( duration >= 1 )
	{
			
		if ( duration == 1 )
			res.push_back( "1 second" ) ;
		else	
			res.push_back( Ceylan::toString( duration ) + " seconds" ) ;
					
	}
	else if ( res.empty() )
		res.push_back( "0 second" ) ;
		
	return join( res, ", " ) ;
	
}


bool Timestamp::operator < (Timestamp & second ) throw() 
{

	/*
	 * Note that reverse comparisons (>) have to be done, otherwise
	 * 8:13:00 < 8:12:59 for example !
	 *
	 */
	if ( _year < second._year )
		return true ;
	
	if ( _year > second._year )
		return false ;
		
		
	if ( _month < second._month )
		return true ;
		
	if ( _month > second._month )
		return false ;

		
	if ( _day < second._day )
		return true ;
		
	if ( _day > second._day )
		return false ;

		
	if ( _hour < second._hour )
		return true ;
		
	if ( _hour > second._hour )
		return false ;
		
		
	if ( _minute < second._minute )
		return true ;
		
	if ( _minute > second._minute )
		return false ;
		
		
	if ( _second < second._second )
		return true ;
		
	if ( _second > second._second )
		return false ;
		
	return false ;
		
}

