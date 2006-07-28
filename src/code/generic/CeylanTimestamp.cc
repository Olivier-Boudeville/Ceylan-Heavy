#include "CeylanTimestamp.h"


#include "CeylanSystem.h"      // for explainError
#include "CeylanOperators.h"


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H

#include <ctime>

using std::string ;

using namespace Ceylan ;



Timestamp::Timestamp() throw( UtilsException )
{

#if CEYLAN_USES_LOCALTIME

	time_t currentMeasuredTime = System::getTime() ; 
	struct tm * currentTime = ::localtime( & currentMeasuredTime ) ;
	
	if ( currentTime == 0 )
		throw UtilsException( "Ceylan::Timestamp::Timestamp() : "
			"unable to determine local time." ) ;

	_year   = currentTime->tm_year + 1900 ;
	_month  = currentTime->tm_mon + 1 ;
	_day    = currentTime->tm_mday ;
	_hour   = currentTime->tm_hour ;
	_minute = currentTime->tm_min ;
	_second = currentTime->tm_sec ;

#else // CEYLAN_USES_LOCALTIME

	throw UtilsException( "Timestamp constructor : "
		"::localtime function not available" ) ;
	
#endif // CEYLAN_USES_LOCALTIME
		
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

