#include "CeylanLogListener.h"


#include "CeylanLogMessage.h"      // for LogMessage
#include "CeylanLogAggregator.h"   // for Aggregator

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"          // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H



using std::string ;

using namespace Ceylan::Log ;



LogListener::LogListener( LogAggregator & aggregator ) throw():
		_aggregator( & aggregator )
{

}



LogListener::~LogListener() throw()
{

}



const string LogListener::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{
	
	if ( _aggregator == 0 )
		return "This LogListener is not linked with any aggregator" ;		
		
	// An aggregator is available:
	
	if ( level == Ceylan::high )
		return "This LogListener is linked with following aggregator: "
			+ _aggregator->toString( Ceylan::low ) ;
	else
		return "This LogListener is linked with an aggregator" ;

}



void LogListener::sendToAggregator( LogMessage & message ) 
	const throw( LogException )
{

#if CEYLAN_DEBUG
	
	if ( ! _aggregator )
		throw LogException( "LogListener::sendToAggregator: "
			"trying to send a message whereas "
			"the internal aggregator has not been initialized." ) ;
			
#endif // CEYLAN_DEBUG

	_aggregator->store( message ) ;
	
}

