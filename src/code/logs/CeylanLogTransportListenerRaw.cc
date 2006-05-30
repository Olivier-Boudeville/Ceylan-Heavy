#include "CeylanLogTransportListenerRaw.h"


#include "CeylanLogMessage.h"
#include "CeylanLogAggregator.h"



using std::string ;


using namespace Ceylan::Log ;


LogTransportListenerRaw::LogTransportListenerRaw( LogAggregator & aggregator ) throw() :
	LogTransport(),
	LogListener( aggregator )
{

}


LogTransportListenerRaw::~LogTransportListenerRaw() throw()
{

}


void LogTransportListenerRaw::propagate( LogMessage & message ) throw( LogException )
{

	// Direct link through references, the communication bus is virtual !
	
	sendToAggregator( message ) ;

}


const string LogTransportListenerRaw::toString( Ceylan::VerbosityLevels level ) const throw()
{
	return "LogTransportListenerRaw : " + LogListener::toString( level ) ;
}
