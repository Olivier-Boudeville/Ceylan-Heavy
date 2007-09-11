#include "CeylanLogPlugHTML.h"

#include "CeylanLogSource.h"
#include "CeylanLogTransportListenerRaw.h"
#include "CeylanLogAggregatorHTML.h"


#include "CeylanOperators.h"
#include "CeylanLogLight.h"



using std::string ;


using namespace Ceylan::Log ;


const string & LogPlugHTML::LogDirectorySuffix = "-logs" ;


void LogPlugHTML::StartService( const string & plugInitiator, bool smart ) 
	throw ( LogException )
{
		
	CEYLAN_LOG( "Starting LogPlug HTML service: "
		"creating aggregator and transport." ) ;
	
	// Plug should start empty:
 	LogPlug::CheckBlank() ;

	// Start by the end of the chain and go back to its beginning:
	
	LogPlug::Aggregator = new LogAggregatorHTML( 
		/* caller description */         plugInitiator,
		/* log ouput directory */        plugInitiator + LogDirectorySuffix,
		/* use Global Level Of Detail */ true, 
		/* be smart */                   smart ) ;
	
	
	/* 
	 * Listener remains blank, since it is integrated with the transport,
	 * with the HTML scheme.
	 *
	 */
	
	LogPlug::Transport = 
		new LogTransportListenerRaw( * LogPlug::Aggregator ) ;
	
	// Creates basic standard channels:
	LogPlug::CreateBasicPlug() ;
	
	// Last check before service is open:
	LogPlug::StartService( plugInitiator ) ;

}


void LogPlugHTML::StopService() throw()
{

	LogPlug::StopService() ;

	CEYLAN_LOG( "Stopping transport and listener." ) ;
	delete LogPlug::Transport ; 
	LogPlug::Transport = 0 ;
	// listener is embedded in transport.
	
	CEYLAN_LOG( "Stopping aggregator." ) ;
	delete LogPlug::Aggregator ; 
	LogPlug::Aggregator = 0 ;
	
}


const string LogPlugHTML::ToString( Ceylan::VerbosityLevels level ) throw()
{

	string result = "LogSystem status: using HTML plug" ;
	
	if ( level != Ceylan::low )
		result += ". " + LogPlug::ToString( level ) ;
	
	return result ;

}



/* 
				
	Not even defined: 


LogPlugHTML::LogPlugHTML() throw( LogException )
{
	throw LogException( 
		"Ceylan::Log::LogPlugHTML should not be instanciated." ) ;
}


LogPlugHTML::~LogPlugHTML() throw()
{

}

*/
