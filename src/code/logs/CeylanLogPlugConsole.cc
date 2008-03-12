#include "CeylanLogPlugConsole.h"


#include "CeylanLogSource.h"                 // for LogSource
#include "CeylanLogTransportListenerRaw.h"   // for Log transports
#include "CeylanLogAggregatorConsole.h"      // for Log aggregators

#include "CeylanOperators.h"                 // for toString operator
#include "CeylanLogLight.h"                  // for CEYLAN_LOG




using std::string ;


using namespace Ceylan::Log ;



void LogPlugConsole::StartService( const string & plugInitiator,  
	bool immediateWrite, bool smart ) throw ( LogException )
{
	
	CEYLAN_LOG( "Starting LogPlug console service: "
		"creating aggregator and transport." ) ;
	
	// Plug should start empty:
 	LogPlug::CheckBlank() ;

	// Start by the end of the chain and go back to its beginning:
	
	/*
	 * Other StandardStream may be LogAggregatorConsole::Output or
	 * LogAggregatorConsole::Error.
	 *
	 */
	LogPlug::Aggregator = new LogAggregatorConsole( 
		/* StandardStream */             LogAggregatorConsole::Log,
		/* immediate Write */            immediateWrite,
		/* use Global Level Of Detail */ true, 
		/* be smart */                   smart ) ;
	
	/*
	 * Listener remains blank, since it is integrated with the 
	 * transport, with the console scheme.
	 *
	 */
	
	LogPlug::Transport = new LogTransportListenerRaw( * LogPlug::Aggregator ) ;	
	
	// Creates basic standard channels:
	LogPlug::CreateBasicPlug() ;
	
	// Last check before service is open:
	LogPlug::StartService( plugInitiator ) ;

}



void LogPlugConsole::StopService() throw()
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



const string LogPlugConsole::ToString( 
	Ceylan::VerbosityLevels level ) throw()
{

	string result = "LogSystem status: using console plug." ;
	
	if ( level != Ceylan::low )
		result += LogPlug::ToString( level ) ;
	
	return result ;

}



/* 
				
	Not even defined: 

LogPlugConsole::LogPlugConsole() throw( LogException )
{
	throw LogException( 
		"Ceylan::Log::LogPlugConsole should not be instanciated." ) ;
}


LogPlugConsole::~LogPlugConsole() throw()
{

}

*/

