#include "CeylanLogPlugNull.h"


#include "CeylanLogSource.h"                 // for LogSource
#include "CeylanLogTransportListenerRaw.h"   // for Log transports
#include "CeylanLogAggregatorRaw.h"          // for Log aggregators

#include "CeylanOperators.h"                 // for toString operator
#include "CeylanLogLight.h"                  // for CEYLAN_LOG




using std::string ;


using namespace Ceylan::Log ;


void LogPlugNull::StartService() throw ( LogException )
{
	
	CEYLAN_LOG( "Starting LogPlug null service: "
		"creating aggregator and transport." ) ;
	
	// Plug should start empty:
 	LogPlug::CheckBlank() ;
    
	LogPlug::CreateNullPlug() ;
	
	// Last check before service is open:
	LogPlug::StartService( "(null plug)" ) ;

}



void LogPlugNull::StopService() throw()
{
	
	LogPlug::StopService() ;
		
}



const string LogPlugNull::ToString( 
	Ceylan::VerbosityLevels level ) throw()
{

	string result = "LogSystem status: using null plug." ;
	
	if ( level != Ceylan::low )
		result += LogPlug::ToString( level ) ;
	
	return result ;

}

