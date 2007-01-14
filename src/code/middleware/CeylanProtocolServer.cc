#include "CeylanProtocolServer.h"

#include "CeylanLogPlug.h"     // for LogPlug


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Middleware ;



ProtocolServer::ProtocolServer( Marshaller & marshaller ) throw() :
	ProtocolEndpoint( marshaller ),
	_shutdownRequested( false )
{

}


ProtocolServer::~ProtocolServer() throw()
{

}


bool ProtocolServer::isShutdownRequested() const throw()
{

	return _shutdownRequested ;
	
}


const string ProtocolServer::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "Protocol server, which is a " 
		+ ProtocolEndpoint::toString( level ) ;
	
	if ( isShutdownRequested() )
		res += ". This protocol server requests the underlying medium "
			"to stop once the current protocol-based exchange is over" ;
	else
		res += ". This protocol server does not request the underlying medium "
			"to stop once the current protocol-based exchange is over" ;
				
	return res ;
	
}


void ProtocolServer::askForShutdown() throw()
{

	_shutdownRequested = true ;
	
}

