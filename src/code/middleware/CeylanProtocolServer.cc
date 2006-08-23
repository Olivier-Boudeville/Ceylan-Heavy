#include "CeylanProtocolServer.h"

#include "CeylanLogPlug.h"     // for LogPlug


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Middleware ;


ProtocolServer::ProtocolServer( System::InputOutputStream & stream,
	Marshaller & marshaller ) throw() :
	ProtocolEndpoint( stream, marshaller )
{

}


ProtocolServer::~ProtocolServer() throw()
{

}


const string ProtocolServer::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Protocol server, which is a " 
		+ ProtocolEndpoint::toString( level ) ;
	
}

