#include "CeylanAnonymousProtocolAwareStreamSocket.h"


#include "CeylanLogPlug.h"        // for LogPlug
#include "CeylanOperators.h"      // for toString
#include "CeylanProtocolServer.h" // for ProtocolServer



using namespace Ceylan::Middleware ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;





AnonymousProtocolAwareStreamSocket::AnonymousProtocolAwareStreamSocket( 
		System::FileDescriptor listeningFD, 
		ProtocolServer & protocolServerToTrigger ) throw( SocketException ):
	AnonymousStreamSocket( listeningFD ),
	_protocolServer( & protocolServerToTrigger )
{
	
}


AnonymousProtocolAwareStreamSocket::AnonymousProtocolAwareStreamSocket( 
		System::FileDescriptor listeningFD ) throw( SocketException ):
	AnonymousStreamSocket( listeningFD ),
	_protocolServer( 0 )
{
	
}


AnonymousProtocolAwareStreamSocket::~AnonymousProtocolAwareStreamSocket()
	throw()
{

	if ( _protocolServer != 0 )
		delete _protocolServer ;
	
}


bool AnonymousProtocolAwareStreamSocket::hasProtocolServer() const throw()
{

	return _protocolServer != 0 ;
	
}


ProtocolServer & AnonymousProtocolAwareStreamSocket::getProtocolServer()
	throw( AnonymousStreamSocketException )
{

	if ( _protocolServer == 0 )
		throw AnonymousStreamSocketException(
			"AnonymousProtocolAwareStreamSocket::getProtocolServer : "
			"no available protocol server." ) ;
			
	return * _protocolServer ;
	
}


void AnonymousProtocolAwareStreamSocket::setProtocolServer( 
	ProtocolServer & newProtocolServer ) throw()
{

	if (  _protocolServer != 0 )
		delete _protocolServer ;
		
	_protocolServer	= & newProtocolServer ;
	
}

	
const std::string AnonymousProtocolAwareStreamSocket::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{
		
	string res= "AnonymousProtocolAwareStreamSocket " ;
	
	if ( _protocolServer != 0 )
		res += "associated with " + _protocolServer->toString( level ) ;
	else	
		res += "not associated with any protocol server" ;
		
	res += ". It is an " + AnonymousStreamSocket::toString( level ) ;
	
	return res ;
	
}	
						
