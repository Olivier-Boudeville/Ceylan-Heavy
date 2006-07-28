#include "CeylanMultiplexedServerStreamSocket.h"


#include "CeylanLogPlug.h"       // for LogPlug
#include "CeylanOperators.h"     // for toString
#include "CeylanThread.h"        // for Sleep


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#ifdef CEYLAN_USES_UNISTD_H
//#include <unistd.h>            // for FIXME
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for setsockopt, bind, accept
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>        // for setsockopt, bind, listen, accept
#endif // CEYLAN_USES_SYS_SOCKET_H

#ifdef CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>         // for htonl
#endif // CEYLAN_USES_ARPA_INET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for htonl
#endif // CEYLAN_USES_NETINET_IN_H

}


using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;






MultiplexedServerStreamSocket::MultiplexedServerStreamSocketException::MultiplexedServerStreamSocketException( const std::string & reason ) throw():
	StreamSocketException( reason )
{

}


MultiplexedServerStreamSocket::MultiplexedServerStreamSocketException::~MultiplexedServerStreamSocketException()
	throw()
{

}



MultiplexedServerStreamSocket::MultiplexedServerStreamSocket( 
	Port port, bool reuse )
		throw( SocketException ):
	ServerStreamSocket( port, reuse )
{

#if CEYLAN_USES_NETWORK

#else // CEYLAN_USES_NETWORK

	throw MultiplexedServerStreamSocketException( 
		"MultiplexedServerStreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}


MultiplexedServerStreamSocket::~MultiplexedServerStreamSocket() throw()
{

#if CEYLAN_USES_NETWORK

	
#endif // CEYLAN_USES_NETWORK
	
}




void MultiplexedServerStreamSocket::accept() 
	throw( ServerStreamSocketException )
{

#if CEYLAN_USES_NETWORK


#endif // CEYLAN_USES_NETWORK	
			
}



const std::string MultiplexedServerStreamSocket::toString(
	Ceylan::VerbosityLevels level ) const throw()
{

#if CEYLAN_USES_NETWORK

	string res = "MultiplexedServerStreamSocket" ;
	
	
	if	( level == Ceylan::medium )
		return res ;
	
	return res + ". " + StreamSocket::toString( level ) ;

#else // CEYLAN_USES_NETWORK

	return "MultiplexedServerStreamSocket (no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
						
