#include "CeylanAnonymousStreamSocket.h"


#include "CeylanLogPlug.h"       // for LogPlug
#include "CeylanOperators.h"     // for toString

// for SystemSpecificSocketAddress :
#include "CeylanSystemSpecificSocketAddress.h"  


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
#include <arpa/inet.h>         // for htonl, sockaddr_in
#endif // CEYLAN_USES_ARPA_INET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for htonl, sockaddr_in
#endif // CEYLAN_USES_NETINET_IN_H

}


using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;



/*
 * Avoid ::htonl, use directy htonl since it is a macro on some platforms
 * (ex : NetBSD)
 *
 */



AnonymousStreamSocket::AnonymousStreamSocketException::AnonymousStreamSocketException( const std::string & reason ) throw():
	StreamSocketException( reason )
{

}


AnonymousStreamSocket::AnonymousStreamSocketException::~AnonymousStreamSocketException() throw()
{

}




AnonymousStreamSocket::AnonymousStreamSocket( 
		System::FileDescriptor listeningFD ) throw( SocketException ):
	StreamSocket()
{

#if CEYLAN_USES_NETWORK
		
	socklen_t addressSize = static_cast<socklen_t>( 
		sizeof( _address->_socketAddress ) ) ;

	LogPlug::debug( "AnonymousStreamSocket constructor : "
		"ready to accept a new connection." ) ;

	_originalFD = ::accept( listeningFD, 
		reinterpret_cast<sockaddr *>( & _address->_socketAddress ),
		& addressSize ) ;

	if ( _originalFD == -1 )
		throw AnonymousStreamSocketException(
			"AnonymousStreamSocket constructor failed : " 
			+ System::explainError() ) ;

	LogPlug::debug( "AnonymousStreamSocket constructor : "
		"new connection accepted." ) ;

#else // CEYLAN_USES_NETWORK

	throw AnonymousStreamSocketException( 
		"AnonymousStreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}


AnonymousStreamSocket::~AnonymousStreamSocket() throw()
{

	// StreamSocket takes care of everything needed.
	
}


bool AnonymousStreamSocket::isConnected() const throw()
{
	return true ;
}


const std::string AnonymousStreamSocket::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{

#if CEYLAN_USES_NETWORK

	string res ;
	
	
	return "AnonymousStreamSocket managing the connection-dedicated "
		"file descriptor " + Ceylan::toString( _originalFD ) ;

#else // CEYLAN_USES_NETWORK

	return "AnonymousStreamSocket (no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
						
