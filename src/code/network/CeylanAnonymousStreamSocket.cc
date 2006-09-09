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


#include <cerrno>  // for EAGAIN


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



AnonymousStreamSocket::NonBlockingAcceptException::NonBlockingAcceptException( const std::string & reason ) throw():
	AnonymousStreamSocketException( reason )
{

}


AnonymousStreamSocket::NonBlockingAcceptException::~NonBlockingAcceptException() throw()
{

}



AnonymousStreamSocket::AnonymousStreamSocket( 
			System::FileDescriptor listeningFD, bool blocking ) 
		throw( SocketException ):
	StreamSocket( /* blocking */ true )
{

#if CEYLAN_USES_NETWORK
	

	LogPlug::debug( "AnonymousStreamSocket constructor : "
		"ready to accept a new connection using listening file descriptor " 
		+ Ceylan::toString( listeningFD ) + "." ) ;
		 
	socklen_t addressSize = static_cast<socklen_t>( 
		sizeof( _address->_socketAddress ) ) ;

	/*
	 * A time-out could be added, in case the connection request was cancelled
	 * after the select but before this accept, if ever blocking sockets 
	 * were chosen (non-blocking are to be preferred anyway) :
	 *
	 */
	_originalFD = ::accept( listeningFD, 
		reinterpret_cast<sockaddr *>( & _address->_socketAddress ),
		& addressSize ) ;

	if ( _originalFD == -1 )
	{
	
		if ( System::getError() == EAGAIN )
		{
			
			/*
			 * With non-blocking sockets, accept with no available connection
			 * returns EAGAIN :
			 *
			 */
			throw NonBlockingAcceptException( 
				"AnonymousStreamSocket constructor : "
				"no available connection found." ) ;
				
		}
		else
		{
		
			throw AnonymousStreamSocketException(
				"AnonymousStreamSocket constructor failed : " 
				+ System::explainError() ) ;
		}
				
	}
	
	
	LogPlug::debug( "AnonymousStreamSocket constructor : "
		"new connection accepted." ) ;

	// This new file descriptor comes blocking :
	if ( blocking == false )
		setBlocking( false ) ;
		
		
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

	return "AnonymousStreamSocket managing the connection-dedicated "
		"file descriptor " + Ceylan::toString( _originalFD ) ;
		
#else // CEYLAN_USES_NETWORK

	return "AnonymousStreamSocket (no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
						
