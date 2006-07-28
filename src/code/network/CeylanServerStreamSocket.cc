#include "CeylanServerStreamSocket.h"


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



#if CEYLAN_USES_NETWORK


/**
 * Avoid exposing system-dependent sockaddr_in in the headers :
 *
 * @note This definition had to be directly duplicated from
 * file CeylanSocket.cc.
 *
 */
struct Socket::SystemSpecificSocketAddress
{
	   sockaddr_in _socketAddress ;

} ;


#endif // CEYLAN_USES_NETWORK



/*
 * Avoid ::htonl, use directy htonl since it is a macro on some platforms
 * (ex : NetBSD)
 *
 */




ServerStreamSocket::ServerStreamSocketException::ServerStreamSocketException( 
		const std::string & reason ) throw():
	StreamSocketException( reason )
{

}


ServerStreamSocket::ServerStreamSocketException::~ServerStreamSocketException()
	throw()
{

}



ServerStreamSocket::ServerStreamSocket( Port port, bool reuse )
		throw( SocketException ):
	StreamSocket( port ),
	_bound( false ),
	_stopRequested( false ),
	_maximumPendingConnectionsCount( DefaultMaximumPendingConnectionsCount )
{

#if CEYLAN_USES_NETWORK
	
	if ( reuse )
	{
	
		// Reuse option set to non-zero to enable option :
		int reuseOption = 1 ;
		
		// See : man 7 socket
		if ( ::setsockopt( getFileDescriptor(), 
			/* socket level */ SOL_SOCKET, 
			/* option name */ SO_REUSEADDR, 
			/* option value buffer */ reinterpret_cast<char *>( &reuseOption ), 
			/* option buffer length */ sizeof( reuseOption ) ) != 0 )
				throw ServerStreamSocketException( 
					"ServerStreamSocket constructor : "
					"could not set reuse option on listening socket : "
					+ System::explainError() ) ;
	}

#else // CEYLAN_USES_NETWORK

	throw ServerStreamSocketException( 
		"ServerStreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}


ServerStreamSocket::~ServerStreamSocket() throw()
{

	// StreamSocket takes care of everything needed.
}


void ServerStreamSocket::run() throw( ServerStreamSocketException )
{

	LogPlug::trace( "Entering in ServerStreamSocket::run" ) ;
	
	Ceylan::Uint32 connectionCount = 0 ;
	
	while ( ! isRequestedToStop() )
	{	
		connectionCount++ ;
		
		LogPlug::info( "ServerStreamSocket::run : waiting for connection #" 
			+ Ceylan::toString( connectionCount ) ) ;
		accept() ;
		
	}
	
	LogPlug::trace( "Exiting from ServerStreamSocket::run" ) ;

}


ServerStreamSocket::ConnectionCount
		ServerStreamSocket::getMaximumPendingConnectionsCount()
	const throw()
{

	return _maximumPendingConnectionsCount ;
	
}	
					

void ServerStreamSocket::setMaximumPendingConnectionsCount( 
	ConnectionCount newMax ) throw()
{
	_maximumPendingConnectionsCount = newMax ;
}	


const std::string ServerStreamSocket::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

#if CEYLAN_USES_NETWORK

	string res ;
	
	
	// @todo : add client IP
	
	if ( _bound )
		res = "ServerStreamSocket bound and listening for new connections" ;
	else		
		res = "ServerStreamSocket not ready to accept new connections" ;

	res += ". " ;
	
	if ( _stopRequested )
		res = "ServerStreamSocket is requested to stop" ;
	else		
		res = "ServerStreamSocket is not requested to stop" ;

	if ( level == Ceylan::low )
		return res ;
			
	res += ". The current maximum number of pending connections "
		"for this socket is " 
		+ Ceylan::toString( getMaximumPendingConnectionsCount() ) ;
	
	if	( level == Ceylan::medium )
		return res ;
	
	return res + ". " + StreamSocket::toString( level ) ;

#else // CEYLAN_USES_NETWORK

	return "ServerStreamSocket (no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
						

void ServerStreamSocket::prepareToAccept() throw( ServerStreamSocketException )
{


#if CEYLAN_USES_NETWORK


	LogPlug::trace( "Entering ServerStreamSocket::prepareToAccept" ) ;

	if ( _bound )
		throw ServerStreamSocketException(
			"ServerStreamSocket::prepareToAccept : socket already bound" ) ;
				
	getAddress()._socketAddress.sin_addr.s_addr = htonl( 
		/* Address to accept any incoming connection */ INADDR_ANY ) ;
 
 	Ceylan::Uint8 bindAttemptCount = 0 ;
	const Ceylan::Uint8 maxBindAttemptCount = 5 ;
	
	for ( ; bindAttemptCount < maxBindAttemptCount; bindAttemptCount++ )
	{

		LogPlug::debug( "ServerStreamSocket::prepareToAccept : "
			"bind attempt #" + Ceylan::toString( bindAttemptCount + 1 ) ) ;
	
		if ( ::bind( getFileDescriptor(), 
				reinterpret_cast<sockaddr *>( 
					& getAddress()._socketAddress ),
				sizeof( sockaddr_in ) ) == 0 ) 
			break ;
	
		Thread::Sleep( 1 /* second */ ) ;
	
	}
	
	
	if ( bindAttemptCount == maxBindAttemptCount )
		throw ServerStreamSocketException(
			"ServerStreamSocket::prepareToAccept : bind attempts failed : "
			+ System::explainError() ) ;
				
	LogPlug::debug( "ServerStreamSocket::prepareToAccept : "
		"bind succeeded." ) ;

 	_bound = true ;
	
	
	if ( ::listen( getFileDescriptor(), _maximumPendingConnectionsCount ) != 0 )
		throw ServerStreamSocketException(
			"ServerStreamSocket::prepareToAccept : listen failed : "
			+ System::explainError() ) ;

	LogPlug::debug( "ServerStreamSocket::prepareToAccept : "
		"listen succeeded." ) ;
 


	/*
	 * From this moment, a socket dedicated to this connection is created,
	 * it is the one that will be used for example by read operations in
	 * the 'accepted( method, thanks to the getFileDescriptorForTransport
	 * method.
	 *
	 * @see StreamSocket::read
	 *
	 */

#else // CEYLAN_USES_NETWORK

	throw ServerStreamSocketException( "ServerStreamSocket::prepareToAccept : "
		"no network support available." ) ;
	
#endif // CEYLAN_USES_NETWORK	
	
}


void ServerStreamSocket::cleanAfterAccept() throw( ServerStreamSocketException )
{


#if CEYLAN_USES_NETWORK


	LogPlug::trace( "Entering ServerStreamSocket::cleanAfterAccept" ) ;
	
	closeAcceptedConnections() ;
	
#else // CEYLAN_USES_NETWORK

	throw ServerStreamSocketException( "ServerStreamSocket::cleanAfterAccept : "
		"no network support available." ) ;
	
#endif // CEYLAN_USES_NETWORK	
	
}



					
void ServerStreamSocket::accepted() throw( ServerStreamSocketException )
{

	// Empty implementation made to be overriden.
	LogPlug::debug( "ServerStreamSocket::accepted : "
		"connection up and running." ) ;
		
}


bool ServerStreamSocket::isRequestedToStop() const throw()
{
	return _stopRequested ;
}			


void ServerStreamSocket::requestToStop() throw()
{

	LogPlug::trace( "ServerStreamSocket::requestToStop" ) ;
	_stopRequested = true ;
	
}			
