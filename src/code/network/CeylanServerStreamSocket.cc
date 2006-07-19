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


/*
 * Avoid ::htonl, use directy htonl since it is a macro on some platforms
 * (ex : NetBSD)
 *
 */



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


/**
 * Avoid exposing system-dependent sockaddr_in in the headers :
 *
 * @note This definition had to be directly duplicated from
 * file CeylanStreamSocket.cc.
 *
 */
struct ServerStreamSocket::SystemSpecificSocketAddress
{
	sockaddr_in _socketAddress ;	
	
} ;

#endif // CEYLAN_USES_NETWORK



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
	_acceptedFileDescriptor( 0 ),
	_clientAddress(0),
	_bound( false ),
	_maximumPendingConnectionsCount( DefaultMaximumPendingConnectionsCount )
{

#if CEYLAN_USES_NETWORK

	_clientAddress = new SystemSpecificSocketAddress ;
	
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
					"ServerStreamSocketException constructor : "
					"could not set reuse option on listening socket : "
					+ System::explainError() ) ;
	}

#else // CEYLAN_USES_NETWORK

	throw ServerStreamSocketException( 
		"ServerStreamSocketException constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}


ServerStreamSocket::~ServerStreamSocket() throw()
{
	// No destructor should throw exception :
	try
	{
		Stream::Close( _acceptedFileDescriptor ) ;
	}
	catch( const Stream::CloseException	& e )
	{
		LogPlug::error( "ServerStreamSocket destructor failed : " 
			+ e.toString() ) ;
	}

	delete _clientAddress ;
	
}




void ServerStreamSocket::accept() throw( ServerStreamSocketException )
{

	prepareToAccept() ;
	
	socklen_t size = static_cast<socklen_t>( 
		sizeof( getAddress()._socketAddress ) ) ;
	
	LogPlug::debug( "ServerStreamSocket::accept : ready to accept." ) ;
	
	/*
	 * Extracts the first connection request on the queue of pending
	 * connections, creates a new connected socket, and returns a new file
	 * descriptor referring to that socket :
	 *
	 * The original file descriptor (returned by getFileDescriptor) is
	 * unaffected by this call.
	 */
	_acceptedFileDescriptor = ::accept( getFileDescriptor(), 
		reinterpret_cast<sockaddr *>( & getAddress()._socketAddress ),
		& size ) ;
		
	if ( _acceptedFileDescriptor == -1 )
		throw ServerStreamSocketException(
			"ServerStreamSocket::accept failed : " + System::explainError() ) ;

	LogPlug::debug( "ServerStreamSocket::accept : accept performed." ) ;

	accepted() ;
			
}


FileDescriptor ServerStreamSocket::getFileDescriptorForTransport() const
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	return _acceptedFileDescriptor ;
		
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"ServerStreamSocket::getFileDescriptorForTransport : "
		"network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

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

	string res ;
	
	
	// @todo : add client IP
	
	if ( _bound )
		res = "ServerStreamSocket linked to client" ;
	else		
		res = "ServerStreamSocket not linked to any client" ;

	if ( level == Ceylan::low )
		return res ;
	
	res += ". File descriptor for accepted connection is " 
		+ Ceylan::toString( _acceptedFileDescriptor	) ;
		
	res += ". The current maximum number of pending connections "
		"for this socket is " 
		+ Ceylan::toString( getMaximumPendingConnectionsCount() ) ;
	
	if	( level == Ceylan::medium )
		return res ;
	
	return res + ". " + StreamSocket::toString( level ) ;
	
}	
						

void ServerStreamSocket::prepareToAccept() throw( ServerStreamSocketException )
{

	LogPlug::trace( "Entering ServerStreamSocket::prepareToAccept" ) ;
	
	if ( _bound )
		throw ServerStreamSocketException( 
			"ServerStreamSocket::prepareToAccept : socket already bound" ) ;
	
	getAddress()._socketAddress.sin_addr.s_addr = 
		htonl( /* Address to accept any incoming message */ INADDR_ANY ) ;
 
 	Ceylan::Uint8 bindAttemptCount = 0 ;
	const Ceylan::Uint8 maxBindAttemptCount = 5 ;
		
	for ( ; bindAttemptCount < maxBindAttemptCount; bindAttemptCount++ )
	{

		LogPlug::debug( "ServerStreamSocket::prepareToAccept : bind attempt #"
			+ Ceylan::toString( bindAttemptCount + 1 ) ) ;
		
		if ( ::bind( getFileDescriptor(), 
				reinterpret_cast<sockaddr *>( & getAddress()._socketAddress ),
				sizeof( sockaddr_in ) ) == 0 ) 
			break ;
		
		Thread::Sleep( 1 /* second */ ) ;
		
	}
		
	if ( bindAttemptCount == maxBindAttemptCount )
		throw ServerStreamSocketException(
			"ServerStreamSocket::prepareToAccept : bind attempts failed : "
			+ System::explainError() ) ;
					
	LogPlug::debug( "ServerStreamSocket::prepareToAccept : bind succeeded." ) ;
	
	if ( ::listen( getFileDescriptor(), _maximumPendingConnectionsCount ) != 0 )
		throw ServerStreamSocketException(
			"ServerStreamSocket::prepareToAccept : listen failed : "
			+ System::explainError() ) ;

	LogPlug::debug( "ServerStreamSocket::prepareToAccept : "
		"listen succeeded." ) ;
 
 	_bound = true ;
	
}

					
void ServerStreamSocket::accepted() throw( ServerStreamSocketException )
{

	// Empty implementation made to be overriden.
	LogPlug::debug( "ServerStreamSocket::accepted : "
		"connection up and running." ) ;
	
}
					
