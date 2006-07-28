#include "CeylanSequentialServerStreamSocket.h"


#include "CeylanLogPlug.h"       // for LogPlug
#include "CeylanOperators.h"     // for toString
#include "CeylanThread.h"        // for Sleep


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{


#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for accept
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>        // for accept
#endif // CEYLAN_USES_SYS_SOCKET_H


#ifdef CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>         // for sockaddr_in
#endif // CEYLAN_USES_ARPA_INET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for sockaddr_in
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


/**
 * Avoid exposing system-dependent sockaddr_in in the headers :
 *
 * @note This definition had to be directly duplicated from
 * file CeylanStreamSocket.cc.
 *
 */
struct SequentialServerStreamSocket::SystemSpecificSocketAddress
{

	sockaddr_in _socketAddress ;
		
} ;


#endif // CEYLAN_USES_NETWORK



SequentialServerStreamSocket::SequentialServerStreamSocketException::SequentialServerStreamSocketException( const std::string & reason ) throw():
	ServerStreamSocketException( reason )
{

}


SequentialServerStreamSocket::SequentialServerStreamSocketException::~SequentialServerStreamSocketException() throw()
{

}



SequentialServerStreamSocket::SequentialServerStreamSocket( Port port, 
		bool reuse ) throw( SocketException ):
	ServerStreamSocket( port, reuse ),
	_acceptedFileDescriptor( 0 ),
	_clientAddress(0)
{

#if CEYLAN_USES_NETWORK

	_clientAddress = new SystemSpecificSocketAddress ;
	

#else // CEYLAN_USES_NETWORK

	throw SequentialServerStreamSocketException( 
		"SequentialServerStreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}


SequentialServerStreamSocket::~SequentialServerStreamSocket() throw()
{

#if CEYLAN_USES_NETWORK

	// The main listening socket is taken care of in mother classes.
	
	// No destructor should throw exception :
	try
	{
		closeAcceptedConnections() ;
	}
	catch( const Stream::CloseException	& e )
	{
		LogPlug::error( "SequentialServerStreamSocket destructor failed : " 
			+ e.toString() ) ;
	}

	delete _clientAddress ;
	
#endif // CEYLAN_USES_NETWORK
	
}


void SequentialServerStreamSocket::accept() 
	throw( ServerStreamSocketException )
{

#if CEYLAN_USES_NETWORK

	if ( ! _bound )
		prepareToAccept() ;
	
	socklen_t size = static_cast<socklen_t>( 
		sizeof( getAddress()._socketAddress ) ) ;
	
	LogPlug::debug( "SequentialServerStreamSocket::accept : "
		"ready to accept a new connection." ) ;
	
	/*
	 * Extracts the first connection request on the queue of pending
	 * connections, creates a new connected socket, and returns a new file
	 * descriptor referring to that socket.
	 *
	 * The original file descriptor (returned by getFileDescriptor) is
	 * unaffected by this call.
	 *
	 * @see man 2 accept
	 *
	 */
	_acceptedFileDescriptor = ::accept( getFileDescriptor(), 
		reinterpret_cast<sockaddr *>( & getAddress()._socketAddress ),
		& size ) ;
		
	if ( _acceptedFileDescriptor == -1 )
		throw SequentialServerStreamSocketException(
			"SequentialServerStreamSocket::accept failed : " 
			+ System::explainError() ) ;

	LogPlug::debug( "SequentialServerStreamSocket::accept : "
		"accept performed." ) ;

	accepted() ;
	
	cleanAfterAccept() ;

#endif // CEYLAN_USES_NETWORK	
			
}


FileDescriptor SequentialServerStreamSocket::getFileDescriptorForTransport()
	const throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	return _acceptedFileDescriptor ;
		
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"SequentialServerStreamSocket::getFileDescriptorForTransport : "
		"network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}



const std::string SequentialServerStreamSocket::toString(
	Ceylan::VerbosityLevels level ) const throw()
{

#if CEYLAN_USES_NETWORK

	string res = "SequentialServerStreamSocket whose file descriptor "
		"for accepted connection is " 
		+ Ceylan::toString( _acceptedFileDescriptor	) ;

	res += ". " + ServerStreamSocket::toString( level ) ;
	
	return res ;
	
#else // CEYLAN_USES_NETWORK

	return "ServerStreamSocket (no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
						

bool SequentialServerStreamSocket::closeAcceptedConnections() 
	throw( Stream::CloseException )
{

	Stream::Close( _acceptedFileDescriptor ) ;
	
	// _clientAddress will be reused as is (reassigned but not reallocated).	
	
	return true ;

}
	
					
