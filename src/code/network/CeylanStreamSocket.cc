#include "CeylanStreamSocket.h"


#include "CeylanLogPlug.h"     // for LogPlug

// for SystemSpecificSocketAddress :
#include "CeylanSystemSpecificSocketAddress.h"  



#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


// Not available in their C++ form :
extern "C"
{


#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>        // for socket
#endif // CEYLAN_USES_SYS_SOCKET_H


#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for htons
#endif // CEYLAN_USES_NETINET_IN_H


#ifdef CEYLAN_USES_RESOLV_H
#include <resolv.h>            // for sockaddr_in
#endif // CEYLAN_USES_RESOLV_H


#ifdef CEYLAN_USES_STRINGS_H
#include <strings.h>           // for AIX
#endif // CEYLAN_USES_STRINGS_H


#ifdef CEYLAN_USES_SYS_SELECT_H
#include <sys/select.h>        // for AIX
#endif // CEYLAN_USES_SYS_SELECT_H


}


using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;



StreamSocket::StreamSocketException::StreamSocketException( 
		const std::string & reason ) throw():
	SocketException( reason )
{

}


StreamSocket::StreamSocketException::~StreamSocketException() throw()
{

}



	
StreamSocket::StreamSocket() throw( Socket::SocketException ) :
	Socket()
{

#if CEYLAN_USES_NETWORK

	LogPlug::trace( "StreamSocket empty constructor" ) ;
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "StreamSocket empty constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


StreamSocket::StreamSocket( Port port ) throw( SocketException ):
	Socket( port )
{

#if CEYLAN_USES_NETWORK

	// It could not be called from Socket mother class :
	createSocket( _port ) ;

#else // CEYLAN_USES_NETWORK

	throw SocketException( "StreamSocket port-based constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


StreamSocket::~StreamSocket() throw()
{

	/*
	 * The Socket mother class destructor takes care of the closing of 
	 * file descriptor and related members (address).
	 *
	 */
	
}


void StreamSocket::createSocket( Port port ) throw( SocketException )
{

#if CEYLAN_USES_NETWORK

	_port = port ;
	
	
	_originalFD = ::socket( /* domain : IPv4 */ PF_INET, 
		/* type */ SOCK_STREAM,	/* protocol : TCP */ 0 ) ;
	
	if ( _originalFD == -1 )
		throw SocketException( "StreamSocket::createSocket failed : "
			+ System::explainError() ) ;
	
	LogPlug::debug( "StreamSocket::createSocket : socket created for "
		+ toString() ) ;

	// Blanks and initializes inherited address :
	_address->blank() ;
					
	_address->_socketAddress.sin_family = 
		/* Internet family, not UNIX */ AF_INET ;
		
	_address->_socketAddress.sin_port = htons( _port ) ;
	
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"StreamSocket::createSocket : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}





const std::string StreamSocket::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Stream" + Socket::toString( level ) ;
	
}	

