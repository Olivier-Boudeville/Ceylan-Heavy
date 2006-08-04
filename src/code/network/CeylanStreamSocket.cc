#include "CeylanStreamSocket.h"


#include "CeylanLogPlug.h"

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
#include <resolv.h>              // for sockaddr_in
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

	throw SocketException( "StreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


StreamSocket::StreamSocket( Port port ) throw( SocketException ):
	Socket( port )
{

#if CEYLAN_USES_NETWORK

	createSocket( _port ) ;

#else // CEYLAN_USES_NETWORK

	throw SocketException( "StreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


StreamSocket::~StreamSocket() throw()
{

	/*
	 * The Socket mother class destructor takes care of the closing of _fdes.
	 * and related members (_address).
	 *
	 */
	
}


void StreamSocket::createSocket( Port port ) throw( SocketException )
{

#if CEYLAN_USES_NETWORK

	_port = port ;
	
	_fdes = ::socket( /* domain : IPv4 */ PF_INET, /* type */ SOCK_STREAM, 
			/* protocol : TCP */ 0 ) ;
	
	if ( _fdes == -1 )
		throw SocketException( "StreamSocket::createSocket failed : "
			+ System::explainError() ) ;
	
	LogPlug::debug( "StreamSocket::createSocket : socket created with "
		+ toString() ) ;

	// Blanks and initializes inherited _address :
	getAddress().blank() ;
					
	getAddress()._socketAddress.sin_family = 
		/* Internet family, not UNIX */ AF_INET ;
		
	getAddress()._socketAddress.sin_port = htons( _port ) ;
	
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"StreamSocket::createSocket : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


FileDescriptor StreamSocket::getFileDescriptorForTransport() const
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	// Default is to return the Socket-inherited file descriptor :
	return _fdes ;
	
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"StreamSocket::getFileDescriptorForTransport : "
		"network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


Size StreamSocket::read( char * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#if CEYLAN_USES_NETWORK

	// There are potentially two file descriptors with streams :
	
	Size n ;
	
	try
	{	
		n = System::FDRead( getFileDescriptorForTransport(), 
			buffer, maxLength ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		throw ReadFailedException( "StreamSocket::read failed : " 
			+ e.toString() ) ;
	}	

	return n ;

#else // CEYLAN_USES_NETWORK	

	throw ReadFailedException( 
		"StreamSocket::read : network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	


}


Size StreamSocket::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	// There are potentially two file descriptors with streams :
	
	Size n ;
	
	try
	{	
		n = FDWrite( getFileDescriptorForTransport(), 
			message.c_str(), message.size() ) ;
	}	
	catch( const Ceylan::Exception & e )
	{
		throw WriteFailedException( 
			"StreamSocket::write (std::string) failed : " + e.toString() ) ;
	}	


	if ( n < message.size() )
		throw WriteFailedException( 
			"StreamSocket::write (std::string) failed : "
			+ System::explainError() ) ;

	return n ;

#else // CEYLAN_USES_NETWORK	
	
	throw WriteFailedException( 
		"StreamSocket::write failed (std::string) : "
		"network support not available." ) ;
			
#endif // CEYLAN_USES_NETWORK	

}


Size StreamSocket::write( const char * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	// There are potentially two file descriptors with streams :
	
	Size n = System::FDWrite( getFileDescriptorForTransport(), 
		buffer, maxLength ) ;

	if ( n < maxLength )
		throw WriteFailedException( "StreamSocket::write (char *) failed : " 
			+ System::explainError() ) ;

	return n ;

#else // CEYLAN_USES_NETWORK	

	throw WriteFailedException( "StreamSocket::write (char *) failed : "
		"network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	

}


const std::string StreamSocket::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Stream" + Socket::toString( level ) ;
	
}	

