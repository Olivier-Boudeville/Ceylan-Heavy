#include "CeylanStreamSocket.h"


#include "CeylanLogPlug.h"


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



// Not available in their C++ form :
extern "C"
{


#ifdef CEYLAN_USES_SYS_TIME_H
//#include <sys/time.h>          // for 
#endif // CEYLAN_USES_SYS_TIME_H


#ifdef CEYLAN_USES_STRING_H
//#include <string.h>            // for 
#endif // CEYLAN_USES_STRING_H


#ifdef CEYLAN_USES_SYS_TYPES_H
//#include <sys/types.h>         // for 
#endif // CEYLAN_USES_SYS_TYPES_H


#ifdef CEYLAN_USES_SYS_SOCKET_H
//#include <sys/socket.h>        // for socket
#endif // CEYLAN_USES_SYS_SOCKET_H


#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for htons
#endif // CEYLAN_USES_NETINET_IN_H


#ifdef CEYLAN_USES_UNISTD_H
//#include <unistd.h>            // for FIXME
#endif // CEYLAN_USES_UNISTD_H


#ifdef CEYLAN_USES_RESOLV_H
#include <resolv.h>              // for sockaddr_in
#endif // CEYLAN_USES_RESOLV_H


#ifdef CEYLAN_USES_STRINGS_H
//#include <strings.h>           // for AIX
#endif // CEYLAN_USES_STRINGS_H


#ifdef CEYLAN_USES_SYS_SELECT_H
//#include <sys/select.h>        // for AIX
#endif // CEYLAN_USES_SYS_SELECT_H



}



using namespace Ceylan::System ;
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


#if CEYLAN_USES_NETWORK

// Avoid exposing system-dependent sockaddr_in in the headers :
struct Socket::SystemSpecificSocketAddress
{
	sockaddr_in _sockaddr ;	
	
} ;

#endif // CEYLAN_USES_NETWORK



	
StreamSocket::StreamSocket() throw( SocketException ) :
	Socket()
{

#if CEYLAN_USES_NETWORK

	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "StreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


StreamSocket::StreamSocket( Port port ) throw( SocketException ):
	Socket( port )
{

#if CEYLAN_USES_NETWORK

	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "StreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


StreamSocket::~StreamSocket() throw()
{

}




struct StreamSocket::SystemSpecificSocketAddress & Socket::getAddress()
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	return *_address ;
		
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"StreamSocket::getAddress : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


FileDescriptor StreamSocket::getSupplementaryFileDescriptor() const
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	return _fdes ;
		
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"StreamSocket::getSupplementaryFileDescriptor : "
		"network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


void StreamSocket::createSocket( Port port ) throw( SocketException )
{

#if CEYLAN_USES_NETWORK

	_port = port ;
	
	_fdes = ::socket( 
		/* domain : IPv4 */ PF_INET, /* type */ SOCK_STREAM, 
			/* protocol : TCP */ 0 ) ;
	
	if ( _fdes == -1 )
		throw SocketException( "StreamSocket::createSocket failed : "
			+ System::explainError() ) ;
				
	_address->_sockaddr.sin_family = AF_INET ;
	_address->_sockaddr.sin_port = ::htons( _port ) ;
	
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"StreamSocket::createSocket : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


Size StreamSocket::read( char * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#if CEYLAN_USES_NETWORK

	SignedSize n = System::FDRead( getFileDescriptor(), buffer, maxLength ) ;

	// Actually, n should never be negative :
	if ( n < 0 )
		throw ReadFailedException( "StreamSocket::read failed : " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;

#else // CEYLAN_USES_NETWORK	

	throw ReadFailed( "StreamSocket::read : network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	


}


Size StreamSocket::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	SignedSize n = FDWrite( getFileDescriptor(), message.c_str(), 
		message.size() ) ;

	if ( n < static_cast<SignedSize>( message.size() ) )
		throw WriteFailedException( "StreamSocket::write failed : " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;

#else // if CEYLAN_USES_NETWORK	
	
	throw WriteFailedException( 
		"StreamSocket::write failed : network support not available." ) ;
			
#endif // if CEYLAN_USES_NETWORK	

}


Size StreamSocket::write( const char * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	SignedSize n = System::FDWrite( getFileDescriptor(), buffer, maxLength ) ;

	if ( n < static_cast<SignedSize>( maxLength ) )
		throw WriteFailedException( "StreamSocket::write failed : " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;

#else // CEYLAN_USES_NETWORK	

	throw WriteFailedException( 
		"StreamSocket::write failed : network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	

}



bool StreamSocket::hasAvailableData() const throw()
{

#if CEYLAN_USES_NETWORK

	struct timeval tv ;
	tv.tv_sec  = 0 ;
	tv.tv_usec = 0 ;
	
	fd_set set ;
	FD_ZERO( & set ) ;
	FD_SET( getFileDescriptor(), & set ) ;
	
	Ceylan::Sint32 n = ::select( getFileDescriptor() + 1, 
		& set, 0, 0, & tv ) ;

	if ( n > 0 )	
		return FD_ISSET( getFileDescriptor(), & set ) ;

	if ( n == -1 )
		LogPlug::error( "StreamSocket::hasAvailableData failed : " 
			+ System::explainError() ) ;
	
	return false ;	

#else // CEYLAN_USES_NETWORK	

	LogPlug::error( "StreamSocket::hasAvailableData failed : "
		"network support not available." ) ; 
		
	return false ;
	
#endif // CEYLAN_USES_NETWORK	


}


void StreamSocket::clearInput() throw( InputStream::ReadFailedException )
{

	char c ;
	
	while ( hasAvailableData() ) 
		read( &c, 1 ) ;
		
}



