#include "CeylanSocket.h"


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
#include <sys/socket.h>        // for socket
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



using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;


#if CEYLAN_USES_NETWORK

/**
 * Avoid exposing system-dependent sockaddr_in in the headers :
 *
 * @note sockaddr_in is described in 'man '.
 *
 */
struct Socket::SystemSpecificSocketAddress
{
	sockaddr_in _socketAddress ;	
	
} ;

#endif // CEYLAN_USES_NETWORK



Socket::SocketException::SocketException( const std::string & reason ) throw():
	SystemException( reason )
{

}


Socket::SocketException::~SocketException() throw()
{

}


	
// Is protected :
Socket::Socket() throw( SocketException ) :
	_fdes   ( 0 ),
	_port   ( 0 ),
	_address( 0 )
{

#if CEYLAN_USES_NETWORK

	_address = new SystemSpecificSocketAddress ;
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "Socket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


Socket::Socket( Port port ) throw( SocketException ):
	_fdes   ( 0 ),
	_port   ( port ),
	_address()
{

#if CEYLAN_USES_NETWORK

	_address = new SystemSpecificSocketAddress ;
	createSocket( _port ) ;
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "Socket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


Socket::~Socket() throw()
{
	close() ;
}


FileDescriptor Socket::getFileDescriptor() const
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	return _fdes ;
	
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"Socket::getFileDescriptor : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


Port Socket::getPort() const throw()
{
	return _port ;
}


struct Socket::SystemSpecificSocketAddress & Socket::getAddress()
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	return *_address ;
		
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"Socket::getAddress : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


FileDescriptor Socket::getSupplementaryFileDescriptor() const
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	return _fdes ;
		
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"Socket::getSupplementaryFileDescriptor : "
		"network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


#if 0

void Socket::createSocket( Port port ) throw( SocketException )
{

#if CEYLAN_USES_NETWORK

	_port = port ;
	
	_fdes = ::socket( 
		/* domain : IPv4 */ PF_INET, /* type */ SOCK_STREAM, 
			/* protocol : TCP */ 0 ) ;
	
	if ( _fdes == -1 )
		throw SocketException( "Socket::createSocket failed : "
			+ System::explainError() ) ;
				
	_address->_socketAddress.sin_family = AF_INET ;
	_address->_socketAddress.sin_port = ::htons( _port ) ;
	
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"Socket::createSocket : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}
#endif


Size Socket::read( char * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#if CEYLAN_USES_NETWORK

	SignedSize n = System::FDRead( getFileDescriptor(), buffer, maxLength ) ;

	// Actually, n should never be negative :
	if ( n < 0 )
		throw ReadFailedException( "Socket::read failed : " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;

#else // CEYLAN_USES_NETWORK	

	throw ReadFailed( "Socket::read : network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	


}


Size Socket::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	SignedSize n = FDWrite( getFileDescriptor(), message.c_str(), 
		message.size() ) ;

	if ( n < static_cast<SignedSize>( message.size() ) )
		throw WriteFailedException( "Socket::write failed : " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;

#else // if CEYLAN_USES_NETWORK	
	
	throw WriteFailedException( 
		"Socket::write failed : network support not available." ) ;
			
#endif // if CEYLAN_USES_NETWORK	

}


Size Socket::write( const char * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	SignedSize n = System::FDWrite( getFileDescriptor(), buffer, maxLength ) ;

	if ( n < static_cast<SignedSize>( maxLength ) )
		throw WriteFailedException( "Socket::write failed : " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;

#else // CEYLAN_USES_NETWORK	

	throw WriteFailedException( 
		"Socket::write failed : network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	

}



bool Socket::hasAvailableData() const throw()
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
		LogPlug::error( "Socket::hasAvailableData failed : " 
			+ System::explainError() ) ;
	
	return false ;	

#else // CEYLAN_USES_NETWORK	

	LogPlug::error( "Socket::hasAvailableData failed : "
		"network support not available." ) ; 
		
	return false ;
	
#endif // CEYLAN_USES_NETWORK	


}


void Socket::clearInput() throw( InputStream::ReadFailedException )
{

	char c ;
	
	while ( hasAvailableData() ) 
		read( &c, 1 ) ;
		
}


StreamID Socket::getInputStreamID() const throw()
{
	return static_cast<StreamID>( getFileDescriptor() ) ;
}


StreamID Socket::getOutputStreamID() const throw()
{
	return static_cast<StreamID>( getFileDescriptor() ) ;
}


bool Socket::close() throw( Stream::CloseException )
{

#if CEYLAN_USES_NETWORK

	return Stream::Close( _fdes ) ;

#else // CEYLAN_USES_NETWORK	

	LogPlug::error( "Socket::close failed : "
		"network support not available." ) ; 
		
	return false ;
	
#endif // CEYLAN_USES_NETWORK	

}
