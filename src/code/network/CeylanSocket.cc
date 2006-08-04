#include "CeylanSocket.h"

#include "CeylanLogPlug.h"                      // for LogPlug
#include "CeylanOperators.h"                    // for toString


// for SystemSpecificSocketAddress :
#include "CeylanSystemSpecificSocketAddress.h"  



#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



// Not available in their C++ form :
extern "C"
{


#ifdef CEYLAN_USES_SYS_TIME_H
#include <sys/time.h>          // for select, on OpenBSD 
#endif // CEYLAN_USES_SYS_TIME_H


#ifdef CEYLAN_USES_STRING_H
#include <string.h>            // for select, on OpenBSD
#endif // CEYLAN_USES_STRING_H


#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for select, on OpenBSD
#endif // CEYLAN_USES_SYS_TYPES_H


#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>        // for socket
#endif // CEYLAN_USES_SYS_SOCKET_H


#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for htons
#endif // CEYLAN_USES_NETINET_IN_H


#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>            // for timeval, select, on OpenBSD
#endif // CEYLAN_USES_UNISTD_H


#ifdef CEYLAN_USES_STRINGS_H
#include <strings.h>           // for AIX
#endif // CEYLAN_USES_STRINGS_H


#ifdef CEYLAN_USES_SYS_SELECT_H
#include <sys/select.h>        // for AIX
#endif // CEYLAN_USES_SYS_SELECT_H


}



using namespace Ceylan::Network ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


using std::string ;




Socket::SocketException::SocketException( const std::string & reason ) throw():
	SystemException( reason )
{

}


Socket::SocketException::~SocketException() throw()
{

}


	
// Is protected :
Socket::Socket() throw( Socket::SocketException ) :
	InputOutputStream(),
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
	InputOutputStream(),
	_fdes   ( 0 ),
	_port   ( port ),
	_address()
{

#if CEYLAN_USES_NETWORK

	_address = new SystemSpecificSocketAddress ;
	
	/*
	 * Cannot use here : 'createSocket( _port ) ;' since it would call
	 * Socket::createSocket in spite of any overloading.
	 * Therefore child classes (ex : CeylanStreamSocket) should call this
	 * method in their own constructor.
	 *
	 */
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "Socket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


Socket::~Socket() throw()
{

	// No destructor should throw exception :
	try
	{
		close() ;
	}
	catch( const Stream::CloseException	& e )
	{
		LogPlug::error( "Socket destructor failed : " + e.toString() ) ;
	}
	
	if ( _address != 0 )
		delete _address ;
	
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


SystemSpecificSocketAddress & Socket::getAddress()
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	return *_address ;
		
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"Socket::getAddress : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


void Socket::createSocket( Port port ) throw( SocketException )
{

	throw SocketException( "Socket::createSocket must be overriden "
		"by child classes." ) ;
		
}


Size Socket::read( char * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#if CEYLAN_USES_NETWORK

	return System::FDRead( getFileDescriptor(), buffer, maxLength ) ;

#else // CEYLAN_USES_NETWORK	

	throw ReadFailedException( 
		"Socket::read : network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	


}


Size Socket::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	Size n = FDWrite( getFileDescriptor(), message.c_str(), 
		message.size() ) ;

	if ( n < message.size() )
		throw WriteFailedException( "Socket::write (std::string) failed : " 
			+ System::explainError() ) ;

	return n ;

#else // if CEYLAN_USES_NETWORK	
	
	throw WriteFailedException( 
		"Socket::write failed : network support not available." ) ;
			
#endif // if CEYLAN_USES_NETWORK	

}


Size Socket::write( const char * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	Size n = System::FDWrite( getFileDescriptor(), buffer, maxLength ) ;

	if ( n < maxLength )
		throw WriteFailedException( "Socket::write (char *) failed : " 
			+ System::explainError() ) ;

	return n ;

#else // CEYLAN_USES_NETWORK	

	throw WriteFailedException( 
		"Socket::write failed : network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	

}



bool Socket::hasAvailableData() const throw()
{

#if CEYLAN_USES_NETWORK

	return System::HasAvailableData( getFileDescriptor() ) ;

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


const std::string Socket::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Socket associated to port " + Ceylan::toString( _port )
		+ " and to file descriptor " + Ceylan::toString( _fdes ) ;
	
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
