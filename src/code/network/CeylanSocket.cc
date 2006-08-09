#include "CeylanSocket.h"

#include "CeylanLogPlug.h"                      // for LogPlug
#include "CeylanOperators.h"                    // for toString
#include "CeylanAnonymousConnection.h"          // for AnonymousConnection


// for SystemSpecificSocketAddress :
#include "CeylanSystemSpecificSocketAddress.h"  



#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



// Not available in their C++ form :
extern "C"
{


#ifdef CEYLAN_USES_SYS_TIME_H
//#include <sys/time.h>          // for select, on OpenBSD 
#endif // CEYLAN_USES_SYS_TIME_H


#ifdef CEYLAN_USES_STRING_H
//#include <string.h>            // for select, on OpenBSD
#endif // CEYLAN_USES_STRING_H


#ifdef CEYLAN_USES_SYS_TYPES_H
//#include <sys/types.h>         // for select, on OpenBSD
#endif // CEYLAN_USES_SYS_TYPES_H


#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>        // for socket
#endif // CEYLAN_USES_SYS_SOCKET_H


#ifdef CEYLAN_USES_UNISTD_H
//#include <unistd.h>            // for timeval, select, on OpenBSD
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
	_localPort( 0 ),
	_peerPort( 0 ),
	_address( 0 ),
	_originalFD( 0 )
{

#if CEYLAN_USES_NETWORK
	
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "Socket empty constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


Socket::Socket( Port localPort ) throw( SocketException ):
	InputOutputStream(),
	_localPort( localPort ) ,
	_peerPort( 0 ),
	_address( 0 ),
	_originalFD( 0 )
{

#if CEYLAN_USES_NETWORK
	
	/*
	 * Cannot use here : 'createSocket( _localPort ) ;' since it would call
	 * Socket::createSocket in spite of any overloading.
	 *
	 * Therefore child classes (ex : CeylanStreamSocket) should call this
	 * method in their own constructor.
	 *
	 */
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "Socket port-based constructor failed : "
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
		
}




Size Socket::read( char * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#if CEYLAN_USES_NETWORK

	try
	{
	
		// FDRead can throw IOException and FeatureNotAvailableException :
		return System::FDRead( getFileDescriptorForTransport(), 
			buffer, maxLength ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		throw ReadFailedException( "Socket::read failed : " + e.toString() ) ;
	}	

#else // CEYLAN_USES_NETWORK	

	throw ReadFailedException( 
		"Socket::read failed : network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	


}


Size Socket::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	Size n ; 
	
	StringSize messageSize =  message.size() ;
	
	try
	{
		// FDWrite can throw IOException and FeatureNotAvailableException :
		n = System::FDWrite( getFileDescriptorForTransport(), 
			message.c_str(), messageSize ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		throw WriteFailedException( "Socket::write (std::string) failed : " 
			+ e.toString() ) ;
	}	

	// Actually if this method returns a value, it is messageSize :
	
	if ( n < messageSize )
		throw WriteFailedException( "Socket::write (std::string) failed : " 
			+ System::explainError() ) ;

	return n ;

#else // if CEYLAN_USES_NETWORK	
	
	throw WriteFailedException( "Socket::write (std::string) failed : "
		"network support not available." ) ;
			
#endif // if CEYLAN_USES_NETWORK	

}


Size Socket::write( const char * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_NETWORK

	Size n ; 
	
	try
	{
		n = System::FDWrite( getFileDescriptorForTransport(), 
			buffer, maxLength ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		throw WriteFailedException( "Socket::write (char *) failed : " 
			+ e.toString() ) ;
	}	

	// Actually if this method returns a value, it is maxLength :

	if ( n < maxLength )
		throw WriteFailedException( "Socket::write (char *) failed : " 
			+ System::explainError() ) ;

	return n ;

#else // CEYLAN_USES_NETWORK	

	throw WriteFailedException( "Socket::write (char *) failed : " 
		"network support not available." ) ;
	
#endif // CEYLAN_USES_NETWORK	

}



bool Socket::hasAvailableData() const throw()
{

#if CEYLAN_USES_NETWORK

	// HasAvailableData does not throw any exception :
	return System::HasAvailableData( getFileDescriptorForTransport() ) ;
	
#else // CEYLAN_USES_NETWORK

	LogPlug::error( "Socket::hasAvailableData failed : "
		"network support not available." ) ;

	return false ;

#endif // CEYLAN_USES_NETWORK

}


void Socket::clearInput() throw( InputStream::ReadFailedException )
{

	Ceylan::Byte b ;
	
	while ( hasAvailableData() ) 
		read( &b, 1 ) ;
		
}


FileDescriptor Socket::getOriginalFileDescriptor() const
	throw( SocketException, Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	return _originalFD ;
				
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"Socket::getOriginalFileDescriptor : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


FileDescriptor Socket::getFileDescriptorForTransport() const
	throw( SocketException, Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	// Basic sockets use the original (and only) file descriptor :
	return _originalFD ;
				
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"Socket::getOriginalFileDescriptor : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}



Port Socket::getLocalPort() const throw()
{
	return _localPort ;
}


Port Socket::getPeerPort() const throw()
{
	return _peerPort ;
}


StreamID Socket::getInputStreamID() const throw( InputStreamException )
{

#if CEYLAN_USES_NETWORK

	try
	{
		return static_cast<StreamID>( getFileDescriptorForTransport() ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		throw InputStreamException( "Socket::getInputStreamID failed : "
			+ e.toString() ) ;
	}
	
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "Socket::getInputStreamID failed : "
		"network feature not available." ) ;
		
#endif // CEYLAN_USES_NETWORK
}


StreamID Socket::getOutputStreamID() const throw( OutputStreamException )
{

#if CEYLAN_USES_NETWORK

	try
	{
		return static_cast<StreamID>( getFileDescriptorForTransport() ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		throw OutputStreamException( "Socket::getOutputStreamID failed : "
			+ e.toString() ) ;
	}
	
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "Socket::getOutputStreamID failed : "
		"network feature not available." ) ;
		
#endif // CEYLAN_USES_NETWORK
}


const std::string Socket::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "Socket associated to local port " 
		+ Ceylan::toString( getLocalPort() ) 
		+ ", with original file descriptor being " 
		+ Ceylan::toString( getOriginalFileDescriptor() ) 
		+ ". The peer port is " + Ceylan::toString( getPeerPort()
		+ ", and the file descriptor for transport is " 
		+ Ceylan::toString( getFileDescriptorForTransport() ) ;
	
	// Add _address interpretation here.
	
	return res ;
	
}




// Protected section.


// Constructors are defined at the top of this file.



SystemSpecificSocketAddress & Socket::getPeerAddress()
	throw( SocketException, Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	if ( _address != 0 )
		return *_address ;
	else
		throw SocketException( "Socket::getPeerAddress : "
			"no available address." ) ;
					
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"Socket::getPeerAddress : network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


void Socket::createSocket( Port port ) throw( SocketException )
{

	throw SocketException( "Socket::createSocket must be overriden "
		"by child classes." ) ;
		
}
					
						
bool Socket::close() throw( Stream::CloseException )
{

#if CEYLAN_USES_NETWORK

	return Stream::Close( _originalFD ) ;
	
#else // CEYLAN_USES_NETWORK	

	LogPlug::error( "Socket::close failed : "
		"network support not available." ) ; 
		
	return false ;
	
#endif // CEYLAN_USES_NETWORK	

}
