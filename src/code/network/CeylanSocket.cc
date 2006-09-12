#include "CeylanSocket.h"

#include "CeylanLogPlug.h"                      // for LogPlug
#include "CeylanOperators.h"                    // for toString
#include "CeylanStringUtils.h"                  // for StringSize

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


#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>            // for fcntl, for timeval, select, on OpenBSD
#endif // CEYLAN_USES_UNISTD_H


#ifdef CEYLAN_USES_STRING_H
#include <string.h>            // for select, on OpenBSD
#endif // CEYLAN_USES_STRING_H


#ifdef CEYLAN_USES_FCNTL_H
#include <fcntl.h>             // for fcntl
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
Socket::Socket( bool blocking ) throw( Socket::SocketException ) :
	InputOutputStream( true ),
	_port( 0 ),
	_address( 0 ),
	_originalFD( 0 )
{

#if CEYLAN_USES_NETWORK
	
	_address = new SystemSpecificSocketAddress ;
	
	/*
	 * @note As long as the socket is not really created (i.e. it has a
	 * non-null file descriptor), it cannot be set really to non-blocking.
	 *
	 * Here the non-blocking request is recorded but not performed yet :
	 *
	 */	 
	if ( ! blocking )
		setBlocking( false ) ;
			
#else // CEYLAN_USES_NETWORK

	throw SocketException( "Socket empty constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


Socket::Socket( Port port, bool blocking ) throw( SocketException ):
	InputOutputStream( blocking ),
	_port( port ) ,
	_address( 0 ),
	_originalFD( 0 )
{

#if CEYLAN_USES_NETWORK
	
	/*
	 * Cannot use here : 'createSocket( _port ) ;' since it would call
	 * Socket::createSocket in spite of any overloading.
	 *
	 * Therefore child classes (ex : CeylanStreamSocket) should call this
	 * method in their own constructor.
	 *
	 * Setting the socket to non-blocking mode, if requested, should be done
	 * there.
	 *
	 */

	_address = new SystemSpecificSocketAddress ;

	if ( ! blocking )
		setBlocking( false ) ;
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "Socket port-based constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


Socket::~Socket() throw()
{

#if CEYLAN_USES_NETWORK

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

#endif // CEYLAN_USES_NETWORK
		
}


bool Socket::hasAvailableData() const throw()
{

#if CEYLAN_USES_NETWORK

	try
	{
		return System::HasAvailableData( getFileDescriptorForTransport() ) ;
	}
	catch( const Ceylan::Exception & e )
	{
	
		LogPlug::error( "Socket::hasAvailableData failed : "
			+ e.toString() ) ;
			
		return false ;	
		
	} 	

#else // CEYLAN_USES_NETWORK

	LogPlug::error( "Socket::hasAvailableData failed : "
		"network support not available." ) ;

	return false ;

#endif // CEYLAN_USES_NETWORK

}


Size Socket::read( char * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#if CEYLAN_USES_NETWORK

	try
	{
	
		setSelected( false ) ;

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
		LogPlug::debug( "Socket::read : using file descriptor "
			+ Ceylan::toString( getFileDescriptorForTransport() ) ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS
			
			
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



Port Socket::getLocalPort() const throw( SocketException )
{
	//throw SocketException( "Socket::getLocalPort : not implemented yet." ) ;
	return 0 ;
}


Port Socket::getPeerPort() const throw( SocketException )
{
	//throw SocketException( "Socket::getPeerPort : not implemented yet." ) ;
	return 0 ;
}



IPAddress * Socket::getLocalIPAddress() const throw( SocketException )
{
	throw SocketException( 
		"Socket::getLocalIPAddress : not implemented yet." ) ;
}


IPAddress * Socket::getPeerIPAddress() const throw( SocketException )
{
	throw SocketException( 
		"Socket::getPeerIPAddress : not implemented yet." ) ;

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

	string res ;
	
	
	try
	{
	
		res = "Socket associated to local port " 
			+ Ceylan::toString( getLocalPort() ) 
			+ ", with original file descriptor being " 
			+ Ceylan::toString( getOriginalFileDescriptor() ) ;
		
		if ( isConnected() )
			res += ". This Socket is currently connected, "
				"and the file descriptor for transport is " 
				+ Ceylan::toString( getFileDescriptorForTransport() ) ;
		else		
			res += ". This Socket is currently not connected" ;

	}
	catch( const Ceylan::Exception & e )
	{
		return "Socket::toString failed (abnormal)" ;
	}
		
	// Add _address interpretation here.
	
	return res ;
	
}




// Protected section.


// Constructors are defined at the top of this file.

					
						
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


void Socket::setBlocking( bool newStatus )
	throw( NonBlockingNotSupportedException )
{

	// Needs to be overriden, otherwise would throw an exception :
	_isBlocking = newStatus ;
	
	/*
	 * Changing of attribute cannot be performed here, delayed to 
	 * the createSocket method call.
	 *
	 */
	
}

	
