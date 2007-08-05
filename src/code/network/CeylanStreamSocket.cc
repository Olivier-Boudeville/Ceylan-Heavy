#include "CeylanStreamSocket.h"


#include "CeylanLogPlug.h"     // for LogPlug
#include "CeylanOperators.h"   // for operators
#include "CeylanNetwork.h"     // for explainSocketError

// for SystemSpecificSocketAddress :
#include "CeylanSystemSpecificSocketAddress.h"  



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


// Not available in their C++ form :
extern "C"
{


#ifdef CEYLAN_USES_XTI_H
#include <xti.h>               // for TCP_NODELAY
#endif // CEYLAN_USES_XTI_H


#ifdef CEYLAN_USES_NETINET_TCP_H
#include <netinet/tcp.h>       // for TCP_NODELAY
#endif // CEYLAN_USES_NETINET_TCP_H


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


#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>            // for fcntl, close
#endif // CEYLAN_USES_UNISTD_H


#ifdef CEYLAN_USES_FCNTL_H
#include <fcntl.h>             // for fcntl
#endif // CEYLAN_USES_STRINGS_H


#ifdef CEYLAN_USES_SYS_IOCTL_H
#include <sys/ioctl.h>        // for FreeBSD ioctl
#endif // CEYLAN_USES_SYS_IOCTL_H


#ifdef CEYLAN_USES_WINSOCK2_H
#include <winsock2.h>         // for IPPROTO_TCP, etc.
#endif // CEYLAN_USES_WINSOCK2_H


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



	
StreamSocket::StreamSocket( bool blocking, 
			bool sacrificeThroughputToPacketTiming ) 
		throw( Socket::SocketException ) :
	Socket( blocking ),	
	_nagleAlgorithmDeactivated( sacrificeThroughputToPacketTiming )
{

	// Used by clients, which have to call createSocket when appropriate.

#if CEYLAN_USES_NETWORK


#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
	LogPlug::trace( "StreamSocket empty constructor" ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS

	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "StreamSocket empty constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK

}


StreamSocket::StreamSocket( Port port, bool blocking, 
		bool sacrificeThroughputToPacketTiming ) throw( SocketException ):
	Socket( port, blocking ),
	_nagleAlgorithmDeactivated( sacrificeThroughputToPacketTiming )
{

	// Used by servers, hence createSocket is called through inheritance.
	
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

#if CEYLAN_ARCH_WINDOWS

	_originalFD = static_cast<System::FileDescriptor>(
		::socket( 
			/* domain : IPv4 */ AF_INET, 
			/* type */ SOCK_STREAM,	
			/* protocol : TCP */ IPPROTO_TCP ) ) ;

	if ( _originalFD == INVALID_SOCKET )
		throw SocketException( "StreamSocket::createSocket failed : "
		+ Network::explainSocketError() ) ;

#else // CEYLAN_ARCH_WINDOWS

	_originalFD = ::socket( /* domain : IPv4 */ PF_INET, 
		/* type */ SOCK_STREAM,	/* protocol : TCP */ 0 ) ;
	
	if ( _originalFD == -1 )
		throw SocketException( "StreamSocket::createSocket failed : "
			+ System::explainError() ) ;

#endif // CEYLAN_ARCH_WINDOWS

	
#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
	LogPlug::debug( "StreamSocket::createSocket : "
		"this socket, whose original file descriptor is " 
		+ Ceylan::toString( _originalFD ) + ", will be associated to port " 
		+ Ceylan::toString( _port ) + "." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS


	if ( ! isBlocking() )
	{
		
		/*
		 * Was recorded but not done since the descriptor is only available
		 * now :
		 *
		 */
		try
		{
			setBlocking( false ) ;
		}	
		catch( Stream::NonBlockingNotSupportedException & e )
		{
			throw SocketException( "Socket port-less constructor : "
				"unable to set this socket in non-blocking mode : "
				+ e.toString() ) ;
		}
	}	

	/*
	 * If the Nagle algorithm is to be deactivated, it will be
	 * triggered for server sockets just after the bind phase
	 * (to discriminate between a bind failed and a 
	 * setsockopt error), whereas for client sockets it will
	 * be performed just before the connect phase.
	 */

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


void StreamSocket::setNagleAlgorithmTo( bool activated ) 
	throw( StreamSocketException )
{


	/*
	 * Maybe, in the future, the TCP_CORK, TCP_NOPUSH etc. options could
	 * be managed.
	 *
	 */
	
#if CEYLAN_USES_NETWORK

	LogPlug::debug( "StreamSocket::setNagleAlgorithmTo : "
		"setting the algorithm to " 
		+ Ceylan::toString( activated ) ) ;

	int optionValue ;

	if ( activated )
		optionValue = 0 ;
	else
		optionValue = 1 ;


#if CEYLAN_ARCH_WINDOWS


	// Sets the Nagle algorithm for send coalescing :

	/*
	 * Avoid warning C4312 : 
	 * "'reinterpret_cast' : conversion from 'int' to 'const char *' 
	 * of greater size", no better work-around.
	 *
	 */
#pragma warning( push )
#pragma warning( disable : 4312 )

	if ( ::setsockopt( 
			/* target socket */ getFileDescriptorForTransport(), 
			/* level */ SOL_SOCKET,
			/* option name */ TCP_NODELAY,
			/* option value */ reinterpret_cast<const char *>( optionValue ),
			/* option length */ sizeof( int ) ) == SOCKET_ERROR )
		throw StreamSocketException( 
			"StreamSocket::setNagleAlgorithmTo failed : "
			+ Network::explainSocketError() ) ;

#pragma warning( pop )

#else // CEYLAN_ARCH_WINDOWS

	if ( ::setsockopt( 
			/* target socket */ getFileDescriptorForTransport(), 
			/* level */ SOL_SOCKET,
			/* option name */ TCP_NODELAY,
			/* option value */ reinterpret_cast<const char *>( optionValue ),
			/* option length */ sizeof( int ) ) == -1 )
		throw StreamSocketException( 
			"StreamSocket::setNagleAlgorithmTo failed : "
			+ explainError() ) ;

#endif // CEYLAN_ARCH_WINDOWS

#else // CEYLAN_USES_NETWORK

	throw StreamSocketException( "StreamSocket::setNagleAlgorithmTo : "
		"network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}

const std::string StreamSocket::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "Stream" + Socket::toString( level ) ;

	if ( level == Ceylan::low )
		return res ;

	if ( _nagleAlgorithmDeactivated )
		res += ". The Nagle algorithm is deactivated for this stream socket" ;
	else
		res += ". The Nagle algorithm is activated for this stream socket" ;
	
	return res ;

}

	

void StreamSocket::setBlocking( bool newStatus )
	throw( NonBlockingNotSupportedException )
{

#if CEYLAN_USES_NETWORK


#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS

	if ( getOriginalFileDescriptor() == 0 )
		throw NonBlockingNotSupportedException( 
			"StreamSocket::setBlocking : null descriptor, "
				"socket not created yet ?" ) ;
	
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS


#if CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS

	// Yes, retrieve current flags :
	int currentFDState = ::fcntl( getOriginalFileDescriptor(),
		F_GETFL, 0 ) ;

	if ( currentFDState < 0 )
		throw NonBlockingNotSupportedException( 
			"StreamSocket::setBlocking : retrieving attributes failed : " 
			+ System::explainError() ) ;

#endif // CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS
	

	// Set newer flags :
	if ( newStatus )
	{

		// Here we set to blocking :

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS	

		LogPlug::trace( "StreamSocket::setBlocking : "
			"setting a non-blocking socket to blocking, "
			"using file descriptor #" 
			+ Ceylan::toString( getOriginalFileDescriptor() )
			+ "." ) ;
			
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS
		
		
			
#if CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS

		if ( ::fcntl( getOriginalFileDescriptor(), F_SETFL, 
				currentFDState & (~O_NONBLOCK) ) < 0 )
			throw NonBlockingNotSupportedException( 
				"StreamSocket::setBlocking : "
				"setting to blocking with fcntl failed : " 
				+ System::explainError() ) ;

#else // CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS

#if CEYLAN_ARCH_WINDOWS

		unsigned long nonBlockingMode = 0 ;

		if ( ::ioctlsocket( getOriginalFileDescriptor(), FIONBIO,
				&nonBlockingMode ) == SOCKET_ERROR )
			throw NonBlockingNotSupportedException( 
				"StreamSocket::setBlocking : "
				"setting to blocking with ioctlsocket failed : " 
				+ Network::explainSocketError() ) ;	

#else // CEYLAN_ARCH_WINDOWS

		int nonBlockingFlag = 0 ;
		if ( ::ioctl( getOriginalFileDescriptor(), FIONBIO, &nonBlockingFlag ) )
			throw NonBlockingNotSupportedException( 
				"StreamSocket::setBlocking : "
				"setting to blocking with ioctl failed : " 
				+ System::explainError() ) ;		

#endif // CEYLAN_ARCH_WINDOWS

#endif // CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS

	}
	else
	{
	
	
#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
	
		LogPlug::trace( 
			"StreamSocket::setBlocking : "
			"setting a blocking socket to non-blocking, "
			"using file descriptor #" 
			+ Ceylan::toString( getOriginalFileDescriptor() )
			+ "." ) ;
			
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS
	
	
	
#if CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS

		if ( ::fcntl( getOriginalFileDescriptor(), F_SETFL, 
				currentFDState | O_NONBLOCK ) < 0 )
			throw NonBlockingNotSupportedException( 
				"StreamSocket::setBlocking : "
				"setting to non-blocking with fcntl failed : " 
				+ System::explainError() ) ;

#else // CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS

#if CEYLAN_ARCH_WINDOWS

		unsigned long nonBlockingMode = 1 ;

		if ( ::ioctlsocket( getOriginalFileDescriptor(), FIONBIO,
				&nonBlockingMode ) == SOCKET_ERROR )
			throw NonBlockingNotSupportedException( 
				"StreamSocket::setBlocking : "
				"setting to non-blocking with ioctlsocket failed : " 
				+ Network::explainSocketError() ) ;	

#else // CEYLAN_ARCH_WINDOWS

		int nonBlockingFlag = 1 ;
		if ( ::ioctl( getOriginalFileDescriptor(), FIONBIO, &nonBlockingFlag ) )
			throw NonBlockingNotSupportedException( 
				"StreamSocket::setBlocking : "
				"setting to non-blocking with ioctl failed : " 
				+ System::explainError() ) ;			

#endif // CEYLAN_ARCH_WINDOWS

#endif // CEYLAN_USES_FCNTL_FOR_NONBLOCKING_SOCKETS
	
	}
	
	_isBlocking = newStatus ;
	
#else // CEYLAN_USES_NETWORK	
		
	throw NonBlockingNotSupportedException( 
		"StreamSocket::setBlocking failed : network support not available." ) ; 
			
#endif // CEYLAN_USES_NETWORK	

}

