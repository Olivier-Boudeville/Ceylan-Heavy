#include "CeylanServerAnonymousInputOutputStream.h"

#include "CeylanOperators.h"          // for operators
#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanNetwork.h"            // for HostDNSEntry
#include "CeylanIPAddressvFour.h"     // for IPAddressvFour

// for SystemSpecificSocketAddress :
#include "CeylanSystemSpecificSocketAddress.h"  


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for accept
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>        // for accept
#endif // CEYLAN_USES_SYS_SOCKET_H

}


using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;
using namespace Ceylan ;


using std::string ;


ServerAnonymousInputOutputStreamException::ServerAnonymousInputOutputStreamException( const std::string & reason ) throw():
	NetworkException( reason )
{

}


ServerAnonymousInputOutputStreamException::~ServerAnonymousInputOutputStreamException() throw()
{

}


/*
 * The 'fd' argument corresponds to the server-owned listening socket, 
 * whereas this aonymous stream has to store the descriptor spawned for the
 * connection it encapsulates.
 *
 */

ServerAnonymousInputOutputStream::ServerAnonymousInputOutputStream(
		FileDescriptor fd ) 
		throw( ServerAnonymousInputOutputStreamException, StreamException ) :
	AnonymousInputOutputStream(),
	_fdes( 0 ),
	_peerAddress( 0 )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	_peerAddress = new SystemSpecificSocketAddress ;
	
	socklen_t size = static_cast<socklen_t>( 
		sizeof( _peerAddress->_socketAddress ) ) ;

	LogPlug::debug( "ServerAnonymousInputOutputStream constructor : "
		"ready to accept a new connection." ) ;

	_fdes = ::accept( fd, 
		reinterpret_cast<sockaddr *>( & _peerAddress->_socketAddress ),
		& size ) ;

	if ( _fdes == -1 )
		throw ServerAnonymousInputOutputStreamException(
			"ServerAnonymousInputOutputStream constructor failed : " 
			+ System::explainError() ) ;

	LogPlug::debug( "ServerAnonymousInputOutputStream constructor : "
		"new connection accepted." ) ;
			
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw ServerAnonymousInputOutputStreamException( 
		"ServerAnonymousInputOutputStream constructor : "
		"file descriptor feature not available." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}


ServerAnonymousInputOutputStream::~ServerAnonymousInputOutputStream() throw()
{
	
	try
	{
		close() ;
	}
	catch( const Stream::CloseException	& e )
	{
		LogPlug::error( "ServerAnonymousInputOutputStream destructor failed : " 
			+ e.toString() ) ;
	}

	
	if ( _peerAddress != 0 )
		delete _peerAddress ;		
		
}


const SystemSpecificSocketAddress &
	ServerAnonymousInputOutputStream::getPeerAddress() const throw()
{
	return *_peerAddress ;
}


StreamID ServerAnonymousInputOutputStream::getInputStreamID() const throw()
{

	return static_cast<StreamID>( _fdes ) ;
	
}


StreamID ServerAnonymousInputOutputStream::getOutputStreamID() const throw()
{

	return static_cast<StreamID>( _fdes ) ;
	
}


bool ServerAnonymousInputOutputStream::hasAvailableData() const throw()
{

#if CEYLAN_USES_NETWORK

	return System::HasAvailableData( _fdes ) ;

#else // CEYLAN_USES_NETWORK

	LogPlug::error( "AnonymousInputOutputStream::hasAvailableData failed : "
		"network support not available." ) ;

	return false ;

#endif // CEYLAN_USES_NETWORK

}


bool ServerAnonymousInputOutputStream::close() throw( CloseException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	if ( _fdes > 0 )
		return Stream::Close( _fdes ) ;
	else
		return false ;

#else // CEYLAN_USES_FILE_DESCRIPTORS
	
	throw CloseException( "ServerAnonymousInputOutputStream::close : "
		"file descriptor feature not available." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}

	
const string ServerAnonymousInputOutputStream::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{

	return "ServerAnonymousInputOutputStream whose file descriptor is "
		+ Ceylan::toString( _fdes ) ;
	
}
	
