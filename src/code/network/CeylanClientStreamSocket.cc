#include "CeylanClientStreamSocket.h"

#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanNetwork.h"            // for HostDNSEntry
#include "CeylanIPAddressvFour.h"     // for IPAddressvFour
#include "CeylanStream.h"             // for CloseException
#include "CeylanOperators.h"          // for toString

// for SystemSpecificSocketAddress :
#include "CeylanSystemSpecificSocketAddress.h"  


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"             // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H




extern "C"
{

#ifdef CEYLAN_USES_NETDB_H
#include <netdb.h>                    // for inet_aton
#endif // CEYLAN_USES_NETDB_H

#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>               // for inet_aton
#endif // CEYLAN_USES_SYS_SOCKET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>               // for inet_aton
#endif // CEYLAN_USES_NETINET_IN_H

#ifdef CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>                // for inet_aton
#endif // CEYLAN_USES_ARPA_INET_H

}

using std::string ;

using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;
using namespace Ceylan ;




ClientStreamSocket::ClientStreamSocketException::ClientStreamSocketException( 
		const std::string & reason ) throw():
	StreamSocketException( reason )
{

}


ClientStreamSocket::ClientStreamSocketException::~ClientStreamSocketException()
	throw()
{

}





ClientStreamSocket::ClientStreamSocket() 
		throw( Socket::SocketException ) :
	StreamSocket(),
	_serverHostName(),
	_serverHostInfo( 0 )
{

#if CEYLAN_USES_NETWORK

#else // CEYLAN_USES_NETWORK

	throw SocketException( "ClientStreamSocket constructor : "
		"network feature not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}


ClientStreamSocket::~ClientStreamSocket() throw()
{

	// The actual socket is taken care of in mother classes.

	if ( _serverHostInfo != 0 )
		delete _serverHostInfo ;

}


bool ClientStreamSocket::isConnected() const throw()
{

	return _serverHostInfo != 0 ;
	
}


void ClientStreamSocket::connect( const string & serverHostname, 
	Port serverPort ) throw( SocketException )
{

#if CEYLAN_USES_NETWORK

	if ( isConnected() )
		throw SocketException( "ClientStreamSocket::connect : "
			"socket already connected" ) ;
			
	_serverHostName = serverHostname ;
	
	/*
	 * Creates the socket and fills sin_family and sin_port :
	 * (createSocket implemented in StreamSocket class)
	 *
	 */
	createSocket( serverPort ) ;
	
	try
	{
		_serverHostInfo = new HostDNSEntry( _serverHostName ) ;
	}
	catch(  const NetworkException & e )
	{
		throw ClientStreamSocketException( "ClientStreamSocket::connect : "
			"DNS look-up failed for server '" + _serverHostName 
			+ "' : " + e.toString() ) ;
	}
	
	
	/*
	 * Here we choose to use only the first IP address found from the 
	 * hostname  :
	 *
	 */
	IPAddressvFour * serverIP = dynamic_cast<IPAddressvFour *>( 
		* _serverHostInfo->getAddresses().begin() ) ;
		
	if ( serverIP == 0 )
		throw ClientStreamSocketException( "ClientStreamSocket::connect : "
			"could not determine an IPv4 address from host '" 
				+ _serverHostName + "'." ) ;
	
	// Blanks inherited address before filling it with the server address :
	_address->blank() ;
	
	
	// It is actually something like an unsigned long (see man inet_ntoa) :
	struct in_addr binaryIP ;
		
	
	/*
	 * Also possible : 
	 * binaryIP.sin_addr.s_addr = inet_addr( "129.199.129.1" ) ;
	 *
	 */
	if ( ::inet_aton( serverIP->toString().c_str(), & binaryIP ) == 0 )
		throw ClientStreamSocketException( "ClientStreamSocket::connect : "
			"could not forge a network address from IP " 
			+ serverIP->toString() + " of host '" 
				+ _serverHostName + "'." ) ;
	
	_address->_socketAddress.sin_addr = binaryIP ;

	if ( ::connect( getOriginalFileDescriptor(),
			reinterpret_cast<sockaddr *>( & _address->_socketAddress ),
			sizeof( sockaddr_in ) ) < 0 )
		throw ClientStreamSocketException( "ClientStreamSocket::connect : "
			"could not connect to IP " 
			+ serverIP->toString() + " for host '" 
			+ _serverHostName + "' : " + System::explainError() ) ;
	
	// Once connected, call the user-supplied code :
	connected() ;

	disconnect() ;
	
#else // CEYLAN_USES_NETWORK

	throw SocketException( "ClientStreamSocket::connect : "
		"network feature not available." ) ;
		
#endif // CEYLAN_USES_NETWORK
	
}


void ClientStreamSocket::disconnect() throw( SocketException )
{

	if ( ! isConnected() )
		throw SocketException( "ClientStreamSocket::disconnect : "
			"this socket was not already connected." ) ;
		
	delete _serverHostInfo ;
	_serverHostInfo = 0 ;
		
	try
	{
		// Inherited from Socket :
		close() ;
	}	
	catch( const Stream::CloseException & e )
	{
		throw SocketException( "ClientStreamSocket::disconnect failed : "
			+ e.toString() ) ;
	}
	
	// _address->blank() is called by connect.
	
}


Port ClientStreamSocket::getPeerPort() const throw( SocketException )
{
	return _port ;
}


const std::string ClientStreamSocket::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res ;
	
	if ( isConnected() )
		res = "ClientStreamSocket linked to server "
			+ _serverHostName + ":" 
			+ Ceylan::toString( getPeerPort() ) + ", DNS info is "
			+ _serverHostInfo->toString( level ) + ". " ;
	else		
		res = "ClientStreamSocket not linked to any specific server. " ;
			
	return res + StreamSocket::toString( level ) ;
	
}	


void ClientStreamSocket::connected() throw( ClientStreamSocketException )
{

	// Empty implementation made to be overriden.
	LogPlug::debug( "ClientStreamSocket::connected : "
		"connection up and running." ) ;
	
}


const std::string & ClientStreamSocket::getServerName() const throw()
{

	return _serverHostName ;
	
}

