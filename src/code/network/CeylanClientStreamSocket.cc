#include "CeylanClientStreamSocket.h"

//#include "System.h"

#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanNetwork.h"            // for HostDNSEntry
#include "CeylanIPAddressvFour.h"     // for IPAddressvFour


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"             // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


//extern "C" int errno;
//extern "C" int h_errno;


extern "C"
{

#ifdef CEYLAN_USES_NETDB_H
//#include <netdb.h>                    // for inet_aton
#endif // CEYLAN_USES_NETDB_H

#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>                 // for inet_aton
#endif // CEYLAN_USES_SYS_SOCKET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>                 // for inet_aton
#endif // CEYLAN_USES_NETINET_IN_H

#ifdef CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>                  // for inet_aton
#endif // CEYLAN_USES_ARPA_INET_H

}

using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;
using namespace Ceylan ;


#if CEYLAN_USES_NETWORK


/**
 * Avoid exposing system-dependent sockaddr_in in the headers :
 *
 * @note This definition had to be directly duplicated from
 * file CeylanStreamSocket.cc.
 *
 */
struct Socket::SystemSpecificSocketAddress
{
	sockaddr_in _socketAddress ;	
	
} ;

#endif // CEYLAN_USES_NETWORK



ClientStreamSocket::ClientStreamSocketException::ClientStreamSocketException( 
		const std::string & reason ) throw():
	StreamSocketException( reason )
{

}


ClientStreamSocket::ClientStreamSocketException::~ClientStreamSocketException()
	throw()
{

}


// Some behaviours are duplicated in clients for Stream and Datagram sockets.



using std::string ;



ClientStreamSocket::ClientStreamSocket() 
		throw( Socket::SocketException ) :
	StreamSocket(),
	_serverHostName(),
	_serverHostInfo( 0 )
{

}


ClientStreamSocket::~ClientStreamSocket() throw()
{

	if ( _serverHostInfo != 0 )
		delete _serverHostInfo ;

}


void ClientStreamSocket::connect( const string & serverHostname, Port port ) 
	throw( SocketException )
{

	_serverHostName = serverHostname ;
	
	createSocket( port ) ;
	
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
	
	// It is actually something like an unsigned long (see man inet_ntoa) :
	struct in_addr binaryIP ;
			
	if ( ::inet_aton( serverIP->toString().c_str(), & binaryIP ) == 0 )
		throw ClientStreamSocketException( "ClientStreamSocket::connect : "
			"could not forge a network address from IP " 
			+ serverIP->toString() + " of host '" 
				+ _serverHostName + "'." ) ;
	
	getAddress()._socketAddress.sin_addr = binaryIP ;

	if ( ::connect( getFileDescriptor(),
			reinterpret_cast<sockaddr *>( & getAddress()._socketAddress ),
			sizeof( sockaddr_in ) ) < 0 )
		throw ClientStreamSocketException( "ClientStreamSocket::connect : "
			"could not connect to IP " 
			+ serverIP->toString() + " for host '" 
			+ _serverHostName + "' : " + System::explainError() ) ;
	
	connected() ;
	
}


const std::string ClientStreamSocket::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res ;
	
	if ( _serverHostInfo == 0 )
		res = "ClientStreamSocket not linked to a specified server. " ;
	else		
		res = "ClientStreamSocket linked to server '"
			+ _serverHostName + "'. " ;
			
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

