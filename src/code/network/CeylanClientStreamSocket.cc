#include "CeylanClientStreamSocket.h"

//#include "System.h"

#include "CeylanLogPlug.h"
#include "CeylanNetwork.h"            // for HostDNSEntry
#include "CeylanIPAddressvFour.h"     // for IPAddressvFour


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


//extern "C" int errno;
//extern "C" int h_errno;


extern "C"
{

#ifdef CEYLAN_USES_NETDB_H
#include <netdb.h> // CEYLAN_USES_NETDB_H
#endif // CEYLAN_USES_NETDB_H

}


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


using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;



ClientStreamSocket::ClientStreamSocket() throw( SocketException ) :
	StreamSocket(),
	_serverHostName(),
	_serverHostinfo( 0 )
{

}


ClientStreamSocket::~ClientStreamSocket() throw()
{

	if ( _serverHostinfo != 0 )
		delete _serverHostinfo ;

}


void ClientStreamSocket::connect( const string & serverHostname, Port port ) 
	throw( SocketException )
{

	_serverHostName = serverHostname ;
	
	createSocket( port ) ;
	
	try
	{
		_serverHostinfo = new HostDNSEntry( _serverHostName ) ;
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
		* _serverHostinfo->getAddresses().begin() ) ;
		
	if ( serverIP == 0 )
		throw ClientStreamSocketException( "ClientStreamSocket::connect : "
			"could not determine an IPv4 address from host '" 
				+ _serverHostName + "'." ) ;
	
	// It is actually something like an unsigned long (see man inet_ntoa) :
	struct in_addr binaryIP ;
			
	if ( ::inet_aton( serverIP.toString().c_str(), & binaryIP ) == 0 )
		throw ClientStreamSocketException( "ClientStreamSocket::connect : "
			"could not forge a network address from IP " 
			+ serverIP.toString() + " of host '" 
				+ _serverHostName + "'." ) ;
	
	getAddress()->_socketAddress.sin_addr = * ( in_addr *)_serverHostinfo->h_addr;

		int n = ::connect( getFileDescriptor(), (sockaddr *) & getAddress(), sizeof( sockaddr_in ) );
	
		if( n < 0 )
		{
			setError( errno );
			clientConnectionFailed();
			return false;
		}
		else connected();

	}

	return ( n == 0 );
}

Client::~Client()
{
}

void Client::connected()
{
}

void Client::clientConnectionFailed()
{
	cerr << "Client::connect("<< _server << "," << getPort() << ") failed: " << System::explainError( getError() ) << endl;
}

void Client::getHostByNameFailed()
{
	const char * str;
	
	switch( getError() )
	{
		case HOST_NOT_FOUND:	str = "The specified host is unknown."; break;
		case NO_DATA:		str = "The requested name is valid but does not have an IP address."; break;
 		case NO_RECOVERY:	str = "A non-recoverable name server error occurred."; break;
		case TRY_AGAIN:		str = "A temporary error occurred on an authoritative name server.  Try again later."; break;
		default: 		str = "Unknown error."; break;	
	}
	
	cerr << "Socket::getHostByNameFailed(port) failed: " << str << endl;
}


const std::string &ClientStreamSocket::getServerName() const throw()
{

	return _server ;
	
}

