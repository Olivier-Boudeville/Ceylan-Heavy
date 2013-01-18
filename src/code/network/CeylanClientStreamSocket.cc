/*
 * Copyright (C) 2003-2013 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option)
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "CeylanClientStreamSocket.h"

#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanNetwork.h"            // for HostDNSEntry
#include "CeylanIPAddressvFour.h"     // for IPAddressvFour
#include "CeylanStream.h"             // for CloseException
#include "CeylanOperators.h"          // for toString

// for SystemSpecificSocketAddress:
#include "CeylanSystemSpecificSocketAddress.h"


#ifdef CEYLAN_USES_CONFIG_H
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
		const std::string & reason ) :
	StreamSocketException( reason )
{

}


ClientStreamSocket::ClientStreamSocketException::~ClientStreamSocketException()
	throw()
{

}





ClientStreamSocket::ClientStreamSocket() :
	StreamSocket(),
	_serverHostName(),
	_serverHostInfo( 0 )
{

#if CEYLAN_USES_NETWORK

#else // CEYLAN_USES_NETWORK

	throw SocketException( "ClientStreamSocket constructor: "
		"network feature not available." ) ;

#endif // CEYLAN_USES_NETWORK

}



ClientStreamSocket::~ClientStreamSocket() throw()
{

	// The actual socket is taken care of in mother classes.

	if ( _serverHostInfo != 0 )
		delete _serverHostInfo ;

}



bool ClientStreamSocket::isConnected() const
{

	return _serverHostInfo != 0 ;

}


void ClientStreamSocket::connect( const string & serverHostname,
	Port serverPort )
{

#if CEYLAN_USES_NETWORK

	if ( isConnected() )
		throw SocketException( "ClientStreamSocket::connect: "
			"socket already connected" ) ;

	_serverHostName = serverHostname ;

	// Blanks inherited address before filling it with the server address:
	_address->blank() ;

	/*
	 * Creates the socket and fills sin_family and sin_port:
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
		throw ClientStreamSocketException( "ClientStreamSocket::connect: "
			"DNS look-up failed for server '" + _serverHostName
			+ "': " + e.toString() ) ;
	}

	std::list<IPAddress *> & serverAddresses = _serverHostInfo->getAddresses() ;

	// Here we choose to use only the first IP address found from the hostname:
	IPAddressvFour * serverIP = dynamic_cast<IPAddressvFour *>(
		* serverAddresses.begin() ) ;

	if ( serverIP == 0 )
	{

	  HostDNSEntry::DeleteAdresses( serverAddresses ) ;

	  throw ClientStreamSocketException( "ClientStreamSocket::connect: "
		  "could not determine an IPv4 address from host '"
		  + _serverHostName + "'." ) ;

	}


	// It is actually something like an unsigned long (see man inet_ntoa):
	struct in_addr binaryIP ;

#if CEYLAN_ARCH_WINDOWS
	binaryIP.s_addr = ::inet_addr( serverIP->toString().c_str() ) ;
	if ( binaryIP.s_addr == INADDR_NONE )
#else // CEYLAN_ARCH_WINDOWS
	if ( ::inet_aton( serverIP->toString().c_str(), &binaryIP ) == 0 )
#endif // CEYLAN_ARCH_WINDOWS
	{

	  string message = "ClientStreamSocket::connect: "
		"could not forge a network address from IP "
		+ serverIP->toString() + " of host '"
		+ _serverHostName + "'." ;

	  HostDNSEntry::DeleteAdresses( serverAddresses ) ;

	  throw ClientStreamSocketException( message ) ;

	}

	_address->_socketAddress.sin_addr = binaryIP ;

	if ( _nagleAlgorithmDeactivated )
		setNagleAlgorithmTo( false ) ;

#if CEYLAN_DEBUG_NETWORK_CLIENTS
	LogPlug::trace( "ClientStreamSocket::connect: connecting to "
		+ _serverHostName + ":" + Ceylan::toString( serverPort )
		+ " with local file descriptor "
		+ Ceylan::toString( getOriginalFileDescriptor() ) ) ;
#endif // CEYLAN_DEBUG_NETWORK_CLIENTS

	// No need for a::bind call before a connect:
	if ( ::connect( getOriginalFileDescriptor(),
			reinterpret_cast<sockaddr *>( & _address->_socketAddress ),
			sizeof( sockaddr_in ) ) < 0 )
	{

	  string message = "ClientStreamSocket::connect: "
		"could not connect to IP "
		+ serverIP->toString() + " for host '"
		+ _serverHostName + "' on port " + Ceylan::toString( serverPort )
		+ ": " + System::explainError() ;

	  HostDNSEntry::DeleteAdresses( serverAddresses ) ;

	  throw ClientStreamSocketException( message ) ;

	}


#if CEYLAN_DEBUG_NETWORK_CLIENTS
	LogPlug::trace( "ClientStreamSocket::connect: successfully connected." ) ;
#endif // CEYLAN_DEBUG_NETWORK_CLIENTS

	HostDNSEntry::DeleteAdresses( serverAddresses ) ;

	// Once connected, call the user-supplied code:
	connected() ;

	disconnect() ;

#else // CEYLAN_USES_NETWORK

	throw SocketException( "ClientStreamSocket::connect: "
		"network feature not available." ) ;

#endif // CEYLAN_USES_NETWORK

}



void ClientStreamSocket::disconnect()
{

#if CEYLAN_DEBUG_NETWORK_CLIENTS
	LogPlug::trace( "ClientStreamSocket::disconnect: disconnecting now." ) ;
#endif // CEYLAN_DEBUG_NETWORK_CLIENTS

	if ( ! isConnected() )
		throw SocketException( "ClientStreamSocket::disconnect: "
			"this socket was not already connected." ) ;

	delete _serverHostInfo ;
	_serverHostInfo = 0 ;

	try
	{
		// Inherited from Socket:
		close() ;
	}
	catch( const Stream::CloseException & e )
	{
		throw SocketException( "ClientStreamSocket::disconnect failed: "
			+ e.toString() ) ;
	}

	// _address->blank() is called by connect.

}



Port ClientStreamSocket::getPeerPort() const
{

	return _port ;

}



const std::string ClientStreamSocket::toString( Ceylan::VerbosityLevels level )
	const
{

	string res ;

	if ( isConnected() )
		res = "ClientStreamSocket linked to server "
			+ _serverHostName + ":"
			+ Ceylan::toString( getPeerPort() ) + ". "
			+ _serverHostInfo->toString( level ) + ". " ;
	else
		res = "ClientStreamSocket not linked to any specific server. " ;

	return res + StreamSocket::toString( level ) ;

}



void ClientStreamSocket::connected()
{

#if CEYLAN_DEBUG_NETWORK_CLIENTS
	// Empty implementation made to be overridden.
	LogPlug::debug( "ClientStreamSocket::connected: "
		"connection up and running." ) ;
#endif // CEYLAN_DEBUG_NETWORK_CLIENTS

}



const std::string & ClientStreamSocket::getServerName() const
{

	return _serverHostName ;

}
