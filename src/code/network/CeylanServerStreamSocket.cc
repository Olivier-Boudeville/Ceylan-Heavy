/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#include "CeylanServerStreamSocket.h"


#include "CeylanLogPlug.h"               // for LogPlug
#include "CeylanOperators.h"             // for toString
#include "CeylanThread.h"                // for Sleep
#include "CeylanAnonymousStreamSocket.h" // for AnonymousStreamSocket



// for SystemSpecificSocketAddress:
#include "CeylanSystemSpecificSocketAddress.h"  


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



extern "C"
{

#ifdef CEYLAN_USES_UNISTD_H
//#include <unistd.h>            // for FIXME
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for setsockopt, bind, accept
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>        // for setsockopt, bind, listen, accept
#endif // CEYLAN_USES_SYS_SOCKET_H

#ifdef CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>         // for htonl, sockaddr_in
#endif // CEYLAN_USES_ARPA_INET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for htonl, sockaddr_in
#endif // CEYLAN_USES_NETINET_IN_H

}


using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;


#ifdef DEBUG_NETWORK_SERVERS

#define DISPLAY_NET_DEBUG(message) LogPlug::trace(message )

#else // DEBUG_NETWORK_SERVERS

#define DISPLAY_NET_DEBUG(message)

#endif // DEBUG_NETWORK_SERVERS



/*
 * Avoid::htonl, use directy htonl since it is a macro on some platforms
 * (ex: NetBSD)
 *
 * @see http://www.amk.ca/python/howto/sockets/ for many socket programming
 * tips.
 *
 */



ServerStreamSocket::ServerStreamSocketException::ServerStreamSocketException( 
		const std::string & reason ) :
	StreamSocketException( reason )
{

}



ServerStreamSocket::ServerStreamSocketException::~ServerStreamSocketException()
	throw()
{

}




ServerStreamSocket::ServerStreamSocket( Port serverPort, bool reuse, 
		bool blocking ) :
	StreamSocket( serverPort, blocking ),
	_bound( false ),
	_stopRequested( false ),
	_maximumPendingConnectionsCount( DefaultMaximumPendingConnectionsCount )
{

#if CEYLAN_USES_NETWORK
	
	if ( reuse )
	{
	
		// Reuse option set to non-zero to enable option:
		int reuseOption = 1 ;
		
		/*
		 * See: man 7 socket or man 7 ip.
		 *
		 * The SO_REUSEADDR socket option must be set prior to executing bind,
		 * to have any effect.
		 *
		 */
		if ( ::setsockopt( getOriginalFileDescriptor(), 
			/* socket level */ SOL_SOCKET, 
			/* option name */ SO_REUSEADDR, 
			/* option value buffer */ reinterpret_cast<char *>( &reuseOption ), 
			/* option buffer length */ sizeof( reuseOption ) ) != 0 )
				throw ServerStreamSocketException( 
					"ServerStreamSocket constructor: "
					"could not set reuse option on listening socket: "
					+ System::explainError() ) ;
	}
	
#else // CEYLAN_USES_NETWORK

	throw ServerStreamSocketException( 
		"ServerStreamSocket constructor failed: "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}



ServerStreamSocket::~ServerStreamSocket() throw()
{

	// StreamSocket takes care of everything needed.
	
}



void ServerStreamSocket::run()
{

	DISPLAY_NET_DEBUG( "Entering in ServerStreamSocket::run" ) ;
	
	// Records the total number of created connections:
	Ceylan::Uint32 connectionCount = 0 ;
	
	while ( ! isRequestedToStop() )
	{	
	
		connectionCount++ ;

		DISPLAY_NET_DEBUG( "ServerStreamSocket::run: waiting for connection #" 
			+ Ceylan::toString( connectionCount ) ) ;

		accept() ;
		
	}
	
	DISPLAY_NET_DEBUG( "Exiting from ServerStreamSocket::run" ) ;
	
}



Port ServerStreamSocket::getLocalPort() const
{

	return _port ;
	
}



ServerStreamSocket::ConnectionCount
	ServerStreamSocket::getMaximumPendingConnectionsCount() const
{

	return _maximumPendingConnectionsCount ;
	
}	
					


void ServerStreamSocket::setMaximumPendingConnectionsCount( 
	ConnectionCount newMax )
{
	_maximumPendingConnectionsCount = newMax ;
}	



const std::string ServerStreamSocket::toString( Ceylan::VerbosityLevels level ) 
	const
{

#if CEYLAN_USES_NETWORK

	string res ;
	
	
	if ( _bound )
		res = "ServerStreamSocket bound and listening for new connections" ;
	else		
		res = "ServerStreamSocket not ready to accept new connections" ;

	res += ". " ;
	
	if ( _stopRequested )
		res = "ServerStreamSocket is requested to stop" ;
	else		
		res = "ServerStreamSocket is not requested to stop" ;

	if ( level == Ceylan::low )
		return res ;
			
	res += ". The current maximum number of pending connections "
		"for this server stream socket is " 
		+ Ceylan::toString( getMaximumPendingConnectionsCount() ) ;
	
	if ( level == Ceylan::medium )
		return res ;
	
	return res + ". " + StreamSocket::toString( level ) ;

#else // CEYLAN_USES_NETWORK

	return "ServerStreamSocket (no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
						


void ServerStreamSocket::prepareToAccept()
{


#if CEYLAN_USES_NETWORK

	/*
	 * createSocket has been called by StreamSocket constructor, hence it
	 * has been initialized, and sin_family and sin_port have been already
	 * set.
	 *
	 */

	DISPLAY_NET_DEBUG( "Entering ServerStreamSocket::prepareToAccept" ) ;

	if ( _bound )
		throw ServerStreamSocketException(
			"ServerStreamSocket::prepareToAccept: socket already bound" ) ;
				
	_address->_socketAddress.sin_addr.s_addr = htonl( 
		/* Address to accept any incoming connection */ INADDR_ANY ) ;

	/*
	 * Could be instead: 
	 *
	 * hostent * localHost = gethostbyname( "" ) ;
	 * char* localIP = inet_ntoa( *(struct in_addr *)*localHost->h_addr_list ) ;
	 * sockaddr_in myServer ;
	 * myServer.sin_addr.s_addr = inet_addr( localIP ) ;
	 *
	 */

 	Ceylan::Uint8 bindAttemptCount = 0 ;
	const Ceylan::Uint8 maxBindAttemptCount = 5 ;
	
	for ( ; bindAttemptCount < maxBindAttemptCount; bindAttemptCount++ )
	{

	DISPLAY_NET_DEBUG( "ServerStreamSocket::prepareToAccept: "
			"bind attempt #" + Ceylan::toString( bindAttemptCount + 1 ) ) ;
	
		if ( ::bind( getOriginalFileDescriptor(), 
				reinterpret_cast<sockaddr *>( 
					& _address->_socketAddress ),
				sizeof( sockaddr_in ) ) == 0 ) 
			break ;
	
		// 0.5 second waiting:
		Thread::Sleep( 0 /* second */, 500000 /* microseconds */ ) ;
	
	}
	
	if ( bindAttemptCount == maxBindAttemptCount )
		throw ServerStreamSocketException(
			"ServerStreamSocket::prepareToAccept: bind attempts failed: "
			+ System::explainError() ) ;

	if ( _nagleAlgorithmDeactivated )
		setNagleAlgorithmTo( false ) ; 

 	_bound = true ;
				
	DISPLAY_NET_DEBUG( "ServerStreamSocket::prepareToAccept: bind succeeded." );
	
	if ( ::listen( getOriginalFileDescriptor(), 
			_maximumPendingConnectionsCount ) != 0 )
		throw ServerStreamSocketException(
			"ServerStreamSocket::prepareToAccept: listen failed: "
			+ System::explainError() ) ;

	DISPLAY_NET_DEBUG( "ServerStreamSocket::prepareToAccept: "
		"listen succeeded." ) ;
		

#else // CEYLAN_USES_NETWORK

	throw ServerStreamSocketException( "ServerStreamSocket::prepareToAccept: "
		"no network support available." ) ;
	
#endif // CEYLAN_USES_NETWORK	
	
}



void ServerStreamSocket::cleanAfterAccept()
{


#if CEYLAN_USES_NETWORK

	DISPLAY_NET_DEBUG( "Entering ServerStreamSocket::cleanAfterAccept" ) ;
	
	closeAcceptedConnections() ;
	
#else // CEYLAN_USES_NETWORK

	throw ServerStreamSocketException( "ServerStreamSocket::cleanAfterAccept: "
		"no network support available." ) ;
	
#endif // CEYLAN_USES_NETWORK	
	
}


			
void ServerStreamSocket::accepted( AnonymousStreamSocket & newConnection )	
{

	// Empty implementation, made to be overriden.

	DISPLAY_NET_DEBUG( "ServerStreamSocket::accepted: "
		"connection up and running: " + newConnection.toString() ) ;
		
}



bool ServerStreamSocket::isRequestedToStop() const
{

	return _stopRequested ;
	
}			



void ServerStreamSocket::requestToStop()
{

	DISPLAY_NET_DEBUG( "ServerStreamSocket::requestToStop" ) ;

	_stopRequested = true ;
	
}		
	
