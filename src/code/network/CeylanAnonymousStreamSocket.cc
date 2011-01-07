/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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


#include "CeylanAnonymousStreamSocket.h"


#include "CeylanLogPlug.h"       // for LogPlug
#include "CeylanOperators.h"     // for toString
#include "CeylanNetwork.h"       // for getSocketError


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
#include <sys/types.h>         // for bind, accept
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>        // for bind, listen, accept
#endif // CEYLAN_USES_SYS_SOCKET_H

#ifdef CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>         // for htonl, sockaddr_in
#endif // CEYLAN_USES_ARPA_INET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for htonl, sockaddr_in
#endif // CEYLAN_USES_NETINET_IN_H

}


#include <cerrno>  // for EAGAIN


using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;



/*
 * Avoid::htonl, use directy htonl since it is a macro on some platforms
 * (ex: NetBSD)
 *
 */



AnonymousStreamSocket::AnonymousStreamSocketException::AnonymousStreamSocketException( const std::string & reason ) :
	StreamSocketException( reason )
{

}



AnonymousStreamSocket::AnonymousStreamSocketException::~AnonymousStreamSocketException() throw()
{

}



AnonymousStreamSocket::NonBlockingAcceptException::NonBlockingAcceptException(
		const std::string & reason ) :
	AnonymousStreamSocketException( reason )
{

}



AnonymousStreamSocket::NonBlockingAcceptException::~NonBlockingAcceptException()
	throw()
{

}



AnonymousStreamSocket::AnonymousStreamSocket( 
		Ceylan::System::FileDescriptor listeningFD, 
		bool blocking, bool sacrificeThroughputToPacketTiming ) :
	StreamSocket( /* blocking */ true,
		sacrificeThroughputToPacketTiming )
{

#if CEYLAN_USES_NETWORK
	
	LogPlug::debug( "AnonymousStreamSocket constructor: "
		"ready to accept a new connection using listening file descriptor " 
		+ Ceylan::toString( listeningFD ) + "." ) ;

#if CEYLAN_ARCH_WINDOWS

	// socklen_t is lacking:
	int addressSize = sizeof( _address->_socketAddress ) ;

#else // CEYLAN_ARCH_WINDOWS

	socklen_t addressSize = static_cast<socklen_t>( 
		sizeof( _address->_socketAddress ) ) ;

#endif // CEYLAN_ARCH_WINDOWS

	/*
	 * A time-out could be added, in case the connection request was cancelled
	 * after the select but before this accept, if ever blocking sockets 
	 * were chosen (non-blocking are to be preferred anyway):
	 *
	 */
	_originalFD = static_cast<System::FileDescriptor>(::accept( listeningFD, 
		reinterpret_cast<sockaddr *>( & _address->_socketAddress ),
		& addressSize ) ) ;

#if CEYLAN_ARCH_WINDOWS
	if ( _originalFD == INVALID_SOCKET )
	{

		if ( Network::getSocketError() == WSAEWOULDBLOCK )
#else // CEYLAN_ARCH_WINDOWS
	if ( _originalFD == -1 )
	{
	
		if ( System::getError() == EAGAIN )
#endif // CEYLAN_ARCH_WINDOWS
		{
			
			/*
			 * With non-blocking sockets, accept with no available connection
			 * returns EAGAIN:
			 *
			 */
			throw NonBlockingAcceptException( 
				"AnonymousStreamSocket constructor: "
				"no available connection found." ) ;
				
		}
		else
		{
		
			throw AnonymousStreamSocketException(
				"AnonymousStreamSocket constructor failed: " 
				+ System::explainError() ) ;
		}
				
	}
	
	
	LogPlug::debug( "AnonymousStreamSocket constructor: "
		"new connection accepted." ) ;

	// This new file descriptor comes blocking:
	if ( blocking == false )
		setBlocking( false ) ;
	
	if ( _nagleAlgorithmDeactivated )
		setNagleAlgorithmTo( false ) ; 

		
#else // CEYLAN_USES_NETWORK

	throw AnonymousStreamSocketException( 
		"AnonymousStreamSocket constructor failed: "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}



AnonymousStreamSocket::~AnonymousStreamSocket() throw()
{

	// StreamSocket takes care of everything needed.
	
}



bool AnonymousStreamSocket::isConnected() const
{

	return true ;
	
}



const std::string AnonymousStreamSocket::toString( 
	Ceylan::VerbosityLevels level ) const
{

#if CEYLAN_USES_NETWORK

	return "AnonymousStreamSocket managing the connection-dedicated "
		"file descriptor " + Ceylan::toString( _originalFD ) ;
		
#else // CEYLAN_USES_NETWORK

	return "AnonymousStreamSocket (no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
						
