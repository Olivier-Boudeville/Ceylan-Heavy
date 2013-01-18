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


#include "CeylanSequentialServerStreamSocket.h"


#include "CeylanLogPlug.h"                // for LogPlug
#include "CeylanOperators.h"              // for toString
#include "CeylanThread.h"                 // for Sleep
#include "CeylanAnonymousStreamSocket.h"  // for AnonymousStreamSocket



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


using namespace Ceylan ;
using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;



#ifdef DEBUG_NETWORK_SERVERS

#define DISPLAY_NET_DEBUG(message) LogPlug::trace(message )

#else // DEBUG_NETWORK_SERVERS

#define DISPLAY_NET_DEBUG(message)

#endif // DEBUG_NETWORK_SERVERS




SequentialServerStreamSocket::SequentialServerStreamSocketException::SequentialServerStreamSocketException( const std::string & reason ) :
	ServerStreamSocketException( reason )
{

}


SequentialServerStreamSocket::SequentialServerStreamSocketException::~SequentialServerStreamSocketException() throw()
{

}





// SequentialServerStreamSocket class.



SequentialServerStreamSocket::SequentialServerStreamSocket( Port listeningPort, 
		bool reuse ) :
	ServerStreamSocket( listeningPort, reuse, /* blocking */ true ),
	_currentConnection( 0 )
{

#if CEYLAN_USES_NETWORK


#else // CEYLAN_USES_NETWORK

	throw SequentialServerStreamSocketException( 
		"SequentialServerStreamSocket constructor failed: "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}



SequentialServerStreamSocket::~SequentialServerStreamSocket() throw()
{

#if CEYLAN_USES_NETWORK

	// The main listening socket is taken care of in mother classes.
	
	// No destructor should throw exception:
	try
	{
	
		closeAcceptedConnections() ;
		
	}
	catch( const Stream::CloseException	& e )
	{
		LogPlug::error( "SequentialServerStreamSocket destructor failed: " 
			+ e.toString() ) ;
	}
	
#endif // CEYLAN_USES_NETWORK
	
}



bool SequentialServerStreamSocket::isConnected() const
{

	return _currentConnection != 0 ;
	
}



AnonymousStreamSocket * SequentialServerStreamSocket::accept() 
{

#if CEYLAN_USES_NETWORK

	if ( _currentConnection != 0 )
		throw SequentialServerStreamSocketException( 
			"SequentialServerStreamSocket::accept: "
			"a connection is still active" ) ;
			
	if ( ! _bound )
		prepareToAccept() ;


	DISPLAY_NET_DEBUG( "SequentialServerStreamSocket::accept: "
		"will accept now connections, state is: " + toString() ) ;
	
	/*
	 * One can notice that constructing next AnonymousStreamSocket implies
	 * that the accept is blocking, i.e. the listening socket is created
	 * as a blocking one. Otherwise a Select operating on this socket would
	 * have to be used here.
	 *
	 */
	 
	try
	{
	
		// Accepts the connection, by passing the listening file descriptor:
		_currentConnection = new AnonymousStreamSocket( 
			getOriginalFileDescriptor() ) ;
			
	}
	catch( const SocketException & e )
	{
		throw SequentialServerStreamSocketException( 
			"SequentialServerStreamSocket::accept failed: "
			+ e.toString() ) ;
	}	
		
	DISPLAY_NET_DEBUG( "SequentialServerStreamSocket::accept: "
		"new connection accepted, will be taken care of now: "
		+ _currentConnection->toString() ) ;
		
	accepted( *_currentConnection ) ;
	
	DISPLAY_NET_DEBUG( "SequentialServerStreamSocket::accept: "
		"connection terminated, cleaning up afterwards" ) ;
	
	cleanAfterAccept() ;

	return 0 ;

#else // CEYLAN_USES_NETWORK	


	throw ServerStreamSocketException( 
		"SequentialServerStreamSocket::accept: "
		"network feature not available." ) ;
		
			
#endif // CEYLAN_USES_NETWORK	
			
}



FileDescriptor SequentialServerStreamSocket::getFileDescriptorForTransport()
	const
{

#if CEYLAN_USES_NETWORK

	if ( _currentConnection != 0 )
		return _currentConnection->getOriginalFileDescriptor() ;
	else	
		throw SequentialServerStreamSocketException( 
			 "SequentialServerStreamSocket::getFileDescriptorForTransport: "
			 "no available connection." ) ;
		
			
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"SequentialServerStreamSocket::getFileDescriptorForTransport: "
		"network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}



const std::string SequentialServerStreamSocket::toString(
	Ceylan::VerbosityLevels level ) const
{

	string res = "SequentialServerStreamSocket " ;
	
#if CEYLAN_USES_NETWORK


	if ( _currentConnection != 0 )
		res += "with a running connection: " 
			+ _currentConnection->toString( level ) ;
	else
		res += "not connected to any peer" ;
	
	if ( level == Ceylan::low )
		return res ;
	
	return res + ". " + ServerStreamSocket::toString( level ) ;
	
	
#else // CEYLAN_USES_NETWORK

	return res + "(no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
						


bool SequentialServerStreamSocket::closeAcceptedConnections() 
{

	if ( _currentConnection != 0 )
	{
		delete _currentConnection ;
		_currentConnection = 0 ;
		return true ;
		
	}
	
	return false ;

}
	
				
