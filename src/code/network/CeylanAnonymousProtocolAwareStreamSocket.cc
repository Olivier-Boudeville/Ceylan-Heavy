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


#include "CeylanAnonymousProtocolAwareStreamSocket.h"


#include "CeylanLogPlug.h"        // for LogPlug
#include "CeylanOperators.h"      // for toString
#include "CeylanProtocolServer.h" // for ProtocolServer


using namespace Ceylan ;
using namespace Ceylan::System ;
using namespace Ceylan::Middleware ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;





AnonymousProtocolAwareStreamSocket::AnonymousProtocolAwareStreamSocket( 
		System::FileDescriptor listeningFD, 
		ProtocolServer & protocolServerToTrigger ) :
	AnonymousStreamSocket( listeningFD ),
	_protocolServer( & protocolServerToTrigger )
{
	
	// Here the connection is accepted.
	
}



AnonymousProtocolAwareStreamSocket::AnonymousProtocolAwareStreamSocket( 
		System::FileDescriptor listeningFD ) :
	AnonymousStreamSocket( listeningFD, /* blocking */ false ),
	_protocolServer( 0 )
{
	
	// Here the connection is accepted.
	
}



AnonymousProtocolAwareStreamSocket::~AnonymousProtocolAwareStreamSocket()
	throw()
{

	if ( _protocolServer != 0 )
		delete _protocolServer ;
	
}



bool AnonymousProtocolAwareStreamSocket::hasProtocolServer() const
{

	return _protocolServer != 0 ;
	
}



ProtocolServer & AnonymousProtocolAwareStreamSocket::getProtocolServer()
	{

	if ( _protocolServer == 0 )
		throw AnonymousStreamSocketException(
			"AnonymousProtocolAwareStreamSocket::getProtocolServer: "
			"no available protocol server." ) ;
			
	return * _protocolServer ;
	
}



void AnonymousProtocolAwareStreamSocket::setProtocolServer( 
	ProtocolServer & newProtocolServer )
{

	if (  _protocolServer != 0 )
		delete _protocolServer ;
		
	_protocolServer	= & newProtocolServer ;
	
}



const std::string AnonymousProtocolAwareStreamSocket::toString( 
	Ceylan::VerbosityLevels level ) const
{
		
	string res= "AnonymousProtocolAwareStreamSocket" ;
	
	if ( level != Ceylan::low )
	{
	
		/*
		 * Avoid cycles in toString calls:
		 * AnonymousProtocolAwareStreamSocket -> ProtocolServer 
		 * -> ProtocolEndpoint -> Marshaller 
		 * -> AnonymousProtocolAwareStreamSocket
		 *
		 */
	 
		if ( _protocolServer != 0 )
			res += " associated with " 
				+ _protocolServer->toString( Ceylan::low ) ;
		else	
			res += " not associated with any protocol server" ;
						
	}
		
	res += ". It is an " + AnonymousStreamSocket::toString( level ) ;
	
	return res ;
	
}	
						
