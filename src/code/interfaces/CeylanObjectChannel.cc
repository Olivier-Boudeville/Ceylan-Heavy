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


#include "CeylanObjectChannel.h"

#include "CeylanOperators.h"
#include "CeylanLogMessage.h"
#include "CeylanObjectIdentifier.h"
#include "CeylanLogLight.h"          // for CEYLAN_LOG
#include "CeylanLoggable.h"          // for Loggable::getEmbeddedChannelName
#include "CeylanStringUtils.h"       // for formatStringList

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


using std::string ;
using std::list ;


using namespace Ceylan::Log ;



ObjectChannel::ObjectChannel( const string & channelName ) :
	LogChannel( "Uninitialized object channel" ),
	_linkedObjectID( 0 )
{

	try 
	{
	
		_linkedObjectID = & 
			ObjectIdentifier::generateFromChannelName( channelName ) ;
			
	} 
	catch( const Identifier::IdentifierException & e )
	{
	
		throw LogException( "ObjectChannel constructor: "
			"unable to construct channel identifier from " 
			+ channelName + ": " + e.toString() ) ;
			
	}
	
	_name = _linkedObjectID->toString() ;
	
}



ObjectChannel::~ObjectChannel() throw() 
{

	for ( list<LogMessage *>::iterator it = _messages.begin(); 
		it != _messages.end(); it++ )
	{
		delete (*it) ;
	}
	
	_messages.clear() ;
	
	if ( _linkedObjectID != 0 )
		delete _linkedObjectID ;
		
}



void ObjectChannel::addMessage( LogMessage & message, bool check ) 
{

	if ( check ) 
	{
	
		if ( Loggable::GetEmbeddedChannelName( 
				message.getChannelName() ) != _name )
			throw LogException( 
				"ObjectChannel::addMessage: trying to add to ObjectChannel "
				+ _name + " a log message whose registered ObjectChannel is "
				+ Loggable::GetEmbeddedChannelName( message.getChannelName() )
				+ " (not " + _name + ")." ) ;	
	}
	
	_messages.push_back( & message ) ;

}



ObjectIdentifier & ObjectChannel::getObjectIdentifier() const 
{

	if ( _linkedObjectID != 0 )
		return * _linkedObjectID ;
	
	throw LogException( 
		"ObjectChannel::getObjectIdentifier: no identifier to return." ) ;
	
}



const string ObjectChannel::toString( Ceylan::VerbosityLevels level ) const
{

	string result = "ObjectChannel " + _name ;
	
	if ( _messages.empty() )
		return result + " contains no message." ;
	
	if ( _messages.size() == 1 )
		result += " contains only one message: " ;
	else
		result += " contains " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( _messages.size() ) ) 
			+ " messages: " ;
	
	list<string> messageList ;
	 	
	for ( list<LogMessage *>::const_iterator it = _messages.begin(); 
		it != _messages.end(); it++ )
	{
	
#if CEYLAN_DEBUG

		if ( ! (*it) )
		{
			CEYLAN_LOG( "Error, ObjectChannel::toString: "
				"null pointer in message list, skipping." ) ;
			break ;
		}	
		
#endif // CEYLAN_DEBUG
		
		messageList.push_back( (*it)->toString( level ) ) ;
		
	}	

	return result + formatStringList( messageList ) ;
	
}	

