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


#include "CeylanLogChannel.h"

#include "CeylanLogMessage.h"
#include "CeylanLogLight.h"      // for CEYLAN_LOG


#include "CeylanOperators.h"
#include "CeylanStringUtils.h"   // for formatStringList

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"        // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


using std::string ;
using std::list ;


using namespace Ceylan::Log ;


LogChannel::LogChannel( const string & name ):
	_name( name )
{

	CEYLAN_LOG( "Creating log channel " + Ceylan::toString( this )
		+ " named '" + name + "'" ) ;
		
}



LogChannel::~LogChannel() throw()
{

	CEYLAN_LOG( "Deleting log channel " + Ceylan::toString( this )
		+ " named '" + _name + "', in which there are " 
		+ Ceylan::toString( _messages.size()  ) + " remaining messages." ) ;
	
	for ( list<LogMessage *>::iterator it = _messages.begin(); 
		it != _messages.end(); it++ )
	{
	
		CEYLAN_LOG( "Deleting message " + (*it)->toString() ) ;
		delete (*it) ;
		
	}

	CEYLAN_LOG( "Log channel " + _name + " deleted." ) ;
		
}



void LogChannel::addMessage( LogMessage & message, bool check ) 
{

	CEYLAN_LOG( "Adding message " + Ceylan::toString( & message ) + ": '"
		+ message.toString() + "'..." ) ;
	 
	// Attempt to allow for more reentrancy in an IRQ-based system:
	static volatile bool inUse = false ;
	
	while ( inUse )
		;

	inUse = true ;
	
	if ( check ) 
	{
		if ( message.getChannelName() != _name )
		{
		
			inUse = false ;
			throw LogException( 
				"LogChannel::addMessage: trying to add to LogChannel "
				+ _name 
				+ " a log message whose registered LogChannel is "
				+ message.getChannelName() + " (not " + _name + ")." ) ;	
				
		}		
	}
	
	_messages.push_back( & message ) ;

	inUse = false ;

	CEYLAN_LOG( "... message added." ) ;
		
}



const string LogChannel::getName() const
{

	return _name ;
	
}



LogChannel::MessageCount LogChannel::getMessageCount() const
{

	/*
	 * If there were null pointers in this list (which should not be),
	 * count would be inaccurate.	
	 *
	 */
	return static_cast<Ceylan::Uint32>( _messages.size() ) ;
	
}



const string LogChannel::toString( Ceylan::VerbosityLevels level ) const
{

	string result = "LogChannel " + _name ;
	
	if ( _messages.empty() )
		return result + " contains no message" ;
	
	if ( _messages.size() == 1 )
	{
		
		result += " contains only one message" ;
		if ( level == low )
			return result ;
			
		result += ": " + _messages.back()->toString( level ) ;
		
		return result ;
	}	

	// More than one message here:
	
	result += " contains " + Ceylan::toString( 
		static_cast<Ceylan::Uint32>( _messages.size() ) ) + " messages" ; 

	if ( level == low )
		return result ;
				
	list<string> res ;
		
	for ( list<LogMessage *>::const_iterator it = _messages.begin(); 
		it != _messages.end(); it++ )
	{
	
#if CEYLAN_DEBUG

		if ( (*it) == 0 )
		{
			CEYLAN_LOG( "Error, LogChannel::toString: "
				"null pointer in message list, skipping." ) ;
			break ;
		}	
		
#endif // CEYLAN_DEBUG

		res.push_back( (*it)->toString( level ) ) ;
		
	}	

	return result + ": " + formatStringList( res ) ;
	
}	

