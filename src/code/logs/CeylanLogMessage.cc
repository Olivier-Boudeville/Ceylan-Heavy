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


#include "CeylanLogMessage.h"

#include "CeylanTimestamp.h"      // for TimeStamp
#include "CeylanStringUtils.h"    // for formatStringList
#include "CeylanOperators.h"
#include "CeylanLogLight.h"


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"         // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <list>

using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;



LogMessage::LogMessage( const string & message,	
		const string & channelName,
		LevelOfDetail levelOfDetail, 
		const Timestamp & timestamp ) :
	_message( message ),
	_channelName( channelName ),
	_levelOfDetail( levelOfDetail ),
	_timestamp( & timestamp )
{

	CEYLAN_LOG( "Creating log message " + Ceylan::toString( this )
		+ ": '" + message + "' in " + channelName ) ;
		
}



LogMessage::LogMessage( const string & message,	
		const string & channelName,
		LevelOfDetail levelOfDetail ) :
	_message( message ),
	_channelName( channelName ),
	_levelOfDetail( levelOfDetail )
{

	CEYLAN_LOG( "Creating log message " + Ceylan::toString( this )
		+ ": '" + message + "' in " + channelName ) ;
		

	try 
	{
	
		_timestamp = new Timestamp() ;
		 
	} 	
	catch( const UtilsException & e )
	{
	
		throw LogException( 
			"LogMessage::LogMessage: unable to generate time-stamp: "
			+ e.toString() ) ;
			
	}
	
}					
				
				
				
LogMessage::~LogMessage() throw()
{

	CEYLAN_LOG( "Deleting log message " + Ceylan::toString( this ) ) ;
		
	if ( _timestamp != 0 )
		delete _timestamp ;

}



const std::string LogMessage::getContent() const
{

	return _message ;
	
}



const std::string LogMessage::getChannelName() const		
{

	return _channelName ;
	
}



void LogMessage::setChannelName( const string & newChannelName )
{

	_channelName = newChannelName ;
	
}



LevelOfDetail LogMessage::getLevelOfDetail() const
{

	return _levelOfDetail ;
	
}

		
		
const Timestamp & LogMessage::getTimestamp() const
{

#if CEYLAN_DEBUG

	if ( _timestamp == 0 )
		throw LogException( 
			"LogMessage::getTimestamp failed: no timestamp available." ) ;
			
#endif // CEYLAN_DEBUG
	
	return * _timestamp ;
	
}



const string LogMessage::getPreformattedText() const
{

#if CEYLAN_DEBUG

	if ( _timestamp == 0 )
		return "Error: LogMessage whose content is " 
			+ _message 
			+ ", whose level of detail is " 
			+ Ceylan::toNumericalString( _levelOfDetail )
			+ " does not have a timestamp, whereas it should." ;
	
#endif // CEYLAN_DEBUG

	return _timestamp->toString() + " " + _message ;
	
}



const string LogMessage::toString( Ceylan::VerbosityLevels level ) const
{
	
#if CEYLAN_DEBUG

	if ( _timestamp == 0 )
		return "Error: LogMessage whose content is " 
			+ _message 
			+ ", whose level of detail is " 
			+ Ceylan::toNumericalString( _levelOfDetail )
			+ " does not have a timestamp, whereas it should" ;
	
#endif // CEYLAN_DEBUG

	// Arbitrary separation for level of details:
	
	if ( level == Ceylan::low )
	{
	
	
#if CEYLAN_ARCH_NINTENDO_DS

		/*
		 * On the DS, no timestamp is currently added, as the log console has
		 * very little room.
		 *
		 * Must be very compact:
		 *
		 */
		return _channelName + ": " + _message ;
			
#else // CEYLAN_ARCH_NINTENDO_DS
	
		// Returns the most useful compact form:
		return _timestamp->toString() + " [" + _channelName + "] " + _message ;

#endif // CEYLAN_ARCH_NINTENDO_DS

	}		
	else
	{
	
		/// Mainly for log debugging purpose:
		
		std::list<string> res ;
		
		res.push_back( "targeted at channel: " 
			+ _channelName ) ;
			
		res.push_back( "content: [" + _message + "]" ) ;
		
		res.push_back( "level of detail: " 
			+ Ceylan::toNumericalString( _levelOfDetail ) ) ;
			
		res.push_back( "timestamp: " + _timestamp->toString() ) ; 	
		  
		return "LogMessage: " + formatStringList( res ) ;   	
		
	}
	
}	

