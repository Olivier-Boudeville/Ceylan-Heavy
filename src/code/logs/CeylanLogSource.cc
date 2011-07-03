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


#include "CeylanLogSource.h"


#include "CeylanLogPlug.h"        // for warning
#include "CeylanLogTransport.h"   // for propagate
#include "CeylanLogMessage.h"     // for its constructor
#include "CeylanOperators.h"      // for its toString
#include "CeylanLogLight.h"       // for CEYLAN_LOG



using std::string ;

using namespace Ceylan::Log ;



LogSource::LogSource( const string & name, LevelOfDetail levelOfDetail ) :
	_channelName( name ),
	_level( DefaultLevelOfDetailForSource )
{

}



LogSource::LogSource( const string & name, LogTransport & transport,
		LevelOfDetail levelOfDetail ) :
	_channelName( name ),
	_level( DefaultLevelOfDetailForSource ),
	_transport( & transport )
{

}



LogSource::LogSource( LogTransport & transport,	LevelOfDetail levelOfDetail ) :
	_channelName(),
	_level( DefaultLevelOfDetailForSource ),
	_transport( & transport )
{

}



LogSource::~LogSource() throw()
{

	unlinkTransport() ;

}



bool LogSource::hasChannelName() const
{

	return ! _channelName.empty() ;

}



void LogSource::setChannelName( const string & channelName )
{

	_channelName = channelName ;

}



std::string LogSource::getChannelName() const
{

	return _channelName ;

}



void LogSource::setLevelOfDetail( LevelOfDetail newLevel )
{

	_level = newLevel ;

}



LevelOfDetail LogSource::getLevelOfDetail() const
{

	return _level ;

}



void LogSource::send( const string & message, LevelOfDetail levelOfDetail )
{

	/*
	 * Too high level of detail will be filtered out in this call to directSend:
	 *
	 */
	directSend( _channelName, message, levelOfDetail ) ;

}



void LogSource::sendToChannel( const string & channel, const string & message,
	LevelOfDetail levelOfDetail ) const
{

	if ( _level >= levelOfDetail )
	{
		directSend( channel, message, levelOfDetail ) ;
	}
#if CEYLAN_DEBUG_LOG
	else
	{
		CEYLAN_LOG( "LogSource::sendToChannel: dropping message [" + message
			+ "] because the source has a LOD of "
			+ Ceylan::toString( _level ) + " and the message "
			+ Ceylan::toString( levelOfDetail ) + "." ) ;
	}
#endif // CEYLAN_DEBUG_LOG

}



void LogSource::directSend( const string & channel, const string & message,
	LevelOfDetail levelOfDetail ) const
{

#if CEYLAN_DEBUG_LOG

	if ( ! _transport )
		throw LogException( "Ceylan::Log::LogSource::send: trying to "
			"send a message whereas LogTransport not available." ) ;

#endif // CEYLAN_DEBUG_LOG

	/*
	 * This created log message will be deallocated in the chain, its ownership
	 * is transferred to the log transport.
	 *
	 */

	LogMessage * newMessage = new LogMessage( message, channel,
		levelOfDetail ) ;

	_transport->propagate( * newMessage ) ;

}



void LogSource::setTransport( LogTransport & newTransport )
{

	if ( hasTransport() )
	{
		LogPlug::warning( "Ceylan::Log::LogSource::setTransport: there "
			"was already a registered log Transport, unlinking it." ) ;

		unlinkTransport() ;

	}

	_transport = & newTransport ;

}



LogTransport * LogSource::getTransport() const
{

	return _transport ;

}



bool LogSource::hasTransport() const
{

	return ( _transport != 0 ) ;

}



const string LogSource::toString( Ceylan::VerbosityLevels level ) const
{

	if ( hasChannelName() )
		return "This Log source corresponds to the channel [" + getChannelName()
			+ "] and its current level of detail of interest is "
			+ Ceylan::toNumericalString( _level ) ;
	else
		return "This Log source has no registered channel name, "
			"and its current level of detail of interest is "
			+ Ceylan::toNumericalString( _level ) ;

}



void LogSource::unlinkTransport()
{

	/*
	 * A LogSource can share with other LogSources a LogTransport. It should not
	 * therefore be blindly deallocated at this step.
	 *
	 * A removeRef() would be suitable, in the unlikely case where transports
	 * were not permanent and unique services.
	 *
	 */

	/*

	if ( hasTransport() )
	{
		_transport->removeRef() ;
		_transport = 0 ;
	}

	*/

}
