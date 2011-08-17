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


#include "CeylanLoggable.h"

#include "CeylanLogPlug.h"  // for LogPlug::GetTransport
#include "CeylanLog.h"      // for ProtocolSeparator

// for getEmbeddedURI, getProtocolName,ProtocolSeparator:
#include "CeylanUniformResourceIdentifier.h"



using std::string ;

using namespace Ceylan::Log ;


const string Loggable::ProtocolName = "loggable" ;



Loggable::Loggable( const string & name ) :
	LogSource( LogPlug::GetTransport() )
{

	setChannelName( name ) ;

}



Loggable::~Loggable() throw()
{

}



void Loggable::setChannelName( const std::string & channelName )
{

	LogSource::setChannelName( ProtocolName + Ceylan::URI::ProtocolSeparator
		+ channelName ) ;

}



bool Loggable::IsALoggableChannelName( const string & channelName )
{

	return ( Ceylan::URI::getProtocolName( channelName,
		Ceylan::URI::ProtocolSeparator ) == ProtocolName ) ;

}



const string Loggable::GetEmbeddedChannelName( const string & fullChannelName )
{

	// Removes protocol separator (typically: '//')
	return Ceylan::URI::getEmbeddedURI( fullChannelName,
		Ceylan::URI::ProtocolSeparator ) ;

}
