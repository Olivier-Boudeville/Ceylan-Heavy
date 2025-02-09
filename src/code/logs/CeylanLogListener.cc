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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#include "CeylanLogListener.h"


#include "CeylanLogMessage.h"      // for LogMessage
#include "CeylanLogAggregator.h"   // for Aggregator

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"          // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H



using std::string ;

using namespace Ceylan::Log ;



LogListener::LogListener( LogAggregator & aggregator ):
	_aggregator( & aggregator )
{

}



LogListener::~LogListener() throw()
{

}



const string LogListener::toString( Ceylan::VerbosityLevels level ) const
{

	if ( _aggregator == 0 )
		return "This LogListener is not linked with any aggregator" ;

	// An aggregator is available:

	if ( level == Ceylan::high )
		return "This LogListener is linked with following aggregator: "
			+ _aggregator->toString( Ceylan::low ) ;
	else
		return "This LogListener is linked with an aggregator" ;

}



void LogListener::sendToAggregator( LogMessage & message ) const
{

#if CEYLAN_DEBUG

	if ( ! _aggregator )
		throw LogException( "LogListener::sendToAggregator: "
			"trying to send a message whereas "
			"the internal aggregator has not been initialized." ) ;

#endif // CEYLAN_DEBUG

	_aggregator->store( message ) ;

}
