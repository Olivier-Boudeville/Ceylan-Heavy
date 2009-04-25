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


#include "CeylanLogPlugHTML.h"

#include "CeylanLogSource.h"
#include "CeylanLogTransportListenerRaw.h"
#include "CeylanLogAggregatorHTML.h"


#include "CeylanOperators.h"
#include "CeylanLogLight.h"



using std::string ;


using namespace Ceylan::Log ;


const string & LogPlugHTML::LogDirectorySuffix = "-logs" ;



void LogPlugHTML::StartService( const string & plugInitiator, bool smart ) 
{
		
	CEYLAN_LOG( "Starting LogPlug HTML service: "
		"creating aggregator and transport." ) ;
	
	// Plug should start empty:
 	LogPlug::CheckBlank() ;

	// Start by the end of the chain and go back to its beginning:
	
	LogPlug::Aggregator = new LogAggregatorHTML( 
		/* caller description */         plugInitiator,
		/* log ouput directory */        plugInitiator + LogDirectorySuffix,
		/* use Global Level Of Detail */ true, 
		/* be smart */                   smart ) ;
	
	
	/* 
	 * Listener remains blank, since it is integrated with the transport,
	 * with the HTML scheme.
	 *
	 */
	
	LogPlug::Transport = new LogTransportListenerRaw( * LogPlug::Aggregator ) ;
	
	// Creates basic standard channels:
	LogPlug::CreateBasicPlug() ;
	
	// Last check before service is open:
	LogPlug::StartService( plugInitiator ) ;

}



void LogPlugHTML::StopService() throw()
{

	LogPlug::StopService() ;

	CEYLAN_LOG( "Stopping transport and listener." ) ;
	delete LogPlug::Transport ; 
	LogPlug::Transport = 0 ;
	// listener is embedded in transport.
	
	CEYLAN_LOG( "Stopping aggregator." ) ;
	delete LogPlug::Aggregator ; 
	LogPlug::Aggregator = 0 ;
	
}



const string LogPlugHTML::ToString( Ceylan::VerbosityLevels level ) throw()
{

	string result = "LogSystem status: using HTML plug" ;
	
	if ( level != Ceylan::low )
		result += ". " + LogPlug::ToString( level ) ;
	
	return result ;

}



/* 
 *
 * Not even defined: 
 *

LogPlugHTML::LogPlugHTML()
{
	throw LogException( 
		"Ceylan::Log::LogPlugHTML should not be instanciated." ) ;
}



LogPlugHTML::~LogPlugHTML() throw()
{

}

 */

