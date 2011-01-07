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


#include "CeylanLogPlugClassical.h"


#include "CeylanLogSource.h"                 // for LogSource
#include "CeylanLogTransportListenerRaw.h"   // for Log transports
#include "CeylanLogAggregatorRaw.h"          // for Log aggregators

#include "CeylanOperators.h"                 // for toString operator
#include "CeylanLogLight.h"                  // for CEYLAN_LOG




using std::string ;


using namespace Ceylan::Log ;



void LogPlugClassical::StartService( const string & plugInitiatorName,  
	bool immediateWrite, bool smart )
{
	
	CEYLAN_LOG( "Starting LogPlug classical service: "
		"creating aggregator and transport." ) ;
	
	// Plug should start empty:
 	LogPlug::CheckBlank() ;
    
    string plugInitiator = LogPlug::GetSpeakerNameFrom( plugInitiatorName ) ;
    
	// Start by the end of the chain and go back to its beginning:
	
	LogPlug::Aggregator = new LogAggregatorRaw( 
		/* log ouput file */             plugInitiator + ".log",
		/* immediate Write */            immediateWrite,
		/* use Global Level Of Detail */ true, 
		/* be smart */                   smart ) ;
	
	/*
	 * Listener remains blank, since it is integrated with the 
	 * transport, with this classical scheme.
	 *
	 */
	
	LogPlug::Transport = new LogTransportListenerRaw( * LogPlug::Aggregator ) ;	
	
	// Creates basic standard channels:
	LogPlug::CreateBasicPlug() ;
	
	// Last check before service is open:
	LogPlug::StartService( plugInitiator ) ;

}



void LogPlugClassical::StopService()
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



const string LogPlugClassical::ToString( Ceylan::VerbosityLevels level )
{

	string result = "LogSystem status: using classical plug." ;
	
	if ( level != Ceylan::low )
		result += LogPlug::ToString( level ) ;
	
	return result ;

}

