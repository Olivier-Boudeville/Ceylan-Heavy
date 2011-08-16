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


#include "CeylanLogPlugNull.h"


#include "CeylanLogSource.h"                 // for LogSource
#include "CeylanLogTransportListenerNull.h"  // for Log transports
#include "CeylanLogAggregatorRaw.h"          // for Log aggregators

#include "CeylanOperators.h"                 // for toString operator
#include "CeylanLogLight.h"                  // for CEYLAN_LOG




using std::string ;


using namespace Ceylan::Log ;


void LogPlugNull::StartService()
{

	CEYLAN_LOG( "Starting LogPlug null service." ) ;

	// Plug should start empty:
	LogPlug::CheckBlank() ;

	LogPlug::Transport = new LogTransportListenerNull() ;

	LogPlug::CreateNullPlug() ;

	// Last check before service is open:
	LogPlug::StartService( "(null plug)" ) ;

}



void LogPlugNull::StopService()
{

  LogPlug::StopService() ;

  CEYLAN_LOG( "Stopping transport." ) ;
  if ( LogPlug::Transport != 0 )
  {

	delete LogPlug::Transport ;
	LogPlug::Transport = 0 ;

  }

}



const string LogPlugNull::ToString( Ceylan::VerbosityLevels level )
{

	string result = "LogSystem status: using null plug." ;

	if ( level != Ceylan::low )
		result += LogPlug::ToString( level ) ;

	return result ;

}
