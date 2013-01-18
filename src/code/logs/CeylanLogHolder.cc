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


#include "CeylanLogHolder.h"

// All plugs needed, to be able to pick one of them:
#include "CeylanLogPlugConsole.h"   // for console plug
#include "CeylanLogPlugClassical.h" // for classical plug
#include "CeylanLogPlugHTML.h"      // for HTML plug
#include "CeylanLogPlugNull.h"      // for null plug


#include "CeylanOperators.h"        // for string operators
#include "CeylanLogLight.h"         // for CEYLAN_LOG
#include "CeylanUtils.h"            // for ExitFailure, emergencyShutdown
#include "CeylanTextDisplayable.h"  // for SetOutputFormat


#include <iostream>                 // for cerr, if abnormal situation occurs


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"           // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


using std::string ;

using namespace Ceylan::Log ;


const string LogHolder::ConsolePlugOption   = "--consolePlug" ;
const string LogHolder::ClassicalPlugOption = "--classicalPlug" ;
const string LogHolder::HTMLPlugOption      = "--HTMLPlug" ;
const string LogHolder::NullPlugOption      = "--nullPlug" ;



#if CEYLAN_ARCH_NINTENDO_DS

KnownPlugs LogHolder::DefaultPlug = consolePlug ;

#else // CEYLAN_ARCH_NINTENDO_DS

KnownPlugs LogHolder::DefaultPlug = classicalPlug ;

#endif // CEYLAN_ARCH_NINTENDO_DS



LogHolder::LogHolder( Ceylan::Uint16 argCount,
		const char * const arguments[], bool immediateWrite ) :
	_chosenPlug( DefaultPlug )
{

	for ( Ceylan::Uint16 i = 0; i < argCount; i++ )
	{

		if ( arguments[ i ] == ConsolePlugOption )
		{
			TextDisplayable::SetOutputFormat(
				Ceylan::TextDisplayable::rawText ) ;
			_chosenPlug = consolePlug ;
			break ;
		}

		if ( arguments[ i ] == ClassicalPlugOption )
		{
			TextDisplayable::SetOutputFormat(
				Ceylan::TextDisplayable::rawText ) ;
			_chosenPlug = classicalPlug ;
			break ;
		}

		if ( arguments[ i ] == HTMLPlugOption )
		{
			TextDisplayable::SetOutputFormat( Ceylan::TextDisplayable::html ) ;
			_chosenPlug = HTMLPlug ;
			break ;
		}

		if ( arguments[ i ] == NullPlugOption )
		{
			_chosenPlug = nullPlug ;
			break ;
		}

		// Do not touch argument if not recognized.

	}

	// Records that path as some libraries (ex: PhysicsFS) need it:
	LogPlug::SetFullExecutablePath( arguments[0] ) ;

	string speakerName = LogPlug::GetSpeakerNameFrom( arguments[0] ) ;


	/*
	 * LogHolder is usually out of a try/catch pair, avoid propagating
	 * exception:
	 *
	 */

	try
	{

		switch( _chosenPlug )
		{

			case consolePlug:
				CEYLAN_LOG( "LogHolder: using the console plug." ) ;
				LogPlugConsole::StartService( speakerName, immediateWrite ) ;
				break ;

			case classicalPlug:
				CEYLAN_LOG( "LogHolder: using the classical plug." ) ;
				LogPlugClassical::StartService( speakerName, immediateWrite ) ;
				break ;

			case HTMLPlug:
				CEYLAN_LOG( "LogHolder: using the HTML plug." ) ;
				// No immediateWrite for LogPlugHTML:
				LogPlugHTML::StartService( speakerName ) ;
				break ;

			case nullPlug:
				CEYLAN_LOG( "LogHolder: using the null plug." ) ;
				LogPlugNull::StartService() ;
				break ;


			default:
				CEYLAN_LOG( "Warning: LogHolder: "
					"no known plug specified, defaulting to classical." ) ;
				LogPlugClassical::StartService( speakerName, immediateWrite ) ;
				break ;

		}

	}
	catch( const Ceylan::Exception & e )
	{
		Ceylan::emergencyShutdown( "LogHolder constructor failed: "
			+ e.toString() ) ;
	}

}



LogHolder::~LogHolder() throw()
{

	switch( _chosenPlug )
	{

		case consolePlug:
			CEYLAN_LOG( "LogHolder: stopping the console plug." ) ;
			LogPlugConsole::StopService() ;
			break ;

		case classicalPlug:
			CEYLAN_LOG( "LogHolder: stopping the classical plug." ) ;
			LogPlugClassical::StopService() ;
			break ;

		case HTMLPlug:
			CEYLAN_LOG( "LogHolder: stopping the HTML plug." ) ;
			LogPlugHTML::StopService() ;
			break ;

		case nullPlug:
			CEYLAN_LOG( "LogHolder: stopping the null plug." ) ;
			LogPlugNull::StopService() ;
			break ;

		default:
			std::cerr << "LogHolder destructor: "
				"no valid plug available! Aborting." << std::endl ;
			::exit( Ceylan::ExitFailure ) ;
			break ;

	}

}



const string LogHolder::toString( Ceylan::VerbosityLevels level ) const
{

	switch( _chosenPlug )
	{

		case consolePlug:
			return "LogHolder uses the console plug" ;
			// break ;

		case classicalPlug:
			return "LogHolder uses the classical plug" ;
			// break ;

		case HTMLPlug:
			return "LogHolder uses the HTML plug" ;
			// break ;

		case nullPlug:
			return "LogHolder uses the null plug" ;
			// break ;

		default:
			return "LogHolder uses an unknown plug! (abnormal)" ;
			// break ;

	}

}



bool LogHolder::IsAKnownPlugOption( const std::string & option )
{

	if ( option == ConsolePlugOption )
		return true ;

	if ( option == ClassicalPlugOption )
		return true ;

	if ( option == HTMLPlugOption )
		return true ;

	if ( option == NullPlugOption )
		return true ;

	return false ;

}
