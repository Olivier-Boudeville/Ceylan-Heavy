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


#include "CeylanLogAggregatorConsole.h"


#include "CeylanLogChannel.h"  // for LogChannel
#include "CeylanLogMessage.h"  // for LogMessage
#include "CeylanLogLight.h"    // for CEYLAN_LOG
#include "CeylanLog.h"         // for MaximumLevelOfDetailForMessage

#include "CeylanOperators.h"
#include "CeylanTimestamp.h"   // for TimeStamp
#include "CeylanStringUtils.h" // for display
#include "CeylanConsole.h"     // for Console


#include <iostream>            // for cout, endl, cerr, etc.


using std::string ;
using std::list ;

using namespace Ceylan::Log ;
using namespace Ceylan::System ;



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



/*
 * In the case of the Nintendo DS, a convenient way of debugging is to use
 * one of the two screens as a debug console. 
 * To allow to look at all logs, even if they are produced quickly, a console
 * (CeylanConsole) is used. It is created as soon as the LogAggregatorConsole
 * is created, and, when this instance is deleted, it gives the opportunity to
 * watch the stored logs.
 *
 * Hence a program using a LogHolder, whether it stops on error or not, should
 * be able in most cases to let the user consult the logs before stopping.
 *
 */

LogAggregatorConsole::LogAggregatorConsole( 
		StandardStream consoleStream,
		bool immediateWrite,
		bool useGlobalLevelOfDetail,
		bool beSmart ) : 
	LogAggregator( beSmart, useGlobalLevelOfDetail ),
	_streamNumber( consoleStream ),
	_outputStream( 0 ),
	_console( 0 ),
	_immediateWrite( immediateWrite )
{

#if CEYLAN_ARCH_NINTENDO_DS

	// Use a default console that is/will be displayed on bottom screen:
	_console = new System::Console( /* startInForeground */ false ) ;
	
	// Collect silently messages in the background or be visible at once:
	if ( _immediateWrite )
		_console->setToForeground( true ) ;
				
#else // CEYLAN_ARCH_NINTENDO_DS
		
	switch( consoleStream )
	{
	
		case Output:
			_outputStream = & std::cout ;
			break ;
			
		case Error:
			_outputStream = & std::cerr ;
			break ;
			
		case Log:
			_outputStream = & std::clog ;
			break ;
		
		default:	
			throw LogAggregatorException( 
				"LogAggregatorConsole constructor: "
				"unknown console stream selected." ) ;				
			break ;
			
	}
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



LogAggregatorConsole::~LogAggregatorConsole() throw() 
{

	CEYLAN_LOG( "LogAggregatorConsole destructor called." ) ;
	
			
	if ( _beSmart )
	{
	
		CEYLAN_LOG( "LogAggregatorConsole is smart, therefore "
			"automatically triggers log aggregation on exit." ) ;
		
		try 
		{	
			aggregate() ;
		}
		catch( const LogAggregatorException & e )
		{
		
			std::cerr << "Error while aggregating logs in "
				"LogAggregatorConsole destructor: "
				<< e.toString() << "." << std::endl ;
				
			// Never throw an exception from a destructor!		
		}	
		
	}
	
	
	if ( _console != 0 )
	{
	
		try
		{
		
			// Allows to read the logs:
			_console->goInteractive() ;
			
			// Notify shutdown:
			_console->blankBuffer() ;
			_console->addInBuffer( "Log system stopped on user request." ) ;
			
			
		}
		catch( const System::Console::ConsoleException & e )
		{
		
			display( "LogAggregatorConsole shutdown failed: " + e.toString() ) ;
			
		}
		
		// When finished with it:
		delete _console ;
		
	}	
	
}



void LogAggregatorConsole::aggregate() 
{


	if ( _immediateWrite )
	{
		CEYLAN_LOG( "LogAggregatorConsole::aggregate: "
			"write mode is immediate, nothing to do." ) ;
		return ;	 
	}

	CEYLAN_LOG( "LogAggregatorConsole aggregation started" ) ; 

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	// Would not really make sense here:
	throw UtilsException( "LogAggregatorConsole::aggregate: "
		"not available on the Nintendo DS ARM7." ) ;
		
#else // CEYLAN_RUNS_ON_ARM7

	/*
	 * Even in non-immediate write mode, nothing special is to be done here,
	 * as, on the DS, messages have already been collected in the text buffer
	 * of the console.
	 *
	 */
	return ;
	
#endif // CEYLAN_RUNS_ON_ARM7
	
#else // CEYLAN_ARCH_NINTENDO_DS

	Timestamp stamp ;
	
	(*_outputStream) << stamp.toString() + " Aggregating Log messages.\n\n" ;
	
	for ( list<LogChannel *>::const_iterator it = _channelList.begin() ;
		it != _channelList.end() ; it++ )
	{
		(*_outputStream) << * (*it) ;		
	}
	
	(*_outputStream) << stamp.toString() + " Log messages aggregated." ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS

}



void LogAggregatorConsole::store( LogMessage & message )
{

	CEYLAN_LOG( "Storing a new message " + message.toString() ) ;
	
	/*
	 * Previously the LogAggregator::store method was used whether immediate
	 * writes were enabled or not, but, if they were enabled, storing messages
	 * twice was useless.
	 *
	 * @note If this aggregator was chosen smart, the message may have a
	 * corrected classname due to ancestor store method.
	 *
	 */

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	// Would not really make sense here:
	throw LogException( "LogAggregatorConsole::store: "
		"not available on the Nintendo DS ARM7." ) ;
		
#else // CEYLAN_RUNS_ON_ARM7

	/*
	 * On the DS ARM9, with immediate write or not, messages are to be stored
	 * in the console text buffer, that will display them at once (if set to
	 * foreground, i.e. in immediate mode), or store them for a later 
	 * displaying.
	 *
	 */
	write( message ) ;	
		
	
#endif // CEYLAN_RUNS_ON_ARM7
	
#else // CEYLAN_ARCH_NINTENDO_DS
	 
	if ( _immediateWrite )
		write( message ) ;	
	else			
		LogAggregator::store( message ) ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS
		
}
	


void LogAggregatorConsole::write( const LogChannel & channel ) const
{

	CEYLAN_LOG( "Writing on console channel " + channel.toString() ) ;

#if CEYLAN_ARCH_NINTENDO_DS

	_console->addInBuffer( channel.toString( getOverallVerbosityLevel() ) ) ;
	
#else // CEYLAN_ARCH_NINTENDO_DS

	(*_outputStream) << '\t' 
		+ channel.toString( getOverallVerbosityLevel() ) ;
	(*_outputStream) << "\n" ;	

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



void LogAggregatorConsole::write( const LogMessage & message ) const
{

	CEYLAN_LOG( "Writing on console message " + message.toString() ) ;

#if CEYLAN_ARCH_NINTENDO_DS

	_console->addInBuffer( 
		message.toString( getMessageVerbosityLevel( message ) ) ) ;
			
#else // CEYLAN_ARCH_NINTENDO_DS
		
	(*_outputStream) <<  
		message.toString( getMessageVerbosityLevel( message ) ) + '\n' ;

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



const string LogAggregatorConsole::toString( Ceylan::VerbosityLevels level ) 
	const
{

	string res = string( "This is LogAggregatorConsole in " )
		+ ( _immediateWrite ? "": "non-" ) 
		+ string( "immediate mode. It " )
		+ ( _useGlobalLevelOfDetail ? "uses" : "does not use" )
		+ string( " a global level of detail for message output. It uses " ) ;

#if CEYLAN_ARCH_NINTENDO_DS
	
	res += "a Ceylan text console" ;
				
#else // CEYLAN_ARCH_NINTENDO_DS

	switch( _streamNumber )
	{
	
		case Output:
			res += "the standard output stream" ;
			break ;	 
		
		case Error:
			res += "the standard error stream" ;
			break ;	 
		
		case Log:
			res += "the standard log stream" ;
			break ;	 
		
		default:
			res += "an unknown stream (abnormal)" ;
			break ;	 
		
	}
	
#endif // CEYLAN_ARCH_NINTENDO_DS

	res += ". " + LogAggregator::toString( level ) ;
	
	return res ;
	
}	

