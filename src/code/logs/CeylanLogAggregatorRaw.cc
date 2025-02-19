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


#include "CeylanLogAggregatorRaw.h"


#include "CeylanOperators.h"
#include "CeylanFile.h"        // for File
#include "CeylanTimestamp.h"   // for TimeStamp

#include "CeylanLogChannel.h"  // for LogChannel
#include "CeylanLogMessage.h"  // for LogMessage
#include "CeylanLogLight.h"    // for CEYLAN_LOG


// for cout, endl, or for cerr, when log system fails badly:
#include <iostream>


using std::string ;
using std::list ;

using namespace Ceylan::Log ;
using namespace Ceylan::System ;



LogAggregatorRaw::LogAggregatorRaw(
		const string & logFilename,
		bool immediateWrite,
		bool useGlobalLevelOfDetail,
		bool beSmart ) :
	LogAggregator( beSmart, useGlobalLevelOfDetail ),
	_logFilename( logFilename ),
	_outputFile( 0 ),
	_immediateWrite( immediateWrite )
{


	try
	{

		CEYLAN_LOG( "LogAggregatorRaw constructor: creating file "
			+ logFilename ) ;

		// Not wanting binary nor reading:
		OpeningFlag logFlags = File::CreateFile | File::TruncateFile
			| File::Write ;

		// Disable buffering to reduce log loss in case of crash:
		if ( _immediateWrite )
			logFlags |= File::Synchronous ;

		_outputFile = & System::File::Create( logFilename,
			/* OpeningFlag */ logFlags ) ;


	}
	catch( const System::FileCreationFailed & e )
	{

		throw LogAggregatorException( "LogAggregatorRaw constructor: "
			"could not create LogAggregatorRaw output file: " + e.toString() ) ;

	}

}



LogAggregatorRaw::~LogAggregatorRaw() throw()
{

	CEYLAN_LOG( "LogAggregatorRaw destructor called." ) ;


	if ( _beSmart )
	{
		CEYLAN_LOG( "LogAggregatorRaw is smart, therefore "
			"automatically triggers log aggregation on exit." ) ;

		try
		{
			aggregate() ;
		}
		catch( const LogAggregatorException & e )
		{

			std::cerr << "Error while aggregating logs in "
				"LogAggregatorRaw destructor: "
				<< e.toString() << "." << std::endl ;

			// Never throw an exception from a destructor!
		}

	}

	// Automatically closed if needed:
	if ( _outputFile != 0 )
		delete _outputFile ;

}



void LogAggregatorRaw::aggregate()
{

	std::cout << "Logs can be inspected in file "
		<< _logFilename << std::endl ;

	if ( _immediateWrite )
	{
		CEYLAN_LOG( "LogAggregatorRaw::aggregate: "
			"write mode is immediate, nothing to do." ) ;
		return ;
	}

	CEYLAN_LOG( "LogAggregatorRaw aggregation started" ) ;

	Timestamp stamp ;

	_outputFile->write( stamp.toString()
		+ " Aggregating Log messages.\n\n" ) ;

	for ( list<LogChannel *>::const_iterator it = _channelList.begin() ;
		it != _channelList.end(); it ++ )
	{
		write( * (*it) ) ;
	}

	_outputFile->write( stamp.toString() + " Log messages aggregated." ) ;

}



void LogAggregatorRaw::store( LogMessage & message )
{

	CEYLAN_LOG( "Storing a new message " + message.toString() ) ;

	// Use standard LogAggregator method in all cases, immediate write or not:
	LogAggregator::store( message ) ;

	/*
	 * If immediate write mode is set, let's write this message into log file.
	 *
	 * @note If this aggregator has been chosen smart, the message may have a
	 * corrected classname due to ancestor store method.
	 *
	 */

	if ( _immediateWrite )
		write( message ) ;

}



void LogAggregatorRaw::write( const LogChannel & channel ) const
{

	CEYLAN_LOG( "Writing on disk channel " + channel.toString() ) ;

	try
	{

		_outputFile->write( '\t'
			+ channel.toString( getOverallVerbosityLevel() ) + '\n' ) ;

	}
	catch( const OutputStream::WriteFailedException & e )
	{

		throw LogException( "LogAggregatorRaw::write (first) failed: "
			+ e.toString() ) ;

	}

}



void LogAggregatorRaw::write( const LogMessage & message ) const
{

	CEYLAN_LOG( "Writing on disk message " + message.toString() ) ;

	try
	{

		_outputFile->write(
			message.toString( getMessageVerbosityLevel( message ) ) + '\n' ) ;
	}
	catch( const OutputStream::WriteFailedException & e )
	{

		throw LogException( "LogAggregatorRaw::write (second) failed: "
			+ e.toString() ) ;

	}



}



const string LogAggregatorRaw::toString( Ceylan::VerbosityLevels level ) const
{

	return string( "This is LogAggregatorRaw in " )
		+ ( _immediateWrite ? "": "non-" )
		+ string( "immediate mode. It " )
		+ ( _useGlobalLevelOfDetail ? "uses" : "does not use" )
		+ string( " a global level of detail for message output. " )
		+ LogAggregator::toString( level ) ;

}
