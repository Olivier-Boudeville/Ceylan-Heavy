#include "CeylanLogAggregatorRaw.h"


#include "CeylanOperators.h"
#include "CeylanFile.h"        // for File
#include "CeylanTimestamp.h"   // for TimeStamp

#include "CeylanLogChannel.h"  // for LogChannel
#include "CeylanLogMessage.h"  // for LogMessage
#include "CeylanLogLight.h"    // for CEYLAN_LOG


// for cout, endl, or for cerr, when log system fails badly :
#include <iostream>           


using std::string ;
using std::list ;

using namespace Ceylan::Log ;
using namespace Ceylan::System ;


const LevelOfDetail LogAggregatorRaw::DefaultGlobalLevelOfDetail 
	= MaximumLevelOfDetailForMessage ;


LogAggregatorRaw::LogAggregatorRaw( 
		const string & logFilename,
		bool immediateWrite,
		bool useGlobalLevelOfDetail,
		bool beSmart ) 
	throw( LogAggregatorException )	: 
		LogAggregator( beSmart ),
		_logFilename( logFilename ),
		_outputFile( 0 ),
		_immediateWrite( immediateWrite ),
		_useGlobalLevelOfDetail( useGlobalLevelOfDetail ),
		_globalLevelOfDetail( DefaultGlobalLevelOfDetail )
{

	try 
	{
	
		CEYLAN_LOG( "LogAggregatorRaw constructor : creating file "
			+ logFilename ) ;
		
		// Not wanting binary nor reading :
		OpeningFlag logFlags = File::Create | File::Write | File::Truncate ;
		
		// Disable buffering to reduce log loss in case of crash :  
		if ( _immediateWrite )
			logFlags |= File::Synchronous ;
			
		_outputFile = new File( logFilename, /* OpeningFlag */ logFlags ) ;
		
		
	} 
	catch( const File::CouldNotOpen & e )
	{
		throw LogAggregatorException( 
			"Could not open LogAggregatorRaw output file : "
			 + e.toString() ) ;
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
				"LogAggregatorRaw destructor : "
				<< e.toString() << "." << std::endl ;
				
			// Never throw an exception from a destructor !		
		}	
		
	}
		
	// Automatically closed if needed :	
	if ( _outputFile != 0 )
		delete _outputFile ;

}


void LogAggregatorRaw::aggregate() throw( LogAggregatorException ) 
{

	std::cout << "Logs can be inspected in file " 
		<< _logFilename << std::endl ;

	if ( _immediateWrite )
	{
		CEYLAN_LOG( "LogAggregatorRaw::aggregate : "
			"write mode is immediate, nothing to do." ) ;
		return ;	 
	}

	CEYLAN_LOG( "LogAggregatorRaw aggregation started" ) ; 
	
	Timestamp stamp ;
	
	_outputFile->write( stamp.toString() 
		+ " Aggregating Log messages.\n\n" ) ;
	
	for ( list<LogChannel *>::const_iterator it = _channelList.begin() ;
		it != _channelList.end() ; it ++ )
	{
		write( * (*it) ) ;		
	}
	
	_outputFile->write( stamp.toString() + " Log messages aggregated." ) ;
	
}


void LogAggregatorRaw::store( LogMessage & message ) throw( LogException )
{

	CEYLAN_LOG( "Storing a new message " + message.toString() ) ;
	
	// Use standard LogAggregator method in all cases, immediate write or not :
	LogAggregator::store( message ) ;

	/*
	 * If immediate write mode is set, let's write this message into 
	 * log file.
	 *
	 * @note If this aggregator has been chosen smart, the message may have a
	 * corrected classname due to ancestor store method.
	 *
	 */
	 
	if ( _immediateWrite )
		write( message ) ;			
		
}


Ceylan::VerbosityLevels LogAggregatorRaw::getOverallVerbosityLevel() 
	const throw()
{

	LevelOfDetail sourceLevelOfDetail ;
	Ceylan::VerbosityLevels targetChannelLevel ;
	
	
	// Level of detail globally overriden ?
	if ( _useGlobalLevelOfDetail )			
	{
		sourceLevelOfDetail = _globalLevelOfDetail ;
	} 
	else
	{
		sourceLevelOfDetail = MaximumLevelOfDetailForMessage ;
	}
	
	
	// Now map the selected level to a verbosity level :
		
	switch( sourceLevelOfDetail )
	{
	
		case MaximumLevelOfDetailForMessage :
	
			/*
			 * If all is being printed, useless to print filtering
			 * metadatas :
			 *
			 */
			CEYLAN_LOG( "LogAggregatorRaw::getOverallVerbosityLevel : "
				"level of detail will be low." ) ;
			targetChannelLevel = Ceylan::low ;
			break ;
			
			
		case DefaultLevelOfDetailForListener :
		
			CEYLAN_LOG( "LogAggregatorRaw::getOverallVerbosityLevel : "
				"level of detail will be medium." ) ;
			// Let's select medium for usual case :
			targetChannelLevel = Ceylan::medium ;
			break ;
			
			
		default:
			
			CEYLAN_LOG( "LogAggregatorRaw::getOverallVerbosityLevel : "
				"level of detail will be high." ) ;
			// Defaulting to maximum verbosity :		
			targetChannelLevel = Ceylan::high ;
			break ;				
			
	}		
	
	return targetChannelLevel ;
	
}


Ceylan::VerbosityLevels LogAggregatorRaw::getMessageVerbosityLevel( 
		const LogMessage & message ) const throw()
{

	LevelOfDetail sourceLevelOfDetail ;
	Ceylan::VerbosityLevels targetMessageLevel ;
	
	
	// Level of detail globally overriden ?
	if ( _useGlobalLevelOfDetail )			
	{
		sourceLevelOfDetail = _globalLevelOfDetail ;
	} 
	else
	{
		sourceLevelOfDetail = message.getLevelOfDetail() ;
	}
	
	
	// Now map the selected level to a verbosity level :
		
	switch( sourceLevelOfDetail )
	{
	
		case MaximumLevelOfDetailForMessage :
		
			/*
			 * If all is being printed, useless to print filtering
			 * metadatas :
			 *
			 */
			targetMessageLevel = Ceylan::low ;
			break ;
			
			
		case DefaultLevelOfDetailForListener :
		
			// Let's select medium for usual case :
			targetMessageLevel = Ceylan::medium ;
			
			
		default:
			
			// Defaulting to maximum verbosity :		
			targetMessageLevel = Ceylan::high ;
			break ;				
			
	}		
	
	return targetMessageLevel ;
}
	

void LogAggregatorRaw::write( const LogChannel & channel ) 
	const throw( LogException )
{

	CEYLAN_LOG( "Writing on disk channel " + channel.toString() ) ;
	
	_outputFile->write( '\t' 
		+ channel.toString( getOverallVerbosityLevel() ) ) ;
	_outputFile->write( "\n" ) ;	
	
}


void LogAggregatorRaw::write( const LogMessage & message ) const throw()
{

	CEYLAN_LOG( "Writing on disk message " + message.toString() ) ;
		
	_outputFile->write( 
		message.toString( getMessageVerbosityLevel( message ) ) + '\n' ) ;
	
}


const string LogAggregatorRaw::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return string( "This is LogAggregatorRaw in " )
		+ ( _immediateWrite ? "" : "non-" ) 
		+ string( "immediate mode. It " )
		+ ( _useGlobalLevelOfDetail ? "uses" : "does not use" )
		+ string( " a global level of detail for message output. " ) 
		+ LogAggregator::toString( level ) ;

}	

