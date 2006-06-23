#include "CeylanLogAggregatorConsole.h"


#include "CeylanOperators.h"
#include "CeylanTimestamp.h"   // for TimeStamp

#include "CeylanLogChannel.h"  // for LogChannel
#include "CeylanLogMessage.h"  // for LogMessage
#include "CeylanLogLight.h"    // for CEYLAN_LOG
#include "CeylanLog.h"         // for MaximumLevelOfDetailForMessage



#include <iostream>            // for cout, endl, cerr, etc.


using std::string ;
using std::list ;

using namespace Ceylan::Log ;
using namespace Ceylan::System ;



LogAggregatorConsole::LogAggregatorConsole( 
		StandardStream consoleStream,
		bool immediateWrite,
		bool useGlobalLevelOfDetail,
		bool beSmart ) 
	throw( LogAggregatorException )	: 
		LogAggregator( beSmart, useGlobalLevelOfDetail ),
		_streamNumber( consoleStream ),
		_outputStream( 0 ),
		_immediateWrite( immediateWrite )
{

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
				"LogAggregatorConsole constructor : "
				"unknown console stream selected." ) ;				
			break ;
			
	}
	
	
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
				"LogAggregatorConsole destructor : "
				<< e.toString() << "." << std::endl ;
				
			// Never throw an exception from a destructor !		
		}	
		
	}
		
}


void LogAggregatorConsole::aggregate() throw( LogAggregatorException ) 
{


	if ( _immediateWrite )
	{
		CEYLAN_LOG( "LogAggregatorConsole::aggregate : "
			"write mode is immediate, nothing to do." ) ;
		return ;	 
	}

	CEYLAN_LOG( "LogAggregatorConsole aggregation started" ) ; 
	
	Timestamp stamp ;
	
	(*_outputStream) << stamp.toString() + " Aggregating Log messages.\n\n" ;
	
	for ( list<LogChannel *>::const_iterator it = _channelList.begin() ;
		it != _channelList.end() ; it ++ )
	{
		(*_outputStream) << * (*it) ;		
	}
	
	(*_outputStream) << stamp.toString() + " Log messages aggregated." ;
	
}


void LogAggregatorConsole::store( LogMessage & message ) throw( LogException )
{

	CEYLAN_LOG( "Storing a new message " + message.toString() ) ;
	
	// Use standard LogAggregator method in all cases, immediate write or not :
	LogAggregator::store( message ) ;

	/*
	 * If immediate write mode is set, let's write this message directly
	 * to the console.
	 *
	 * @note If this aggegator was chosen smart, the message may have a
	 * corrected classname due to ancestor store method.
	 *
	 */
	 
	if ( _immediateWrite )
		write( message ) ;			
		
}
	

void LogAggregatorConsole::write( const LogChannel & channel ) 
	const throw( LogException )
{

	CEYLAN_LOG( "Writing on console channel " + channel.toString() ) ;
	
	(*_outputStream) << '\t' 
		+ channel.toString( getOverallVerbosityLevel() ) ;
	(*_outputStream) << "\n" ;	
	
}


void LogAggregatorConsole::write( const LogMessage & message ) const throw()
{

	CEYLAN_LOG( "Writing on console message " + message.toString() ) ;
		
	(*_outputStream) <<  
		message.toString( getMessageVerbosityLevel( message ) ) + '\n' ;
	
}


const string LogAggregatorConsole::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = string( "This is LogAggregatorConsole in " )
		+ ( _immediateWrite ? "" : "non-" ) 
		+ string( "immediate mode. It " )
		+ ( _useGlobalLevelOfDetail ? "uses" : "does not use" )
		+ string( " a global level of detail for message output. " 
			"It uses the standard " ) ;
	
	switch( _streamNumber )
	{
	
		case Output:
			res += "output" ;
			break ;	 
		
		case Error:
			res += "error" ;
			break ;	 
		
		case Log:
			res += "log" ;
			break ;	 
		
		default:
			res += "unknown (abnormal)" ;
			break ;	 
		
	}
	
	res += " stream. " + LogAggregator::toString( level ) ;
	
	return res ;
	
}	

