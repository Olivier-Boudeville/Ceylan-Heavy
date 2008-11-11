#include "CeylanLogAggregatorHTML.h"


#include "CeylanOperators.h"
#include "CeylanFile.h"          // for File
#include "CeylanHolder.h"        // for Holder
#include "CeylanDirectory.h"     // for Directory
#include "CeylanTimestamp.h"
#include "CeylanStringUtils.h"   // for substituteInString

#include "CeylanLogPlug.h"
#include "CeylanLogChannel.h"
#include "CeylanLogMessage.h"
#include "CeylanLogLight.h"


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"        // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


// for cout, endl or for cerr, when log system fails badly:
#include <iostream>            


using std::string ;
using std::list ;

using namespace Ceylan::Log ;
using namespace Ceylan::System ;


#include "CeylanLogAggregatorHTMLFragments.h"


  
const LevelOfDetail LogAggregatorHTML::DefaultGlobalLevelOfDetail 
	= MaximumLevelOfDetailForMessage ;

const string LogAggregatorHTML::HTMLPageSuffix = ".html" ;



LogAggregatorHTML::LogAggregatorHTML( 
		const string & callerDescription,
		const string & logDirectoryName,
		bool useGlobalLevelOfDetail,
		bool beSmart ) 
	throw( LogAggregatorException )	: 
		LogAggregator( beSmart, useGlobalLevelOfDetail ),
		_callerDescription( callerDescription ),
		_logDirectoryName( logDirectoryName ),
		_outputDirectory( 0 )
{

		
	try 
	{
	
		/*
		 * Constructs, if possible, a reference to the specified 
		 * directory, creating it if needed.
		 *
		 */
		  
		CEYLAN_LOG( "LogAggregatorHTML constructor: creating directory '"
			+ _logDirectoryName + "'" ) ;
			
		_outputDirectory = & System::Directory::Create( _logDirectoryName ) ;
				
	} 
	catch( const System::DirectoryException & e )
	{
		throw LogAggregatorException( "LogAggregatorHTML constructor: "
			"could not create LogAggregatorHTML output directory, "
			+ _logDirectoryName + ": " + e.toString() ) ;
	}	

}


LogAggregatorHTML::~LogAggregatorHTML() throw() 
{

	CEYLAN_LOG( "LogAggregatorHTML destructor called" ) ;
	
	if ( _beSmart )
	{
	
		CEYLAN_LOG( "LogAggregatorHTML is smart, "
			"therefore automatically triggers log aggregation on exit." ) ;
			
		try 
		{	
			aggregate() ;
		}
		catch( const LogAggregatorException & e )
		{
		
			std::cerr << "Error while aggregating logs "
				"in LogAggregatorHTML destructor: "
				<< e.toString() << std::endl ;
			// Never throw an exception from a destructor !		
		}
			
	}
			
	if ( _outputDirectory != 0 )
		delete _outputDirectory ;
		
}


void LogAggregatorHTML::aggregate() throw( LogAggregatorException ) 
{

	CEYLAN_LOG( "LogAggregatorHTML aggregation started" ) ; 

	
#if CEYLAN_DEBUG
	if ( _outputDirectory == 0 )
		throw LogAggregatorException( "LogAggregatorHTML::aggregate: "
			"null output directory pointer." ) ;
#endif // CEYLAN_DEBUG

	try
	{
		
		Timestamp stampBegin ;
	
	
		// First, the frameset:
		File & framesetPage = File::Create( 
			Directory::JoinPath( _outputDirectory->getPath(), 
				"index" + HTMLPageSuffix ) ) ;
	
		string newFrameset = FrameSet ;
		
		Ceylan::substituteInString( newFrameset, "ST_CALLER_DESCRIPTION", 
			Ceylan::encodeToHTML( _callerDescription ) ) ; 
			
		framesetPage.write( newFrameset ) ;
		
		delete & framesetPage ;
	
	
		/*
		 * Second, let's begin the default page (use holders to manage files
		 * life-cycle):
		 *
		 */
		Holder<File> defaultPageFileHolder( File::Create(
			Directory::JoinPath( _outputDirectory->getPath(),
				"MainLog" + HTMLPageSuffix ) ) ) ;
		
	
		defaultPageFileHolder->write( DefaultPageHeader ) ;
	
		defaultPageFileHolder->write( "Log plug session initiated for source " 
			+ LogPlug::GetSourceName() ) ;
		
		defaultPageFileHolder->write( "<p>" + stampBegin.toString() 
			+ " Aggregation of Log messages started.</p>\n" ) ;

	
		/*
		 * Third, builds in parallel the browser menu and the pages it 
		 * references:
		 *
		 */
		Holder<File> logBrowserMenuFileHolder( File::Create(
			Directory::JoinPath( _outputDirectory->getPath(),
				"LogSystem" + HTMLPageSuffix ) ) ) ;
	
		logBrowserMenuFileHolder->write( MenuHeader ) ;
		
	
		for ( list<LogChannel *>::const_iterator it = _channelList.begin() ;
			it != _channelList.end() ; it++ )
		{
	
			CEYLAN_LOG( "Creating page for channel " + (*it)->getName()
				+ " which has " + Ceylan::toString( (*it)->getMessageCount() )
				+ " messsage(s)." ) ;
			
			// Add a menu reference, each channel with its message count:
			logBrowserMenuFileHolder->write( 
				"<tr><td>[<a href=\"" 
				+ File::TransformIntoValidFilename( (*it)->getName() )
				+ ".html\" target=\"mainFrame\">"
				+ Ceylan::toString( (*it)->getMessageCount() )
				+ "</a>]</td><td><a href=\"" 
				+ File::TransformIntoValidFilename( (*it)->getName() )
				+ ".html\" target=\"mainFrame\">" 
				+ Ceylan::encodeToHTML( (*it)->getName() ) + "</a></td></tr>\n" 
			) ;
			
			// Builds the corresponding channel page: 	
			write( * (*it) ) ;
		
		}
	
	
		logBrowserMenuFileHolder->write( MenuFooter ) ;


		// Let's finish the default page.
		Timestamp stampEnd ;
		defaultPageFileHolder->write( "<p>" + stampEnd.toString() 
			+ " Aggregation of Log messages ended.</p>\n" ) ;
		defaultPageFileHolder->write( DefaultPageFooter ) ;
	
		std::cout << "Logs can be inspected from file://"
			<< _outputDirectory->getPath() << "/index.html" << std::endl ;
		
		// Automatic files will be automatically closed thanks to holders.
		
	} 
	catch( const SystemException & e )
	{
		
		// This exception covers all file and directories operations:
		
		throw LogAggregatorException( "LogAggregatorHTML::aggregate: "
			"file-related operation failed: " + e.toString() ) ;
		
	}			
	
}


void LogAggregatorHTML::store( LogMessage & message ) throw( LogException )
{

	CEYLAN_LOG( "Storing a new message " + message.toString() ) ;
		
	// Use standard LogAggregator method in all cases (no immediate write):
	LogAggregator::store( message ) ;

}


const string LogAggregatorHTML::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{
	return "This is an HTML log aggregator. " 
		+ LogAggregator::toString( level ) ;

}	





// Write section.


void LogAggregatorHTML::write( const LogChannel & channel )
	const throw( LogException )
{

	CEYLAN_LOG( "Writing on disk channel " + channel.toString() ) ;
		

	Holder<File> logChannelPageHolderFile( File::Create(
		Directory::JoinPath( _outputDirectory->getPath(),
			File::TransformIntoValidFilename( channel.getName() )
				+ HTMLPageSuffix ) ) ) ;

	LevelOfDetail sourceLevelOfDetail ;
	
	WriteChannelHeader( channel, logChannelPageHolderFile.get() ) ;
		
	// Level of detail globally overriden ?
	if ( _useGlobalLevelOfDetail )			
		sourceLevelOfDetail = _globalLevelOfDetail ;
	else
		sourceLevelOfDetail = MaximumLevelOfDetailForMessage ;
			
	for ( list<LogMessage *>::const_iterator it = 
		channel._messages.begin(); it != channel._messages.end() ; it++ )
	{
	
#if CEYLAN_DEBUG
		if ( (*it) == 0 )
		{
			CEYLAN_LOG( "Error, LogAggregatorHTML::write: "
				"null pointer in message list, skipping." ) ;
			break ;
		}	
#endif // CEYLAN_DEBUG

		write( * (*it), logChannelPageHolderFile.get() ) ;
		
	}	
	
	WriteChannelFooter( channel, logChannelPageHolderFile.get() ) ;
			

}


void LogAggregatorHTML::write( const LogMessage & message, 
	Ceylan::System::File & targetFile ) const throw( LogException )
{

	CEYLAN_LOG( "Writing on disk message " + message.toString() ) ;
	
	try
	{
	
		/*
		 * HTML logs are clear and separated, no level-of-detail filtering:
		 * everything is written.
		 *
		 */	
		targetFile.write( "<li>" 
			+ /* Ceylan::encodeToHTML( */ message.getPreformattedText() /* ) */
			+ "</li>\n" ) ;
			
	}
	catch( const OutputStream::WriteFailedException & e )
	{

		throw LogException( "LogAggregatorHTML::write failed: "
			+ e.toString() ) ;
	
	}
	
}


void LogAggregatorHTML::WriteChannelHeader( const LogChannel & channel,
	Ceylan::System::File & targetFile ) throw( LogException )
{

	string newHeader = ChannelHeader ;
	
	Ceylan::substituteInString( newHeader, "ST_CHANNEL_NAME", 
		Ceylan::encodeToHTML( channel.getName() ) ) ; 
	
	try
	{
	
		targetFile.write( newHeader ) ;
		
	}
	catch( const OutputStream::WriteFailedException	& e )
	{
		throw LogException( "LogAggregatorHTML::WriteChannelHeader failed: "
			+ e.toString() ) ;
	}
	
	
}


void LogAggregatorHTML::WriteChannelFooter( const LogChannel & channel,
	Ceylan::System::File & targetFile ) throw( LogException )
{

	string newFooter = ChannelFooter ;
	
	Ceylan::substituteInString( newFooter, "ST_AGGREGATION_DATE", 
		Ceylan::encodeToHTML( timeToString( getTime() ) ) ) ; 
	
	try
	{
	
		targetFile.write( newFooter ) ;
		
	}
	catch( const OutputStream::WriteFailedException	& e )
	{
		throw LogException( "LogAggregatorHTML::WriteChannelFooter failed: "
			+ e.toString() ) ;
	}
	
		
}

