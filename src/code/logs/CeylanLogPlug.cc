#include "CeylanLogPlug.h"

#include "CeylanLogSource.h"     // for setLevelOfDetail, etc.
#include "CeylanMuteLogSource.h" // for MuteLogSource

#include "CeylanLogTransport.h"  // for Transport
#include "CeylanLogListener.h"   // for Listener
#include "CeylanLogAggregator.h" // for Aggregator

#include "CeylanLogLight.h"      // for CEYLAN_LOG
#include "CeylanDirectory.h"     // for StripFilename
#include "CeylanUtils.h"         // for GetVersion
#include "CeylanStringUtils.h"   // for formatStringList
#include "CeylanOperators.h"     // for Ceylan string operators

#include <iostream>              // for cerr


using std::string ;
using std::list ;


using namespace Ceylan::Log ;


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



/*
 * The few global variables that will be set when the actual plug will be 
 * created:
 *
 */
 
LogSource * LogPlug::InfoLogSource     = 0 ;
LogSource * LogPlug::TraceLogSource    = 0 ;
LogSource * LogPlug::DebugLogSource    = 0 ;
LogSource * LogPlug::WarningLogSource  = 0 ;
LogSource * LogPlug::ErrorLogSource    = 0 ;
LogSource * LogPlug::FatalLogSource    = 0 ;

LogSource * LogPlug::LogrootLogSource  = 0 ;

LogTransport  * LogPlug::Transport  = 0 ;
LogListener   * LogPlug::Listener   = 0 ;
LogAggregator * LogPlug::Aggregator = 0 ;

// Stored by the plug for later use by various libraries (ex: PhysicsFS):
string LogPlug::FullExecutablePath = "" ;


string LogPlug::SourceName = "<no source specified>" ;



const string LogPlug::LogSystemNotInitialized = 
	"Attempt of using the Log system whereas it has not been initialized yet. "
	"Consider using LogPlug<Implementation>::StartService, "
	"for example LogPlugClassical::StartService" ;



void LogPlug::SetInfoLogSource( LogSource & newInfoLogSource ) throw()
{

    InfoLogSource = & newInfoLogSource ;
	
}



LogSource & LogPlug::GetInfoLogSource() throw()
{

    return * InfoLogSource ;
	
}



void LogPlug::SetTraceLogSource( LogSource & newTraceLogSource ) throw()
{

    TraceLogSource = & newTraceLogSource ;
	
}



LogSource & LogPlug::GetTraceLogSource() throw()
{

    return * TraceLogSource ;
	
}



void LogPlug::SetDebugLogSource( LogSource & newDebugLogSource ) throw()
{

    DebugLogSource = & newDebugLogSource ;
	
}



LogSource & LogPlug::GetDebugLogSource() throw()
{

    return * DebugLogSource ;
	
}



void LogPlug::SetWarningLogSource( LogSource & newWarningLogSource ) throw()
{

    WarningLogSource = & newWarningLogSource ;
	
}



LogSource & LogPlug::GetWarningLogSource() throw()
{

    return * WarningLogSource ;
	
}



void LogPlug::SetErrorLogSource( LogSource & newErrorLogSource ) throw()
{

    ErrorLogSource = & newErrorLogSource ;
	
}



LogSource & LogPlug::GetErrorLogSource() throw()
{

    return * ErrorLogSource ;
	
}



void LogPlug::SetFatalLogSource( LogSource & newFatalLogSource ) throw()
{

    FatalLogSource = & newFatalLogSource ;
	
}



LogSource & LogPlug::GetFatalLogSource() throw()
{

    return * FatalLogSource ;
	
}



bool LogPlug::IsFatalLogSourceAvailable() throw()
{

	return ( FatalLogSource != 0 ) ;
	
}



void LogPlug::SetLogRootLogSource( LogSource & newLogRootLogSource ) throw()
{

    LogrootLogSource = & newLogRootLogSource ;
	
}



LogSource & LogPlug::GetLogRootLogSource() throw()
{

    return * LogrootLogSource ;
	
}



void LogPlug::SetTransport( LogTransport & newTransport ) throw()
{

    Transport = & newTransport ;
	
}



LogTransport & LogPlug::GetTransport() throw()
{

    return *Transport ;
	
}



void LogPlug::SetListener( LogListener & newListener ) throw()
{

    Listener = & newListener ;
	
}



LogListener & LogPlug::GetListener() throw()
{

    return * Listener ;
	
}



void LogPlug::SetAggregator( LogAggregator & newAggregator ) throw()
{

    Aggregator = & newAggregator ;
	
}



LogAggregator & LogPlug::GetAggregator() throw()
{

    return * Aggregator ;
	
}



void LogPlug::CheckBlank() throw( LogException )
{

    if ( LogrootLogSource != 0 )
        throw LogException( "LogPlug::StartService: there was already a Log "
			"source assigned to LogPlug::logroot." ) ;

    if ( InfoLogSource != 0 )
        throw LogException( "LogPlug::StartService: there was already a Log "
			"source assigned to LogPlug::info." ) ;

    if ( TraceLogSource != 0 )
        throw LogException( "LogPlug::StartService: there was already a Log "
			"source assigned to LogPlug::trace." ) ;

    if ( DebugLogSource != 0 )
        throw LogException( "LogPlug::StartService: there was already a Log "
			"source assigned to LogPlug::debug." ) ;

    if ( WarningLogSource != 0 )
        throw LogException( "LogPlug::StartService: there was already a Log "
			"source assigned to LogPlug::warning." ) ;

    if ( ErrorLogSource != 0 )
        throw LogException( "LogPlug::StartService: there was already a Log "
			"source assigned to LogPlug::error." ) ;

    if ( FatalLogSource != 0 )
        throw LogException( "LogPlug::StartService: there was already a Log "
			"source assigned to LogPlug::fatal." ) ;

			
    if ( Transport != 0 )
        throw LogException( "LogPlug::StartService: LogPlug::Transport "
			"was already assigned." ) ;
			
    if ( Listener != 0 )
        throw LogException( "LogPlug::StartService: LogPlug::Listener "
			"was already assigned." ) ;

    if ( Aggregator != 0 )
        throw LogException( "LogPlug::StartService: LogPlug::Aggregator "
			"was already assigned." ) ;

}



void LogPlug::SetFullExecutablePath( const string & plugInitiatorFullName )	
	throw ( LogException )
{

	if ( FullExecutablePath.size() != 0 )
    	throw LogException( "LogPlug::SetFullExecutablePath: "
        	"path already set." ) ;
            
    FullExecutablePath = plugInitiatorFullName ;
        
}
    
    
std::string LogPlug::GetFullExecutablePath() throw ( LogException )
{
    
	if ( FullExecutablePath.size() == 0 )
    	throw LogException( "LogPlug::GetFullExecutablePath: "
        	"no path available." ) ;
    
    return FullExecutablePath ;
    
}

                    
                    
std::string LogPlug::GetSpeakerNameFrom( const string & plugInitiatorFullName )
	throw ( LogException )                    
{

	// plugInitiatorFullName is usually argv[0].
	
	/*
	 * If speakerName was 'arguments[0]', then all log files would be 
	 * created alongside the test executable, in the same directory, 
	 * no matter from which directory they were launched.
	 *
	 * For example, if a 'testLockable' executable lies in
	 * 'Ceylan/Ceylan-0.2/bin/interfaces', and if we execute it from
	 * 'Ceylan/Ceylan-0.2/tests-outputs'  (hence with
	 * '../bin/interfaces/testLockable'), with speakerName set to 
	 * 'arguments[0]' the log files would be created under
	 * 'Ceylan/Ceylan-0.2/bin/interfaces'.
	 *
	 * The preferred behaviour would be to write them under current
	 * directory (hence, 'tests-outputs'). 
	 * In this case StripFilename should be used:
	 *
	 */
	string speakerName ;

    
#if CEYLAN_ARCH_NINTENDO_DS

	/*
	 * On the Nintendo DS, argv[0] is a null pointer, a default value is to
	 * be used instead;
	 *
	 */

	speakerName = "DS" ;

#else // CEYLAN_ARCH_NINTENDO_DS
	
    try
    { 	
		
        Ceylan::System::Directory::StripFilename( 
    		/* fullExecutablePath */ plugInitiatorFullName, 
			/* base path */ 0, & speakerName ) ;

	}
    catch( const System::DirectoryException & e )
    {
    	throw LogException( "GetSpeakerNameFrom failed: " + e.toString() ) ;
    }
    
    
#endif // CEYLAN_ARCH_NINTENDO_DS
		
	CEYLAN_LOG( "LogHolder: speaker name for logs will be " + speakerName ) ;
    
    return speakerName ;

}


    
void LogPlug::CreateBasicPlug() throw( LogException )
{
	
	CEYLAN_LOG( "Creating default standard channels." ) ;
		
	if ( Transport == 0 )
		throw LogException( "LogPlug::CreateBasicPlug: "
			"no transport available" ) ;
	
	// These log sources are static variables:
						
	LogrootLogSource = new LogSource( "Log root", *Transport ) ;
	
	FatalLogSource   = new LogSource( "Fatal", *Transport ) ;
	
	ErrorLogSource   = new LogSource( "Error", *Transport ) ;
		
	WarningLogSource = new LogSource( "Warning", *Transport ) ;
	
	DebugLogSource   = new LogSource( "Debug", *Transport ) ;

	TraceLogSource   = new LogSource( "Trace", *Transport ) ;
	
	InfoLogSource    = new LogSource( "Info", *Transport ) ;


	LogrootLogSource->send( "Starting log plug service, from Ceylan "
		+ Ceylan::GetVersion().toString() + "." ) ; 
	
	
	// Avoid having too many logs on the DS small screen:
#if ! CEYLAN_ARCH_NINTENDO_DS

	LogrootLogSource->send( "Fatal standard log channel created." ) ;
	LogrootLogSource->send( "Error standard log channel created." ) ;
	LogrootLogSource->send( "Warning standard log channel created." ) ;
	LogrootLogSource->send( "Debug standard log channel created." ) ;
	LogrootLogSource->send( "Trace standard log channel created." ) ;
	LogrootLogSource->send( "Info standard log channel created." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void LogPlug::CreateNullPlug() throw( LogException )
{
	
	CEYLAN_LOG( "Creating muted null channels." ) ;

	/*
	 * Instances must be created, as LogPlug::info for example dereferences
	 * the associated log source pointer, and would throw an exception should
	 * that pointer be null.
	 *
	 * As many instances should be created as standard channels, since they
	 * will be deallocated as if they were different insteances (no multiple
	 * deletes of the same pointer wanted), in StopService.
	 *
	 */
	 
	LogrootLogSource = new MuteLogSource() ;
	
	FatalLogSource   = new MuteLogSource() ;
	
	ErrorLogSource   = new MuteLogSource() ;
		
	WarningLogSource = new MuteLogSource() ;
	
	DebugLogSource   = new MuteLogSource() ;

	TraceLogSource   = new MuteLogSource() ;
	
	InfoLogSource    = new MuteLogSource() ;
		
}




void LogPlug::StartService( const string & plugCreator ) throw( LogException )
{

	SourceName = plugCreator ;
	
    if ( LogrootLogSource == 0 )
        throw LogException( "No Log source assigned to LogPlug::logroot" ) ;

    if ( InfoLogSource == 0 )
        throw LogException( "No Log source assigned to LogPlug::info" ) ;

    if ( TraceLogSource == 0 )
        throw LogException( "No Log source assigned to LogPlug::trace" ) ;

    if ( DebugLogSource == 0 )
        throw LogException( "No Log source assigned to LogPlug::debug" ) ;

    if ( WarningLogSource == 0 )
        throw LogException( "No Log source assigned to LogPlug::warning" ) ;

    if ( ErrorLogSource == 0 )
        throw LogException( "No Log source assigned to LogPlug::error" ) ;

    if ( FatalLogSource == 0 )
        throw LogException( "No Log source assigned to LogPlug::fatal" ) ;

}



void LogPlug::StopService( bool warnIfAlreadyStopped ) throw()
{

	if ( InfoLogSource != 0 ) 
	{
	
#if ! CEYLAN_ARCH_NINTENDO_DS
		info( "Stopping channel." ) ;
		logroot( "Stopping channel Info." ) ;
#endif // CEYLAN_ARCH_NINTENDO_DS

		delete InfoLogSource ;
		InfoLogSource = 0 ;
	}
	
	
	if ( WarningLogSource != 0  ) 
	{
	
#if ! CEYLAN_ARCH_NINTENDO_DS
		warning( "Stopping channel." ) ;
		logroot( "Stopping channel Warning." ) ;
#endif // CEYLAN_ARCH_NINTENDO_DS

    	delete WarningLogSource ;
		WarningLogSource = 0 ;
	}
	
	
	if ( TraceLogSource != 0  ) 
	{
	
#if ! CEYLAN_ARCH_NINTENDO_DS
		trace( "Stopping channel." ) ;
		logroot( "Stopping channel Trace." ) ;
#endif // CEYLAN_ARCH_NINTENDO_DS

    	delete TraceLogSource ;
		TraceLogSource = 0 ;
	}
	
	
	if ( DebugLogSource != 0  ) 
	{
	
#if ! CEYLAN_ARCH_NINTENDO_DS
		debug( "Stopping channel." ) ;
		logroot( "Stopping channel Debug." ) ;
#endif // CEYLAN_ARCH_NINTENDO_DS

    	delete DebugLogSource ;
		DebugLogSource = 0 ;
	}
		
	
	if ( ErrorLogSource != 0  ) 
	{
	
#if ! CEYLAN_ARCH_NINTENDO_DS
		error( "Stopping channel." ) ;
		logroot( "Stopping channel Error." ) ;
#endif // CEYLAN_ARCH_NINTENDO_DS

    	delete ErrorLogSource ;
		ErrorLogSource = 0 ;
	}
	
	
	if ( FatalLogSource != 0  ) 
	{
	
#if ! CEYLAN_ARCH_NINTENDO_DS
		fatal( "Stopping channel." ) ;
		logroot( "Stopping channel Fatal." ) ;
#endif // CEYLAN_ARCH_NINTENDO_DS

    	delete FatalLogSource ;
		FatalLogSource = 0 ;
	}
	
	
	if ( LogrootLogSource != 0  )
	{
	
#if ! CEYLAN_ARCH_NINTENDO_DS
		logroot( "Stopping log plug service." ) ;
		delete LogrootLogSource ;
#endif // CEYLAN_ARCH_NINTENDO_DS

		LogrootLogSource = 0 ;
	}
	else
	{
	
		if ( warnIfAlreadyStopped )
			std::cerr << "Error in LogPlug::StopService: "
				"no log root channel available, "
				"maybe LogPlug::StopService was called more than once?" 
				<< std::endl ;	
	}
		
}



const string LogPlug::GetSourceName() throw()
{

	return SourceName ;
	
}



const string LogPlug::ToString( Ceylan::VerbosityLevels level ) throw()
{

	
	std::list<string> res ;
		
	if ( Aggregator != 0 )
		res.push_back( Aggregator->toString() ) ;
	else
		res.push_back( "no aggregator connected." ) ;
	
		
	if ( InfoLogSource != 0 )
		res.push_back( InfoLogSource->toString() ) ;
	else
		res.push_back( "no info log source connected." ) ;
	
	if ( TraceLogSource != 0 )
		res.push_back( TraceLogSource->toString() ) ;
	else
		res.push_back( "no trace log source connected." ) ;
	
	if ( DebugLogSource != 0 )
		res.push_back( DebugLogSource->toString() ) ;
	else
		res.push_back( "no debug log source connected." ) ;
	
	if ( WarningLogSource != 0 )
		res.push_back( WarningLogSource->toString() ) ;
	else
		res.push_back( "no warning log source connected." ) ;
	
	if ( ErrorLogSource != 0 )
		res.push_back( ErrorLogSource->toString() ) ;
	else
		res.push_back( "no error log source connected." ) ;
	
	if ( FatalLogSource != 0 )
		res.push_back( FatalLogSource->toString() ) ;
	else
		res.push_back( "no fatal log source connected." ) ;
	
	if ( LogrootLogSource != 0 )
		res.push_back( LogrootLogSource->toString() ) ;
	else
		res.push_back( "no root log source connected." ) ;
	
	if ( Transport != 0 )
		res.push_back( Transport->toString() ) ;
	else
		res.push_back( "no log transport connected." ) ;
	
	if ( Listener != 0 )
		res.push_back( Listener->toString() ) ;
	else
		res.push_back( "no log listener connected." ) ;
	
	
	return "LogSystem status: " + Ceylan::formatStringList( res ) ;

}



void LogPlug::info( const string & message, LevelOfDetail levelOfDetail ) 
	throw( LogException )
{

#if CEYLAN_DEBUG

    if ( InfoLogSource == 0 )
    {
        std::cerr << LogSystemNotInitialized << std::endl << std::flush ;
        throw LogException( LogSystemNotInitialized ) ;
    }

#endif // CEYLAN_DEBUG

    InfoLogSource->send( message, levelOfDetail ) ;
	
}



void LogPlug::trace( const string & message, LevelOfDetail levelOfDetail ) 
	throw( LogException )
{

#if CEYLAN_DEBUG

    if ( TraceLogSource == 0 )
    {
        std::cerr << LogSystemNotInitialized << std::endl << std::flush ;
        throw LogException( LogSystemNotInitialized ) ;
    }

#endif // CEYLAN_DEBUG

    TraceLogSource->send( message, levelOfDetail ) ;
	
}



void LogPlug::debug( const string & message, LevelOfDetail levelOfDetail ) 
	throw( LogException )
{

#if CEYLAN_DEBUG

    if ( DebugLogSource == 0 )
    {
        std::cerr << LogSystemNotInitialized << std::endl << std::flush ;
        throw LogException( LogSystemNotInitialized ) ;
    }

#endif // CEYLAN_DEBUG

    DebugLogSource->send( message, levelOfDetail ) ;
	
}



void LogPlug::warning( const string & message, LevelOfDetail levelOfDetail ) 
	throw( LogException )
{

#if CEYLAN_DEBUG

    if ( WarningLogSource == 0 )
    {
        std::cerr << LogSystemNotInitialized << std::endl << std::flush ;
        throw LogException( LogSystemNotInitialized ) ;
    }

#endif // CEYLAN_DEBUG

    WarningLogSource->send( message, levelOfDetail ) ;
	
}



void LogPlug::error( const string & message, LevelOfDetail levelOfDetail ) 
	throw( LogException )
{

#if CEYLAN_DEBUG

    if ( ErrorLogSource == 0 )
    {
        std::cerr << LogSystemNotInitialized << std::endl << std::flush ;
        throw LogException( LogSystemNotInitialized ) ;
    }

#endif // CEYLAN_DEBUG

    ErrorLogSource->send( message, levelOfDetail ) ;
	
}



void LogPlug::fatal( const string & message, LevelOfDetail levelOfDetail ) 
	throw( LogException )
{

#if CEYLAN_DEBUG

    if ( FatalLogSource == 0 )
    {
        std::cerr << LogSystemNotInitialized << std::endl << std::flush ;
        throw LogException( LogSystemNotInitialized ) ;
    }

#endif // CEYLAN_DEBUG

    FatalLogSource->send( message, levelOfDetail ) ;
	
}



void LogPlug::logroot( const string & message, LevelOfDetail levelOfDetail ) 
	throw( LogException )
{

#if CEYLAN_DEBUG

    if ( LogrootLogSource == 0 )
    {
        std::cerr << LogSystemNotInitialized << std::endl << std::flush ;
        throw LogException( LogSystemNotInitialized ) ;
    }

#endif // CEYLAN_DEBUG

    LogrootLogSource->send( message, levelOfDetail ) ;
	
}




LogPlug::LogPlug() throw( LogException )
{

	throw LogException( 
		"Ceylan::Log::LogPlug should not be instanciated directly." ) ;
		
}



LogPlug::~LogPlug() throw()
{

}

