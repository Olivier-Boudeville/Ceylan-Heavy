#include "CeylanLogHolder.h"

// All plugs needed, to be able to pick one of them:
#include "CeylanLogPlugConsole.h"   // for console plug
#include "CeylanLogPlugClassical.h" // for classical plug
#include "CeylanLogPlugHTML.h"      // for HTML plug


#include "CeylanOperators.h"        // for string operators
#include "CeylanLogLight.h"         // for CEYLAN_LOG
#include "CeylanUtils.h"            // for ExitFailure, emergencyShutdown
#include "CeylanTextDisplayable.h"  // for SetOutputFormat


#include <iostream>                 // for cerr, if abnormal situation occurs


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


using std::string ;

using namespace Ceylan::Log ;


const string LogHolder::ConsolePlugOption   = "--consolePlug" ;
const string LogHolder::ClassicalPlugOption = "--classicalPlug" ;
const string LogHolder::HTMLPlugOption      = "--HTMLPlug" ;


#if CEYLAN_ARCH_NINTENDO_DS

KnownPlugs LogHolder::DefaultPlug = consolePlug ;

#else // CEYLAN_ARCH_NINTENDO_DS

KnownPlugs LogHolder::DefaultPlug = classicalPlug ;

#endif // CEYLAN_ARCH_NINTENDO_DS



LogHolder::LogHolder( Ceylan::Uint16 argCount, 
	const char * const arguments[], bool immediateWrite ) 
		throw( LogException ):
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
				CEYLAN_LOG( "LogHolder: using console plug." ) ;
				LogPlugConsole::StartService( speakerName, immediateWrite ) ;
				break ;
	
			case classicalPlug:
				CEYLAN_LOG( "LogHolder: using classical plug." ) ;
				LogPlugClassical::StartService( speakerName, immediateWrite ) ;
				break ;
	
			case HTMLPlug:
				CEYLAN_LOG( "LogHolder: using HTML plug." ) ;
				// No immediateWrite for LogPlugHTML:
				LogPlugHTML::StartService( speakerName ) ;
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
			CEYLAN_LOG( "LogHolder: stopping console plug." ) ;
			LogPlugConsole::StopService() ;
			break ;
	
		case classicalPlug:
			CEYLAN_LOG( "LogHolder: stopping classical plug." ) ;
			LogPlugClassical::StopService() ;
			break ;
	
		case HTMLPlug:
			CEYLAN_LOG( "LogHolder: stopping HTML plug." ) ;
			LogPlugHTML::StopService() ;
			break ;
			
		default:	
			std::cerr << "LogHolder destructor: "
				"no valid plug available ! Aborting." << std::endl ;
			::exit( Ceylan::ExitFailure ) ;
			break ;
	
	}

}


const string LogHolder::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	switch( _chosenPlug )
	{
	
		case consolePlug:
			return "LogHolder uses console plug" ;
			// break ;
			
		case classicalPlug:
			return "LogHolder uses classical plug" ;
			// break ;
	
		case HTMLPlug:
			return "LogHolder uses HTML plug" ;
			// break ;
			
		default:	
			return "LogHolder uses an unknown plug ! (abnormal)" ;
			// break ;
	
	}


}	


bool LogHolder::IsAKnownPlugOption( const std::string & option ) throw()
{

	if ( option == ConsolePlugOption )
		return true ;
		
	if ( option == ClassicalPlugOption )
		return true ;
		
	if ( option == HTMLPlugOption )
		return true ;
	
	return false ;
		
}
