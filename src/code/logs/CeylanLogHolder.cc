#include "CeylanLogHolder.h"

// All plugs needed, to be able to pick one of them :
#include "CeylanLogPlugConsole.h"   // for console plug
#include "CeylanLogPlugClassical.h" // for classical plug
#include "CeylanLogPlugHTML.h"      // for HTML plug


#include "CeylanOperators.h"        // for string operators
#include "CeylanLogLight.h"         // for CEYLAN_LOG
#include "CeylanUtils.h"            // for ExitFailure, emergencyShutdown
#include "CeylanDirectory.h"        // for StripFilename
#include "CeylanTextDisplayable.h"  // for SetOutputFormat


#include <iostream>           // for cerr, if abnormal situation occurs



using std::string ;

using namespace Ceylan::Log ;


const string LogHolder::ConsolePlugOption   = "--consolePlug" ;
const string LogHolder::ClassicalPlugOption = "--classicalPlug" ;
const string LogHolder::HTMLPlugOption      = "--HTMLPlug" ;

KnownPlugs LogHolder::DefaultPlug = classicalPlug ;


LogHolder::LogHolder( Ceylan::Uint16 argCount, 
		const char * const arguments[] ) throw( LogException ) :
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
	
	
	/*
	 * If speakerName was 'arguments[0]', then all log files would be 
	 * created alongside the test executable, in the same directory, 
	 * no matter from which directory they were launched.
	 *
	 * For example, if a 'testLockable' executable lies in
	 * 'Ceylan/Ceylan-0.2/bin/interfaces', and if we execute it from
	 * 'Ceylan/Ceylan-0.2/tests-outputs'  (hence with
	 * '../bin/interfaces/testLockable'), with speakerName set to 
	 * 'arguments[0]' the log files 
	 * would be created under 'Ceylan/Ceylan-0.2/bin/interfaces'.
	 *
	 * The preferred behaviour would be to write them under current
	 * directory (hence, 'tests-outputs'). 
	 * In this case StripFilename should be used :
	 *
	 */
	string speakerName ;
	Ceylan::System::Directory::StripFilename( arguments[0], 
		/* base path */ 0, & speakerName ) ;
	CEYLAN_LOG( "LogHolder : speaker name for logs will be " 
		+ speakerName ) ;
	
	
	/*
	 * LogHolder usually out of a try/catch pair, avoid propagating 
	 * exception :
	 *
	 */
	  
	try
	{
	
		switch( _chosenPlug )
		{
	
			case consolePlug :
				CEYLAN_LOG( "LogHolder : using console plug." ) ;
				LogPlugConsole::StartService( speakerName ) ;
				break ;
	
			case classicalPlug :
				CEYLAN_LOG( "LogHolder : using classical plug." ) ;
				LogPlugClassical::StartService( speakerName ) ;
				break ;
	
			case HTMLPlug :
				CEYLAN_LOG( "LogHolder : using HTML plug." ) ;
				LogPlugHTML::StartService( speakerName ) ;
				break ;
			
			default:	
				CEYLAN_LOG( "Warning : LogHolder : "
					"no known plug specified, defaulting to classical." ) ;
				LogPlugClassical::StartService( speakerName ) ;
				break ;
	
		}
		
	}
	catch( const Ceylan::Exception & e )
	{
		Ceylan::emergencyShutdown( "LogHolder constructor failed : "
			+ e.toString() ) ;
	}

}


LogHolder::~LogHolder() throw()
{

	switch( _chosenPlug )
	{
	
		case consolePlug :
			CEYLAN_LOG( "LogHolder : stopping console plug." ) ;
			LogPlugConsole::StopService() ;
			break ;
	
		case classicalPlug :
			CEYLAN_LOG( "LogHolder : stopping classical plug." ) ;
			LogPlugClassical::StopService() ;
			break ;
	
		case HTMLPlug :
			CEYLAN_LOG( "LogHolder : stopping HTML plug." ) ;
			LogPlugHTML::StopService() ;
			break ;
			
		default:	
			std::cerr << "LogHolder destructor : "
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
	
		case consolePlug :
			return "LogHolder uses console plug" ;
			// break ;
			
		case classicalPlug :
			return "LogHolder uses classical plug" ;
			// break ;
	
		case HTMLPlug :
			return "LogHolder uses HTML plug" ;
			// break ;
			
		default:	
			return "LogHolder uses an unknown plug ! (abnormal)" ;
			// break ;
	
	}


}	
