#include "CeylanUtils.h"


#include "CeylanLogPlug.h"      // for LogPlug::fatal
#include "CeylanStringUtils.h"  // for display
#include "CeylanOperators.h"

#include "CeylanSystem.h"       // for basicSleep, FileDescriptor
#include "CeylanFeatures.h"     // for areFileDescriptorsSupported


#include <cstdio>               // for KeyboardHit.
#include <ctime>

#include <algorithm>            // for Split*
#include <iostream>				// for cerr, endl, cout


#ifdef CEYLAN_USES_CONFIG_H
#include <CeylanConfig.h>       // for the actual CEYLAN_LIBTOOL_VERSION
#endif // CEYLAN_USES_CONFIG_H



extern "C"
{

#ifdef CEYLAN_USES_TERMIOS_H
#include <termios.h>            // for UNIX KeyboardHit.
#endif // CEYLAN_USES_TERMIOS_H

#ifdef CEYLAN_USES_CONIO_H
#include <conio.h>              // for Windows kbhit and getch
#endif // CEYLAN_USES_CONIO_H

}



const Ceylan::Sint16 Ceylan::ExitSuccess      =  0 ;
const Ceylan::Sint16 Ceylan::ExitFailure      =  1 ;
const Ceylan::Sint16 Ceylan::ExitDebugFailure = 10 ;

const std::string Ceylan::DefaultWaitForKeyMessage( 
	"Press any key to continue" ) ;


using std::string ;
using std::list ;


using namespace Ceylan ;




#define CEYLAN_DEBUG_VERSION 0

const Ceylan::LibtoolVersion & Ceylan::GetVersion() throw()
{


#if	CEYLAN_DEBUG_VERSION

	// Intentional memory leak :
	
	LibtoolVersion * ceylanVersion ;
	
	try
	{
		ceylanVersion = new LibtoolVersion( CEYLAN_LIBTOOL_VERSION ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		Ceylan::emergencyShutdown( "Ceylan::GetVersion failed : "
			+ e.toString() ) ;
	}	
		
	return * ceylanVersion ;


#else // CEYLAN_DEBUG_VERSION

	static LibtoolVersion ceylanVersion( CEYLAN_LIBTOOL_VERSION ) ;
	return ceylanVersion ;

#endif // CEYLAN_DEBUG_VERSION	
	
}


void Ceylan::parseCommandLineOptions( std::string & readExecutableName, 
	list<string> & readOptions, Ceylan::Uint16 argumentCount, 
	char ** argumentVector ) throw()
{

	readExecutableName = string( argumentVector[0] ) ;
	
	for ( Ceylan::Uint16 arg = 1 ; arg < argumentCount; arg++ )
		readOptions.push_back( argumentVector[arg] ) ;	

}


void Ceylan::emergencyShutdown( const string & message ) throw() 
{

	string outputMessage = "Fatal error : "
		"Ceylan is performing an emergency shutdown "
		"since an abnormal situation occured. "
		+ message ;
		
	std::cerr << std::endl << outputMessage << std::endl << std::flush ;
	
	// Nothing to loose at this point...
	if ( Log::LogPlug::IsFatalLogSourceAvailable() )
		Log::LogPlug::fatal( outputMessage ) ;
		
	// Try to save the log as well :	
	Log::LogPlug::StopService( /* warnIfAlreadyStopped */ false ) ;
	
	::exit( ExitFailure ) ;	 
	
}


bool Ceylan::keyboardHit() throw()	
{


#ifdef CEYLAN_USES_TERMIOS_H

	// Needs : termios, tcsetattr, tcgetattr, memcpy, getchar, ungetc :
	
	struct termios term, oterm ;
	System::FileDescriptor fd = 0 ;
	KeyChar c = 0 ;

	// Gets the terminal settings.
	::tcgetattr( fd, & oterm ) ;

	// Gets a copy of the settings, which we modify.
	::memcpy( &term, &oterm, sizeof( term ) ) ;

	/*
	 * Puts the terminal in non-canonical mode, any reads timeout after 0.1
	 * seconds or when a single character is read.
	 *
	 */
	term.c_lflag &= ! ICANON ;
	term.c_cc[ VMIN  ] = 0 ;
	term.c_cc[ VTIME ] = 1 ;
	::tcsetattr( fd, TCSANOW, & term ) ;

	/* 
	 * Gets input - timeout after 0.1 seconds or when one character is read. 
	 * If timed out, getchar() returns -1, otherwise it returns the character.
	 *
	 */
	c = ::getchar() ;

	// Resets the terminal to original state.
	::tcsetattr( fd, TCSANOW, & oterm ) ;

	/* 
	 * If we retrieved a character, put it back on the input stream.
	 *
	 */
	if ( c != -1 )
		::ungetc( c, stdin ) ;

	// Returns 1 if the keyboard was hit, or 0 if it was not hit :
	return ( c != -1  ) ;


#elif defined(CEYLAN_USES_CONIO_H)

	return ( ::_kbhit() != 0 ) ;

#else // CEYLAN_USES_CONIO_H

#error Ceylan::KeyboardHit not available for your architecture.

#endif // CEYLAN_USES_TERMIOS_H

}


KeyChar Ceylan::getChar() throw()
{


#ifdef CEYLAN_USES_TERMIOS_H


	KeyChar c ;
	int fd = 0 ;
	
	struct termios term, oterm ;
	
	// Gets the terminal settings.
	::tcgetattr( fd, & oterm ) ;

	// Gets a copy of the settings, which we modify.
	::memcpy( &term, & oterm, sizeof( term ) ) ;

	/* 
	 * Puts the terminal in non-canonical mode, any reads will 
	 * wait until a character has been pressed. 
	 * This function will not time out.
	 *
	 */
	 
	term.c_lflag &= ! ICANON ;
	term.c_cc[ VMIN ]  = 1 ;
	term.c_cc[ VTIME ] = 0 ;
	::tcsetattr( fd, TCSANOW, & term ) ;

	// Get a character :
	c = ::getchar() ;

	// Resets the terminal to its original state :
	::tcsetattr( fd, TCSANOW, & oterm ) ;

	// Returns the character :
	return c ;
	
	
#elif defined(CEYLAN_USES_CONIO_H)

	return ::_getch() ;

#else

#error Ceylan::GetChar() not defined for your architecture.

#endif // CEYLAN_USES_TERMIOS_H
	
	
}


KeyChar Ceylan::waitForKey( const string & message ) throw() 
{

	// Leave the terminal untouched if no display wanted.
	if ( ! message.empty() )
	{
		display( message ) ;
	}
	
	while ( ! Ceylan::keyboardHit() )
	{
		// Wait a bit if possible, to save CPU time and laptop batteries : 
		if ( Features::areFileDescriptorsSupported() )
			Ceylan::System::basicSleep() ;
	}
	
	return Ceylan::getChar() ;	
		
}


void Ceylan::checkpoint( const std::string & message ) throw()
{
 
 	static Ceylan::Uint32 checkpointCount = 1 ;
 
 	if ( message.empty() )
		std::cout << "Checkpoint [" << checkpointCount++ 
			<< "]" << std::endl ;
	else
		std::cout << "Checkpoint [" << checkpointCount++ << "] : " 
			<< message << std::endl ;
		
}


void Ceylan::breakpoint( const std::string & message ) throw()
{
 
 	static Ceylan::Uint32 breakpointCount = 1 ;

	if ( ! message.empty() )
		std::cout << std::endl 
			<< "New breakpoint : " << message ;
	
 	std::cout << std::endl 
		<< "Successfully arrived at breakpoint number #"
		<< breakpointCount++ << std::endl ;

	 waitForKey() ;
 
}
