#include "CeylanUtils.h"


#include "CeylanLogPlug.h"             // for LogPlug::fatal
#include "CeylanStringUtils.h"         // for display
#include "CeylanOperators.h"


// for basicSleep, FileDescriptor, InitializeInterrupts:
#include "CeylanSystem.h"       
#include "CeylanFeatures.h"            // for areFileDescriptorsSupported


#include <cstdio>                      // for kbhit.
#include <ctime>

#include <algorithm>                   // for Split*
#include <iostream>				       // for cerr, endl, cout


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for keysDown (ARM9)
#endif // CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_USES_CONFIG_H
#include <CeylanConfig.h>              // for the actual CEYLAN_LIBTOOL_VERSION
#endif // CEYLAN_USES_CONFIG_H



extern "C"
{

#ifdef CEYLAN_USES_TERMIOS_H
#include <termios.h>            // for UNIX KeyboardHit
#endif // CEYLAN_USES_TERMIOS_H

#ifdef CEYLAN_USES_CONIO_H
#include <conio.h>              // for Windows kbhit and getch
#endif // CEYLAN_USES_CONIO_H

}



const Ceylan::Sint16 Ceylan::ExitSuccess      =  0 ;
const Ceylan::Sint16 Ceylan::ExitFailure      =  1 ;
const Ceylan::Sint16 Ceylan::ExitDebugFailure = 10 ;

const std::string Ceylan::DefaultWaitForKeyMessage( 
	"< Press any key to continue >" ) ;


using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;




#define CEYLAN_DEBUG_VERSION 0

const Ceylan::LibtoolVersion & Ceylan::GetVersion() throw()
{


#if	CEYLAN_DEBUG_VERSION

	// Intentional memory leak:
	
	LibtoolVersion * ceylanVersion ;
	
	try
	{
		ceylanVersion = new LibtoolVersion( CEYLAN_LIBTOOL_VERSION ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		Ceylan::emergencyShutdown( "Ceylan::GetVersion failed: "
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

	string outputMessage = "Fatal error: "
		"Ceylan is performing an emergency shutdown "
		"since an abnormal situation occured. "
		+ message ;
		
	std::cerr << std::endl << outputMessage << std::endl << std::flush ;
	
	// Nothing to loose at this point...
	if ( Log::LogPlug::IsFatalLogSourceAvailable() )
		Log::LogPlug::fatal( outputMessage ) ;
		
	// Try to save the log as well:	
	Log::LogPlug::StopService( /* warnIfAlreadyStopped */ false ) ;
	
	::exit( ExitFailure ) ;	 
	
}


bool Ceylan::keyboardHit() throw( UtilsException )	
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw UtilsException( "Ceylan::keyboardHit: only available on the ARM9.") ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	// Ensures the interrupts are initialized (once):
	Ceylan::System::InitializeInterrupts() ;

	// Lid is not an input:
	const int AllInputs = KEY_A | KEY_B	| KEY_SELECT | KEY_START 
		| KEY_RIGHT | KEY_LEFT | KEY_UP | KEY_DOWN | KEY_R | KEY_L 
		| KEY_X | KEY_Y | KEY_TOUCH ;
		
	//swiWaitForVBlank();
		
	// Update key state:
	scanKeys() ;
	
	// Touch-screen pen down taken into account here:
	return ( ( keysDown() & AllInputs ) != 0 ) ;

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_USES_TERMIOS_H

	/*
	 * Taken from http://www.geocities.com/SiliconValley/Park/4572/tips.html.
	 *
	 * Thanks Petey Leinonen !
	 *
	 */
	 
	// Needs: termios, tcsetattr, tcgetattr, memcpy, getchar, ungetc:
	
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
	c =::getchar() ;

	// Resets the terminal to original state.
	::tcsetattr( fd, TCSANOW, & oterm ) ;

	/* 
	 * If we retrieved a character, put it back on the input stream.
	 *
	 */
	if ( c != -1 )
		::ungetc( c, stdin ) ;

	// Returns 1 if the keyboard was hit, or 0 if it was not hit:
	return ( c != -1  ) ;


#elif defined(CEYLAN_USES_CONIO_H)

	return (::_kbhit() != 0 ) ;

#else // CEYLAN_USES_CONIO_H

#error Ceylan::KeyboardHit not available for your architecture.

#endif // CEYLAN_USES_TERMIOS_H

#endif // CEYLAN_ARCH_NINTENDO_DS

}


KeyChar Ceylan::getChar() throw( UtilsException )
{

#if CEYLAN_ARCH_NINTENDO_DS

	// Would not really make sense here:
	throw UtilsException( "Ceylan::getChar: "
		"not available on the Nintendo DS." ) ;
	
#else // CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_USES_TERMIOS_H

	/*
	 * Portable old-fashioned getchar.
	 *
	 * UNIX port taken from
	 * http://www.geocities.com/SiliconValley/Park/4572/tips.html.
	 *
	 * Thanks Petey Leinonen !
	 *
	 */

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

	// Get a character:
	c =::getchar() ;

	// Resets the terminal to its original state:
	::tcsetattr( fd, TCSANOW, & oterm ) ;

	// Returns the character:
	return c ;
	
	
#elif defined(CEYLAN_USES_CONIO_H)

	return::_getch() ;

#else // defined(CEYLAN_USES_CONIO_H)

#error Ceylan::GetChar() not defined for your architecture.

#endif // CEYLAN_USES_TERMIOS_H
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}


KeyChar Ceylan::waitForKey( const string & message ) throw( UtilsException ) 
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw UtilsException( 
		"Ceylan::waitForKey: only available on the ARM9." ) ;

#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	bool sleepFailed = false ;

#endif // CEYLAN_ARCH_NINTENDO_DS


	// Leave the terminal untouched if no display wanted.
	if ( ! message.empty() )
	{
		display( message ) ;
	}
	
		
	while ( ! Ceylan::keyboardHit() )
	{

#if CEYLAN_ARCH_NINTENDO_DS

		// Wait at most about 16 ms:
		swiWaitForVBlank() ;
		
#else // CEYLAN_ARCH_NINTENDO_DS
	
		// Wait a bit if possible, to save CPU time and laptop batteries: 
		if ( Features::areFileDescriptorsSupported() && ! sleepFailed )
		{
			try
			{
				Ceylan::System::atomicSleep() ;
			}
			catch( const System::SystemException & e )
			{
				LogPlug::error( "Ceylan::waitForKey: sleep failed: "
					+ e.toString() ) ;
				
				// Avoids saturating logs, switch to busy waiting:	
				sleepFailed = true ;
					
			}
			
		}		

#endif // CEYLAN_ARCH_NINTENDO_DS
		
	}
	
	
#if CEYLAN_ARCH_NINTENDO_DS

	return 0 ;	
	
#else // CEYLAN_ARCH_NINTENDO_DS

	return Ceylan::getChar() ;	
	
#endif // CEYLAN_ARCH_NINTENDO_DS

		
}


void Ceylan::checkpoint( const std::string & message ) throw()
{
 
 	static Ceylan::Uint32 checkpointCount = 1 ;
 
 	if ( message.empty() )
		std::cout << "Checkpoint [" << checkpointCount++ 
			<< "]" << std::endl ;
	else
		std::cout << "Checkpoint [" << checkpointCount++ << "]: " 
			<< message << std::endl ;
		
}


void Ceylan::breakpoint( const std::string & message ) throw()
{
 
 	static Ceylan::Uint32 breakpointCount = 1 ;

	if ( ! message.empty() )
		std::cout << std::endl 
			<< "New breakpoint: " << message ;
	
 	std::cout << std::endl 
		<< "Successfully arrived at breakpoint number #"
		<< breakpointCount++ << std::endl ;

	 waitForKey() ;
 
}

