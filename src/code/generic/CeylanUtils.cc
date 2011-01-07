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


#include "CeylanUtils.h"


#include "CeylanLogPlug.h"             // for LogPlug::fatal
#include "CeylanStringUtils.h"         // for display
#include "CeylanOperators.h"


// for basicSleep, FileDescriptor, InitializeInterrupts:
#include "CeylanSystem.h"       
#include "CeylanFeatures.h"            // for areFileDescriptorsSupported


#include <cstdio>                      // for kbhit
#include <ctime>
#include <cstring>                     // for memcpy

#include <algorithm>                   // for Split*
#include <iostream>				       // for cerr, endl, cout



#ifdef CEYLAN_USES_CONFIG_H
#include <CeylanConfig.h>              // for the actual CEYLAN_LIBTOOL_VERSION
#endif // CEYLAN_USES_CONFIG_H

#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for keysDown (ARM9)
#endif // CEYLAN_ARCH_NINTENDO_DS



#if defined(CEYLAN_ARCH_NINTENDO_DS) && CEYLAN_ARCH_NINTENDO_DS == 1

#ifdef CEYLAN_RUNS_ON_ARM9

// Mapping of libnds defines for the ARM9:

extern const Ceylan::DSBinaryInput Ceylan::ButtonX             = KEY_X ;
extern const Ceylan::DSBinaryInput Ceylan::ButtonY             = KEY_Y ;

extern const Ceylan::DSBinaryInput Ceylan::ButtonA             = KEY_A ;
extern const Ceylan::DSBinaryInput Ceylan::ButtonB             = KEY_B ;

extern const Ceylan::DSBinaryInput Ceylan::ButtonStart         = KEY_START ;
extern const Ceylan::DSBinaryInput Ceylan::ButtonSelect        = KEY_SELECT ;

extern const Ceylan::DSBinaryInput Ceylan::ButtonLeft          = KEY_LEFT ;
extern const Ceylan::DSBinaryInput Ceylan::ButtonRight         = KEY_RIGHT ;

extern const Ceylan::DSBinaryInput Ceylan::ButtonUp            = KEY_UP ;
extern const Ceylan::DSBinaryInput Ceylan::ButtonDown          = KEY_DOWN ;

extern const Ceylan::DSBinaryInput Ceylan::ShoulderButtonLeft  = KEY_L ;
extern const Ceylan::DSBinaryInput Ceylan::ShoulderButtonRight = KEY_R ;

extern const Ceylan::DSBinaryInput Ceylan::StylusContact       = KEY_TOUCH ;
extern const Ceylan::DSBinaryInput Ceylan::LidOpen             = KEY_LID ;


// Lid status change is not deemed a user input:
extern const Ceylan::DSBinaryInput Ceylan::AllUserInputs = ButtonX | ButtonY 
	| ButtonA | ButtonB | ButtonStart | ButtonSelect 
	| ButtonLeft | ButtonRight | ButtonUp | ButtonDown 
	| ShoulderButtonLeft | ShoulderButtonRight | StylusContact ;


#endif // CEYLAN_RUNS_ON_ARM9


#endif // CEYLAN_ARCH_NINTENDO_DS



extern "C"
{

#ifdef CEYLAN_USES_TERMIOS_H
#include <termios.h>            // for UNIX KeyboardHit
#endif // CEYLAN_USES_TERMIOS_H

#ifdef CEYLAN_USES_CONIO_H
#include <conio.h>              // for Windows kbhit and getch
#endif // CEYLAN_USES_CONIO_H

}



const Ceylan::ExitCode Ceylan::ExitSuccess      =  0 ;
const Ceylan::ExitCode Ceylan::ExitFailure      =  1 ;
const Ceylan::ExitCode Ceylan::ExitDebugFailure = 10 ;

const std::string Ceylan::DefaultWaitForKeyMessage( 
	"< Press any key to continue >" ) ;


using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;




#define CEYLAN_DEBUG_VERSION 0

const Ceylan::LibtoolVersion & Ceylan::GetVersion()
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
	
		throw Ceylan::Exception( "Ceylan::GetVersion failed: " 
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
	char ** argumentVector )
{

	readExecutableName = string( argumentVector[0] ) ;
	
	for ( Ceylan::Uint16 arg = 1; arg < argumentCount; arg++ )
		readOptions.push_back( argumentVector[arg] ) ;	

}



void Ceylan::emergencyShutdown( const string & message ) 
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



bool Ceylan::keyboardHit()	
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw UtilsException( "Ceylan::keyboardHit: only available on the ARM9.") ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	// Ensures the interrupts are initialized (once):
	Ceylan::System::InitializeInterrupts() ;

				
	// Update key state:
	scanKeys() ;
	
	//CEYLAN_DS_LOG( "Key held   = " + Ceylan::toString( keysHeld() ) ) ;
	//CEYLAN_DS_LOG( "Key down   = " + Ceylan::toString( keysDown() ) ) ;
	//CEYLAN_DS_LOG( "Key repeat = " + Ceylan::toString( keysDownRepeat() ) ) ;
	
	/*
	 * Touch-screen pen down taken into account here:
	 *  - keysDown would only take held keys once (no autorepeat managed)
	 *  - keysDownRepeat would suppress all key hits (always resetting the
	 * repeat count)
	 *
	 * So: keysHeld (direct reading) must be used here.
	 *
	 */
	return ( ( keysHeld() & AllUserInputs ) != 0 ) ;

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

	return ( ::_kbhit() != 0 ) ;

#else // CEYLAN_USES_CONIO_H

#error Ceylan::KeyboardHit not available for your architecture.

#endif // CEYLAN_USES_TERMIOS_H

#endif // CEYLAN_ARCH_NINTENDO_DS

}



KeyChar Ceylan::getChar()
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM7

	// Would not really make sense here:
	throw UtilsException( "Ceylan::getChar: "
		"not available on the Nintendo DS ARM7." ) ;
		
#else // CEYLAN_RUNS_ON_ARM7

	// Ensures the interrupts are initialized (once):
	Ceylan::System::InitializeInterrupts() ;
	
	KeyChar key ;
	
	do
	{
	
		System::atomicSleep() ;
		scanKeys() ;
		key = keysDownRepeat() & AllUserInputs ;
	
	}	
	while ( key == 0 ) ;
	
	return key ;
		
#endif // CEYLAN_RUNS_ON_ARM7

	
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



KeyChar Ceylan::waitForKey( const string & message ) 
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw UtilsException( 
		"Ceylan::waitForKey: only available on the ARM9." ) ;

#else // CEYLAN_RUNS_ON_ARM7

	// Leave the terminal untouched if no display wanted.
	if ( ! message.empty() )
		display( message ) ;

	return Ceylan::getChar() ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS


	bool sleepFailed = false ;

	// Leave the terminal untouched if no display wanted.
	if ( ! message.empty() )
		display( message ) ;
	
		
	while ( ! Ceylan::keyboardHit() )
	{

		
		// Wait a bit if possible, to save CPU time and laptop batteries: 
		if ( System::areSubSecondSleepsAvailable() && ! sleepFailed )
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

		
	}
			
	return Ceylan::getChar() ;
		
#endif // CEYLAN_ARCH_NINTENDO_DS
		
}



void Ceylan::checkpoint( const std::string & message )
{
 
 	static Ceylan::Uint32 checkpointCount = 1 ;
 
 	if ( message.empty() )
		display( "Checkpoint [" + Ceylan::toString( checkpointCount ) 
			+ "]" ) ;
	else
		display( "Checkpoint [" + Ceylan::toString( checkpointCount ) 
			+ "]: " + message ) ;
				
	checkpointCount++ ;		
		
}



void Ceylan::breakpoint( const std::string & message )
{
 
 	static Ceylan::Uint32 breakpointCount = 1 ;

	if ( ! message.empty() )
		display( "Breakpoint number #" + Ceylan::toString( breakpointCount )
			+ ": " + message ) ;
	else
		display( "Successfully arrived at breakpoint number #"
			+ Ceylan::toString( breakpointCount ) + "." ) ;

	breakpointCount++ ;
	
	waitForKey() ;
 
}

