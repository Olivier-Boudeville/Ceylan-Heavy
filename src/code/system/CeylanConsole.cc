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


#include "CeylanConsole.h"


#include "CeylanOperators.h"           // for toNumericalString
#include "CeylanUtils.h"               // for KeyChar

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for powerON, videoSetMode, etc.
#endif // CEYLAN_ARCH_NINTENDO_DS


#include <iostream>                    // for cout
#include <cstdio>                      // for putchar


using namespace Ceylan ;
using namespace Ceylan::System ;


using std::string ;
using std::list ;


#if CEYLAN_DEBUG_CONSOLE

#include "CeylanLogLight.h"
#define CEYLAN_CONSOLE_LOG(message) CEYLAN_LOG((string(message)))

#else // CEYLAN_DEBUG_CONSOLE

#define CEYLAN_CONSOLE_LOG(message)

#endif // CEYLAN_DEBUG_CONSOLE


const char * const Console::ForegroundColor::Red     = "\033[91m" ;
const char * const Console::ForegroundColor::Green   = "\033[92m" ;
const char * const Console::ForegroundColor::Blue    = "\033[94m" ;
const char * const Console::ForegroundColor::Cyan    = "\033[96m" ;
const char * const Console::ForegroundColor::White   = "\033[97m" ;
const char * const Console::ForegroundColor::Yellow  = "\033[93m" ;
const char * const Console::ForegroundColor::Magenta = "\033[95m" ;
const char * const Console::ForegroundColor::Grey    = "\033[90m" ;
const char * const Console::ForegroundColor::Black   = "\033[90m" ;
const char * const Console::ForegroundColor::Default = "\033[99m" ;


const char * const Console::BackgroundColor::Red     = "\033[101m" ;
const char * const Console::BackgroundColor::Green   = "\033[102m" ;
const char * const Console::BackgroundColor::Blue    = "\033[104m" ;
const char * const Console::BackgroundColor::Cyan    = "\033[106m" ;
const char * const Console::BackgroundColor::White   = "\033[107m" ;
const char * const Console::BackgroundColor::Yellow  = "\033[103m" ;
const char * const Console::BackgroundColor::Magenta = "\033[105m" ;
const char * const Console::BackgroundColor::Grey    = "\033[100m" ;
const char * const Console::BackgroundColor::Black   = "\033[100m" ;
const char * const Console::BackgroundColor::Default = "\033[109m" ;


const char * const Console::DefaultColors     = "\033[0m"  ;

const char * const Console::Bold              = "\033[1m"  ;
const char * const Console::Faint             = "\033[2m"  ;
const char * const Console::BoldAndFaintOff   = "\033[22m" ;

const char * const Console::Underline         = "\033[4m"  ;
const char * const Console::UnderlineOff      = "\033[24m" ;

const char * const Console::Blinking          = "\033[5m"  ;
const char * const Console::BlinkingOff       = "\033[25m" ;

const char * const Console::NegativeImage     = "\033[7m"  ;
const char * const Console::NegativeImageOff  = "\033[27m" ;

const char * const Console::InvisibleImage    = "\033[8m"  ;
const char * const Console::InvisibleImageOff = "\033[28m" ;




Console::Console( bool startInForeground ) :
	_buffer( 0 ),
	_inForeground( false )
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM7

	throw ConsoleException(
		"Console constructor: only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	// Take the full LCD:
	initConsole( 0, 0, 32, 24, TextBuffer::Raw, /* useBottomScreen */ true,
		/* useSubCore */ true ) ;

	if ( startInForeground )
		setToForeground( true ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	// Assumes fully-capable windowed terminal, but mimics DS size:
	initConsole( 0, 0, 32, 24, TextBuffer::Raw ) ;

	if ( startInForeground )
		setToForeground( true ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



Console::Console(
	TextBuffer::CharAbscissa startingX, TextBuffer::CharOrdinate startingY,
	TextBuffer::CharAbscissa width, TextBuffer::CharOrdinate height,
	TextBuffer::TextLayout layout, bool useBottomScreen, bool useSubCore,
	bool startInForeground ) :
		_buffer( 0 ),
		_inForeground( false )
{

	initConsole( startingX, startingY, width, height, layout,
		useBottomScreen, useSubCore ) ;

	if ( startInForeground )
		setToForeground( true ) ;

}



Console::~Console() throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	// Power, LCD, video mode, banks, etc. unchanged for the moment.

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	// Nothing special for classical terminals.

#endif // CEYLAN_ARCH_NINTENDO_DS

	if ( _buffer != 0 )
		delete _buffer ;

	// No video or input device modified by deletion.

}



void Console::goInteractive()
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	addInBuffer( "<Entered interactive mode, key controls are: "
	  "X: go to previous paragraph, B: go to next paragraph, "
	  "up: go to previous line, down: go to next line, "
	  "Y: toggle text layout (raw/justified/word-wrapped), A: quit>" ) ;

	setToForeground( true ) ;

	bool quit = false ;
	KeyChar readKey ;

	do
	{

		readKey = getChar() ;

		if ( readKey & ButtonX )
			jumpPreviousText() ;

		if ( readKey & ButtonB )
			jumpNextText() ;

		if ( readKey & ButtonUp )
			jumpPreviousLine() ;

		if ( readKey & ButtonDown )
			jumpNextLine() ;


		// Select next layout:
		if ( readKey & ButtonY )
		{

			TextBuffer::TextLayout layout ;

			switch( getTextLayout() )
			{

				case TextBuffer::Raw:
					layout = TextBuffer::WordWrapped ;
					break ;

				case TextBuffer::WordWrapped:
					layout = TextBuffer::Justified ;
					break ;

				case TextBuffer::Justified:
					layout = TextBuffer::Raw ;
					break ;

				default:
					layout = TextBuffer::Raw ;
					break ;

			}

			setTextLayout( layout ) ;

		}


		if ( readKey & ButtonA )
			quit = true ;

	}
	while( ! quit ) ;

	addInBuffer( "<Quitting interactive mode>" ) ;

#endif // CEYLAN_RUNS_ON_ARM9


#else // CEYLAN_ARCH_NINTENDO_DS

	// Nothing special for classical terminals.

#endif // CEYLAN_ARCH_NINTENDO_DS

}



Ceylan::TextBuffer::TextLayout Console::getTextLayout() const
{

	return _buffer->getTextLayout() ;

}



void Console::setTextLayout( TextBuffer::TextLayout newLayout )
{

	try
	{

		_buffer->setTextLayout( newLayout ) ;

	}
	catch( const TextBuffer::TextBufferException & e )
	{

		throw ConsoleException( "Console::setTextLayout failed: "
			+ e.toString() ) ;

	}

	// Update the rendering:
	render() ;

}



bool Console::jumpNextText()
{

	// _buffer should be already allocated.

	if ( _buffer->jumpNextText() )
	{
		render() ;
		return true ;
	}
	else
	{
		return false ;
	}

}



bool Console::jumpPreviousText()
{

	// _buffer should be already allocated.

	if ( _buffer->jumpPreviousText() )
	{
		render() ;
		return true ;
	}
	else
	{
		return false ;
	}

}



bool Console::jumpNextLine()
{

	// _buffer should be already allocated.

	if ( _buffer->jumpNextLine() )
	{
		render() ;
		return true ;
	}
	else
	{
		return false ;
	}

}



bool Console::jumpPreviousLine()
{

	// _buffer should be already allocated.

	if ( _buffer->jumpPreviousLine() )
	{
		render() ;
		return true ;
	}
	else
	{
		return false ;
	}

}



void Console::addInBuffer( const std::string & text )
{

	// _buffer should be already allocated.

	_buffer->add( text ) ;
	render() ;

}



void Console::blankBuffer()
{

	// _buffer should be already allocated.

	_buffer->blank() ;

}



void Console::setToForeground( bool toForeground )
{

	// No transition, nothing done:
	if ( _inForeground == toForeground )
		return ;

	if ( toForeground )
	{

		// Was previously in background.
		Initialize( _useBottomScreen, _useSubCore, /* force */ true ) ;
		_inForeground = true ;

	}
	else
	{

		// Was previously in foreground.
		_inForeground = false ;

		// Nothing special to be done.

	}

}



void Console::render()
{

	if ( _buffer == 0 || _inForeground == false )
		return ;

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	consoleClear() ;

	const std::list<char *> & charGrid( _buffer->getScreenLines() ) ;

	TextBuffer::CharAbscissa width = _buffer->getWidth() ;

	const char * currentLine ;

	for ( list<char *>::const_iterator it = charGrid.begin() ;
			it != charGrid.end(); it++ )
		{

			currentLine = (*it) ;

			for ( TextBuffer::CharAbscissa i = 0; i < width; i++ )
				putchar( currentLine[i] ) ;

		}


#endif // CEYLAN_RUNS_ON_ARM9


#else // CEYLAN_ARCH_NINTENDO_DS

	const std::list<char *> & charGrid( _buffer->getScreenLines() ) ;

	TextBuffer::CharAbscissa width = _buffer->getWidth() ;

	const char * currentLine ;

	for ( list<char *>::const_iterator it = charGrid.begin() ;
			it != charGrid.end(); it++ )
		{

			currentLine = (*it) ;

			for ( TextBuffer::CharAbscissa i = 0; i < width; i++ )
				putchar( currentLine[i] ) ;

			putchar( '\n' ) ;

		}

	putchar( '\n' ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



const std::string Console::toString( Ceylan::VerbosityLevels level ) const
{

	string res = "Console whose upper-left corner is at ("
		+ Ceylan::toNumericalString( _xstart ) + ","
		+ Ceylan::toNumericalString( _ystart ) + ")" ;

	if ( _buffer != 0 )
		res += ", storing its content in " + _buffer->toString( level ) ;
	else
		res += ", not having a text buffer" ;

	return res ;

}



void Console::SetKeyRepeat( Millisecond durationBeforeFirstRepeat,
	Millisecond durationBetweenRepeats )
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	/*
	 * On the DS VBL are paced at 60 Hz, we suppose here scanKeys is called at
	 * each VBL.
	 *
	 * @see also test/cross-tests/CeylanDefaultMain.arm7.cc
	 *
	 * Current libnds implementation of keysDownRepeat resets the 'keysrepeat'
	 * variable, which seems to prevent use of key repeats.
	 *
	 */

	keysSetRepeat(
		/* VBL count before repeat */
			( durationBeforeFirstRepeat * 60 ) / 1000,
		/* VBL count between repeats when repeating */
			( durationBetweenRepeats * 60 ) / 1000 ) ;

#else // CEYLAN_RUNS_ON_ARM9

	throw ConsoleException( "Console::SetKeyRepeat: "
		"not available on this platform." ) ;

#endif // CEYLAN_RUNS_ON_ARM9


#else // CEYLAN_ARCH_NINTENDO_DS

	throw ConsoleException( "Console::SetKeyRepeat: "
		"not available on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void Console::Initialize( bool useBottomScreen, bool useSubCore, bool force )
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM7

	throw ConsoleException( "Console::Initialize: not supported on the ARM7" ) ;

#else // CEYLAN_RUNS_ON_ARM7


	static bool alreadyInitialized = false ;

	// Do nothing if not needed (supposing settings are constant):
	if ( alreadyInitialized && (!force) )
		return ;


	// Here we have to initialize it properly, from scratch:

	/*
	 * Powers the relevant 2D core and select the adequat screen layout:
	 *
	 * @see http://dev-scene.com/NDS/DOCgraphicmodes
	 *
	 */
	if ( useSubCore )
	{

		powerON( POWER_2D_B ) ;

		if ( useBottomScreen )
			lcdMainOnTop() ;
		else
			lcdMainOnBottom() ;

		// Sub background #0 will be used to print text:
		videoSetModeSub( MODE_0_2D | DISPLAY_BG0_ACTIVE ) ;

		// Maps the VRAM bank C for that:
		vramSetBankC( VRAM_C_SUB_BG ) ;

		SUB_BG0_CR = BG_MAP_BASE( 31 ) ;

		// By default, font will be rendered with color 255:
		BG_PALETTE_SUB[255] = RGB15(31,31,31) ;

		consoleInitDefault(
			/* map */       (u16*) SCREEN_BASE_BLOCK_SUB(31),
			/* char base */ (u16*) CHAR_BASE_BLOCK_SUB(0),
			/* bit depth */ 16 ) ;

	}
	else
	{

		// Using main core:

		powerON( POWER_2D_A ) ;

		if ( useBottomScreen )
			lcdMainOnBottom() ;
		else
			lcdMainOnTop() ;

		// Main background #0 will be used to print text:
		videoSetMode( MODE_0_2D | DISPLAY_BG0_ACTIVE ) ;

		// Maps the VRAM bank C for that:
		vramSetBankC( VRAM_C_MAIN_BG ) ;

		BG0_CR = BG_MAP_BASE( 31 ) ;

		// By default, font will be rendered with color 255:
		PALETTE_SUB[255] = RGB15(31,31,31) ;

		consoleInitDefault(
			/* map */       (u16*) SCREEN_BASE_BLOCK(31),
			/* char base */ (u16*) CHAR_BASE_BLOCK(0),
			/* bit depth */ 16 ) ;

	}

	alreadyInitialized = true ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	// Nothing special to do.

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void Console::initConsole(
	TextBuffer::CharAbscissa startingX,
	TextBuffer::CharOrdinate startingY,
	TextBuffer::CharAbscissa width,
	TextBuffer::CharOrdinate height,
	TextBuffer::TextLayout   layout,
	bool useBottomScreen,
	bool useSubCore )
{

	if ( _buffer != 0 )
		delete _buffer ;

	_buffer = new TextBuffer( width, height, layout ) ;

	_useBottomScreen = useBottomScreen ;
	_useSubCore = useSubCore ;

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	SetKeyRepeat() ;

#endif // CEYLAN_RUNS_ON_ARM9


#else // CEYLAN_ARCH_NINTENDO_DS

	// Nothing special for classical terminals.

#endif // CEYLAN_ARCH_NINTENDO_DS


	_xstart = startingX ;
	_ystart = startingY ;

	CEYLAN_CONSOLE_LOG( "Console created" ) ;

}
