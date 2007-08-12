#include "CeylanConsole.h" 


#include "CeylanOperators.h"           // for toNumericalString



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for powerON, videoSetMode, etc.
#endif // CEYLAN_ARCH_NINTENDO_DS


#include <iostream>                    // for cout
#include <cstdio>                      // for putchar


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



Console::Console() throw( ConsoleException ):	
	_buffer( 0 )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw ConsoleException( 
		"Console constructor: only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	// Take the full LCD:
	initConsole( 0, 0, 32, 24, TextBuffer::Raw ) ;  

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS

	// Assumes fully-capable windowed terminal:
	initConsole( 0, 0, 32, 24, TextBuffer::Raw ) ;  

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}

		
Console::Console(
	TextBuffer::CharAbscissa startingX, TextBuffer::CharOrdinate startingY,
	TextBuffer::CharAbscissa width, TextBuffer::CharOrdinate height,
	TextBuffer::TextLayout layout ) 
		throw( ConsoleException ):
	_buffer( 0 )
	
{

	initConsole( startingX, startingY, width, height, layout ) ;  
	
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
		
}



Ceylan::TextBuffer::TextLayout Console::getTextLayout() const throw()
{

	return _buffer->getTextLayout() ;
	
}


void Console::setTextLayout( TextBuffer::TextLayout newLayout ) 
	throw( ConsoleException )
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



bool Console::jumpNextText() throw()
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


bool Console::jumpPreviousText() throw()
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



bool Console::jumpNextLine() throw()
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


bool Console::jumpPreviousLine() throw()
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



void Console::addInBuffer( const std::string & text ) throw( ConsoleException )
{

	// _buffer should be already allocated.
	
	_buffer->add( text ) ;
	
}


void Console::blankBuffer() throw( ConsoleException )	
{

	// _buffer should be already allocated.
		
	_buffer->blank() ;
		
}

					
void Console::render() throw( ConsoleException )
{

	if ( _buffer == 0 )
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


				
const std::string Console::toString( Ceylan::VerbosityLevels level ) 
	const throw()
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
	Millisecond durationBetweenRepeats ) throw( ConsoleException )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM9

	/*
	 * On the DS VBL are paced at 60 Hz, we suppose here scanKeys is 
	 * called at each VBL.
	 *
	 * @see also test/cross-tests/CeylanDefaultMain.arm7.cc
	 *
	 * Current libnds implementation of keysDownRepeat resets the
	 * 'keysrepeat' variable, which seems to prevent use of key repeats.
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



void Console::initConsole( 
	TextBuffer::CharAbscissa startingX, 
	TextBuffer::CharOrdinate startingY,
	TextBuffer::CharAbscissa width, 
	TextBuffer::CharOrdinate height,
	TextBuffer::TextLayout   layout ) throw( ConsoleException )
{

	if ( _buffer != 0 )
		delete _buffer ;
		
	_buffer = new TextBuffer( width, height, layout ) ;
		
#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM9

	SetKeyRepeat() ; 
	
	// Powers the 2D cores:
	powerON( POWER_ALL_2D ) ;

	// Puts the main screen on the top LCD:
	lcdMainOnTop() ;

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


#endif // CEYLAN_RUNS_ON_ARM9

	
#else // CEYLAN_ARCH_NINTENDO_DS

	// Nothing special for classical terminals.
	
#endif // CEYLAN_ARCH_NINTENDO_DS


	_xstart = startingX ;
	_ystart = startingY ;
	
	CEYLAN_CONSOLE_LOG( "Console created") ;

}

	
