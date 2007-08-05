#include "CeylanConsole.h" 


#include "CeylanOperators.h"           // for toNumericalString


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for powerON, videoSetMode, etc.
#endif // CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H



using namespace Ceylan::System ;


using std::string ;


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



Console::Console() throw( ConsoleException )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw ConsoleException( 
		"Console constructor: only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	// Take the full LCD:
	initConsole( 0, 0, 32, 24 ) ;  

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS

	// Assumes fully-capable windowed terminal:
	initConsole( 0, 0, UnlimitedWidth, UnlimitedHeight ) ;  

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}

		
Console::Console( CharAbscissa startingX, CharOrdinate startingY,
	CharAbscissa width, CharOrdinate height ) throw( ConsoleException )
{

	initConsole( startingX, startingY, width, height ) ;  
	
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

}


void Console::addInBuffer( const std::string & text ) throw( ConsoleException )
{
	_buffer = text ;
}
				
		
					
void Console::render() throw( ConsoleException )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM9

	iprintf( _buffer.c_str() ) ;

#endif // CEYLAN_RUNS_ON_ARM9

	
#else // CEYLAN_ARCH_NINTENDO_DS

	std::cout << _buffer ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS

}


				
const std::string Console::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "Console whose upper-left corner is at ("
		+ Ceylan::toNumericalString( _xstart ) + ","
		+ Ceylan::toNumericalString( _ystart ) + "), whose width is " 
		+ Ceylan::toNumericalString( _width ) + ", whose width is "
		+ Ceylan::toNumericalString( _height ) + ", whose height is " ;

	return res ;
	
}


void Console::initConsole( CharAbscissa startingX, CharOrdinate startingY,
	CharAbscissa width, CharOrdinate height ) throw( ConsoleException )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM9


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

	_width  = width  ;
	_height = height ;
	
}

	
