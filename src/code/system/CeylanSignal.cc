#include "Signal.h"


#include <utility>
using std::pair;

extern "C"
{
#include <signal.h> 

typedef void (*SignalHandler)(int);
}

/*
 * Use sigaction instead of signal.
 *
 *
 *
 */

bool Signal::setHandler( Signal::Type t, SignalHandler handler )
{
	return ::signal( (int)t, handler ) != SIG_ERR;
}

bool Signal::setDefaultBehavior( Signal::Type t )
{
	return ::signal( (int)t, SIG_DFL ) != SIG_ERR;
}

bool Signal::ignore( Signal::Type t )
{
	return ::signal( (int)t, SIG_IGN ) != SIG_ERR;
}

void Signal::raise( Type t )
{
	::raise( (int)t );
}

namespace
{

pair<Signal::Type,const char *> _signame[] = 
{
pair<Signal::Type,const char *>( Signal::HANGUP 			  , "HANGUP"				),
pair<Signal::Type,const char *>( Signal::INTERRUPT_FROM_KEYBOARD 	  , "INTERRUPT_FROM_KEYBOARD"		),
pair<Signal::Type,const char *>( Signal::QUIT_FROM_KEYBOARD		  , "QUIT_FROM_KEYBOARD"		),
pair<Signal::Type,const char *>( Signal::ILLEGAL_INSTRUCTION		  , "ILLEGAL_INSTRUCTION"		),
pair<Signal::Type,const char *>( Signal::ABORT				  , "ABORT"				),
pair<Signal::Type,const char *>( Signal::FLOATING_POINT_EXCEPTION	  , "FLOATING_POINT_EXCEPTION"		),
pair<Signal::Type,const char *>( Signal::KILL				  , "KILL"				),
pair<Signal::Type,const char *>( Signal::INVALID_MEMORY_REFERENCE	  , "INVALID_MEMORY_REFERENCE"		),
pair<Signal::Type,const char *>( Signal::BROKEN_PIPE			  , "BROKEN_PIPE"			),
pair<Signal::Type,const char *>( Signal::TIMER_FROM_ALARM		  , "TIMER_FROM_ALARM"			),
pair<Signal::Type,const char *>( Signal::TERMINATION			  , "TERMINATION"			),
pair<Signal::Type,const char *>( Signal::USER_DEFINED_1  		  , "USER_DEFINED_1"			),
pair<Signal::Type,const char *>( Signal::USER_DEFINED_2  		  , "USER_DEFINED_2"			),
pair<Signal::Type,const char *>( Signal::CHILD_STOPPED			  , "CHILD_STOPPED"			),
pair<Signal::Type,const char *>( Signal::CONTINUE_IF_STOPPED		  , "CONTINUE_IF_STOPPED"		),
pair<Signal::Type,const char *>( Signal::STOP_PROCESS			  , "STOP_PROCESS"			),
pair<Signal::Type,const char *>( Signal::STOP_TYPED_AT_TTY		  , "STOP_TYPED_AT_TTY" 		),
pair<Signal::Type,const char *>( Signal::TTY_INPUT_FOR_BACKGROUND_PROCESS , "TTY_INPUT_FOR_BACKGROUND_PROCESS"  ),
pair<Signal::Type,const char *>( Signal::TTY_OUTPUT_FOR_BACKGROUND_PROCESS, "TTY_OUTPUT_FOR_BACKGROUND_PROCESS" )
};

}

const char * Signal::typeToString( Type type )
{
	for( unsigned s = 0; s < sizeof( _signame ) / sizeof( pair<Signal::Type,const char *> ); s++ )
	{
		if( _signame[s].first == type ) return _signame[s].second;
	}
	
	return "UNKNOWN_SIGNAL";
}
