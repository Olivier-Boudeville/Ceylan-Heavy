#include "CeylanSignal.h"


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#ifdef CEYLAN_USES_SIGNAL_H
#include <signal.h>            // for SIGHUP, etc.
#endif // CEYLAN_USES_SIGNAL_H

}



/*
 * Maybe later : 
 *  - use sigaction instead of signal
 *  - support the signals not in the POSIX.1 standard but described in SUSv2
 * and SUSv3 / POSIX 1003.1-2001, including real-time signals.
 *
 */

#ifdef CEYLAN_USES_SIGNAL_H


const Ceylan::System::Signal::SignalHandler
	Ceylan::System::Signal::IgnoringHandler = SIG_IGN ;
	
const Ceylan::System::Signal::SignalHandler
	Ceylan::System::Signal::DefaultHandler  = SIG_DFL ;


#else // CEYLAN_USES_SIGNAL_H


const Ceylan::System::Signal::SignalHandler
	Ceylan::System::Signal::IgnoringHandler = 0 ;
	
const Ceylan::System::Signal::SignalHandler
	Ceylan::System::Signal::DefaultHandler  = 0 ;


#endif // CEYLAN_USES_SIGNAL_H


/// Shortcut notation :
typedef Ceylan::System::Signal::SignalNumber Sig ;


#ifdef CEYLAN_USES_SIGNAL_H

/*
 * These constants are the ones for i386, ppc and sh. 
 * For alpha and sparc, and for mips, there are other values.
 *
 */


/*
 * The first set of signal constants is known from both the UNIX
 * and the Windows world.
 *
 */

const Sig Ceylan::System::Signal::InterruptFromKeyboard  = SIGINT  ;
const Sig Ceylan::System::Signal::IllegalInstruction	 = SIGILL  ;
const Sig Ceylan::System::Signal::Abort 				 = SIGABRT ;
const Sig Ceylan::System::Signal::FloatingPointException = SIGFPE  ;
const Sig Ceylan::System::Signal::InvalidMemoryReference = SIGSEGV ;
const Sig Ceylan::System::Signal::Termination			 = SIGTERM ;


/*
 * The second set of signal constants is known from the UNIX
 * world only (not defined on Windows).
 *
 */

#if CEYLAN_ARCH_WINDOWS

// Fake values since symbols have to be defined :

const Sig Ceylan::System::Signal::HangUp                 = 1  ;
const Sig Ceylan::System::Signal::QuitFromKeyboard  	 = 3  ;
const Sig Ceylan::System::Signal::Kill  				 = 7  ;
const Sig Ceylan::System::Signal::BrokenPipe			 = 9  ;
const Sig Ceylan::System::Signal::TimerSignal			 = 10 ;
const Sig Ceylan::System::Signal::FirstUserDefined  	 = 12 ;
const Sig Ceylan::System::Signal::SecondUserDefined 	 = 13 ;
const Sig Ceylan::System::Signal::ChildEnded			 = 14 ;
const Sig Ceylan::System::Signal::Continue  			 = 15 ;
const Sig Ceylan::System::Signal::Stop  				 = 16 ;
const Sig Ceylan::System::Signal::TtyStopped			 = 17 ;
const Sig Ceylan::System::Signal::TtyInput  			 = 18 ;
const Sig Ceylan::System::Signal::TtyOutput 			 = 19 ;

#else // CEYLAN_ARCH_WINDOWS

const Sig Ceylan::System::Signal::HangUp				 = SIGHUP  ;
const Sig Ceylan::System::Signal::QuitFromKeyboard  	 = SIGQUIT ;
const Sig Ceylan::System::Signal::Kill  				 = SIGKILL ;
const Sig Ceylan::System::Signal::BrokenPipe			 = SIGPIPE ;
const Sig Ceylan::System::Signal::TimerSignal			 = SIGALRM ;
const Sig Ceylan::System::Signal::FirstUserDefined  	 = SIGUSR1 ;
const Sig Ceylan::System::Signal::SecondUserDefined 	 = SIGUSR2 ;
const Sig Ceylan::System::Signal::ChildEnded			 = SIGCHLD ;
const Sig Ceylan::System::Signal::Continue  			 = SIGCONT ;
const Sig Ceylan::System::Signal::Stop  				 = SIGSTOP ;
const Sig Ceylan::System::Signal::TtyStopped			 = SIGTSTP ;
const Sig Ceylan::System::Signal::TtyInput  			 = SIGTTIN ;
const Sig Ceylan::System::Signal::TtyOutput 			 = SIGTTOU ;

#endif // CEYLAN_ARCH_WINDOWS

#else // CEYLAN_USES_SIGNAL_H


// Fake values since symbols have to be defined :

const Sig Ceylan::System::Signal::HangUp                 = 1  ;
const Sig Ceylan::System::Signal::InterruptFromKeyboard  = 2  ;
const Sig Ceylan::System::Signal::QuitFromKeyboard  	 = 3  ;
const Sig Ceylan::System::Signal::IllegalInstruction	 = 4  ;
const Sig Ceylan::System::Signal::Abort 				 = 5  ;
const Sig Ceylan::System::Signal::FloatingPointException = 6  ;
const Sig Ceylan::System::Signal::Kill  				 = 7  ;
const Sig Ceylan::System::Signal::InvalidMemoryReference = 8  ;
const Sig Ceylan::System::Signal::BrokenPipe			 = 9  ;
const Sig Ceylan::System::Signal::TimerSignal			 = 10 ;
const Sig Ceylan::System::Signal::Termination			 = 11 ;
const Sig Ceylan::System::Signal::FirstUserDefined  	 = 12 ;
const Sig Ceylan::System::Signal::SecondUserDefined 	 = 13 ;
const Sig Ceylan::System::Signal::ChildEnded			 = 14 ;
const Sig Ceylan::System::Signal::Continue  			 = 15 ;
const Sig Ceylan::System::Signal::Stop  				 = 16 ;
const Sig Ceylan::System::Signal::TtyStopped			 = 17 ;
const Sig Ceylan::System::Signal::TtyInput  			 = 18 ;
const Sig Ceylan::System::Signal::TtyOutput 			 = 19 ;

#endif // CEYLAN_USES_SIGNAL_H



using namespace Ceylan::System::Signal ;
 
using std::string ;


SignalException::SignalException( const string & reason ) throw() :
	SystemException( reason )
{

}


SignalException::~SignalException() throw()
{

}

 
 
void Ceylan::System::Signal::setHandler( SignalNumber signalNumber,
	SignalHandler newHandler ) throw( SignalException )
{

#if CEYLAN_USES_SIGNALS

	if ( ::signal( 
			static_cast<int>( signalNumber ), newHandler ) == SIG_ERR )
		throw SignalException( "Signal::setHandler failed for signal "
			+ Signal::toString( signalNumber ) ) ;
		
#else // CEYLAN_USES_SIGNALS

	throw SignalException( "Signal::setHandler : "
		"signal feature not available." ) ;
		
#endif // CEYLAN_USES_SIGNALS
		
}


void Ceylan::System::Signal::setToDefaultHandler( SignalNumber signalNumber )
	throw( SignalException )
{

	setHandler( signalNumber, DefaultHandler ) ;
		
}


void Ceylan::System::Signal::ignore( SignalNumber signalNumber ) 
	throw( SignalException )
{

	setHandler( signalNumber, IgnoringHandler ) ;
	
}


void Ceylan::System::Signal::raise( SignalNumber signalNumber ) 
	throw( SignalException )
{

#if CEYLAN_USES_SIGNALS

	::raise( static_cast<int>( signalNumber ) ) ;
		
#else // CEYLAN_USES_SIGNALS

	throw SignalException( "Signal::raise : signal feature not available." ) ;
		
#endif // CEYLAN_USES_SIGNALS

}


const std::string Ceylan::System::Signal::toString( 
	SignalNumber signalNumber ) throw()
{


#if CEYLAN_USES_SIGNALS


	if ( signalNumber == Ceylan::System::Signal::HangUp )
		return "Hangup detected on controlling terminal "
			"or death of controlling process (SIGHUP)" ;

	if ( signalNumber == Ceylan::System::Signal::InterruptFromKeyboard )
		return "Interrupt from keyboard (SIGINT)" ;

	if ( signalNumber == Ceylan::System::Signal::QuitFromKeyboard )
		return "Quit from keyboard (SIGQUIT)" ;

	if ( signalNumber == Ceylan::System::Signal::IllegalInstruction )
		return "Illegal Instruction (SIGILL)" ;

	if ( signalNumber == Ceylan::System::Signal::Abort )
		return "Abort signal from abort (SIGABRT)" ;

	if ( signalNumber == Ceylan::System::Signal::FloatingPointException )
		return "Floating point exception (SIGFPE)" ;

	if ( signalNumber == Ceylan::System::Signal::Kill )
		return "Kill signal (SIGKILL)" ;

	if ( signalNumber == Ceylan::System::Signal::InvalidMemoryReference )
		return "Invalid memory reference (SIGSEGV)" ;

	if ( signalNumber == Ceylan::System::Signal::BrokenPipe )
		return "Broken pipe: write to pipe with no readers (SIGPIPE)" ;

	if ( signalNumber == Ceylan::System::Signal::TimerSignal )
		return "Timer signal from alarm (SIGALRM)" ;

	if ( signalNumber == Ceylan::System::Signal::Termination )
		return "Termination signal (SIGTERM)" ;

	if ( signalNumber == Ceylan::System::Signal::FirstUserDefined )
		return "First user-defined signal (SIGUSR1)" ;

	if ( signalNumber == Ceylan::System::Signal::SecondUserDefined )
		return "Second user-defined signal (SIGUSR2)" ;

	if ( signalNumber == Ceylan::System::Signal::ChildEnded )
		return "Child stopped or terminated (SIGCHLD)" ;

	if ( signalNumber == Ceylan::System::Signal::Continue )
		return "Continue if stopped (SIGCONT)" ;

	if ( signalNumber == Ceylan::System::Signal::Stop )
		return "Stop process (SIGSTOP)" ;

	if ( signalNumber == Ceylan::System::Signal::TtyStopped )
		return "Stop typed at tty (SIGTSTP)" ;

	if ( signalNumber == Ceylan::System::Signal::TtyInput )
		return "tty input for background process (SIGTTIN)" ;

	if ( signalNumber == Ceylan::System::Signal::TtyOutput )
		return "tty output for background process (SIGTTOU)" ;


	return "unknown signal (abnormal)" ;		
		

#else // CEYLAN_USES_SIGNALS

	return "Signal::toString : signal feature not available." ;
		
#endif // CEYLAN_USES_SIGNALS
				
}


