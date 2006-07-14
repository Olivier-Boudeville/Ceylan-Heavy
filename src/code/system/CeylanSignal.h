#ifndef CEYLAN_SIGNAL_H_
#define CEYLAN_SIGNAL_H_


#include "CeylanTypes.h"         // for Ceylan::Uint16
#include "CeylanSystem.h"        // for SystemException


#include <string>



/**
 * UNIX-style POSIX reliable signal management.
 *
 * @note The signal feature must be available to successfully manage signals.
 *
 * @see Features::areSignalsSupported
 *
 */
namespace Ceylan
{


	namespace System
	{
	
	
		namespace Signal 
		{
	

			/// Mother class for all signal-related exceptions.
			class SignalException: public SystemException
			{ 
				public: 
				
					explicit SignalException( const std::string & reason )
						throw() ;
					
					virtual ~SignalException() throw() ; 
						
			} ;
	
	
	
			/// SignalHandler is a pointer to function.
			typedef void (*SignalHandler)(int) ;
		
		
			/// Pre-defined handler which ignores signals.
			extern const SignalHandler IgnoringHandler ;
		
		
			/**
			 * The default handler, triggering the corresponding action as
			 * defined for the actual signal (ignore, terminate, core dump, 
			 * etc.).
			 *
			 */
			extern const SignalHandler DefaultHandler ;
		
		
		
			/// Signal numbers, as described in the original POSIX.1 standard.
			typedef Ceylan::Uint16 SignalNumber ;
		
		
			/**
			 * Hangup detected on controlling terminal or death of controlling
			 * process (SIGHUP).
			 *
			 * Default action is to terminate the process.
			 *
			 */
			extern const SignalNumber HangUp ;
	
	
			/**
			 * Interrupt from keyboard (SIGINT).
			 *
			 * Default action is to terminate the process.
			 *
			 */
			extern const SignalNumber InterruptFromKeyboard ;
	
	
			/**
			 * Quit from keyboard (SIGQUIT).
			 *
			 * Default action is to terminate the process and dump core.
			 *
			 */
			extern const SignalNumber QuitFromKeyboard ;
	
	
			/**
			 * Illegal Instruction (SIGILL).
			 *
			 * Default action is to terminate the process and dump core.
			 *
			 */
			extern const SignalNumber IllegalInstruction ;
	
	
			/**
			 * Abort signal from abort (SIGABRT).
			 *
			 * Default action is to terminate the process and dump core.
			 *
			 */
			extern const SignalNumber Abort ;
	
	
			/**
			 * Floating point exception (SIGFPE).
			 *
			 * Default action is to terminate the process and dump core.
			 *
			 */
			extern const SignalNumber FloatingPointException ;
	
	
			/**
			 * Kill signal (SIGKILL).
			 *
			 * Default action is to terminate the process.
			 *
			 * @note This signal cannot be caught, blocked, or ignored.
			 *
			 */
			extern const SignalNumber Kill ;
	
	
			/**
			 * Invalid memory reference (SIGSEGV).
			 *
			 * Default action is to terminate the process and dump core.
			 *
			 */
			extern const SignalNumber InvalidMemoryReference ;
	
	
			/**
			 * Broken pipe: write to pipe with no readers (SIGPIPE).
			 *
			 * Default action is to terminate the process.
			 *
			 */
			extern const SignalNumber BrokenPipe ;
	
	
			/**
			 * Timer signal from alarm (SIGALRM).
			 *
			 * Default action is to terminate the process.
			 *
			 */
			extern const SignalNumber TimerSignal ;
	
	
			/**
			 * Termination signal (SIGTERM).
			 *
			 * Default action is to terminate the process.
			 *
			 */
			extern const SignalNumber Termination ;
	
	
			/**
			 * First user-defined signal (SIGUSR1).
			 *
			 * Default action is to terminate the process.
			 *
			 */
			extern const SignalNumber FirstUserDefined ;
	
	
			/**
			 * Second user-defined signal (SIGUSR2).
			 *
			 * Default action is to terminate the process.
			 *
			 */
			extern const SignalNumber SecondUserDefined ;
	
	
			/**
			 * Child stopped or terminated (SIGCHLD).
			 *
			 * Default action is to ignore the signal.
			 *
			 */
			extern const SignalNumber ChildEnded ;
	
	
			/**
			 * Continue if stopped (SIGCONT).
			 *
			 * No default action.
			 *
			 */
			extern const SignalNumber Continue ;
	
	
			/**
			 * Stop process (SIGSTOP).
			 *
			 * Default action is to stop the process.
			 *
			 * @note This signal cannot be caught, blocked, or ignored.
			 *
			 */
			extern const SignalNumber Stop ;
	
	
			/**
			 * Stop typed at tty (SIGTSTP).
			 *
			 * Default action is to stop the process.
			 *
			 */
			extern const SignalNumber TtyStopped ;
	
	
			/**
			 * tty input for background process (SIGTTIN).
			 *
			 * Default action is to stop the process.
			 *
			 */
			extern const SignalNumber TtyInput ;
	
	
			/**
			 * tty output for background process (SIGTTOU).
			 *
			 * Default action is to stop the process.
			 *
			 */
			extern const SignalNumber TtyOutput ;
	
	
	
			/**
			 * Sets the default handler for the specified signal.
			 *
			 * @throw SignalException iff the operation failed, including
			 * if signal support is not available.
			 *
			 */
			void setToDefaultHandler( SignalNumber signalNumber )
				throw( SignalException ) ;
	

			/**
			 * Ignores specified signal from now on.
			 *
			 * @throw SignalException iff the operation failed, including
			 * if signal support is not available.
			 *
			 */
			void ignore( SignalNumber signalNumber ) throw( SignalException ) ;

	
			/**
			 * Sets a new handler for the specified signal type.
			 *
			 * @throw SignalException iff the operation failed, including
			 * if signal support is not available.
			 *
			 */
			void setHandler( SignalNumber signalNumber, 
				SignalHandler newHandler ) throw( SignalException ) ;
	
	
			/**
			 * Raises the specified signal : sends the signal to the current
			 * process.
			 *
			 * @throw SignalException iff the operation failed, including
			 * if signal support is not available.
			 *
			 */
			void raise( SignalNumber signalNumber ) throw( SignalException ) ;

	
            /**
             * Returns a user-friendly description of the signal.
             *
			 * @param signal the signal to describe.
			 *
			 */
			const std::string toString( SignalNumber signalNumber ) 
				throw() ;


	
		}
		
	}
	
}

	
	
#endif // CEYLAN_SIGNAL_H_
