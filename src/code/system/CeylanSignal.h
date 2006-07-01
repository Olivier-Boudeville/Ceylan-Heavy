#ifndef CEYLAN_SIGNAL_H_
#define CEYLAN_SIGNAL_H_


extern "C"
{

	typedef void (*SignalHandler)(int);
	
}


/**
 * UNIX-style signal management.
 *
 *
 */
namespace Ceylan
{


	namespace Signal 
	{
	
	

		// Signal types.
		
		typedef Ceylan::Uint16 Signal ;
		
		/// Signal 
		extern const Signal HangUp ;
		
		extern const Signal  ;
		
		enum Type
		{
        	HANGUP 			     	  = SIGHUP,
         	INTERRUPT_FROM_KEYBOARD 	  = SIGINT,
         	QUIT_FROM_KEYBOARD		  = SIGQUIT,
         	ILLEGAL_INSTRUCTION		  = SIGILL,
         	ABORT				  = SIGABRT,
         	FLOATING_POINT_EXCEPTION	  = SIGFPE,
         	KILL				  = SIGKILL,
         	INVALID_MEMORY_REFERENCE	  = SIGSEGV,
         	BROKEN_PIPE			  = SIGPIPE,
         	TIMER_FROM_ALARM		  = SIGALRM,
         	TERMINATION			  = SIGTERM,
         	USER_DEFINED_1  		  = SIGUSR1,
         	USER_DEFINED_2  		  = SIGUSR2,
         	CHILD_STOPPED			  = SIGCHLD,
         	CONTINUE_IF_STOPPED		  = SIGCONT,
         	STOP_PROCESS			  = SIGSTOP,
         	STOP_TYPED_AT_TTY		  = SIGTSTP,
         	TTY_INPUT_FOR_BACKGROUND_PROCESS  = SIGTTIN,
         	TTY_OUTPUT_FOR_BACKGROUND_PROCESS = SIGTTOU
		} ;
	
	/// Sets a new handler for the signal type <b>t</b>.
	bool setHandler( Type t, SignalHandler handler );
	
	/// Turns on the default bahviour on signal type <b>t</b>.
	bool setDefaultBehavior( Type t );
	
	/// Makes process ignore the signal type <b>t</b>.
	bool ignore( Type t );
	
	/// Raises the signal <b>t</b>.
	void raise( Type t );
	
	/// Converts the sygnal number to human readable string
	const char * typeToString( Type type );

	
	}
	
}

	
	
#endif // CEYLAN_SIGNAL_H_
