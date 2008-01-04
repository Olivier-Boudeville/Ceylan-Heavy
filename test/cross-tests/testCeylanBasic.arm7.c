

/**
 * ARM7 side of the Ceylan basic test.
 *
 * @see testCeylanBasic.arm9.cc for the peer implementation.
 *
 * @see CeylanFIFO.h and CeylanFIFO.cc for its base implementation, and
 * CeylanARM7Base.h for the Ceylan base common code.
 * 
 */
#include "CeylanARM7Base.h"


/* Defines the actual ARM7 status words and error codes, and CEYLAN_SAFE_FIFO */
#include "CeylanARM7Codes.h"

/* Defines IPC command identifiers */
#include "CeylanIPCCommands.h"


		

/* Section dedicated to application-specific IPC */



/* Example of application-specific command handler */
void handleReceivedApplicationCommand( FIFOCommandID id, FIFOElement element )
{
		
	switch ( id )
	{
	
			
		default:
			setError( UnexpectedApplicationCommand ) ;
			break ;
				
	}
		
}



/* External library-specific command handler. */
void handleReceivedIntegratingLibrarySpecificCommand( FIFOCommandID commandID,
	FIFOElement firstElement )
{
		
	/*
	 * Here we are dealing with an external library-specific command.
	 *
	 * @note Cannot use switch here, as if using:
	 * const int MyConstant = 1 ;
	 * switch ( aValue )
	 * {
	 *   case MyConstant:
	 * etc.
	 *
	 * gcc says: 'case label does not reduce to an integer constant'
	 *
	switch ( commandID )
	{
				
		default:
			setError( UnexpectedExternalLibraryCommand ) ;
			break ;
				
	}
	
	*/
	 
	setError( UnexpectedExternalLibraryCommand ) ;
		
}



int main(int argc, char ** argv) 
{

	/* Initializes Ceylan and its prerequesites, and setup the IPC system. */
	initCeylan() ;

	/*
	 * Wait for ever, otherwise the runtime will believe the ROM has crashed.
	 *
	 */
	while( true )
		atomicSleep() ;

}

