

/**
 * ARM7 side of the Ceylan FIFO test.
 *
 * @see testCeylanFIFO.arm9.cc for the peer implementation.
 *
 * @see CeylanFIFO.h and CeylanFIFO.cc for its base implementation, and
 * CeylanARM7Base.h for the Ceylan base common code.
 * 
 */ 
#include "CeylanARM7Base.h"


#include "stdlib.h" /* for rand */



/*
 * Implementation notes:
 * 
 * @see libnds include/nds/ipc.h for defines.
 *
 * @see http://www.neimod.com/dstek/dstek2.xml#Interprocessor%20Communication
 *
 */


/* Defines the actual ARM7 status words and error codes, and CEYLAN_SAFE_FIFO */
#include "CeylanARM7Codes.h"

/* Defines IPC command identifiers */
#include "CeylanIPCCommands.h"





/* Ceylan FIFO-based IPC section */



/* CEYLAN_SAFE_FIFO read from CeylanARM7Codes.h */

/* Disturbs tests with random waitings */
#define CEYLAN_TEST_WITH_RANDOM 1




/*
 * Disturbs the test by sometimes adding random delays.
 *
 */
void disturbTest()
{

#if CEYLAN_TEST_WITH_RANDOM

	static unsigned int count = 0 ;
	
	if ( count++ % 7 == 0 )
		swiDelay( /* cycles */ rand() % 500 + 1 ) ;

#endif // CEYLAN_TEST_WITH_RANDOM

}

	
				

/* Section dedicated to application-specific IPC */


void sendSumRequest()
{

	InterruptMask previous = setEnabledInterrupts( AllInterruptsDisabled ) ;

	/* 130: application-specific sum request ID, other bytes unused: */
	writeBlocking( prepareFIFOCommand( 130 ) ) ;
	
	writeBlocking( 234 ) ;
	writeBlocking( 1000 ) ;
	
	disturbTest() ;
	
	setEnabledInterrupts( previous ) ;
	
	notifyCommandToARM9() ;

}



void sendHelloRequest()
{

	InterruptMask previous = setEnabledInterrupts( AllInterruptsDisabled ) ;

	writeBlocking( prepareFIFOCommand( HelloToTheARM9 ) ) ;

	setEnabledInterrupts( previous ) ;
	
	notifyCommandToARM9() ;

}



void handleComputeRequest()
{
					
	/* Command identifier ok, needing the value now: */
	
	FIFOElement readElement = readBlocking() ;

	/* Add 42 to the specified value: */
	uint32 returnedValue = readElement + 42 ;

	/* Sends answer identifier (129), other bytes unused: */		
	writeBlocking( prepareFIFOCommand( 129 ) ) ; 

	disturbTest() ;
	
	/* Sends computed value: */
	writeBlocking( returnedValue ) ;
	
	notifyCommandToARM9() ;
	
}



void handleSumAnswer()
{

	FIFOElement readElement = readBlocking() ;
	
	disturbTest() ;
	
	if ( readElement != 1234 )
		setError( IncorrectApplicationAnswer ) ;
		
}
  
  

void handleBufferSharingRequest()  
{

	/* Reads the buffer address: */
	Byte * buf = (Byte *) readBlocking() ; 

	/* Reads the buffer size: */
	Size size = (Size) readBlocking() ; 
	
	/* Scan it for expected value: */
	
	Size incorrectCount = 0 ;
	
	Size indexOfFirstIncorrect = 0 ;
	
	const Byte Filler = 170 ;
	
	Size i ;
	
	for ( i = 0; i < size; i++ )
	{
	
		if ( buf[i] != Filler )
		{
		
			incorrectCount++ ; 	
		
			if ( indexOfFirstIncorrect == 0 )
				indexOfFirstIncorrect = i ;
				
		}	
	
	}

	/* Sends answer identifier (136): */		
	FIFOElement firstElement = prepareFIFOCommand( 136 ) ;
	
	/* Store count in command element: */
	firstElement = ( firstElement & 0xffff0000 ) 
		| ( (FIFOElement) incorrectCount ) ;
	
	writeBlocking( firstElement ) ; 

	writeBlocking( (FIFOElement) indexOfFirstIncorrect ) ;
	
	disturbTest() ;
		
	notifyCommandToARM9() ;
		
}



/* Example of application-specific command handler */
void handleReceivedApplicationCommand( FIFOCommandID id, FIFOElement element )
{
		
	switch ( id )
	{
	
		case 128:
			handleComputeRequest() ;
			break ;
		
		case 131:
			handleSumAnswer() ;
			break ;
			
		case 135:
			handleBufferSharingRequest() ;
			break ;
			
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

	/* Uncomment to test other features quietly: uint32 count = 0 ; */
	uint32 count = 2000000 ;
		
	while( count > 0 )
	{
	
		sendSumRequest() ;

		/*
		 * Without a bit of waiting, apparently the ARM7 succeeds in flooding
		 * the ARM9: only sum requests are managed, no compute request is
		 * managed.
		 *
		 */
		if ( count % 50 == 0 )
			atomicSleep() ;

		count-- ;
		
	}	
	
	
	/*
	 * Wait for ever, otherwise the runtime will believe the ROM has crashed.
	 * (no need to shutdown anything)
	 *
	 */
	while( true )
		atomicSleep() ;

}

