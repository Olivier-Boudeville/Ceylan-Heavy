#include "Ceylan.h"

#include <iostream>

#include <string>
using std::string ;

#include <list>
using std::list ;

#include <exception>


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;


/*
 * Test with or without log as they change timings quite a lot:
 * (with logs on, one may experience a crash due to a St9bad_alloc, whereas 
 * there remains a lot of memory, see testCeylanLogSystem that may send more
 * than 10 000 log messages and no new/delete/malloc/free operation is 
 * performed...), thus our log system may be not reentrant. 
 * Hence prefer never using it in the context of an IRQ handler. 
 *
 */
//#define TEST_FIFO_LOG(message) LogPlug::trace(message)
#define TEST_FIFO_LOG(message)



/**
 * ARM9 side of the Ceylan FIFO test.
 *
 * Implementation of a FIFO example with following application-specific 
 * protocol:
 *
 *  - ARM9 sends a compute request to the ARM7, specifying in the first FIFO
 * element the command (message ID: 128 in the first byte of a 32-bit int,
 * second byte being the command count, the two next bytes being unused)
 * then the (32-bit) value itself is specified in the next FIFO element.
 * This is directly the value, and not a pointer to a shared variable, as it 
 * would imply allocating this variable on the heap, offsetting this address
 * to the mirrored memory area, staying out of the ARM9 cache (that cannot
 * be viewed from the ARM7), etc.
 *
 *  - ARM7 adds 42 to this value, and returns it thanks to the FIFO (message
 * ID: 129 and command count in the first element), then the value in the next
 * FIFO element
 *
 * @note The ARM7 could have sent an identifier of 128 as well (each direction
 * has its own independent identifiers).
 *
 * The ARM7 can report errors by sending special command identifiers.
 * @see CeylanARM7Codes.h
 *
 * @see testCeylanFIFO.arm7.c for the peer implementation
 * @see CeylanFIFO.h and CeylanFIFO.cc for the C++ (ARM9) implementation
 *
 * @see CeylanIPCCommands.h for a map of command identifiers
 *
 */
class myFIFOExample: public Ceylan::System::FIFO
{


	public:
	
	
		explicit myFIFOExample( Ceylan::Uint32 value ) 
				throw( FIFOException ):
			FIFO(),
			_value( value ),
			_firstWaitedValue( value + 42 ),
			_inError( false ),
			_hasWorkedAtLeastOnce( false )
		{
		
			TEST_FIFO_LOG( "myFIFOExample created." ) ;

		} 
		
		
		
		virtual ~myFIFOExample() throw()
		{
		
			TEST_FIFO_LOG( "myFIFOExample deleted." ) ;
			
		}
		
		
		
		virtual bool isInError() const throw() 
		{
		
			return _inError ;
			
		}
		
		
		
		virtual bool hasWorked() const throw() 
		{
		
			return _hasWorkedAtLeastOnce ;
			
		}
		
		
		
		virtual void sendComputeRequest() throw()
		{
			
			 		
			TEST_FIFO_LOG( "sending compute request." ) ;
			

			/*
			 * 128: application-specific compute request ID, remaining bytes 
			 * in FIFO element not used:
			 *
			 */
			FIFOElement commandElement = prepareFIFOCommand( 128 ) ;
			
			TEST_FIFO_LOG( "compute command is: " 
				+ DescribeCommand( commandElement ) ) ;
			
			try
			{	
			
				TEST_FIFO_LOG( "sending command" ) ;
				writeBlocking( commandElement ) ;

				TEST_FIFO_LOG( "sending value." ) ;
			
				writeBlocking( static_cast<FIFOElement>( _value ) ) ;

				TEST_FIFO_LOG( "command sent" ) ;
				
				notifyCommandToARM7() ;

				TEST_FIFO_LOG( "command notified" ) ;
							
			}
			catch( const FIFOException & e )
			{
			
				LogPlug::error( "sendComputeRequest failed: " + e.toString() ) ;
				return ;	
					
			}
			
			TEST_FIFO_LOG( "sent compute request ended." ) ;
			
			// Prepare next request:
			_value++ ;
			
			
			/*
			 * Disturb the test by adding random delays (and otherwise the ARM7
			 * would be overloaded and would never send requests):
			 *
			 */
			//swiDelay( /* cycles */ ::rand() % 500 + 10 ) ;
			
		}	
		
		
		
		/**
		 * The actual overriden method implementing the protocol.
		 * 
		 * Command count has already been checked by handleReceivedCommand.
		 *
		 */
		virtual void handleReceivedApplicationCommand(
			FIFOCommandID commandID, FIFOElement firstElement )	throw()
		{

			/*
			 * All logs disabled in this method as called from an IRQ handler,
			 * and the log system may be not reentrant.
			 *
			 * Error detection performed thanks to the _inError flag instead.
			 *
			 */

			TEST_FIFO_LOG( "receiving command " 
				+ Ceylan::toNumericalString( commandID ) + "." ) ;


			if ( _inError )
			{
				
				LogPlug::error( "In error, nothing done." ) ;
				return ;
				
			}	
				
			
			switch( commandID )
			{
			
				case 129:
					handleComputeAnswer() ;
					break ;
					
				case 130:
					handleSumRequest() ;
					break ;
				
				default:
					handleUnexpectedApplicationCommand( commandID ) ;
					break ;
					
			}

			/*
			 * Disturb the test by adding random delays (and otherwise the ARM7
			 * would be overloaded and would never send requests):
			 *
			 */
			//swiDelay( /* cycles */ ::rand() % 500 + 10 ) ;
				
		}		

		
		
		virtual void handleComputeAnswer() throw() 
		{
			
			LogPlug::trace( "handleComputeAnswer" ) ;
					
			TEST_FIFO_LOG( "Received correct command header for "
				"handleComputeAnswer" ) ;
				
			FIFOElement readElement ;
				
			try
			{	
			
				// Then read the payload (ARM7-computed value):		
				readElement = readBlocking() ;
			
			}
			catch( const FIFOException & e )
			{
			
				_inError = true ;

				/*
				 * Log left, disable it in case of unexplained bad_alloc
				 * exception:
				 *
				 */
				LogPlug::error( "handleComputeAnswer failed: "
					+ e.toString() ) ;
				
				waitForKey() ;
				
				return ;	
					
			}
			
						 
			if ( readElement == _firstWaitedValue )
			{
				
				TEST_FIFO_LOG( 
					"handleComputeAnswer: received correct computed value: "
					+ Ceylan::toString( _firstWaitedValue ) ) ;
			}		
			else
			{
				
				_inError = true ;

				LogPlug::error( "handleComputeAnswer failed: "
					"unexpected computed value: " 
					+ Ceylan::toString( readElement ) + ", instead of "
					+ Ceylan::toString( _firstWaitedValue ) ) ;
						
				
				waitForKey() ;
									
			}
			
			// No two transactions will be the same:
			_firstWaitedValue++ ;
			
			_hasWorkedAtLeastOnce = true ;
								
			//TEST_FIFO_LOG( "Handled answer." ) ;
								
		}
		
		
		
		virtual void handleSumRequest() throw() 
		{
		
		
			LogPlug::info( "handleSumRequest!!!!!!!!!!!!!!!!!!!!!!!!!" ) ;
			
			TEST_FIFO_LOG( "Received correct command header for "
				"handleSumRequest" ) ;
				
			
			
			FIFOElement firstParameter, secondParameter ;
				
			try
			{	
			
				// Read the first parameter:		
				firstParameter = readBlocking() ;
				
				// Then the second one:		
				secondParameter = readBlocking() ;
			
				// Sum answer chosen to be 131 (remaining FIFO bytes not used):
				writeBlocking( prepareFIFOCommand( 131 ) ) ;
			
				writeBlocking( static_cast<FIFOElement>( firstParameter
					+ secondParameter ) ) ;
				
				LogPlug::info( "#########################" ) ;
				notifyCommandToARM7() ;

				//waitForKey() ;
							
			}
			catch( const FIFOException & e )
			{
			
				LogPlug::error( "handleSumRequest failed: " + e.toString() ) ;
				return ;	
					
			}
									
		}
		
		
		
		virtual void handleUnexpectedApplicationCommand( 
			FIFOCommandID commandID	) throw()
		{	
				
			_inError = true ;


			/*
			 * Log left, disable it in case of unexplained bad_alloc
			 * exception:
			 *
			 */
			LogPlug::error( "handleReceivedApplicationCommand failed: "
				"unexpected application-specific command identifier: " 
				+ Ceylan::toNumericalString( commandID ) ) ;
						
				
			waitForKey() ;
			
		}	
		
		
		
	protected:	
		

		/**
		 * The value that will be send to the ARM7.
		 *
		 */
		Ceylan::Uint32 _value ;
		
		
		/**
		 * The value expected for the next ARM7 answer.
		 *
		 */
		Ceylan::Uint32 _firstWaitedValue ;
		
		
		/// Useful for the test:		
		volatile bool _inError ;
		
		
		/// Otherwise test would not fail if no answer was received:
		volatile bool _hasWorkedAtLeastOnce ;
		
		
} ;



void makeCrash()
{

	int a = 1 ;
	
	int b= 0 ;
	
	volatile int c = a / b ;

	c++ ;
	
	int * d = 0 ;
	
	*d = 1 ;
	
	free( d ) ;
	free( d+1 ) ;
	
	//exit(1) ;
}



/**
 * Test for the FIFO support offered by the Ceylan library on the Nintendo DS.
 *
 */
int main( int argc, char * argv[] )
{

	// makeCrash() ;
	LogHolder myLog( argc, argv ) ;

	
    try
    {
	
		// For the test:
		//bool interactive = true ;
		bool interactive = false ;
		
		
		LogPlug::info( "Test of Ceylan support for FIFO transfers 5" ) ;		
	
		myFIFOExample myFifo( 100 ) ;

		
		if ( interactive )
		{
		
			LogPlug::info( "Press any key to activate the FIFO" ) ;
			waitForKey() ;
		
		}

		LogPlug::info( "Current ARM7 status just before activation is: "
			 + myFifo.interpretLastARM7StatusWord() ) ;

		// Set-up the ARM7 report mechanism:
		myFifo.activate() ;

		LogPlug::info( "Current ARM7 status just after activation is: "
			 + myFifo.interpretLastARM7StatusWord() ) ;
					 
		
		if ( interactive )
		{
		
			LogPlug::info( "Press any key to send first IPC request" ) ;
			waitForKey() ;
		
		}

		
		Ceylan::Uint32 requestCount ;
		
		
		if ( interactive )
			requestCount = 1 ;
		else
			requestCount = 1 ;

		LogPlug::info( "Current ARM7 status just before request sending is: "
			 + myFifo.interpretLastARM7StatusWord() ) ;
	

		/*
		 * Note: there is no direct flow control, the ARM9 sends requests
		 * as fast as possible, without waiting for answers first.
		 *
		 *
		 */
		
		for ( Ceylan::Uint32 i = 0; i < requestCount; i++ )
		{
		
			if ( ( i % 5000 ) == 0 )
				LogPlug::trace( "Sent #" + Ceylan::toString( i ) ) ;

			
			if ( ( i % 10000 ) == 0 )
				LogPlug::trace( myFifo.interpretLastARM7StatusWord() ) ;
			
				
			myFifo.sendComputeRequest() ;
			TEST_FIFO_LOG( "...sent" ) ;
			//waitForKey() ;
			atomicSleep() ;
			
		}

		
		// Wait a few seconds:
		Ceylan::Uint32 u = 0 ;
		while ( u < 180 )
		{
			
			if ( u++ % 100 == 0 )
				LogPlug::info(  myFifo.interpretLastARM7StatusWord() ) ;
			atomicSleep() ;
			
		}
		
		LogPlug::info( "Current ARM7 status just after request sending is: "
			 + myFifo.interpretLastARM7StatusWord() ) ;


		bool testFailed = false ;


		if ( myFifo.getLastARM7StatusWord() != ARM7Running )
		{
		
			LogPlug::error( "after request sending, "
				"ARM7 status is not in expected running state: "
				+ myFifo.interpretLastARM7StatusWord() ) ;
			testFailed = true ;
			
		}	
		
		
		if ( interactive )
		{
		
			LogPlug::info( "Press any key to check ARM7 after IPC answer" ) ;
			waitForKey() ;
		
		}


		// To test ARM7 shutdown:
		const Ceylan::Uint32 displayCount = 1 ;

		for ( Ceylan::Uint32 i = 0; i < displayCount; i++ )
			LogPlug::info( myFifo.interpretLastARM7StatusWord() ) ;

		LogPlug::info( "Current ARM7 status just after key waiting is: "
			 + myFifo.interpretLastARM7StatusWord() ) ;

		if ( interactive )
		{
		
			LogPlug::info( "Press any key to stop waiting" ) ;
			waitForKey() ;
		
		}

		for ( Ceylan::Uint32 i = 0; i < displayCount; i++ )
			LogPlug::info( myFifo.interpretLastARM7StatusWord() ) ;
		
		
		/*
		 * Wait so that operation in error can be detected and the error flag
		 * be set:
		 *
		 */	
		for ( Ceylan::Uint32 i = 0; i < 50; i++ )
			; //atomicSleep() ;


		if ( myFifo.isInError() )
		{
		
			LogPlug::error( "FIFO finished in error" ) ;
			testFailed = true ;
			
		}	
		else
		{
		
			LogPlug::info( "FIFO not in error" ) ;
			
		}
		
				
		if ( myFifo.hasWorked() )
		{
		
			LogPlug::info( "FIFO performed planned operations" ) ;
		
		}	
		else
		{
			
			LogPlug::error(
				"FIFO did not performed any complete operation " ) ;
			testFailed = true ;
			
		}
		
		
		if ( testFailed )
			throw TestException( "Test failed because of error(s) "
				"previously displayed." ) ;
				
		if ( interactive )
		{
		
			LogPlug::info( "Press any key to end the test" ) ;
			waitForKey() ;
		
		}
			
		// LogHolder out of scope: log browser triggered.
		
    }
   
    catch ( const Ceylan::Exception & e )
    {
	
        LogPlug::error( "Ceylan exception caught: " 
			+ e.toString( Ceylan::high ) ) ;
			
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
	
        LogPlug::error( string( "Standard exception caught: " ) + e.what() ) ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
	
        LogPlug::error( "Unknown exception caught" ) ;
		return Ceylan::ExitFailure ;

    }

	LogPlug::info( "Exit on success (no error)" ) ;
	
    return Ceylan::ExitSuccess ;

}

