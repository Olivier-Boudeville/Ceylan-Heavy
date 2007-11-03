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




/**
 * ARM9 side of the Ceylan FIFO test.
 *
 * Implementation of a FIFO example with following protocol:
 *
 *  - ARM9 sends a compute request to the ARM7, specifying the address of a
 * value is main memory (message ID: 128 in the first byte of a 32-bit int,
 * then the value address is specified in next element)
 *
 *  - ARM7 adds 42 to this value, and returns it thanks to the FIFO (message
 * ID: 129, then the value; had the value been on 24 bits or less, one
 * element could have carried both the command identifier and the return value)
 *
 * The ARM7 can report errors by sending special command identifiers:
 *   - 1 if a system-specific (Ceylan) error
 *	 - 2 if an application-specific error
 *
 * @note The ARM7 could have sent an identifier of 128 as well (each direction
 * has its own independent identifiers).
 *
 * @see testCeylanFIFO.arm7.c for the peer implementation
 * @see CeylanFIFO.h and CeylanFIFO.cc for the implementation
 *
 * Map of ARM9 -> ARM7 command identifiers:
 *   - 0: hello to ARM7
 *   - 1: system-specific (Ceylan) error on the ARM9 side 
 *   - 2: application-specific error on the ARM9 side 
 *   - 3: specification of the address of the variable to be used by the ARM7
 * to report its status
 *
 * Map of ARM7 -> ARM9 command identifiers:
 *   - 0: hello to ARM9
 *   - 1: system-specific (Ceylan) error on the ARM7 side 
 *   - 2: application-specific error on the ARM7 side 
 *
 */
class myFIFOExample: public Ceylan::System::FIFO
{


	public:
	
	
		explicit myFIFOExample( Ceylan::Uint32 & value ) 
				throw( FIFOException ):
			FIFO(),
			_pointedValue( & value ),
			_inError( false ),
			_hasWorked( false )
		{
		
			LogPlug::trace( "myFIFOExample created." ) ;
			
		} 
		
		
		
		virtual ~myFIFOExample() throw()
		{
		
			LogPlug::trace( "myFIFOExample deleted." ) ;
			
		}
		
		
		
		virtual bool isInError() const throw() 
		{
		
			return _inError ;
			
		}
		
		
		virtual bool hasWorked() const throw() 
		{
		
			return _hasWorked ;
			
		}
		
		
		
		virtual void sendComputeRequest() throw()
		{
						
			LogPlug::trace( "sending compute request." ) ;
			
			FIFOElement commandElement ;
			
			// 128: application-specific compute request ID
			FIFO::SetFIFOCommandIDTo( commandElement, 128 ) ;
			
			writeBlocking( commandElement ) ;

			//LogPlug::trace( "sending address." ) ;
			
			writeBlocking( /* value address */ 
				reinterpret_cast<FIFOElement>( _pointedValue ) ) ;
			
			//LogPlug::trace( "sent compute request." ) ;
			
		}	
		
		
		
		/// The actual overriden method implementing the protocol.
		virtual void handleReceivedApplicationCommand(
			FIFOCommandID commandID, FIFOElement firstElement )	throw()
		{

			LogPlug::trace( "receiving answer." ) ;

			if ( _inError )
				return ;
				
			const FIFOCommandID expectedID = 129 ;
			 	
			if ( commandID != expectedID )
			{
				
				LogPlug::error( 
					"unexpected application-specific command identifier: " 
					+ Ceylan::toNumericalString( commandID ) 
					+ ", instead of " 
					+ Ceylan::toNumericalString( expectedID ) ) ;
						
				_inError = true ;
						
			}		
			else
			{
				
				LogPlug::trace( "Received correct command header ("
					+ Ceylan::toNumericalString( expectedID ) + ")" ) ;
						
			}
				
				
			// Then read the payload (ARM7-computed value):		
			FIFOElement readElement = readBlocking() ;
				
			Ceylan::Uint32 expectedValue = *_pointedValue + 42 ;
				
			if ( readElement == expectedValue )
			{
				
				LogPlug::trace( "Received correct computed value: "
					+ Ceylan::toString( expectedValue ) ) ;
						
			}		
			else
			{
				
				LogPlug::error( "unexpected computed value: " 
					+ Ceylan::toString( readElement ) + ", instead of "
					+ Ceylan::toString( expectedValue ) ) ;
						
				_inError = true ;
					
			}
			
			_hasWorked = true ;
				
			// No two transactions will be the same:
			(*_pointedValue)++ ;
				
			LogPlug::trace( "handled answer." ) ;
			
		}
		
		
		
	protected:	
		
		
		/*
		 * Pointer to volatile not needed, as not modified by the ARM7 nor the
		 * ARM9 cache.
		 *
		 */
		Ceylan::Uint32 * _pointedValue ;
		
		
		/// Useful for the test:		
		bool _inError ;
		
		/// Otherwise test would not fail if no answer received.
		bool _hasWorked ;
		
} ;




/**
 * Test for the FIFO support offered by the Ceylan library on Nintendo DS.
 *
 */
int main( int argc, char * argv[] )
{

	 
	LogHolder myLog( argc, argv ) ;

	
    try
    {
	
		// For the test:
		bool interactive = false ;
		
		
		LogPlug::info( "Test of Ceylan support for FIFO transfers" ) ;


		// Test of command ID management:
		
		// Decimal for 0b101010101010101010101010:
		FIFOElement testElement = 2796202 ; 
		
		// Decimal for 0b11001010:
		const FIFOCommandID testID = 202 ;
		FIFO::SetFIFOCommandIDTo( testElement, testID ) ;
		FIFOCommandID readID = FIFO::GetFIFOCommandIDFrom( testElement ) ;
		
		if ( readID != testID )
			throw TestException( "Writing and reading back "
				"a command identifier (" + Ceylan::toNumericalString( testID ) 
				+ ") returns a different value (" 
				+ Ceylan::toNumericalString( readID ) + ")" ) ;
		
				

		// Complete test:


		/*
		 * Note: using 'Ceylan::Uint32 myValue = 100 ;' results in allocating
		 * that variable in the ARM9 stack (ex: address 0xb003c30), which is
		 * not in the main RAM and cannot be accessed though the non-cachable
		 * mirror.
		 *
		 */
		
		/*
		 * Created in the heap (ex: 0x209ebb8):
		 *
		 * In main RAM thus may end up in the ARM9 data cache.
		 *
		 */
		Ceylan::Uint32 & myValue = * new Ceylan::Uint32( 100 ) ;
		
		// Flush the cache out of safety, probably useless:
		DC_FlushRange( &myValue, sizeof(myValue) ) ;
		 					
		Ceylan::Uint32 * safeAddressOfMyValue =
			ConvertToNonCacheable<Ceylan::Uint32>( &myValue ) ;
										
		if ( *safeAddressOfMyValue != 100 )
			throw TestException( "Conversion to non-cacheable memory failed, "
				"converted address: " + Ceylan::toString( safeAddressOfMyValue )
				+ ", value: " + Ceylan::toString( *safeAddressOfMyValue ) ) ;
		
	
		myFIFOExample myFifo( *safeAddressOfMyValue ) ;

		if ( interactive )
		{
		
			LogPlug::info( "Press any key to activate the FIFO" ) ;
			waitForKey() ;
		
		}

		myFifo.activate() ;
		
		
		
		if ( interactive )
		{
		
			LogPlug::info( "Press any key to send IPC request" ) ;
			waitForKey() ;
		
		}
		
		Ceylan::Uint32 requestCount ;
		
		
		if ( interactive )
			requestCount = 1 ;
		else
			requestCount = 100 ;
		
		/*
		 * Note: there is no direct flow control, the ARM9 sends requests
		 * as fast as possible, without waiting for answers first.
		 *
		 */
		for ( Ceylan::Uint32 i = 0; i < requestCount; i++ )
			myFifo.sendComputeRequest() ;
		
		
		/*
		 * Wait so that operation in error can be detected and the error flag
		 * be set:
		 *
		 */	
		for ( Ceylan::Uint32 i = 0 ; i < 50 ; i++ )
			atomicSleep() ;

		
		if ( myFifo.isInError() )
			throw TestException( "Test failed: FIFO finished in error" ) ;
		else
			LogPlug::info( "FIFO not in error" ) ;
			
				
		if ( myFifo.hasWorked() )
			LogPlug::info( "FIFO performed planned operations" ) ;
		else
			throw TestException( "Test failed: "
				"FIFO did not performed any complete operation " ) ;
		
		Ceylan::Uint32 plannedValue = 100 + requestCount ;	
		
		if ( *safeAddressOfMyValue != plannedValue )
			throw TestException( "Test failed: "
				"FIFO led to faulty computations: expecting " 
				+ Ceylan::toString( plannedValue )
				+ ", obtained: " + Ceylan::toString( *safeAddressOfMyValue ) ) ;
		else
			LogPlug::info( "FIFO led to correct computations" ) ;
			
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
