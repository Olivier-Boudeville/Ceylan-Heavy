/* 
 * Copyright (C) 2003-2013 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


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
 *
 * It may also freeze your tests, so avoid sending logs in handle* by all means.
 * 
 * Hence prefer never using it in the context of an IRQ handler. 
 *
 */
//#define TEST_FIFO_LOG(message) LogPlug::trace(message)
#define TEST_FIFO_LOG(message)


//#define TEST_REQUEST_SHOWN(message) LogPlug::trace(message)
#define TEST_REQUEST_SHOWN(message)



/**
 * ARM9 side of the Ceylan FIFO test.
 *
 * Implementation of a FIFO example with following application-specific 
 * protocol for the compute request that can be sent by the ARM9:
 *
 *  - ARM9 sends a compute request to the ARM7, specifying in the first FIFO
 * element the command (message ID: 128 in the first byte of a 32-bit int,
 * second byte being the command count if in safe mode, the two next bytes 
 * being unused) then the (32-bit) value itself is specified in the next FIFO
 * element.
 * This is directly the value, and not a pointer to a shared variable, as it 
 * would imply allocating this variable on the heap, offsetting this address
 * to the mirrored memory area, staying out of the ARM9 cache (that cannot
 * be viewed from the ARM7), etc.
 *
 *  - ARM7 adds 42 to this value, and returns it thanks to the FIFO (message
 * ID: 129 and command count in the first element, if in safe mode), then the
 * value in the next FIFO element
 *
 * @note The ARM7 could have sent an identifier of 128 as well (each direction
 * has its own independent identifiers).
 *
 * This ARM9 can handle sum request (request ID: 130), it will send answers
 * whose ID will be 131.
 *
 * The ARM7 can report errors by sending special command identifiers.
 * @see CeylanARM7Codes.h
 *
 * @see testCeylanFIFO.arm7.c for the peer implementation
 * @see CeylanFIFO.h and CeylanFIFO.cc for the C++ (ARM9) implementation
 *
 * @see CeylanIPCCommands.h for a map of command identifiers
 *
 * Random waitings can be disabled for full-speed more or enabled to explore
 * all timing possibilities.
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
			_testBuffer( 0 ),
			_incorrectCount( 0 ),
			_inError( false ),
			_hasWorkedAtLeastOnce( false )
		{
		
			TEST_FIFO_LOG( "myFIFOExample created." ) ;

		} 
		
		
		
		virtual ~myFIFOExample() throw()
		{
				
			// Must be called from FIFO child class:		
			deactivate() ;

			if ( _testBuffer != 0 )
				delete [] _testBuffer ;
				
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
		
		
		
		virtual bool hasBuffer() const throw() 
		{
		
			return _testBuffer != 0 ;
			
		}		
		
		
		virtual Ceylan::Uint32 getIncorrectCount() const throw() 
		{
		
			return _incorrectCount ;
			
		}		
		
		
		virtual void resetIncorrectCount() throw() 
		{
		
			_incorrectCount = 0 ;
			
		}		
		
		
		virtual void sendComputeRequest() throw()
		{
			
			TEST_FIFO_LOG( "sending compute request." ) ;
			
			
			try
			{	
			
				InterruptMask previous = SetEnabledInterrupts(
					AllInterruptsDisabled ) ;

				/*
				 * 128: application-specific compute request ID, the 2/3
				 * remaining bytes in FIFO element not used.
				 *
				 */
				FIFOElement commandElement = prepareFIFOCommand( 128 ) ;

				TEST_FIFO_LOG( "compute command is: " 
					+ DescribeCommand( commandElement ) ) ;
					
				TEST_FIFO_LOG( "sending command" ) ;
				writeBlocking( commandElement ) ;

				TEST_FIFO_LOG( "sending value." ) ;
			
				writeBlocking( static_cast<FIFOElement>( _value ) ) ;

				TEST_FIFO_LOG( "command sent" ) ;
				
				SetEnabledInterrupts( previous ) ;
				
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
			 * Disturb the test by adding random delays:
			 *
			 */
			if ( _value % 17 == 0 )
				swiDelay( /* cycles */ ::rand() % 500 + 1 ) ;
			
		}	
		

		
		/**
		 * Simulates the loading and transfer of data, without any special
		 * effort to ensure the buffer is cache-boundary aligned and flushed.
		 *
		 */
		virtual void sendBufferSharingUnsafe() throw()
		{
		
			// To remember how the buffer has been allocated:
			_safeSharing = false ;
		
			Size bufSize = ::rand() % 10000 + 1 ;
			
			TEST_FIFO_LOG( "sendBufferSharingUnsafe: preparing buffer of size "
				+ Ceylan::toString( bufSize ) ) ;
			
			if ( _testBuffer != 0 )
			{
			
				LogPlug::error( "sendBufferSharingUnsafe: "
					"non-empty test buffer." ) ;
				return ;
				
			}	
					
			_testBuffer = new Ceylan::Byte[ bufSize ] ;

			
			// 0b10101010:
			const Ceylan::Byte Filler = 170 ;
			
			
			for ( Size i = 0; i < bufSize; i++ )
				_testBuffer[i] = Filler ;
			
			/*
			
			if ( bufSize > 17 )
				_testBuffer[17] = 46 ;	
			
			 */
			 
			try
			{	
			
				InterruptMask previous = SetEnabledInterrupts(
					AllInterruptsDisabled ) ;

				FIFOElement commandElement = prepareFIFOCommand( 135 ) ;

				TEST_FIFO_LOG( "sending command" ) ;
				writeBlocking( commandElement ) ;

				TEST_FIFO_LOG( "sending buffer address." ) ;
			
				writeBlocking( reinterpret_cast<FIFOElement>( _testBuffer ) ) ;
			
				TEST_FIFO_LOG( "sending buffer size." ) ;
			
				writeBlocking( static_cast<FIFOElement>( bufSize ) ) ;

				TEST_FIFO_LOG( "command sent" ) ;
				
				SetEnabledInterrupts( previous ) ;
				
				notifyCommandToARM7() ;

				TEST_FIFO_LOG( "command notified" ) ;
							
			}
			catch( const FIFOException & e )
			{
			
				LogPlug::error( "sendBufferSharingUnsafe failed: " 
					+ e.toString() ) ;
				return ;	
					
			}
			
			TEST_FIFO_LOG( "sendBufferSharingUnsafe request ended." ) ;
			
		}
		
		
		
		/**
		 * Simulates the loading and transfer of data, with buffer protection
		 * activated, to ensure the buffer is cache-boundary aligned and
		 * flushed.
		 *
		 */
		virtual void sendBufferSharing() throw()
		{
		
			// To remember how the buffer has been allocated:
			_safeSharing = true ;
			
			
			Size bufSize = ::rand() % 10000 + 1 ;
			
			TEST_FIFO_LOG( "sendBufferSharing: preparing buffer of size "
				+ Ceylan::toString( bufSize ) ) ;
			
			if ( _testBuffer != 0 )
			{
			
				LogPlug::error( "sendBufferSharing: "
					"non-empty test buffer." ) ;
				return ;
				
			}	
					
			_testBuffer = CacheProtectedNew( bufSize ) ;

			
			// 0b10101010:
			const Ceylan::Byte Filler = 170 ;
			
			
			for ( Size i = 0; i < bufSize; i++ )
				_testBuffer[i] = Filler ;
			
			
			// Key point: this cache-aligned buffer can be safely flushed:
			DC_FlushRange( (void*) _testBuffer, bufSize ) ;

			
			/*
			 * Allows to make sure that the test can fail:
			 
			if ( bufSize > 17 )
				_testBuffer[17] = 46 ;	
			
			 */
			 
			try
			{	
			
				InterruptMask previous = SetEnabledInterrupts(
					AllInterruptsDisabled ) ;

				FIFOElement commandElement = prepareFIFOCommand( 135 ) ;

				TEST_FIFO_LOG( "sending command" ) ;
				writeBlocking( commandElement ) ;

				TEST_FIFO_LOG( "sending buffer address." ) ;
			
				writeBlocking( reinterpret_cast<FIFOElement>( _testBuffer ) ) ;
			
				TEST_FIFO_LOG( "sending buffer size." ) ;
			
				writeBlocking( static_cast<FIFOElement>( bufSize ) ) ;

				TEST_FIFO_LOG( "command sent" ) ;
				
				SetEnabledInterrupts( previous ) ;
				
				notifyCommandToARM7() ;

				TEST_FIFO_LOG( "command notified" ) ;
							
			}
			catch( const FIFOException & e )
			{
			
				LogPlug::error( "sendBufferSharing failed: " 
					+ e.toString() ) ;
				return ;	
					
			}
			
			TEST_FIFO_LOG( "sendBufferSharing request ended." ) ;
			
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
					handleComputeAnswer( firstElement ) ;
					break ;
					
				case 130:
					handleSumRequest() ;
					break ;
				
				case 136:
					handleBufferSharingAnswer( firstElement ) ;
					break ;
				
				default:
					handleUnexpectedApplicationCommand( commandID ) ;
					break ;
					
			}

			/*
			 * Disturb the test by adding random delays:
			 *
			 */
			if ( _value % 11 == 0 )
				swiDelay( /* cycles */ ::rand() % 500 + 1 ) ;
				
		}		

		
		
		virtual void handleComputeAnswer( FIFOElement firstElement ) throw() 
		{
			
			// Here the remaining bytes of firstElement are not used.
			
			static int i = 0 ;
			
			if ( i++ % 100 == 0 )
				TEST_REQUEST_SHOWN( "  ---- compute ----" ) ;
			 
			 				
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
					+ e.toString() + ", while ARM7 state is " 
					+ interpretLastARM7StatusWord() ) ;
				
				
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
					+ Ceylan::toString( _firstWaitedValue ) 
					+ ", while ARM7 state is " 
					+ interpretLastARM7StatusWord() ) ;	
				
				waitForKey() ;
									
			}
			
			// No two transactions will be the same:
			_firstWaitedValue++ ;
			
			_hasWorkedAtLeastOnce = true ;
								
			//TEST_FIFO_LOG( "Handled answer." ) ;
								
		}
		
		
		
		virtual void handleSumRequest() throw() 
		{
		
			static int i = 0 ;
			
			if ( i++ % 100 == 0 )
				TEST_REQUEST_SHOWN( "+++++++++ SUM +++++++++++" ) ;

			TEST_FIFO_LOG( "Received correct command header for "
				"handleSumRequest" ) ;
				
			
			FIFOElement firstParameter, secondParameter ;
				
			try
			{	
			
				// Note that this method reads in and then writes to the FIFOs.
				
				// Read the first parameter:		
				firstParameter = readBlocking() ;
				
				// Then the second one:		
				secondParameter = readBlocking() ;
			

				// Sum answer chosen to be 131 (remaining FIFO bytes not used):
				writeBlocking( prepareFIFOCommand( 131 ) ) ;
			
				writeBlocking( static_cast<FIFOElement>( firstParameter
					+ secondParameter ) ) ;

				notifyCommandToARM7() ;
				
				//waitForKey() ;
							
			}
			catch( const FIFOException & e )
			{
			
				LogPlug::error( "handleSumRequest failed: " + e.toString() ) ;
				return ;	
					
			}
									
		}
		
		
		
		/**
		 * Checks the ARM7 answer.
		 *
		 */
		virtual void handleBufferSharingAnswer( FIFOElement first ) throw() 
		{
		

			TEST_FIFO_LOG( "Received correct command header for "
				"handleBufferSharingAnswer" ) ;
			
			_incorrectCount = first & 0x0000ffff ;
			
				
			TEST_FIFO_LOG( "handleBufferSharingAnswer: incorrect count is "
				+ Ceylan::toString( incorrectCount ) ) ;
				
			FIFOElement index ;
				
			try
			{	
							
				// Read the first parameter:		
				index = readBlocking() ;
							
			}
			catch( const FIFOException & e )
			{
			
				LogPlug::error( "handleBufferSharingAnswer failed: " 
					+ e.toString() ) ;
				return ;	
					
			}
			
			if ( _incorrectCount != 0 )
				LogPlug::error( "handleBufferSharingAnswer: "
					"index of first incorrect byte is "
					+ Ceylan::toString( index ) ) ;
		
		
			if ( _testBuffer != 0 )
			{
			
				if ( _safeSharing )
					CacheProtectedDelete( _testBuffer ) ;
				else
					delete [] _testBuffer ;
					
				_testBuffer = 0 ;
				
			}
			 
			 																
		}
		
				
		
		virtual void handleUnexpectedApplicationCommand( 
			FIFOCommandID commandID	) throw()
		{	
				
			_inError = true ;


			/*
			 * Log left, disable it in case of unexplained bad_alloc
			 * exception (log system not reentrant):
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
		volatile Ceylan::Uint32 _value ;
		
		
		/**
		 * The value expected for the next ARM7 answer.
		 *
		 */
		volatile Ceylan::Uint32 _firstWaitedValue ;
		
		
		/// The buffer to test caching (non-volatile pointer to volatile).
		Ceylan::Byte * volatile _testBuffer ;
		
		
		/**
		 * The incorrect count for buffer checking.
		 *
		 */
		volatile Ceylan::Uint32 _incorrectCount ;
		
		
		/// Useful for the test:		
		volatile bool _inError ;
		
		
		/// Otherwise test would not fail if no answer was received:
		volatile bool _hasWorkedAtLeastOnce ;
		
		/// To know how a test buffer should be deallocated.
		volatile bool _safeSharing ;
		
} ;





/**
 * Test for the FIFO support offered by the Ceylan library on the Nintendo DS.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder myLog( argc, argv ) ;

	
    try
    {
	
		
		// For the test:
		//bool interactive = true ;
		bool interactive = false ;
		
		bool testRequestSending = true ;
		bool testBufferSharing = true ;
		
		LogPlug::info( "Test of Ceylan support for FIFO transfers" ) ;		
	
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
					 

		bool testFailed = false ;		
		
		if ( testRequestSending )
		{
		
		
			if ( interactive )
			{
		
				LogPlug::info( "Press any key to send first IPC request" ) ;
				waitForKey() ;
		
			}

		
			Ceylan::Uint32 requestCount ;
		
		
			if ( interactive )
				requestCount = 5 ;
			else
				requestCount = 2000000 ;

			LogPlug::info( "Current ARM7 status "
				"just before request sending is: "
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
					LogPlug::trace( "Report " 
						+ myFifo.interpretLastARM7StatusWord() ) ;
			
				
				myFifo.sendComputeRequest() ;
				//atomicSleep() ;
			
				TEST_FIFO_LOG( "...sent" ) ;
				//waitForKey() ;
			
			}


			LogPlug::info( "Sending finished, waiting about 3 seconds" ) ;
		
			// Wait a few seconds:
			Ceylan::Uint32 u = 0 ;
		
			while ( u < 180 )
			{
			
				if ( u % 100 == 0 )
					LogPlug::info(  myFifo.interpretLastARM7StatusWord() ) ;
				
				atomicSleep() ;
			
				u-- ;
			
			}
		
			LogPlug::info( "Current ARM7 status just after request sending is: "
				+ myFifo.interpretLastARM7StatusWord() ) ;


			if ( myFifo.getLastARM7StatusWord() != ARM7Running )
			{
		
				LogPlug::error( "after request sending, "
					"ARM7 status is not in expected running state: "
					+ myFifo.interpretLastARM7StatusWord() ) ;
				testFailed = true ;
			
			}	
		
		
			if ( interactive )
			{
		
				LogPlug::info( "Press any key to check ARM7 "
					"after IPC answer" ) ;
				waitForKey() ;
		
			}

		
		} // if testRequestSending
		
		
		bool safeSharingFailed = false ;
		
		
		if ( testBufferSharing )
		{
		
			LogPlug::info( "Press any key to test buffer transfers, "
				"starting with unsafe ones." ) ;

			waitForKey() ;
		
			const Ceylan::Uint32 testBufferCount = 15 ;
		
			Ceylan::Uint32 failedTransferCount = 0 ;
			
			Ceylan::Uint32 incorrectCount ;
		
			for ( Ceylan::Uint32 i = 0; i < testBufferCount; i++ )
			{
		
				// Wait for last buffer command to complete:
				while ( myFifo.hasBuffer() )
					;
			
				incorrectCount = myFifo.getIncorrectCount() ;
			
				/*
				LogPlug::debug( "Incorrect count: " 
					+ Ceylan::toString( incorrectCount ) ) ;
				 */
			 
				if ( incorrectCount != 0 )
				{
					
					LogPlug::error( "Unsafe: non-null incorrect count: " 
						+ Ceylan::toString( incorrectCount ) ) ;
					
					failedTransferCount++ ;
					
				}
					
				myFifo.sendBufferSharingUnsafe() ;
	
			}
		
			// Wait for last buffer command to complete:
			while ( myFifo.hasBuffer() )
					; 
					
			LogPlug::info( Ceylan::toString( failedTransferCount ) 
				+ " buffer unsafe transfer(s) failed." ) ;
				
			myFifo.resetIncorrectCount() ;
			failedTransferCount = 0 ;
			
			LogPlug::trace( "#############################" ) ;
		
			
			LogPlug::info( "Press any key to test buffer transfers, "
				"continuing now with safe ones." ) ;
					
			waitForKey() ;
			
		
			for ( Ceylan::Uint32 i = 0; i < testBufferCount; i++ )
			{
		
				// Wait for last buffer command to complete:
				while ( myFifo.hasBuffer() )
					;
			
				incorrectCount = myFifo.getIncorrectCount() ;
			
				/*
				LogPlug::debug( "Incorrect count: " 
					+ Ceylan::toString( incorrectCount ) ) ;
				 */
			 
				if ( incorrectCount != 0 )
				{
				
					safeSharingFailed = true ;
					
					LogPlug::error( "Safe: non-null incorrect count : " 
						+ Ceylan::toString( incorrectCount ) ) ;

					failedTransferCount++ ;
					
				}
				
				myFifo.sendBufferSharing() ;
	
			}
		

			LogPlug::info( Ceylan::toString( failedTransferCount ) 
				+ " buffer safe transfer(s) failed." ) ;
		
			waitForKey() ;
			
		
		} // if testBufferSharing
		

				
		
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
			; 


		if ( myFifo.isInError() )
		{
		
			LogPlug::error( "FIFO finished in error" ) ;
			testFailed = true ;
			
		}	
		else
		{
		
			LogPlug::info( "FIFO not in error" ) ;
			
		}
		
		if ( testBufferSharing )
		{
		
			if ( safeSharingFailed )
			{
		
				LogPlug::error( "Safe sharing failed" ) ;
				testFailed = true ;
			
			}	
			else
			{
		
				LogPlug::info( "Safe sharing succedeed" ) ;
			
			}
		
		}
		
		
		if ( testRequestSending	)	
		{
		
		
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

