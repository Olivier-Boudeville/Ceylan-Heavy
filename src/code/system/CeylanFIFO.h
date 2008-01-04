#ifndef CEYLAN_FIFO_H_
#define CEYLAN_FIFO_H_

#include "CeylanARM7Codes.h"         // for ARM7StatusWord, ARM7ErrorCode, etc.
#include "CeylanTextDisplayable.h"   // for inheritance
#include "CeylanSystem.h"            // for SystemException, BatteryStatus, etc.
#include "CeylanTypes.h"             // for Ceylan::Uint32
#include "CeylanSystemInformation.h" // for BatteryStatus




// Cannot be in a namespace, as to be included in C code:




/**
 * Describes a number of FIFO commands.
 *
 * Allows to count how many commands were processed on both sides, and 
 * to include that number in each command to spot eventual commands lost in the
 * FIFO.
 *
 */
typedef Ceylan::Uint8 FIFOCommandCount ;



namespace Ceylan
{


	namespace System
	{
	
	

		/// The atomic data that can be sent through the FIFO.
		typedef Ceylan::Uint32 FIFOElement ;
		
			
		/**
		 * Interrupt-based FIFO (First In, First Out) class for
		 * command-based Inter-Process Communication (IPC).
		 *
		 * This logical bidirectional IPC bus is synchronized by interrupts only
		 * (no polling), for the platforms that support that, i.e. the
		 * Nintendo DS.
		 *
		 * This class is dedicated to the encapsulation of two hardware FIFO, 
		 * notably for the two of the Nintendo DS, between the two ARMs.
		 *
		 * The FIFO in itself communicates only using the hardware FIFO: no
		 * shared variable is used, except two that are used to report ARM7 
		 * status and error code. But of course this communication system
		 * can be used to send pointers to (shared) variables.
		 *
		 * The system in itself is in C for the ARM7, as it has quite little
		 * memory for its executable.
		 *
		 * @see testCeylanFIFO.arm7.c for a full example implementation.
		 *
		 * The ARM9 can rely on this C++ abstraction for simpler management.
		 *
		 * @see testCeylanFIFO.arm9.cc for a full usage example.
		 *
		 * Each FIFO register is only 32-bit, thus as soon as a command needs to
		 * specify an address (32 bits), it has to use more than one FIFO 
		 * register by sending a series of FIFO elements. The reading callback
		 * will block until a full command is received.
		 *
		 * A generic and empty FIFO class is provided here. It is made to be
		 * subclassed by adding as many methods as needed to send and receive
		 * messages from the end of the FIFO. For example the sendPlayRequest
		 * and onPlayNotification methods could be added.
		 * They act as callbacks. Their processing should not be too long, and
		 * it should not trigger interrupts or use I/O (ex: no libfat call 
		 * allowed).
		 *
		 * An application-specific protocol shall be built on top of this 
		 * generic FIFO. 
		 *
		 * Request management should be asynchronous: one should send a request,
		 * store in the state of the FIFO child class, and return, without 
		 * waiting directly in this send function the answer: the other ARM may
		 * send a request of its own just after this ARM sent his request, and
		 * the request of the other ARM would be read instead of the expected
		 * answer.
		 *
		 * The FIFO can work on normal mode, or a more paranoid one, if
		 * CEYLAN_SAFE_FIFO is equal to 1. According to the tests, both are
		 * 100% reliable (if used properly of course), but the later may
		 * detect errors by, notably, keeping track of command numbers to
		 * ensure none is lost and they are managed in the right order.
		 * Note that the CEYLAN_SAFE_FIFO flag must be set for both ARM
		 * executables.
		 *
		 * There could be a second fail-over mechanism, based on the 4 bits 
		 * that each ARM can set in the IPC sync register. It is for the moment
		 * only used during the ARM handshake, in activation step.
		 *
		 * Ceylan, for its inner working, has to use too that FIFO, notably so
		 * that the ARM7 state can be transferred and managed on the ARM9.
		 * Thus the FIFO has to be shared between Ceylan and the user code, 
		 * and conventions have to be respected. 
		 *
		 * Each command can be made of any number of FIFO elements (they can be
		 * read one after the other and thus the size of the FIFO does not 
		 * matter), but the first byte of its first element must be the command
		 * identifier.
		 *
		 * Command identifiers range from 0 to 255. Ceylan reserves all command
		 * ID between 0 and 127, thus the application must choose its
		 * identifiers in the 128-255 range (128 free slots).
		 *
		 * Note that they are 128 free application-specific command identifiers 
		 * from the ARM9 to the ARM7, and another 128 identifier in the other
		 * way round (they have nothing in common).
		 *
		 * Note also that the answer to a command has to use itself a command
		 * identifier, to be able to distinguish an answer for a new request.
		 *
		 * If not in safe mode (CEYLAN_SAFE_FIFO not equal to 1), the three
		 * remaining bytes and the potential next FIFO elements are
		 * command-specific. 
		 * 
		 * If in safe mode (CEYLAN_SAFE_FIFO equal to 1), only the two last
		 * bytes of the command element can be used by the application, as the
		 * command ID (first byte) is followed by a command count (second byte)
		 * which allows to catch any lost FIFO element. For the moment no
		 * element has been reported lost on a real DS, neither on the emulator.
		 * 
		 * To send requests, use in a SetEnabledInterrupts pair (to avoid being
		 * interrupts during the sending):
		 * 'FIFOElement commandElement = prepareFIFOCommand( myID ) ;' followed
		 * by the sending of this command element (writeBlocking) and any number
		 * of other elements. Then, just out of the SetEnabledInterrupts pair,
		 * the other ARM must be notified of the request 
		 * (ex: notifyCommandToARM7).
		 *
		 * @note Once a command has been written in the FIFO, the 
		 * 'notifyCommandToARM7' method must be called, otherwise the ARM7 may
		 * not manage it and block, as it does not perform any polling. There
		 * is nevertheless a fail-over mechanism: when an ARM uses blocking
		 * reads or writes, and if the FIFO cannot perform it (as respectively
		 * empty or full), the sending ARM will trigger the receiving ARM so
		 * that it manages any elements in its receive queue (thus emptying it)
		 * and, possibly, outputs answers to found requests (thus filling its
		 * send queue).
		 *
		 * To handle incoming requests, override
		 * handleReceivedApplicationCommand, which will be given the incoming
		 * command ID, and just read the relevant number of elements.
		 *
		 * The overriden handleReceivedApplicationCommand method is responsible
		 * for reading all the associated elements, so that they will not 
		 * disturb the next decoding of the handler. 
		 *
		 * @see testCeylanFIFO.arm9.cc and testCeylanFIFO.arm7.c for a complete 
		 * example.
		 *
		 * Due to the use of a callback based on a static method, the FIFO
		 * instance is expected to be a singleton.
		 *
		 * @see testCeylanFIFO.arm9.cc to have a complete example showing how
		 * to avoid the numerous pitfalls of IPC programming.
		 *
		 * For example, the deactivate method should be called from the 
		 * destructor of the child FIFO class, not from the mother FIFO one,
		 * otherwise an IRQ after the child destructor but before the end of 
		 * the mother destructor would trigger a call to a pure virtual
		 * method.
		 *
		 * @see the Ceylan::System::Pipe class for another means, more general,
		 * of doing IPC.
		 *
		 * Inspired from: http://www.double.co.nz/nintendo_ds/nds_develop7.html
		 *
		 */
		class CEYLAN_DLL FIFO: public Ceylan::TextDisplayable 
		{

	
			public:
	
	

				/// Mother class for all FIFO-related exceptions.
				class FIFOException: public SystemException
				{ 
					public: 
					
						explicit FIFOException( const std::string & reason )
							throw() ;
						
						virtual ~FIFOException() throw() ; 
				} ;



				/// Raised whenever trying to write to a full FIFO.
				class FIFOFull: public FIFOException
				{ 
					public: 
					
						explicit FIFOFull( const std::string & reason ) 
							throw() ;
						
				} ;



				/// Raised whenever trying to read from an empty FIFO.
				class FIFOEmpty: public FIFOException
				{ 
					public: 
					
						explicit FIFOEmpty( const std::string & reason ) 
							throw() ;
							 
				} ;



		
				/**
				 * Constructs a new bidirectional logical FIFO, notably from
				 * two unidirectional hardware FIFO.
				 *
				 * @throw FIFOException on failure.
				 *
				 * @see activate to trigger the use of the FIFO.
				 *
				 */
				explicit FIFO() throw( FIFOException ) ;
		
	

				/// Virtual destructor.
				virtual ~FIFO() throw() ;
		
				
				
				// Activation section.
				
				
				/// Returns whereas this FIFO is currently active.
				virtual bool isActive() const throw() ;
		
		
				/**
				 * Activates the FIFO, and notifies the ARM7 of the variables
				 * it should use to store its status and error code.
				 *
				 * @note This method cannot be automatically called from the
				 * constructor, as an asynchronous IRQ could be triggered on the
				 * receive between the call to this method and the end of the
				 * constructor. In that case handleReceivedCommand would
				 * be called, whereas at this moment is still pure virtual
				 * (since still in FIFO constructor).
				 *
				 * @note This method will wait a certain amount of time to
				 * let the ARM7 update its status. If the ARM7 implementation
				 * is faulty (ex: there is no code on the ARM7 executable to 
				 * handle the FIFO and the ARM7 report mechanism), an exception
				 * will be thrown.
				 *
				 * @throw FIFOException, should the ARM9 time-out short of an
				 * ARM7 update of its status word.
				 *
				 */
				virtual void activate() throw( FIFOException ) ;
				
		
				/**
				 * Deactivates the FIFO.
				 *
				 * @note This method can be called from the destructor of the 
				 * most specialized child class inheriting from this one.
				 * It cannot be called directly for the destructor of this
				 * mother FIFO, as an IRQ could trigger a pure virtual
				 * method call before this deactivate method is called.
				 *
				 */
				virtual void deactivate() throw() ;

				
				// Some FIFO-provided IPC services.
				
				
				// Status word and error code section.


				/**
				 * Returns the latest status word set by the ARM7.
				 *
				 * Reads the relevant shared variable.
				 *
				 * @see CeylanARM7Codes.h for list of known status codes.
				 *
				 */
				virtual ARM7StatusWord getLastARM7StatusWord() throw() ;


				/**
				 * Returns an interpretation of the latest status word set by
				 * the ARM7.
				 *
				 * Reads the relevant shared variable.
				 *
				 */
				virtual std::string interpretLastARM7StatusWord() throw() ;


				
				/**
				 * Returns the latest error code set by the ARM7.
				 *
				 * Reads the relevant shared variable.
				 *
				 * @see CeylanARM7Codes.h for list of known error codes.
				 *
				 */
				virtual ARM7ErrorCode getLastARM7ErrorCode() throw() ;
				
				
				/**
				 * Returns an interpretation of the latest error code set by
				 * the ARM7.
				 *
				 * Reads the relevant shared variable.
				 *
				 */
				virtual std::string interpretLastARM7ErrorCode() throw() ;



				/*
				 * Section for basic request/answer services.
				 *
				 * Could be added:
				 *   - get/set brightness
				 *   - get/set backlight
				 *	 - get/set blink status
				 *	 - get/set blink speed
				 *
				 * @see http://licklick.wordpress.com/
				 *
				 */

				
				/**
				 * Sends to the ARM7 a request to update the battery status.
				 *
				 * @note Non-blocking, not waiting for the answer.
				 *
				 * @throw FIFOException if the operation failed.
				 *
				 * @see getBatteryStatus for a blocking wait for the update.
				 *
				 */
				virtual void sendBatteryStatusRequest() 
					throw( FIFOException ) ;
				
				
				/**
				 * Returns the battery status.
				 *
				 * If the status is 'BatteryStatusUnknown', will wait until
				 * either a supposedly-called 'sendBatteryStatusRequest' method
				 * completes (resulting in an update of the status), or a
				 * time-out expires.
				 *
				 * @note Uses waiting based on atomic sleeps.
				 *
				 * @throw FIFOException if the operation failed, including
				 * if the time-out expires.
				 *
				 */
				virtual BatteryStatus getBatteryStatus() 
					throw( FIFOException ) ;
					
				
				/**
				 * Sends to the ARM7 a request to update the DS type.
				 *
				 * @note Non-blocking, not waiting for the answer.
				 *
				 * @throw FIFOException if the operation failed.
				 *
				 * @see getDSType for a blocking wait for the update.
				 *
				 */
				virtual void sendDSTypeRequest() 
					throw( FIFOException ) ;
				
				
				/**
				 * Returns the DS actual type (DS Fat,Lite, etc.).
				 *
				 * If the status is 'DSTypeUnknown', will wait until
				 * either a supposedly-called 'sendDSTypeRequest' method
				 * completes (resulting in an update of the DS type), or a
				 * time-out expires.
				 *
				 * @note Uses waiting based on atomic sleeps.
				 *
				 * @throw FIFOException if the operation failed, including
				 * if the time-out expires.
				 *
				 */
				virtual DSType getDSType() throw( FIFOException ) ;
					
				

         	  	/**
            	 * Returns an user-friendly description of the state of
				 * this object.
            	 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall 
				 * settings.
				 *
				 * @see TextDisplayable
				 *
				 */
            	virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) 
						const throw() ;
				
				
				
				// Static section.
				
		
				/**
				 * Returns the command identifier read from specified FIFO
				 * element.
				 *
				 */
				static FIFOCommandID GetFIFOCommandIDFrom( 
					const FIFOElement & element ) throw() ;
				
				
				/**
				 * Returns the command count read from specified FIFO
				 * element.
				 *
				 */
				static FIFOCommandCount GetFIFOCommandCountFrom( 
					const FIFOElement & element ) throw() ;
				
				
				/**
				 * Returns the number of commands processed by the ARM7, based
				 * on the IPC sync register.
				 *
				 * @note Only the 4 lower bits are used, for a count in
				 * [0,15].
				 * 
				 */
				static FIFOCommandCount GetARM7ProcessedCount() throw() ;
				
				
				/**
				 * Returns the number of commands processed by the ARM9, based
				 * on the IPC sync register.
				 *
				 * @note Only the 4 lower bits are used, for a count in
				 * [0,15].
				 * 
				 */
				static FIFOCommandCount GetARM9ProcessedCount() throw() ;
				
								
				/**
				 * Callback to catch the reception of a FIFO element by polling
				 * during the Vblank interrupt.
				 * Expected to be called regularly in the VBlank handler,
				 * to add a polling-based layer of security to the IRQ-based
				 * approach, in case an interrupt is lost.
				 *
				 * This signature is mandatory. Public so that it can be 
				 * registered as an handler externally.
				 *
				 * Just checks whether the FIFO is empty. If not, calls
				 * the handleReceivedCommand method.
				 *
				 * @note Not used here anymore as not needed: no polling needed,
				 * and used to create synchronization issues.
				 *
				 */
				static void VBlankHandlerForFIFO() ;
	
				
				/**
				 * Returns the supposedly already-created and already-activated
				 * singleton FIFO.
				 *
				 * @throw FIFOException if the operation failed, including if
				 * no FIFO is available or if it is not active.
				 *
				 */
				static FIFO & GetActivatedFIFO() throw ( FIFOException ) ;


				/**
				 * Returns the supposedly already-created singleton FIFO.
				 *
				 * @throw FIFOException if the operation failed, including if
				 * not FIFO is available.
				 *
				 */
				static FIFO & GetExistingFIFO() throw ( FIFOException ) ;
				
				
				/**
				 * Returns the singleton FIFO, creates it if needed.
				 *
				 * @throw FIFOException if the operation failed.
				 *
				 */
				static FIFO & GetFIFO() throw ( FIFOException ) ;
				
				
				/**
				 * Removes the singleton FIFO, if any.
				 *
				 * @return true iff there was a FIFO to remove.
				 *
				 * @throw FIFOException if the operation failed.
				 *
				 */
				static bool RemoveFIFO() throw() ;
				
				
				/**
				 * The unique FIFO instance.
				 *
				 */
				static FIFO * _FIFO ;   
				 
			
				 

		protected:
				
				
				
				/**
				 * Method responsible for the actual decoding of an incoming
				 * command.
				 *
				 * Discriminates between Ceylan commands, that are managed
				 * automatically, and application-specific commands, that result
				 * in an appropriate call to handleReceivedApplicationCommand,
				 * which is most probably overriden.
				 *
				 * @note Called automatically by ManageReceivedCommand.
				 *
				 */
				virtual void handleReceivedCommand() throw() ;
				
				
				/**
				 * Method responsible for the actual decoding and management of
				 * an incoming system-specific (Ceylan) command.
				 *
				 * Implements the system-specific protocol for these commands.
				 *
				 * @param commandID the system-specific command ID read
				 * from the first FIFO element of the command.
				 *
				 * @param firstElement the full (first) FIFO element
				 * corresponding to the command (thus containing commandID).
				 *
				 * @note Called automatically by handleReceivedCommand when
				 * relevant.
				 *
				 * @note Only lightweight operations should be performed here.
				 *
				 */
				virtual void handleReceivedSystemSpecificCommand(
						FIFOCommandID commandID, FIFOElement firstElement )
					throw() ;
				
				
				/**
				 * Method responsible for the actual decoding and management of
				 * an incoming command specific to an integrating library
				 * (ex: OSDL).
				 *
				 * Implements the library-specific protocol for these commands.
				 *
				 * @param commandID the library-specific command ID read
				 * from the first FIFO element of the command.
				 *
				 * @param firstElement the full (first) FIFO element
				 * corresponding to the command (thus containing commandID).
				 *
				 * @note Called automatically by handleReceivedCommand when
				 * relevant, made to be overriden by the integrating library.
				 *
				 * @note Only lightweight operations should be performed here.
				 *
				 */
				virtual void handleReceivedIntegratingLibrarySpecificCommand(
						FIFOCommandID commandID, FIFOElement firstElement )
					throw() ;
				
				
				/**
				 * Method responsible for the actual decoding and management of
				 * an incoming application-specific command.
				 *
				 * Meant to be overriden according to the chosen
				 * application-specific protocol.
				 *
				 * @param commandID the application-specific command ID read
				 * from the first FIFO element of the command.
				 *
				 * @param firstElement the full (first) FIFO element
				 * corresponding to the command (thus containing commandID).
				 *
				 * @note Called automatically by handleReceivedCommand when
				 * relevant.
				 *
				 * @note Only lightweight operations should be performed here.
				 *
				 */
				virtual void handleReceivedApplicationCommand(
						FIFOCommandID commandID, FIFOElement firstElement )
					throw() ;
				
				
				/**
				 * Basic helper method called whenever an unexpected application
				 * command is received.
				 *
				 * @param commandID the identifier of the unexpected command.
				 *
				 * @note Basic implementation made to be overriden.
				 *
				 */
				virtual void handleUnexpectedApplicationCommand( 
					FIFOCommandID commandID ) throw() ;		
			
				
				/**
				 * Prepare the first FIFO element for specified command.
				 * Sets the specified ID in the returned FIFO element and,
				 * if used (in safe mode), sets as well the command number.
				 *
				 * @note The two (if using command number) or three (if not)
				 * remaining bytes can be used freely by application code.
				 *
				 * @note Ensure there is exactly one command sent for each
				 * call to this method, otherwise the command count will be
				 * incorrect.
				 *
				 * @param id the command ID to set in the FIFO element.
				 *
				 * @return a FIFO element patched with the specified ID and,
				 * possibly, command number.
				 *
				 */
				virtual FIFOElement prepareFIFOCommand( FIFOCommandID id )
					throw() ;
					
					
				/**
				 * Notifies the ARM7 that a new command has been placed in the
				 * FIFO.
				 *
				 * Increments the sent count, and triggers an IPC IRQ in the
				 * ARM7.
				 *
				 */
				virtual void notifyCommandToARM7() throw() ;
				
				
				/**
				 * Sends a synchronize IRQ to the ARM7, for example to make it
				 * check its receive FIFO.
				 *
				 */				
				virtual void sendSynchronizeInterruptToARM7() throw() ;
				
				

				/**
				 * Tells whether there is data available for reading in the
				 * ARM9 receiving queue.
				 *
				 */
				bool dataAvailableForReading() const throw() ;
				
				
				/**
				 * Tells whether there is space available for writing in the
				 * ARM9 sending queue.
				 *
				 */
				bool spaceAvailableForWriting() const throw() ;
				
	
				
				/**
				 * Helper method that returns the first FIFO element available
				 * in the FIFO.
				 *
				 * @throw FIFOException, including FIFOEmpty if the FIFO has
				 * no such element.
				 *
				 */
				FIFOElement read() throw( FIFOException ) ;
	
	
				/**
				 * Helper method that returns the first FIFO element available
				 * in the FIFO. If not already available, the method will wait
				 * for it (potentially forever), while helping notifying 
				 * never-ending reads and overcoming them, by triggering the
				 * ARM7.
				 *
				 * @throw FIFOException in case a FIFO error is reported.
				 *
				 */
				FIFOElement readBlocking() throw( FIFOException ) ;
	
	
				/**
				 * Helper methods that write the specified FIFO element in the
				 * send queue.
				 *
				 * @throw FIFOException, including FIFOFull if the FIFO has no
				 * free space for that.
				 *
				 */
				void write( FIFOElement toSend ) throw( FIFOException ) ;
	
	
	
				/**
				 * Helper methods that write the specified FIFO element in the
				 * send queue. If there is no more space available in that
				 * queue, the method will wait for it (potentially forever),
				 * while helping notifying never-ending writes and overcoming
				 * them, by triggering the ARM7.
				 *
				 * @throw FIFOException in case a FIFO error is reported.
				 *
				 */
				void writeBlocking( FIFOElement toSend ) 
					throw( FIFOException ) ;


				/**
				 * Returns the current count of processed commands, in [0,15].
				 *
				 */
				FIFOCommandCount getProcessedCount() const throw() ;
				
				
				/**
				 * Returns the current count of sent commands, in [0,15].
				 *
				 */
				FIFOCommandCount getSentCount() const throw() ;
				
				
				/**
				 * Increments the current count of processed commands, in
				 * [0,15], and updates accordingly the IPC sync register.
				 *
				 */
				virtual void incrementProcessedCount() throw() ;
				
				 				
				
				
				// Static section.
				
					
				/**
				 * Callback called whenever the ARM7 triggers an IPC
				 * Synchronize interrupt on the ARM9.
				 *
				 * This signature is mandatory.
				 *
				 * @see handleReceivedCommand
				 *
				 * @see VBlankHandlerForFIFO
				 *
				 */
				static void SyncHandlerForFIFO() ;
	
	
				/**
				 * Manages a command sent through the FIFO.
				 *
				 * Static, to be called as an IRQ handler.
				 *
				 * @see handleReceivedCommand
				 *
				 */
				static void ManageReceivedCommand() ;
				
			
				/**
				 * Returns a textual description of the specified command.
				 *
				 */
				static std::string DescribeCommand( FIFOElement element )
					throw() ;
				
				
				
				// Data members section.
				
				
				/**
				 * ARM9-allocated variable whose address will be sent to the
				 * ARM7 when the activate method will be called so that the
				 * ARM7 can report its status.
				 *
				 * @note We expected to declare it only as 
				 * 'ARM7StatusWord volatile *', as only the pointed value can
				 * be modified by the ARM7. It worked on NoCashGBA emulator,
				 * but not on the DS, until we declared as well the pointer
				 * itself to be volatile. Maybe a problem due to the ARM9 cache.
				 *
				 */
				ARM7StatusWord volatile * volatile _arm7StatusWordPointer ;
		
				
				/**
				 * ARM9-allocated variable whose address will be sent to the
				 * ARM7 when the activate method will be called so that the
				 * ARM7 can report its last error.
				 *
				 * @note We expected to declare it only as 
				 * 'ARM7ErrorCode volatile *', as only the pointed value can
				 * be modified by the ARM7. It worked on NoCashGBA emulator,
				 * but not on the DS, until we declared as well the pointer
				 * itself to be volatile. Maybe a problem due to the ARM9 cache.
				 *
				 */
				ARM7ErrorCode volatile * volatile _arm7ErrorCodePointer ;
				

				/**
				 * Records the overall number of sent commands to the FIFO.
				 *
				 * The number of a command may be stored in its FIFO element
				 * for increased reliability, in safe mode.
				 *
				 * @note 8-bit, hence wraps around after 255.
				 *
				 * @note Automatically incremented when using the 
				 * prepareFIFOCommand method, hence not to be especially
				 * managed by user code.
				 *
				 */
				volatile FIFOCommandCount _localCommandCount ;
				
				
				/**
				 * Records the overall number of received commands to the FIFO.
				 *
				 * The number of a command may be stored in its FIFO element
				 * for increased reliability, in safe mode.
				 *
				 * @note 8-bit, hence wraps around after 255.
				 *
				 * @note Automatically incremented by the handleReceivedCommand
				 * method, hence not to be especially managed by user code.
				 *
				 */
				volatile FIFOCommandCount _remoteCommandCount ;
				
				
				/**
				 * Records the number of commands processed by the ARM9.
				 *
				 * @note Only the 4 lower bits are used, for a count in
				 * [0,15].
				 *
				 * @note Automatically incremented by the handleReceivedCommand
				 * method, hence not to be especially managed by user code.
				 *
				 */
				volatile FIFOCommandCount _processedCount ;
				 
				 
				/**
				 * Records the number of commands sent by the ARM9 to the ARM7.
				 *
				 * @note Only the 4 lower bits are used, for a count in
				 * [0,15].
				 * 
				 * @note Automatically incremented by the notifyCommandToARM7
				 * method, hence not to be especially managed by user code.
				 *
				 */
				volatile FIFOCommandCount _sentCount ;
				
				
				/**
				 * Tells whether this FIFO is activated.
				 * 
				 * @note No need to have ot declared volatile normally.
				 *
				 */
				volatile bool _activated ;
				 
				 
				/**
				 * Records the last battery status retrieved.
				 *
				 */
				volatile System::BatteryStatus _batteryStatus ; 
	
	
				/**
				 * Records the DS type.
				 *
				 */
				volatile System::DSType _dsType ; 
	
	
	
		private:


			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				FIFO( const FIFO & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				FIFO & operator = ( const FIFO & source ) throw() ;
	
			
		} ;	

	}

}


#endif // CEYLAN_FIFO_H_

