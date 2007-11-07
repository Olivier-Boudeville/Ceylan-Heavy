#ifndef CEYLAN_FIFO_H_
#define CEYLAN_FIFO_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanSystem.h"           // for SystemException
#include "CeylanTypes.h"            // for Ceylan::Uint32




// Cannot be in a namespace, as to be included in C code:



/**
 * Describes an ARM7 status word.
 *
 * @note The definition of their meaning is centralized in 
 * CeylanARM7Codes.h
 *
 */
typedef Ceylan::Uint16 ARM7StatusWord ;


/**
 * Describes an ARM7 error word.
 *
 * @note The definition of their meaning is centralized in 
 * CeylanARM7Codes.h
 *
 */
typedef Ceylan::Uint16 ARM7ErrorCode ;


/**
 * The identifier of a FIFO command, to be read from the first byte of 
 * its first FIFO element, using the FIFO command mask.
 *
 * @note The definition of their meaning is centralized in 
 * /CeylanIPCCommands.h
 *
 */
typedef Ceylan::Uint8 FIFOCommandID ;




namespace Ceylan
{


	namespace System
	{
	
	
	
	
		/**
		 * FIFO facilities, made of a logical bidirectional interrupt-based
		 * FIFO.
		 *
		 * The FIFO in itself communicates only using the hardware FIFO: no
		 * shared variable is used. But of course this communication system
		 * can be used to send pointers to (shared) variables.
		 *
		 * The system in itself is in C, as the ARM7 has quite little memory for
		 * its executable. However the ARM9 can rely on this C++ abstraction for
		 * simpler management.
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
		 * Ceylan, for its inner working, has to use too that FIFO, notably so
		 * that logs from the ARM7 can be transferred and managed on the ARM9.
		 * Thus the FIFO has to be shared, and conventions have to be 
		 * respected. 
		 * Each command can be made of any number of FIFO elements (they can be
		 * read one after the other and thus the size of the FIFO does not 
		 * matter), but the first byte of its first element must be the command
		 * identifier: 'commandID = GetFIFOCommandIDFrom( firstFIFOElement )'.
		 * Command identifiers range from 0 to 255. Ceylan reserves all command
		 * ID between 0 and 127, thus the application must choose its
		 * identifiers in the 128-255 range (128 free slots). The three
		 * remaining bytes and the potential next FIFO elements are
		 * command-specific.
		 *
		 * Note that they are 128 free application-specific command identifiers 
		 * from the ARM9 to the ARM7, and another 128 identifier in the other
		 * way round (they have nothing in common).
		 *
		 * Note also that the answer to a command has to use itself a command
		 * identifier, to be able to distinguish an answer for a new request.
		 *
		 * The overriden handleReceivedApplicationCommand method is responsible
		 * for reading all the associated elements, so that they will not 
		 * disturb the next decoding of the handler. 
		 *
		 * A fail-over polling-based FIFO manager may be added for increased
		 * safety, see it in the VBlank handler of the examples.
		 *
		 * @see testCeylanFIFO.arm9.cc and testCeylanFIFO.arm7.c for a complete 
		 * example.
		 *
		 * Due to the use of a callback based on a static method, the FIFO
		 * instance is expected to be a singleton.
		 *
		 */
	

		/// The atomic data that can be sent through the FIFO.
		typedef Ceylan::Uint32 FIFOElement ;
		
		
	
		/**
		 * Bidirectional interrupt-based FIFO (First In, Firt Out) class for
		 * command-based Inter-Process Communication (IPC).
		 *
		 * This class is dedicated to the encapsulation of hardware FIFO, 
		 * notably for the two of the Nintendo DS, between the two ARMs.
		 *
		 * @see the FIFO class for another means, more general, of doing IPC.
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
		
		
		
				/**
				 * Activates the FIFO, and notify the ARM7 of the variables
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
				 * handle the FIFO and the ARM7 report mechanism, an exception
				 * will be thrown.
				 *
				 * @throw FIFOException should the ARM9 time-out short of an
				 * ARM7 update of its status word.
				 *
				 */
				virtual void activate() throw( FIFOException ) ;
				
		
				/**
				 * Deactivates the FIFO.
				 *
				 * @note This method cannot be automatically called from the
				 * constructor, as an asynchronous IRQ could be triggered on the
				 * receive between the call to this method and the end of the
				 * constructor. In that case handleReceivedCommand would
				 * be called, whereas at this moment is still pure virtual
				 * (since still in FIFO constructor).
				 *
				 */
				virtual void deactivate() throw() ;
				
				
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
				 * @note Only lightweight operations should be performed here,
				 * to avoid the saturation of the FIFO slots and 
				 *
				 */
				virtual void handleReceivedApplicationCommand(
						FIFOCommandID commandID, FIFOElement firstElement )
					throw() = 0 ;
				
				
				
				// Status word and error code section.


				/**
				 * Returns the latest status word set by the ARM7.
				 *
				 * Reads the relevant shared variable.
				 *
				 * A zero status word means either no status available, or 
				 * that the status word shared variable was not set already.
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
				 * A zero error code means either no error, or that the error
				 * shared variable was not set already.
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
				 * Sets the specified ID in specified FIFO element.
				 *
				 * @param targetElement the FIFO element that will be patched
				 * with the specified ID.
				 *
				 * @param id the ID to set in specifed FIFO element.
				 *
				 */
				static void SetFIFOCommandIDTo( 
					FIFOElement & targetElement, FIFOCommandID id ) throw() ;
				
				
				/**
				 * Callback to catch the reception of a FIFO element by polling
				 * during the Vblank interrupt.
				 * Expected to be called regularly in the VBlank handler,
				 * to add a polling-based layer of security to the IRQ-based
				 * approach, in case an interrupt is lost.
				 *
				 * Do not use elsewhere than in the VBlank handler, as it
				 * acknowledges the IRQ has been managed.
				 *
				 * This signature is mandatory. Public so that it can be 
				 * registered as an handler externally.
				 *
				 * @note Just checks whether the FIFO is empty. If not, calls
				 * the handleReceivedCommand method.
				 *
				 * @see FIFOHandlerForFIFO
				 *
				 */
				static void VBlankHandlerForFIFO() ;
	
				
				/**
				 * The unique FIFO instance.
				 *
				 */
				static FIFO * _FIFO ;   
				 
			
				 

		protected:
				
				
				/**
				 * ARM9-allocated variable whose address will be sent to the
				 * ARM7 when the activate method will be called so that the
				 * ARM7 can report its status.
				 *
				 */
				ARM7StatusWord volatile * _arm7StatusWordPointer ;
				
				
				/**
				 * ARM9-allocated variable whose address will be sent to the
				 * ARM7 when the activate method will be called so that the
				 * ARM7 can report its last error.
				 *
				 */
				ARM7ErrorCode volatile * _arm7ErrorCodePointer ;
				

				/**
				 * Callback to catch the reception of a FIFO element when the
				 * FIFO not empty IRQ has been triggered.
				 *
				 * This signature is mandatory.
				 *
				 * @note Just results in a call to its virtual method 
				 * counterpart.
				 *
				 * @see handleReceivedCommand
				 *
				 * @see VBlankHandlerForFIFO
				 */
				static void FIFOHandlerForFIFO() ;


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
				 * Tells whether there is data available for reading in the
				 * receiving queue.
				 *
				 */
				bool dataAvailableForReading() const throw() ;
				
				
				/**
				 * Tells whether there is space available for writing in the
				 * sending queue.
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
				 * @note The associated interrupt will be acknowledged, even
				 * if there was more than one element in the queue: elements may
				 * remain there.
				 *
				 */
				FIFOElement read() throw( FIFOException ) ;
	
	
				/**
				 * Helper method that returns the first FIFO element available
				 * in the FIFO. If not already available, the method will wait
				 * for it (potentially forever), using atomic sleeps.
				 *
				 * @note The associated interrupt will be acknowledged, even
				 * if there was more than one element in the queue: elements may
				 * remain there.
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
				 * using atomic sleeps.
				 *
				 * @throw FIFOException in case a FIFO error is reported.
				 *
				 */
				void writeBlocking( FIFOElement toSend ) 
					throw( FIFOException ) ;
	
	
				/**
				 * Manages a command being sent through the FIFO.
				 *
				 * @see handleReceivedCommand
				 *
				 */
				static void ManageReceivedCommand() ;
				
	
	
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

