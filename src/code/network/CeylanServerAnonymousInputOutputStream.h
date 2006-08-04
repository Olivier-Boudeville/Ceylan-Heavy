#ifndef CEYLAN_SERVER_ANONYMOUS_INPUT_OUTPUT_STREAM_H_
#define CEYLAN_SERVER_ANONYMOUS_INPUT_OUTPUT_STREAM_H_


#include "CeylanAnonymousInputOutputStream.h" // for inheritance
#include "CeylanNetwork.h"                    // for NetworkException
#include "CeylanSystem.h"                     // for FileDescriptor
#include "CeylanStream.h"                     // for System::StreamID


#include <string>




namespace Ceylan
{


	namespace Network
	{

	
		/// Server anonymous I/O streams records informations from their peer.
		class SystemSpecificSocketAddress ;
		

		/// Exception class for server anonymous stream issues.
		class ServerAnonymousInputOutputStreamException: 
			public NetworkException
		{
			public:
				explicit ServerAnonymousInputOutputStreamException( 
					const std::string & message ) throw() ;
				virtual ~ServerAnonymousInputOutputStreamException() throw() ;
		} ;
		
		
		
		/**
		 * Server-side spawned connection.
		 *
		 * @see SequentialServerStreamSocket, MultiplexedServerStreamSocket.
		 *
		 */
		class ServerAnonymousInputOutputStream: 
			public System::AnonymousInputOutputStream
		{
		

			public:
	

	
				/**
				 * Basic constructor for server-side anonymous streams, whose
				 * instances are created not selected.
				 *
				 * @param fd the file descriptor whose ownership is taken 
				 * by this stream.
				 *
				 * Extracts the first connection request on the queue of 
				 * pending connections, creates a new connected system socket, 
				 * and stores :
				 *   - a new file descriptor referring to that socket
				 *   - informations about peer address
				 *
				 * The specified file descriptor is unaffected by this call.
				 *
				 * @see man 2 accept
				 *
				 * @throw ServerAnonymousInputOutputStreamException if the
				 * operation failed, included if the file descriptor feature
				 * is not available or if accept failed.
				 *
				 */
				ServerAnonymousInputOutputStream( System::FileDescriptor fd ) 
					throw( ServerAnonymousInputOutputStreamException,
						StreamException ) ;
	
	
				/// Virtual destructor.
				virtual ~ServerAnonymousInputOutputStream() throw() ;
	
		
				/**
				 * Returns the system-specific peer address.
				 *
				 */
				virtual const SystemSpecificSocketAddress & 
					getPeerAddress() const throw() ;
				
				
				/// Returns the input stream's unique ID.
				virtual System::StreamID getInputStreamID() const throw() ;

				/// Returns the output stream's unique ID.
				virtual System::StreamID getOutputStreamID() const throw() ;


				/**
				 * Tells whether there is data available on input.
				 *
				 */
				virtual bool hasAvailableData() const throw() ;


				/**
				 * Closes the stream.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 */
				virtual bool close() throw( CloseException ) ;
				
				
            	/**
            	 * Returns a user-friendly description of the state of 
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
	
	
	
			protected:
	
	
				/// Sets this input stream's unique ID.
				virtual void setInputStreamID( 
					System::StreamID newInputStreamID )	throw() ;
	
	
				/// Sets this input stream's unique ID.
				virtual void setOutputStreamID( 
					System::StreamID newOutputStreamID ) throw() ;
	
	
				/// Stores the file descriptor of this stream.
				System::FileDescriptor _fdes ;
	
				/// The system-specific address of the peer.
				SystemSpecificSocketAddress * _peerAddress ;
	
	
	
	
			private:
	
		
	
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				ServerAnonymousInputOutputStream( 
					const ServerAnonymousInputOutputStream & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				ServerAnonymousInputOutputStream & operator = ( 
					const ServerAnonymousInputOutputStream & source ) throw() ;
					
					
		} ;

	}
	
} 


#endif // CEYLAN_SERVER_ANONYMOUS_INPUT_OUTPUT_STREAM_H_
