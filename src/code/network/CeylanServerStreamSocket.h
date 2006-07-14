#ifndef CEYLAN_SERVER_STREAM_SOCKET_H_
#define CEYLAN_SERVER_STREAM_SOCKET_H_


#include "CeylanStreamSocket.h"      // for inheritance
#include "CeylanTypes.h"             // for Ceylan::Uint32
#include "CeylanSystem.h"            // for FileDescriptor

#include <string>



namespace Ceylan
{


	namespace Network
	{



		/**
		 * Server-side implementation of connection-based socket.
		 *
		 * @note For read/write operations with such server socket, 
		 * the file descriptor being used is the one returned by 
		 * accept (i.e. the per-connection socket), not the main
		 * listening socket.
		 *
		 * @see ClientStreamSocket
		 *
		 */
		class ServerStreamSocket: public StreamSocket
		{
		

			/**
			 * Opaque handle for forward-declared but undefined struct
			 * pointer to system socket address, used to avoid
			 * including system-specific headers which define for 
			 * example sockaddr_in.
			 *
			 * Otherwise the API exposed by Ceylan would depend on these
			 * headers, then on a config.h that should then be installed
			 * but may clash with others, and so on.
			 *
			 */
			struct SystemSpecificSocketAddress ;


			/// Records connection count.
			typedef Ceylan::Uint32 ConnectionCount ;
			

			public:
	


				/**
				 * Mother class for all stream socket-related exceptions, 
				 * on the server side.
				 *
				 */
				class ServerStreamSocketException: public StreamSocketException
				{ 
					public: 
					
						explicit ServerStreamSocketException( 
							const std::string & reason ) throw() ;
						
						virtual ~ServerStreamSocketException() throw() ; 
							
				} ;



	
				/**
				 * Constructs a new server-side socket.
				 *
				 * @param port the TCP port of this server socket.
				 *
				 * @param reuse tells whether the local addresses are allowed
				 * to be reused in bind().
				 *
				 * @throw SocketException if socket creation failed.
				 *
				 */
				explicit ServerStreamSocket( Port port, bool reuse = true )
					throw( SocketException ) ;
	
				
				/// Virtual destructor.
				virtual ~ServerStreamSocket() throw() ;
	
	
				/**
				 * Accepts first available incoming connection.
				 *
				 * If there is no pending connection present on the queue,
				 * blocks the caller until a connection is present. 
				 *
				 * @throw ServerStreamSocketException on failure.
				 *
				 */
				virtual void accept() throw( ServerStreamSocketException ) ;
	

				/**
				 * Returns the new file descriptor, obtained after accept(),
				 * corresponding to a new connected socket taken from the
				 * queue of pending connections.
				 *
				 * This file descriptor will no longer be the same after next
				 * accept().
				 *
				 * @throw FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 * @see getFileDescriptor()
				 *
				 */
				virtual System::FileDescriptor 
						getFileDescriptorForTransport() const
					throw( Features::FeatureNotAvailableException ) ;
		
	
				/**
				 * Returns the current maximum number of pending connections
				 * for this socket.
				 *
				 */
				virtual ConnectionCount getMaximumPendingConnectionsCount()
					const throw() ;

					
				/**
				 * Sets the current maximum number of pending connections
				 * for this socket.
				 *
				 */
				virtual void setMaximumPendingConnectionsCount( 
					ConnectionCount newMax ) throw() ;	


				/**
				 * Default value of the maximum length the queue of pending
				 * connections.
				 *
				 */
				static const Ceylan::Uint32
					DefaultMaximumPendingConnectionsCount = 20 ;
	
	
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

		
				/** 
				 * Prepares the socket for accepting connections.
				 * It calls bind() and listen().
				 *
				 * @throw ServerStreamSocketException if the operation failed,
				 * including if bind or listen failed.
				 *
				 */
				virtual void prepareToAccept() 
					throw( ServerStreamSocketException ) ;
		
		
				/**
				 * Called whenever the accept method succeeds.
				 *
				 * @note This method is made to be overriden by actual 
				 * specialized servers.
				 *
				 * @throw ServerStreamSocketException on failure.
				 *
				 */
				virtual void accepted() throw( ServerStreamSocketException ) ;



			private:
	
			
				/**
				 * Stores the latest file descriptor corresponding to 
				 * an accepted incoming connection.
				 *
				 */
				System::FileDescriptor _acceptedFileDescriptor ;
		
		
				/**
				 * The system-specific socket address for the latest accepted 
				 * client.
				 *
				 */
				SystemSpecificSocketAddress * _clientAddress ;
		

				/**
				 * Tells whether this server socket is already bound, i.e. if a
				 * bind operation has already been performed successfully.
				 *
				 */
				bool _bound ;
				
				
				/**
				 * This socket current maximum length for the queue of pending
				 * connections.
				 *
				 * @see DefaultMaximumPendingConnectionsCount
				 *
				 */
				Ceylan::Uint32 _maximumPendingConnectionsCount ;
				
		
		
		} ;
		
	}
	
}		



#endif // CEYLAN_SERVER_STREAM_SOCKET_H_
