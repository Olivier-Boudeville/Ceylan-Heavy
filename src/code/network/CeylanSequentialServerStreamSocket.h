#ifndef CEYLAN_SEQUENTIAL_SERVER_STREAM_SOCKET_H_
#define CEYLAN_SEQUENTIAL_SERVER_STREAM_SOCKET_H_


#include "CeylanServerStreamSocket.h"  // for inheritance
#include "CeylanSystem.h"              // for FileDescriptor

#include <string>



namespace Ceylan
{


	namespace Network
	{



		/**
		 * Server, based on stream sockets, that can serve any number of 
		 * clients, although not in parallel : clients are taken care of 
		 * sequentially, one after the other. 
		 *
		 * Connection requests that are received while one connection is 
		 * already running are queued.
		 * If the queue is full, next connections will be refused. 
		 *
		 * This server basically manages two sockets : the listening one, 
		 * which, whenever incoming connections are accepted, leads to a 
		 * new socket creation. This service is designed to take care of at 
		 * most a client at a time, but, depending on how the accepted method
		 * is overriden, a given server can handle sequentially any number of
		 * clients. 
		 *
		 * On exit, after the process is terminated, the socket port should
		 * be readily available for next socket creations.
		 *
		 * More advanced servers can handle one listening socket and as 
		 * many sockets as there are accepted connections.
		 *
		 * @see ServerMultiplexedStreamSocket for a server that manages any
		 * number of clients too, but in parallel rather than sequentially.
		 *
		 * @see ClientStreamSocket
		 *
		 */
		class SequentialServerStreamSocket: public ServerStreamSocket
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

			

			public:
	


				/**
				 * Mother class for all stream socket-related exceptions, 
				 * on the server side.
				 *
				 */
				class SequentialServerStreamSocketException: 
					public ServerStreamSocketException
				{ 
					public: 
					
						explicit SequentialServerStreamSocketException( 
							const std::string & reason ) throw() ;
						
						virtual ~SequentialServerStreamSocketException() 
							throw() ; 
							
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
				explicit SequentialServerStreamSocket( Port port, 
					bool reuse = true )	throw( SocketException ) ;
	
				
				/// Virtual destructor.
				virtual ~SequentialServerStreamSocket() throw() ;
	
					
				
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
				 * Closes any accepted connection, including its socket.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 */
				virtual bool closeAcceptedConnections() 
					throw( Stream::CloseException ) ;
				 


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
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				SequentialServerStreamSocket( 
					const SequentialServerStreamSocket & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				SequentialServerStreamSocket & operator = ( 
					const SequentialServerStreamSocket & source ) throw() ;

		
		
		} ;
		
	}
	
}		



#endif // CEYLAN_SEQUENTIAL_SERVER_STREAM_SOCKET_H_
