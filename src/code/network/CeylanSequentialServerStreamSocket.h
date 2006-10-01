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
		 * A sequential stream server owns at most one anonymous stream
		 * socket at a time, which is spawned each time a new connection 
		 * is accepted. 
		 *
		 */
		class AnonymousStreamSocket ;



		/**
		 * Server, based on stream sockets, that can serve any number of 
		 * clients, although not in parallel : clients are taken care of 
		 * sequentially, one after the other, each one thanks to a dedicated
		 * anonymous stream socket. 
		 *
		 * Connection requests that are received while one connection is 
		 * already running are queued.
		 * If the queue is full, next connections will be refused. 
		 *
		 * This server basically manages two sockets : the listening one, 
		 * which, whenever incoming connections are accepted, leads to a 
		 * new (anonymous) socket creation. 
		 *
		 * This service is designed to take care of at most a client at 
		 * a time, but, depending on how the accepted method is overriden, 
		 * a given server can handle sequentially any number of clients. 
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
		 * @see ClientStreamSocket a client that is designed for both 
		 * sequential and multiplexed servers.
		 *
		 * Following methods have to be subclassed so that the server can
		 * perform its specific task :
		 *   - accepted
		 *
		 * Read/write methods communicate with the connection socket, not the
		 * listening socket.
		 * 
		 */
		class CEYLAN_DLL SequentialServerStreamSocket: public ServerStreamSocket
		{
		


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
				 * Constructs a new server-side sequential stream socket.
				 *
				 * @param port the TCP port of this listening server socket.
				 *
				 * @param reuse tells whether the local addresses are allowed
				 * to be reused in bind().
				 *
				 * @throw SocketException if socket creation failed.
				 *
				 */
				explicit SequentialServerStreamSocket( Port listeningPort, 
					bool reuse = true )	throw( SocketException ) ;
	
				
				/// Virtual destructor.
				virtual ~SequentialServerStreamSocket() throw() ;
	

				/**
				 * Tells whether this socket is currently connected to a 
				 * client.
				 *
				 */
				virtual bool isConnected() const throw() ;

					
				
				/**
				 * Accepts first available incoming connection.
				 *
				 * If there is no pending connection present on the queue,
				 * blocks the caller until a connection is present. 
				 *
				 * @return a null pointer, since the return of this call
				 * corresponds to the end of the related connection.
				 *
				 * @throw ServerStreamSocketException on failure.
				 *
				 */
				virtual AnonymousStreamSocket * accept() 
					throw( ServerStreamSocketException )  ;
	

				/**
				 * Returns the new file descriptor, obtained after accept(),
				 * corresponding to a new connected socket taken from the
				 * queue of pending connections.
				 *
				 * This file descriptor will no longer be the same after next
				 * accept().
				 *
				 * @throw SocketException if the operation failed, including
				 * if there is no anonymous socket available for transport,
				 * or FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 * @see getFileDescriptor()
				 *
				 */
				virtual System::FileDescriptor getFileDescriptorForTransport()
					const throw( SocketException, 
						Features::FeatureNotAvailableException ) ;
		
	
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
				 * Stores informations about the latest accepted incoming
				 * connection and the corresponding remote peer.
				 *
				 * It does not correspond to the listening socket, but to
				 * an anonymous socket especially spawned for the current
				 * connection.
				 *
				 */
				AnonymousStreamSocket * _currentConnection ;
				
		

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
