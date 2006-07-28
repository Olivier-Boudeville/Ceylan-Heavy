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
		 * This server basically factorizes all primitives common to all
		 * stream server implementations, including one managing at most one 
		 * connection at a time (SequentialServerStreamSocket) and one able
		 * to deal with any number of simultaneous connections 
		 * (MultiplexedServerStreamSocket).
		 *
		 * On exit, after the process is terminated, the socket port should
		 * be readily available for next socket creations.
		 *
		 * @note For read/write operations with such server socket, 
		 * the file descriptor being used is one returned by accept 
		 * (i.e. a per-connection socket), not the main listening socket.
		 *
		 * @see ClientStreamSocket
		 *
		 * @see ServerMultiplexedStreamSocket for a server that manages any
		 * number of clients too, but in parallel rather than sequentially.
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
				 * Activates this server so that it can handle incoming 
				 * requests.
				 *
				 * This main loop while last as long as the server is not 
				 * requested to stop.
				 *
				 * @see requestToStop()
				 *
				 */
				virtual void run() throw( ServerStreamSocketException ) ;
				
				
				/**
				 * Accepts first available incoming connection.
				 *
				 * If there is no pending connection present on the queue,
				 * blocks the caller until a connection is present. 
				 *
				 * @throw ServerStreamSocketException on failure.
				 *
				 */
				virtual void accept() 
					throw( ServerStreamSocketException ) = 0  ;
			
	
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
				 * Restores the server state after an accepted connection is
				 * over, so that next connections in the queue can be accepted
				 * in turn.
				 *
				 * @throw ServerStreamSocketException if the operation failed.
				 *
				 */
				virtual void cleanAfterAccept()
					throw( ServerStreamSocketException ) ;
		
		
				/** 
				 * Closes any accepted connection, including its socket.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 */
				virtual bool closeAcceptedConnections() 
					throw( Stream::CloseException ) = 0 ;
		
		
				/**
				 * Called whenever the accept method succeeds.
				 *
				 * If this server has to handle (sequentially) multiple
				 * clients, then the overriden implementation of this method
				 * should end up at least with a close, with _bound set to
				 * false. And the server must loop with regular calls to the 
				 * accept method.
				 *
				 * Otherwise, if the server stops accepting connections after
				 * the one being processed, any other connection initiated 
				 * after the current one was begun will wait till this latter
				 * is terminated, and will have this exception thrown :
				 * StreamSocket::read failed : Ceylan::System::FDRead failed :
				 * Connection reset by peer, since the server refuses upcoming
				 * connections.
				 *
				 * @note This method is made to be overriden by actual 
				 * specialized servers.
				 *
				 * @throw ServerStreamSocketException on failure.
				 *
				 * @see testCeylanServerStream.cc
				 *
				 */
				virtual void accepted() throw( ServerStreamSocketException ) ;


				/**
				 * Tells whether this server will stop as soon as possible,
				 * no later than any current connection is over.
				 *
				 */
				virtual bool isRequestedToStop() const throw() ;
				
				
				/**
				 * Requests the server to stop just after having completed an
				 * eventual current connection.
				 *
				 * Multiple stop requests can be sent.
				 *
				 */
				virtual void requestToStop() throw() ;
				 


				/**
				 * Tells whether this server socket is already bound, i.e. if a
				 * bind operation has already been performed successfully.
				 *
				 */
				bool _bound ;




			private:
	
				
				
				/**
				 * Tells whether this server socket is requested to stop, i.e.
				 * whether, when any currently processed connection is over,
				 * the server will stop responding to new requests, whether
				 * they are already waiting in the connection queue or not.
				 * 
				 */
				bool _stopRequested ;
				
				
				/**
				 * This socket current maximum length for the queue of pending
				 * connections.
				 *
				 * @see DefaultMaximumPendingConnectionsCount
				 *
				 */
				Ceylan::Uint32 _maximumPendingConnectionsCount ;
				
				
				
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				ServerStreamSocket( const ServerStreamSocket & source ) 
					throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				ServerStreamSocket & operator = ( 
					const ServerStreamSocket & source )	throw() ;

		
		
		} ;
		
	}
	
}		



#endif // CEYLAN_SERVER_STREAM_SOCKET_H_
