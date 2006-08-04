#ifndef CEYLAN_MULTIPLEXED_SERVER_STREAM_SOCKET_H_
#define CEYLAN_MULTIPLEXED_SERVER_STREAM_SOCKET_H_


#include "CeylanServerStreamSocket.h" // for inheritance
#include "CeylanSystem.h"             // for FileDescriptor

#include <string>
#include <set>                        // for _currentConnections



namespace Ceylan
{


	namespace System
	{
	
		/// Multiplexed servers create anonymous IO streams.
		class AnonymousInputOutputStream ;
		
	}
		

	namespace Network
	{



		/**
		 * Server-side implementation of a server listening socket and its 
		 * connection-triggered sockets, each one corresponding to a 
		 * connected client.
		 *
		 * This server basically manages n+1 sockets : the listening one, 
		 * which, whenever incoming connections are accepted, leads to a 
		 * new socket creation being created, and n simultaneously connected
		 * sockets, each one being viewed as an anonymous input/output stream.
		 *
		 * @see ServerMultiplexedStreamSocket for a server that manages any
		 * number of clients too, but sequentially rather than in parallel.
		 *
		 * @see ClientStreamSocket
		 *
		 * The clearInput method operates on the listening socket, it does
		 * not apply to any of the transport streams.
		 *
		 */
		class MultiplexedServerStreamSocket: public ServerStreamSocket
		{
		

			public:
	

				/**
				 * Mother class for all stream socket-related exceptions, 
				 * on the server side.
				 *
				 */
				class MultiplexedServerStreamSocketException: 
					public StreamSocketException
				{ 
					public: 
					
						explicit MultiplexedServerStreamSocketException( 
							const std::string & reason ) throw() ;
						
						virtual ~MultiplexedServerStreamSocketException()
							 throw() ; 
							
				} ;



	
				/**
				 * Constructs a new multiplexed server-side socket.
				 *
				 * @param port the TCP port of this server socket.
				 *
				 * @param reuse tells whether the local addresses are allowed
				 * to be reused in bind().
				 *
				 * @throw SocketException if socket creation failed.
				 *
				 */
				explicit MultiplexedServerStreamSocket( Port port, 
						bool reuse = true )	throw( SocketException ) ;
	
				
				/// Virtual destructor.
				virtual ~MultiplexedServerStreamSocket() throw() ;
	
	
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
				 * Accepts all incoming connections, as long as it is not
				 * requested to stop.
				 *
				 * If there is no pending connection present on the queue,
				 * blocks the caller until a connection is present. 
				 *
				 * @throw ServerStreamSocketException on failure.
				 *
				 * @see stop
				 *
				 */
				virtual void accept() throw( ServerStreamSocketException ) ;
	


				/**
				 * Manages a connection for which data is available.
				 *
				 * This method is to be subclassed so that this multiplexed
				 * server can perform its task on each per-connection socket,
				 * viewed as an abstract input/output stream.
				 *
				 * @note It is the responability of this method to return 
				 * approriately, i.e. if it does not return or if it returns
				 * after a long time, all other connections will be frozen
				 * in the meantime.
				 *
				 * @param stream the input/output stream that is dedicated to
				 * a particular connection to this server. When this method is
				 * called by the server, the stream is selected, i.e. has 
				 * already data available for reading. This data may be 
				 * interpreted by this method as a request that can be decoded,
				 * then applied by the server, then its result can be sent 
				 * back to the client thanks to the same I/O stream.
				 *
				 * @throw MultiplexedServerStreamSocketException if an error
				 * occurred.
				 *
				 */
				virtual void handleConnection( 
						System::AnonymousInputOutputStream & stream )
					throw( MultiplexedServerStreamSocketException ) = 0 ;
					
	
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
				 * The list of current accepted connection-based sockets.
				 *
				 * It is a set of pointers and not a list, since not only it
				 * has to be enumerated, but also it is searched for specific
				 * elements (with find).
				 *
				 */
				std::set<System::AnonymousInputOutputStream *>
					_currentConnections ;


			private:
	
			
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				MultiplexedServerStreamSocket( 
					const MultiplexedServerStreamSocket & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				MultiplexedServerStreamSocket & operator = ( 
					const MultiplexedServerStreamSocket & source ) throw() ;
		
		
		} ;
		
	}
	
}		



#endif // CEYLAN_MULTIPLEXED_SERVER_STREAM_SOCKET_H_
