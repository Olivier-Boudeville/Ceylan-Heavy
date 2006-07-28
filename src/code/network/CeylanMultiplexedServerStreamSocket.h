#ifndef CEYLAN_MULTIPLEXED_SERVER_STREAM_SOCKET_H_
#define CEYLAN_MULTIPLEXED_SERVER_STREAM_SOCKET_H_


#include "CeylanServerStreamSocket.h" // for inheritance
#include "CeylanSystem.h"             // for FileDescriptor

#include <string>



namespace Ceylan
{


	namespace System
	{
	
		/// Multiplexed servers have anonymous IO streams.
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
		 * new socket creation being created. Thus there can be n 
		 * simultaneously connected sockets, each one being viewed as an 
		 * anonymous input/output stream.
		 *
		 * @see ServerMultiplexedStreamSocket for a server that manages any
		 * number of clients too, but sequentially rather than in parallel.
		 *
		 * @see ClientStreamSocket
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

				


			private:
	
			
		
		
		} ;
		
	}
	
}		



#endif // CEYLAN_MULTIPLEXED_SERVER_STREAM_SOCKET_H_
