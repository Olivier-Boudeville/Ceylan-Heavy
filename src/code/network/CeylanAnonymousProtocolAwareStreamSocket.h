#ifndef CEYLAN_ANONYMOUS_PROTOCOL_AWARE_STREAM_SOCKET_H_
#define CEYLAN_ANONYMOUS_PROTOCOL_AWARE_STREAM_SOCKET_H_


#include "CeylanAnonymousStreamSocket.h"   // for inheritance
#include "CeylanSystem.h"                  // for FileDescriptor

#include <string>



namespace Ceylan
{


	namespace Middleware
	{

		/*
		 * An anonymous protocol-aware stream sockets triggers a protocol 
		 * server.
		 *
		 */
		class ProtocolServer ;
		
	}
	
	
	
	namespace Network
	{



		/**
		 * Anonymous connection-based server-side stream socket linked with
		 * a protocol server, which manages higher-level message exchanges
		 * for that connection.
		 *
		 * When a server has multiple connections to manage, it can spawn a 
		 * protocol server whose marshaller is associated with the anonymous
		 * stream socket which is in charge of this connection.
		 *
		 * @see MultiplexedProtocolBasedServer
		 *
		 */
		class AnonymousProtocolAwareStreamSocket: public AnonymousStreamSocket
		{
		
			

			public:
	
	
				/**
				 * Constructs a new server-side socket dedicated to an
				 * accepted connection, and links it with a protocol server
				 * so that this socket can notice it of incoming data.
				 *
				 * This socket will use specified listening server file
				 * descriptor to accept a new connection that it will manage
				 * as long as the connection is running, according to the
				 * rules enforced by the associated protocol server.
				 *
				 * @param listeningFD listening file descriptor of this 
				 * server socket, used to accept a new connection.
				 *
				 * @param protocolServerToTrigger the protocol server instance
				 * that this protocol-aware socket will have to trigger 
				 * whenever some data is to be read. This socket takes 
				 * ownership of the protocol server, since when the socket is
				 * deallocated the server is of no use.
				 *
				 * @throw SocketException if socket creation failed.
				 *
				 */
				AnonymousProtocolAwareStreamSocket( 
						System::FileDescriptor listeningFD,
						Middleware::ProtocolServer & protocolServerToTrigger ) 
					throw( SocketException ) ;
	
				
				/**
				 * Constructs a new server-side socket dedicated to an
				 * accepted connection. 
				 *
				 * This socket shall be linked with a protocol server
				 * so that this socket can notice it of incoming data.
				 *
				 * This socket will use specified listening server file
				 * descriptor to accept a new connection that it will manage
				 * as long as the connection is running, according to the
				 * rules enforced by the protocol server that will be
				 * associated with it.
				 *
				 * @param listeningFD listening file descriptor of this 
				 * server socket, used to accept a new connection.
				 *
				 * @throw SocketException if socket creation failed.
				 *
				 */
				explicit AnonymousProtocolAwareStreamSocket( 
						System::FileDescriptor listeningFD ) 
					throw( SocketException ) ;
	
				
				/// Virtual destructor.
				virtual ~AnonymousProtocolAwareStreamSocket() throw() ;

	
	
				/**
				 * Returns whether this socket is associated with a protocol
				 * server.
				 *
				 * @return iff there is a registered protocol server for this
				 * socket.
				 *
				 */
				virtual bool hasProtocolServer() const throw() ;

	
				/**
				 * Returns the owned protocol server managing this socket
				 * communications.
				 *
				 * @throw AnonymousStreamSocketException if no protocol server
				 * is associated with this socket.
				 *
				 */
				virtual Middleware::ProtocolServer & getProtocolServer() 
					throw( AnonymousStreamSocketException ) ;
				
				
				/**
				 * Sets the new protocol server managing this socket
				 * communications. 
				 *
				 * It will be owned (hence deallocated) by this socket.
				 *
				 * If a protocol server was already set, the former will be
				 * deallocated and then replaced by the newer.
				 *
				 */
				virtual void setProtocolServer( 
					Middleware::ProtocolServer & newProtocolServer ) throw() ;
				
				
				
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
				 * The protocol server this socket is associated with.
				 *
				 * @note The server is owned by the socket.
				 *
				 */
				Middleware::ProtocolServer * _protocolServer ;
				


			private:
	
				
				
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				AnonymousProtocolAwareStreamSocket( 
						const AnonymousProtocolAwareStreamSocket & source ) 
					throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				AnonymousProtocolAwareStreamSocket & operator = ( 
						const AnonymousProtocolAwareStreamSocket & source )	
					throw() ;

		
		
		} ;
		
	}
	
}		



#endif // CEYLAN_ANONYMOUS_PROTOCOL_AWARE_STREAM_SOCKET_H_
