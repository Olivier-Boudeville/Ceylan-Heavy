#ifndef CEYLAN_CLIENT_STREAM_SOCKET_H_
#define CEYLAN_CLIENT_STREAM_SOCKET_H_


#include "CeylanStreamSocket.h"      // for inheritance

#include <string>




namespace Ceylan
{


	namespace Network
	{




		/// A ClientStreamSocket owns a HostDNSEntry :
		class HostDNSEntry ;
		

		/**
		 * Client-side implementation of connection-based socket.
		 *
		 * @see ServerStreamSocket
		 *
		 */
		class ClientStreamSocket: public StreamSocket
		{
		

			public:
	


				/**
				 * Mother class for all stream socket-related exceptions, 
				 * on the client side.
				 *
				 */
				class ClientStreamSocketException: public StreamSocketException
				{ 
					public: 
					
						explicit ClientStreamSocketException( 
							const std::string & reason ) throw() ;
						
						virtual ~ClientStreamSocketException() throw() ; 
							
				} ;



	
				/**
				 * Constructs a client-side stream socket.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				ClientStreamSocket() throw( SocketException ) ;
	
				virtual ~ClientStreamSocket() throw() ;
	
	
				/**
				 * Connects this socket to the specified server.
				 *
				 * @param serverHostname the hostname of the server.
				 *
				 * @param port the TCP port of the server.
				 *
				 * @throw SocketException if the operation failed.
				 *				 
				 */
				virtual void connect( const std::string & serverHostname, 
						Port port )
					throw( SocketException ) ;
	
	
	
			protected:
	
	
				/// Called after the connection is established.
				virtual void connected() ;
	

				/// Returns the hostname of the server.
				const std::string & getServerName() const throw() ;
	
	
			private:
	
				
				/// The hostname of the server this socket is to be linked to.
				std::string _serverHostName ;
	
				/// DNS record for linked server.
				HostDNSEntry * _serverHostInfo ;
	
	
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				ClientStreamSocket( 
					const ClientStreamSocket & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				ClientStreamSocket & operator = ( 
					const ClientStreamSocket & source )	throw() ;
					
					
		} ;

	}
	
} 


#endif // CEYLAN_CLIENT_STREAM_SOCKET_H_
