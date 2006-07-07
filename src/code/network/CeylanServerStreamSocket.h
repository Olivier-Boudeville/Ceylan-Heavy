#ifndef CEYLAN_SERVER_STREAM_SOCKET_H
#define CEYLAN_SERVER_STREAM_SOCKET_H


#include "CeylanStreamSocket.h"      // for inheritance

#include <string>



namespace Ceylan
{


	namespace Network
	{



		/**
		 * Server-side implementation of connection-based socket.
		 *
		 * @see ClientStreamSocket
		 *
		 */
		class ServerStreamSocket: public StreamSocket
		{
		

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
		* Constructs a new server side socket.
		* @param port server port.
		* @param reuse says if the local addresses are allowed to be reused in bind().
		*/
		Server( Port port, bool reuse = true );
	
		virtual ~Server();
	
		/**
		* Acceps a connection.
		*/
		bool accept();
	
		/**
		* Returns the new file description after accept().
		* This fd will no longer be the same after next accept().
		*/
		inline int getNewFileDescriptor() const;
	
	protected:

		virtual int getFD() const;
		
		/// Called after accept();
		virtual void accepted();
	
		/// This method is called when bind() fails.
		virtual void bindFailed();
	
		/// This method is called when listen() fails.
		virtual void listenFailed();
	
		/// This method is called when accept() fails.
		virtual void acceptFailed();

		/** 
		* Prepares the socket for accepting connections.
		* It calls bind() and listen().
		*/
		bool prepareToAccept();
		

	private:
	
		#ifdef NO_SOCKLEN_T
		typedef int socklen_t;
		#endif
		
		int _nfdes;
		
		sockaddr_in _client;
		
		bool _bind;
		
};


int Server::getNewFileDescriptor() const
{
	return _nfdes;
}


#endif // CEYLAN_SERVER_STREAM_SOCKET_H
