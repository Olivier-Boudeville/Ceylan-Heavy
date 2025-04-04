/*
 * Copyright (C) 2003-2013 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option)
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef CEYLAN_CLIENT_STREAM_SOCKET_H_
#define CEYLAN_CLIENT_STREAM_SOCKET_H_


#include "CeylanStreamSocket.h"      // for inheritance

#include <string>




namespace Ceylan
{


	namespace Network
	{



		/// A ClientStreamSocket owns a HostDNSEntry:
		class HostDNSEntry ;



		/**
		 * Client-side implementation of connection-based socket.
		 *
		 * At any time, a client stream socket can be connected at most at one
		 * server.
		 *
		 * These client sockets do not care about the nature of their servers,
		 * which can be indifferently sequential or multiplexed.
		 *
		 * @see SequentialServerStreamSocket, MultiplexedServerStreamSocket.
		 *
		 */
		class CEYLAN_DLL ClientStreamSocket : public StreamSocket
		{


			public:



				/**
				 * Mother class for all stream socket-related exceptions, on the
				 * client side.
				 *
				 */
				class CEYLAN_DLL ClientStreamSocketException :
					public StreamSocketException
				{

					public:

						explicit ClientStreamSocketException(
							const std::string & reason ) ;

						virtual ~ClientStreamSocketException() throw() ;

				} ;




				/**
				 * Constructs a client-side stream socket.
				 *
				 * @throw SocketException if socket creation failed.
				 *
				 */
				ClientStreamSocket() ;



				/// Virtual destructor.
				virtual ~ClientStreamSocket() throw() ;



				/**
				 * Tells whether this socket is currenty connnected to a server.
				 *
				 */
				virtual bool isConnected() const ;



				/**
				 * Connects this socket to the specified server.
				 *
				 * Once the connection succeeds, the connected() method is
				 * automatically called.
				 *
				 * @see connected
				 *
				 * Then when this method returns, the disconnect() method is
				 * called.
				 *
				 * @param serverHostname the hostname of the server.
				 *
				 * @param serverPort the TCP port of the server.
				 *
				 * @throw SocketException if the operation failed.  If no server
				 * is available at the target endpoint, this method will fail
				 * with a SocketException carrying the message "Connection
				 * refused", ex: "could not connect to IP 127.0.0.1 for host
				 * 'localhost': Connection refused". The same error will occur
				 * if there is a server at the endpoint but its queue of pending
				 * connections is full.
				 *
				 * @see ServerStreamSocket::setMaximumPendingConnectionsCount
				 *
				 */
				virtual void connect( const std::string & serverHostname,
					Port serverPort ) ;



				/**
				 * Disconnects this socket so that it will be able to connect to
				 * servers again in the future.
				 *
				 * @throw SocketException if the socket is not already
				 * connected.
				 *
				 * @see isConnected
				 *
				 */
				virtual void disconnect() ;



				/**
				 * Returns the remote port number this socket is linked to,
				 * i.e. the port of the peer of this socket.
				 *
				 * @throw SocketException if this operation failed, including if
				 * this socket is not connected.
				 *
				 */
				virtual Port getPeerPort() const ;



				/**
				 * Returns a user-friendly description of the state of this
				 * object.
				 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				virtual const std::string toString(
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;




			protected:



				/**
				 * Called, after the connection is established, by connect().
				 * In connect(), just after this connected() method returns, the
				 * disconnect() method is automatically called.
				 *
				 * @note This method is made to be overriden by actual
				 * specialized clients. This is the place where the client-side
				 * of the client/server protocol will have to be implemented.
				 *
				 * @throw ClientStreamSocketException on failure.
				 *
				 */
				virtual void connected() ;


				/// Returns the hostname of the server.
				const std::string & getServerName() const ;




			private:



				/// The hostname of the server this socket is to be linked to.
				std::string _serverHostName ;


				/**
				 * DNS record for linked server.
				 *
				 * It acts also like a flag telling whether the socket is
				 * currently connected.
				 *
				 */
				HostDNSEntry * _serverHostInfo ;



				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				ClientStreamSocket( const ClientStreamSocket & source ) ;



				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				ClientStreamSocket & operator = (
					const ClientStreamSocket & source ) ;


		} ;


	}


}



#endif // CEYLAN_CLIENT_STREAM_SOCKET_H_
