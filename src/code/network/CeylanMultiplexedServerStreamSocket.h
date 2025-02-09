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


#ifndef CEYLAN_MULTIPLEXED_SERVER_STREAM_SOCKET_H_
#define CEYLAN_MULTIPLEXED_SERVER_STREAM_SOCKET_H_



#include "CeylanServerStreamSocket.h" // for inheritance
#include "CeylanSystem.h"             // for FileDescriptor

#include <string>
#include <set>                        // for _currentConnections



namespace Ceylan
{


	namespace Network
	{



		/// Multiplexed servers create anonymous socket streams.
		class AnonymousStreamSocket ;



		/**
		 * Server-side implementation of a server listening socket and its
		 * connection-triggered sockets, each one corresponding to a connected
		 * client.
		 *
		 * This server basically manages n+1 sockets: the listening one, which,
		 * whenever incoming connections are accepted, leads to a new socket
		 * being created, and n simultaneously connected sockets, each one being
		 * viewed as an anonymous socket dedicated to one of the living
		 * connections.
		 *
		 * @see SequentialServerStreamSocket for a server that manages any
		 * number of clients too, but sequentially rather than in parallel.
		 *
		 * @see ClientStreamSocket
		 *
		 * The clearInput method operates on the listening socket, it does not
		 * apply to any of the transport streams.
		 *
		 * Following methods have to be subclassed so that the server can
		 * perform its specific task:
		 *
		 *   - handleConnection
		 *
		 * Following methods may be subclassed for specific servers:
		 *   - accepted
		 *
		 * Inherited read/write methods communicate with the listening socket,
		 * as there may be more than one living connection at a time, which
		 * therefore would need to be specifically chosen thanks to the
		 * interface.
		 *
		 * @note Such servers are designed to be data-driven: they block most of
		 * the time, waiting for incoming requests, but they cannot perform
		 * actions on their own, in the background, between two requests.
		 *
		 */
		class CEYLAN_DLL MultiplexedServerStreamSocket :
			public ServerStreamSocket
		{



			public:



				/**
				 * Mother class for all stream socket-related exceptions, on the
				 * multiplexed server side.
				 *
				 */
				class CEYLAN_DLL MultiplexedServerStreamSocketException :
					public ServerStreamSocketException
				{
					public:

						explicit MultiplexedServerStreamSocketException(
							const std::string & reason ) ;

						virtual ~MultiplexedServerStreamSocketException()
							throw() ;

				} ;



				/**
				 * Exception to be raised whenever the underlyng network server
				 * is requested to shutdown.
				 *
				 */
				class NetworkServerShutdownException :
					public MultiplexedServerStreamSocketException
				{

					public:

						explicit NetworkServerShutdownException(
							const std::string & reason ) ;

						virtual ~NetworkServerShutdownException() throw() ;

				} ;



				/**
				 * Constructs a new multiplexed non-blocking server-side socket.
				 *
				 * @param listeningPort the TCP port of this listening server
				 * socket.
				 *
				 * @param reuse tells whether the local addresses are allowed to
				 * be reused in bind().
				 *
				 * @throw SocketException if socket creation failed.
				 *
				 */
				explicit MultiplexedServerStreamSocket( Port listeningPort,
					bool reuse = true ) ;



				/// Virtual destructor.
				virtual ~MultiplexedServerStreamSocket() throw() ;



				/**
				 * Tells whether this socket is currently connected to a client.
				 *
				 */
				virtual bool isConnected() const ;



				/**
				 * Activates this server so that it can handle incoming requests
				 * in parallel.
				 *
				 * This main loop while last as long as the server is not
				 * requested to stop.
				 *
				 * @see requestToStop()
				 *
				 */
				virtual void run() ;



				/**
				 * Accepts all incoming connections, as long as it is not
				 * requested to stop.
				 *
				 * If there is no pending connection present on the queue,
				 * blocks the caller until a connection is present.
				 *
				 * @return a pointer to a newly created AnonymousStreamSocket,
				 * corresponding to a new connection, or null, if ever no
				 * connection was available and the server socket is
				 * non-blocking. This might happen even after a select, since
				 * the connection request can have been cancelled in the
				 * meantime by the client.
				 *
				 * @throw ServerStreamSocketException on failure.
				 *
				 * @see stop
				 *
				 */
				virtual AnonymousStreamSocket * accept() ;



				/**
				 * Manages a connection for which data is available.
				 *
				 * The server calls regularly this method, as soon as a
				 * connection has data waiting to be read. This method, designed
				 * to be overriden so that the connection can be managed
				 * according to the user needs, is expected to read these data,
				 * to process them and, if needed, to write data back to the
				 * corresponding client.
				 *
				 * Then this method must return so that this server can go on
				 * serving requests.
				 *
				 * These slices of communication should not be too long,
				 * otherwise such request being processed would freeze all other
				 * connections.
				 *
				 * This method is to be subclassed so that this multiplexed
				 * server can perform its task on each per-connection socket,
				 * viewed as an abstract input/output stream.
				 *
				 * @note It is the responability of this method to return
				 * appropriately, i.e. if it does not return or if it returns
				 * after a long time, all other connections will be frozen in
				 * the meantime.
				 *
				 * @param connection the connection-based anonymous socket in
				 * charge of this connection that has data to be read.
				 *
				 * @return true iff the connection is still alive after this
				 * operation. Otherwise, i.e. if false is returned, it will be
				 * closed and removed by this server.
				 *
				 * This input/output stream is dedicated to a particular
				 * connection to this server, and when this method is called by
				 * the server, the stream is selected, i.e. has already data
				 * available for reading. This data may be interpreted by this
				 * method as a request that can be decoded, then applied by the
				 * server, then its result can be sent back to the client thanks
				 * to the same I/O stream.
				 *
				 * @throw MultiplexedServerStreamSocketException if an error
				 * occurred, and more precisely its
				 * NetworkServerShutdownException child class whenever a
				 * connection determined that the network server should be
				 * stopped.
				 *
				 * @see closeConnection, which should be used by this method
				 * when the underlying protocol determines the connection is to
				 * be terminated.
				 *
				 */
				virtual bool handleConnection(
					AnonymousStreamSocket & connection ) = 0 ;



				/**
				 * Closes specified connection.
				 *
				 * @param connection a reference to the connection that is to be
				 * terminated by this server.
				 *
				 */
				virtual void closeConnection(
					AnonymousStreamSocket & connection ) ;



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
				 * Closes any accepted connection, including its socket.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 * @note This method just closes blindly all currently accepted
				 * connections, it does not use the (possibly overriden)
				 * closeConnection method.
				 *
				 */
				virtual bool closeAcceptedConnections() ;


/*
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt to understand
 * it, and to be aware of the associated risks.
 *
 */
#pragma warning( push )
#pragma warning( disable: 4251 )

				/**
				 * The list of current accepted connection-based anonymous
				 * sockets.
				 *
				 * It is a set of pointers and not a list, since not only it has
				 * to be enumerated, but also it has to be searched for specific
				 * elements (with find).
				 *
				 */
				std::set<AnonymousStreamSocket *> _currentConnections ;

#pragma warning( pop )



			private:


				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				MultiplexedServerStreamSocket(
					const MultiplexedServerStreamSocket & source ) ;



				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				MultiplexedServerStreamSocket & operator = (
					const MultiplexedServerStreamSocket & source ) ;


		} ;


	}


}


#endif // CEYLAN_MULTIPLEXED_SERVER_STREAM_SOCKET_H_
