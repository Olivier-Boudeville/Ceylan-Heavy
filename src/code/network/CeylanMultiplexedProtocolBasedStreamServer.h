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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#ifndef CEYLAN_MULTIPLEXED_PROTOCOL_BASED_STREAM_SERVER_H_
#define CEYLAN_MULTIPLEXED_PROTOCOL_BASED_STREAM_SERVER_H_


#include "CeylanMultiplexedServerStreamSocket.h" // for inheritance
#include "CeylanSystem.h"                        // for FileDescriptor

#include <string>




namespace Ceylan
{


	namespace Network
	{



		/// Multiplexed servers create anonymous socket streams.
		class AnonymousStreamSocket ;



		/**
		 * Server-side implementation of a server listening socket and its
		 * connection-triggered sockets, each one corresponding to a connected
		 * client managed by a dedicated protocol server.
		 *
		 * Following methods have to be subclassed so that the multiplexed
		 * server can perform its specific task:
		 *
		 *  - accepted, which must associate a specific protocol server to the
		 * AnonymousProtocolAwareStreamSocket instance created for a newly
		 * accepted connection (see
		 * AnonymousProtocolAwareStreamSocket::setProtocolServer)
		 *
		 *
		 * Inherited read/write methods communicate with the listening socket,
		 * as there may be more than one living connection at a time, which
		 * therefore would need to be specifically chosen thanks to the
		 * interface.
		 *
		 * @see testCeylanMultiLwProtocolServer.cc
		 *
		 */
		class CEYLAN_DLL MultiplexedProtocolBasedStreamServer :
			public MultiplexedServerStreamSocket
		{



			public:



				/**
				 * Constructs a new multiplexed server-side protocol-based
				 * server.
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
				explicit MultiplexedProtocolBasedStreamServer(
					Port listeningPort, bool reuse = true ) ;



				/// Virtual destructor.
				virtual ~MultiplexedProtocolBasedStreamServer() throw() ;




				/**
				 * Accepts all incoming connections, as long as it is not
				 * requested to stop.
				 *
				 * If there is no pending connection present on the queue,
				 * blocks the caller until a connection is present.
				 *
				 * @return A newly created and registered
				 * AnonymousProtocolAwareStreamSocket instance.
				 *
				 * @throw ServerStreamSocketException on failure.
				 *
				 * @see stop
				 *
				 */
				virtual AnonymousStreamSocket * accept() ;



				/**
				 * Called whenever a new connection is established, i.e.  when
				 * the accept method succeeds.
				 *
				 * It is up to this method, designed to be overriden (this
				 * implementation throws a ServerStreamSocketException in all
				 * cases), to initialize the AnonymousProtocolAwareStreamSocket
				 * instance it is given, which includes setting its associated
				 * protocol server.
				 *
				 * @example:
				 *
				 * void MyActualServer::accepted(
				 *    AnonymousStreamSocket & newConnection )
				 *		throw( ServerStreamSocketException )
				 * {
				 *
				 *  AnonymousProtocolAwareStreamSocket * toCustomize =
				 *    dynamic_cast<AnonymousProtocolAwareStreamSocket *>(
				 *        newConnection ) ;
				 *
				 *  toCustomize->setProtocolServer(
				 *    new MySpecificProtocol(...) ) ;
				 *
				 * }
				 *
				 * @note This method must be overridden, and must set a protocol
				 * server for the given AnonymousProtocolAwareStreamSocket
				 * instance.
				 *
				 * @throw ServerStreamSocketException in all cases.
				 *
				 * @see testCeylanMultiLwProtocolServer.cc
				 *
				 */
				virtual void accepted( AnonymousStreamSocket & newConnection ) ;



				/**
				 * Manages a connection for which data is available, by
				 * triggering the dedicated protocol server.
				 *
				 * The multiplexed server calls this method as soon as a
				 * connection has data waiting to be read.
				 *
				 * This method triggers the protocol server linked with the
				 * specified protocol-based socket, so that the actual
				 * high-level communication can take place.
				 *
				 * The lower-level communication details are managed by the
				 * marshaller the protocol server uses.
				 *
				 * @param connection the connection-based anonymous
				 * protocol-based socket in charge of this connection that has
				 * data to be read. It is actually an
				 * AnonymousProtocolAwareStreamSocket instance.
				 *
				 * @return true iff the connection is to be considered being
				 * still alive after this operation. Otherwise, i.e. if false is
				 * returned, it will be closed and removed by this server.
				 *
				 * This input/output stream is dedicated to a particular
				 * connection to this server, and when this method is called by
				 * the multiplexed server, the stream is selected, i.e. has
				 * already data available for reading.  This data may be
				 * interpreted by the protocol server of the connection socket
				 * as a request that can be decoded, then applied by the server,
				 * then its result can be sent back to the client thanks to the
				 * same I/O stream.
				 *
				 * @throw MultiplexedServerStreamSocketException if an error
				 * occurred.
				 *
				 */
				virtual bool handleConnection(
					AnonymousStreamSocket & connection ) ;



				/**
				 * Closes specified connection, and manages shutdown requests.
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




			private:



				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				MultiplexedProtocolBasedStreamServer(
					const MultiplexedProtocolBasedStreamServer & source ) ;



				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				MultiplexedProtocolBasedStreamServer & operator = (
					const MultiplexedProtocolBasedStreamServer & source ) ;



		} ;


	}


}



#endif // CEYLAN_MULTIPLEXED_PROTOCOL_BASED_STREAM_SERVER_H_
