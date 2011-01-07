/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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


#ifndef CEYLAN_ANONYMOUS_STREAM_SOCKET_H_
#define CEYLAN_ANONYMOUS_STREAM_SOCKET_H_


#include "CeylanStreamSocket.h"      // for inheritance
#include "CeylanSystem.h"            // for FileDescriptor

#include <string>




namespace Ceylan
{


	namespace Network
	{



		/**
		 * Anonymous connection-based server-side stream socket.
		 *
		 * This anonymous socket is useful for servers, which spawns one or
		 * multiple sockets to handle incoming connections while the original
		 * socket is still listening for new clients.
		 *
		 * @see SequentialServerStreamSocket, MultiplexedServerStreamSocket
		 *
		 */
		class CEYLAN_DLL AnonymousStreamSocket: public StreamSocket
		{
		
			

			public:
	


				/**
				 * Mother class for all anonymous stream socket-related
				 * exceptions.
				 *
				 */
				class AnonymousStreamSocketException: 
					public StreamSocketException
				{ 
				
					public: 
					
						explicit AnonymousStreamSocketException( 
							const std::string & reason ) ;
						
						virtual ~AnonymousStreamSocketException() throw() ; 
							
				} ;



				/**
				 * Raised whenever a non-blocking accept did not find any 
				 * connection (this is not a fatal error).
				 *
				 */
				class NonBlockingAcceptException: 
					public AnonymousStreamSocketException
				{ 
					public: 
					
						explicit NonBlockingAcceptException( 
							const std::string & reason ) ;
						
						virtual ~NonBlockingAcceptException() throw() ; 
							
				} ;



	
				/**
				 * Constructs a new server-side socket dedicated to an
				 * accepted connection.
				 *
				 * This socket will use specified listening server file
				 * descriptor to accept a new connection that it will manage
				 * as long as the connection is running.
				 *
				 * @param listeningFD listening file descriptor of this 
				 * server socket, used to accept a new connection.
				 *
				 * @param blocking tells whether this socket should be
				 * created in blocking mode (the default) or in non-blocking
				 * mode.
				 *
				 * @param sacrificeThroughputToPacketTiming tells whether
				 * the snappiest possible response (packet timing) should be
				 * searched for (if true), even though it would be obtained at 
				 * the expense of usable network bandwidth. This would be done
				 * by deactivating the Nagle algorithm, which is seldom
				 * recommended except for remote GUI or multiplayer 
				 * network games.
				 * Hence the default is false, and the Nagle algorithm is used.
				 *
				 * @see
				 * http://tangentsoft.net/wskfaq/intermediate.html#nagle-desc
				 *
				 * @throw SocketException if socket creation failed.
				 *
				 */
				explicit AnonymousStreamSocket( 
					System::FileDescriptor listeningFD,
					bool blocking = true,
					bool sacrificeThroughputToPacketTiming = false ) ;
	
	
				
				/// Virtual destructor.
				virtual ~AnonymousStreamSocket() throw() ;
	
	
			
				/**
				 * Tells whether this socket is currently connected to a 
				 * client.
				 *
				 * @note AnonymousStreamSocket instances are created connected
				 * (by construction), and should exist as long as their 
				 * connection exists, hence this method always returns true.
				 *
				 */
				virtual bool isConnected() const ;

			
	
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
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;
	
	
	
	
			private:
	
				
				
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				AnonymousStreamSocket( const AnonymousStreamSocket & source ) ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				AnonymousStreamSocket & operator = ( 
					const AnonymousStreamSocket & source );

		
		
		} ;
		
		
	}
	
}		



#endif // CEYLAN_ANONYMOUS_STREAM_SOCKET_H_

