/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#ifndef CEYLAN_SOCKET_H_
#define CEYLAN_SOCKET_H_


#include "CeylanInputOutputStream.h" // for inheritance
#include "CeylanFeatures.h"          // for FeatureNotAvailableException
#include "CeylanTypes.h"             // for Ceylan::Uint32

// for SystemException, FileDescriptor, etc. :
#include "CeylanSystem.h"            



namespace Ceylan
{


	namespace Network
	{



		/// Sockets have to handle IP address about their peers and themselves.
		class IPAddress ;
		
		
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
		class SystemSpecificSocketAddress ;
		

		/**
		 * Port number, as specified for sockets.
		 *
		 */
		typedef Ceylan::Uint32 Port ;
		
		
		
		/**
		 * Simple general socket I/O class, both for connection-based (stream)
		 * sockets and packet-based (datagram) sockets, both for client-side
		 * and server-side sockets.
		 *
		 * Designed to be subclassed.
		 *
		 * @see StreamSocket
		 * @see DatagramSocket when available
		 *
		 */
		class CEYLAN_DLL Socket: public System::InputOutputStream
		{			
			
			public:
		
		
		
				/// Mother class for all socket-related exceptions.
				class CEYLAN_DLL SocketException: 
					public System::SystemException
				{ 
					public: 
					
						explicit SocketException( const std::string & reason )
							throw() ;
						
						virtual ~SocketException() throw() ; 
							
				} ;
		
		
		
				// No public constructor for Socket.
		
				/// Virtual destructor.
				virtual ~Socket() throw() ;
				

				/**
				 * Tells whether this socket is currently connected to a 
				 * peer.
				 *
				 */
				virtual bool isConnected() const throw() = 0 ;



				// Read section.
				
				
				/**
				 * Tells whether there is data available on input, i.e.
				 * on the file descriptor used for transport.
				 *
				 */
				virtual bool hasAvailableData() const throw() ;

								 
				/**
				 * Reads up to maxLength bytes from this socket to specified
				 * buffer.
				 *
				 * @param buffer the buffer where to store read bytes. 
				 * Its size must be at least maxLength bytes.
				 *
				 * @param maxLength the maximum number of bytes that should 
				 * be read.
				 *
				 * @return The number of bytes actually read, which should
				 * be maxLength or lower.
				 *
				 * @throw ReadFailedException if a read error occurred.
				 *
				 */
		 		virtual System::Size read( char * buffer, 
						System::Size maxLength ) 
					throw( InputStream::ReadFailedException ) ;
				

				
				// Write section.
				
		
				/**
				 * Writes message to this socket.
				 *
				 * @param message the message to write to this socket.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to the size of the string or lower.
				 *
				 * @throw WriteFailedException if a write error occurred.
				 *
				 */
				virtual System::Size write( const std::string & message ) 
					throw( OutputStream::WriteFailedException ) ;


				/**
				 * Writes up to maxLength bytes from the specified buffer
				 * to this socket.
				 *
				 * @param buffer the buffer where to find bytes that must
				 * be written to this socket.
				 * Its size must be at least maxLength bytes.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to maxLength.
				 *
				 * @throw WriteFailedException if a write error occurred.
				 *
				 */
				virtual System::Size write( const char * buffer, 
						System::Size maxLength ) 
					throw( OutputStream::WriteFailedException ) ;

		

				/** 
				 * Returns the original file descriptor associated with the
				 * socket on its creation.
				 *
				 * This file descriptor does not change, both for client and
				 * server sides.
				 *
				 * @throw SocketException if the operation failed, or
				 * FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 * @see getFileDescriptorForTransport
				 *
				 */
				System::FileDescriptor getOriginalFileDescriptor() const 
					throw( SocketException,
						Features::FeatureNotAvailableException ) ;
	
					
				/** 
				 * Returns the file descriptor that should be used for that
				 * socket to communicate with its peer(s).
				 *
				 * @note Depending on this socket being client-side or
				 * server-side, it may use a different file descriptor from
				 * the original one, this method tells which one is to be
				 * used to read or write data.
				 *
				 * @throw SocketException if the operation failed, or
				 * FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 * @see getOriginalFileDescriptor
				 *
				 */
				virtual System::FileDescriptor getFileDescriptorForTransport()
					const throw( SocketException,
						Features::FeatureNotAvailableException ) ;
							
							

				/**
				 * Returns the local port number of the socket.
				 *
				 * @throw SocketException if this operation failed.
				 *
				 */
				virtual Port getLocalPort() const throw( SocketException ) ;
		
		
				/**
				 * Returns the remote port number this socket is linked to,
				 * i.e. the port of the peer of this socket.
				 *
				 * @throw SocketException if this operation failed, including 
				 * if this socket is not connected.
				 *
				 */
				virtual Port getPeerPort() const throw( SocketException ) ;
		
		
							
				/**
				 * Returns the local IP address corresponding to this socket.
				 *
				 * Ownership of the returned object is transferred to the
				 * caller, which has to deallocate it when of no more use.
				 *
				 * @note It is not an obvious question, since computers may
				 * have more than one interface, and often the socket are 
				 * bound thanks to INADDR_ANY, which means any interface.
				 *
				 * @throw SocketException if this operation failed.
				 *
				 */
				virtual IPAddress * getLocalIPAddress() const 
					throw( SocketException ) ;
		
		
				/**
				 * Returns the remote IP address this socket is linked to,
				 * i.e. the IP address of the peer of this socket.
				 *
				 * @throw SocketException if this operation failed, including 
				 * if this socket is not connected.
				 *
				 */
				virtual IPAddress * getPeerIPAddress() const 
					throw( SocketException ) ;
		
		
		
				/**
				 * Returns the file descriptor for this socket, which is a 
				 * StreamID.
				 *
				 * @throw InputStreamException if no identifier is available,
				 * or if the file descriptor feature is not available.
				 *
				 */
				virtual System::StreamID getInputStreamID() const 
					throw( InputStreamException ) ;


				/**
				 * Returns the file descriptor for this socket, which is a 
				 * StreamID.
				 *
				 * @throw OutputStreamException if no identifier is available,
				 * or if the file descriptor feature is not available.
				 *
				 */
				virtual System::StreamID getOutputStreamID() const
					throw( OutputStreamException) ;


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
				 * Client-side constructor.
				 *
				 * @param blocking tells whether this socket should be
				 * created in blocking mode (the default) or in non-blocking
				 * mode.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				explicit Socket( bool blocking = true ) 
					throw( SocketException ) ;


				/**
				 * Server-side constructor.
				 *
				 * @param localPort the local port to bind to.
				 *
				 * @param blocking tells whether this socket should be
				 * created in blocking mode (the default) or in non-blocking
				 * mode.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				Socket( Port localPort, bool blocking = true ) 
					throw( SocketException ) ;
		
		
				/**
				 * Returns the socket address structure reference of the 
				 * connected peer, if any.
				 *
				 * @throw SocketException if there is no connected peer, or 
				 * FeatureNotAvailableException if the network feature
				 * is not available.
				 *
				virtual SystemSpecificSocketAddress & getAddress()
					throw( SocketException,
						Features::FeatureNotAvailableException ) ;
		
				 */
	
		
				/** 
				 * Creates the socket associated with the port <b>port</b>.
				 *
				 * @param localPort the specified local port.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 * @note It is the place where socket attributes are 
				 * effectively set (ex : non-blocking).
				 *
				 */
				virtual void createSocket( Port localPort ) 
					throw( SocketException ) = 0 ;
		
		
				/** 
				 * Closes the socket.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 */
				virtual bool close() throw( Stream::CloseException ) ;

		
				/**
				 * Sets the blocking mode of this socket.
				 *
				 * @param newStatus if true, sets the socket in blocking mode,
				 * if false set to non-blocking mode. If the socket is 
				 * already in the target state, nothing is done.
				 *
				 * @throw NonBlockingNotSupportedException if the operation
				 * failed.
				 *
				 */
				virtual void setBlocking( bool newStatus )
					throw( NonBlockingNotSupportedException ) ;


				/**
				 * The port this socket is created with.
				 *
				 * For client sockets, the port of the remote server will be
				 * recorded here.
				 *
				 * For server sockets, it will be the local port where the
				 * server will be listening.
				 *
				 */
				Port _port ;
				
		
				/**
				 * The system-specific socket address for this socket.
				 *
				 * It will be used by server sockets to bind to the target
				 * port, whereas client sockets will use it to connect to
				 * the target server, and anonymous sockets will store 
				 * informations about the client.
				 *
				 */
				SystemSpecificSocketAddress * _address ;

				
				/**
				 * Internal file descriptor, used if this feature is
				 * available.
				 * The local port this socket will be bound to.
				 *
 				 * This is the original file descriptor for this socket.
				 * Depending on the specialization of this socket, it may
				 * or may not be the file descriptor that is used for
				 * transport.
				 *
				 * @note The Windows SOCKET datatype is mapped here to
				 * a file descriptor.
				 *
				 */
				System::FileDescriptor _originalFD ;
			



			private:

	
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				Socket( const Socket & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				Socket & operator = ( const Socket & source ) throw() ;


		} ;

	}
	
}	


#endif // CEYLAN_SOCKET_H_
