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
		 * @see DatagramSocket
		 *
		 */
		class Socket: public System::InputOutputStream
		{			
			
			public:
		
		
		
				/// Mother class for all socket-related exceptions.
				class SocketException: public System::SystemException
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
				 * Reads up to maxLength bytes from this file to specified
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
				 * @throw ReadFailed if a read error occurred.
				 *
				 */
		 		virtual System::Size read( char * buffer, 
						System::Size maxLength ) 
					throw( InputStream::ReadFailedException ) = 0 ;
				
		
				/**
				 * Writes message to this socket.
				 *
				 * @param message the message to write to this file.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to the size of the string or lower.
				 *
				 * @throw WriteFailed if a write error occurred.
				 *
				 */
				virtual System::Size write( const std::string & message ) 
					throw( OutputStream::WriteFailedException ) = 0 ;


				/**
				 * Writes up to maxLength bytes from the specified buffer
				 * to this file.
				 *
				 * @param buffer the buffer where to find bytes that must
				 * be written to this file.
				 * Its size must be at least maxLength bytes.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to maxLength.
				 *
				 * @throw WriteFailed if a write error occurred.
				 *
				 */
				virtual System::Size write( const char * buffer, 
						System::Size maxLength ) 
					throw( OutputStream::WriteFailedException ) = 0 ;

		
				/**
				 * Tells whether there is data available on input.
				 *
				 */
				virtual bool hasAvailableData() const throw() = 0 ;
		
		
				/**
				 * Clears the input stream.
				 *
				 * @throw InputStream::ReadFailedException if the operation
				 * failed.
				 *
				 */
				virtual void clearInput()
					throw( InputStream::ReadFailedException ) ;


				/** 
				 * Returns the original file descriptor associated with the
				 * socket.
				 *
				 * This file descriptor does not change, both for client and
				 * server sides.
				 *
				 * @throw FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 */
				System::FileDescriptor getFileDescriptor() const 
					throw( Features::FeatureNotAvailableException ) ;
	
					
				/** 
				 * Returns the file descriptor that should be used for that
				 * socket so that 
				 *
				 * This file descriptor does not change, both for client and
				 * server sides.
				 *
				 * @throw FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 */
				virtual System::FileDescriptor getFileDescriptorForTransport()
					const throw( Features::FeatureNotAvailableException ) ;
							

				/// Returns the port number of the socket.
				Port getPort() const throw() ;
		
		
				/**
				 * Returns this socket descriptor for this file, or -1 if 
				 * the file descriptor feature is not available.
				 *
				 */
				virtual System::StreamID getInputStreamID() const throw() ;


				/**
				 * Returns this socket descriptor for this file, or -1 if 
				 * the file descriptor feature is not available.
				 *
				 */
				virtual System::StreamID getOutputStreamID() const throw() ;


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
				 * @throw SocketException if the operation failed.
				 *
				 */
				Socket() throw( SocketException ) ;


				/**
				 * Server-side constructor.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				explicit Socket( Port port ) throw( SocketException ) ;
		
		
				/**
				 * Returns the socket address structure reference.
				 *
				 * @throw FeatureNotAvailableException if the network feature
				 * is not available.
				 *
				 */
				virtual SystemSpecificSocketAddress & getAddress()
					throw( Features::FeatureNotAvailableException ) ;
		
		
				/** 
				 * Creates the socket associated with the port <b>port</b>.
				 *
				 * This method must be overriden by child classes, this
				 * basic implementation throws a SocketException in all cases
				 * to force redefinition.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				virtual void createSocket( Port port ) 
					throw( SocketException ) ;
		
		
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
				 * Internal file descriptor, used if this feature is 
				 * available.
				 *
				 * This is the original file descriptor for this socket.
				 * Depending on the specialization of this socket, it may
				 * or may not be the file descriptor that is used for 
				 * transport.
				 *
				 */
				System::FileDescriptor _fdes ;
			
			
				/**
				 * Internal file descriptor, used if this feature is 
				 * available.
				 *
				 * This is the original file descriptor for this socket.
				 */
				Port _port ;
		
		
		
			private:

	
		
				/**
				 * The system-specific socket address for this socket.
				 *
				 * It will be used by server sockets to bind to the target
				 * port, whereas client sockets will use it to connect to
				 * the target server.
				 *
				 */
				SystemSpecificSocketAddress * _address ;
				
				
				
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
