#ifndef CEYLAN_STREAM_SOCKET_H_
#define CEYLAN_STREAM_SOCKET_H_


#include "CeylanSocket.h"       // for inheritance

#include <string>



namespace Ceylan
{


	namespace Network
	{


		
		/**
		 * Simple connection-based socket I/O class.
		 *
		 * These are Internet sockets, not the ones used for UNIX IPC
		 * (interprocess communication).
		 *
		 * Designed to be subclassed.
		 *
		 * @see StreamClient
		 * @see StreamServer
		 *
		 */
		class StreamSocket: public Socket 
		{

			
			
			public:
		
				
				/// Mother class for all stream socket-related exceptions.
				class StreamSocketException: public SocketException
				{ 
					public: 
					
						explicit StreamSocketException( 
							const std::string & reason ) throw() ;
						
						virtual ~StreamSocketException() throw() ; 
							
				} ;
		
		
				/**
				 * Client-side constructor for connection-based sockets.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				StreamSocket() throw( SocketException ) ; 
		

				/**
				 * Server-side constructor for connection-based sockets.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				explicit StreamSocket( Port port ) throw( SocketException ) ;
		

				/// Virtual destructor.
				virtual ~StreamSocket() throw() ;
				
				
				
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
				 * @throw ReadFailed if a read error occurred.
				 *
				 */
		 		virtual System::Size read( char * buffer, 
						System::Size maxLength ) 
					throw( InputStream::ReadFailedException ) ;
				
		
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
					throw( OutputStream::WriteFailedException ) ;


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
					throw( OutputStream::WriteFailedException ) ;

		
				/**
				 * Tells whether there is data available on input.
				 *
				 */
				virtual bool hasAvailableData() const throw() ;
		
		
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
            	 * Returns an user-friendly description of the state of
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
				 * Creates the socket associated with the port <b>port</b>.
				 *
				 * @throw SocketException if the operation failed.
				 *
				 */
				virtual void createSocket( Port port ) 
					throw( SocketException )  ;
		
		
				/**
				 * Returns the file descriptor that should be used by this 
				 * stream socket to read/write data from/to peers. 
				 *
				 * For client stream socket, the returned file descriptor
				 * corresponds to the original socket, whereas for servers
				 * it corresponds to the socket created especially for an
				 * already accepted connection, not the original one.
				 *
				 * @throw FeatureNotAvailableException if the file descriptor
				 * feature is not available.
				 *
				 * @see getFileDescriptor()
				 *
				 */
				virtual System::FileDescriptor 
						getFileDescriptorForTransport() const
					throw( Features::FeatureNotAvailableException ) ;
				
		
			private:

				
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				StreamSocket( const StreamSocket & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				StreamSocket & operator = ( const StreamSocket & source )
					throw() ;

		} ;

	}
	
}	


#endif // CEYLAN_STREAM_SOCKET_H_
