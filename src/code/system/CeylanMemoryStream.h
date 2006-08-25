#ifndef CEYLAN_MEMORY_STREAM_H_
#define CEYLAN_MEMORY_STREAM_H_


#include "CeylanSystem.h"              // for Size, etc.
#include "CeylanTypes.h"               // for Ceylan::Byte
#include "CeylanInputOutputStream.h"   // for inheritance


#include <string>




namespace Ceylan
{


	namespace System
	{




		/**
		 * Makes a buffer in memory behaving like an InputOutputStream.
		 *
		 * Useful to test some modules like Marshaller ones.
		 *
		 */
		class MemoryStream: public InputOutputStream
		{
		

			public:



				/// Mother class for all memory-related exceptions.
				class MemoryStreamException: public SystemException
				{ 
					public: 
					
						explicit MemoryStreamException( 
							const std::string & reason ) throw() ;
						
						virtual ~MemoryStreamException() throw() ; 
							
				} ;



				/**
				 * Constructs a memory stream.
				 *
				 * @param bufferSize the size in bytes of the buffer.
				 *
				 */
				explicit MemoryStream( Size bufferSize ) throw() ;



				/**
				 * Destroys the file reference object, does not remove the
				 * file itself.
				 *
				 * To remove the file from disk, use remove().
				 *
				 * @see remove
				 *
				 */
				virtual ~MemoryStream() throw() ;


				/**
				 * Closes the stream for read/write actions.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed, 
				 * including if the file was not already opened.
				 *
				 */
				bool close() throw( Stream::CloseException ) ;


				/// Returns the current index in buffer.
				virtual Size getIndex() const throw() ;


				/**
				 * Returns the buffer size.
				 *
				 * @see getSize
				 *
				 */
				virtual Size getSize() const throw() ;


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
		 		virtual Size read( Ceylan::Byte * buffer, Size maxLength ) 
					throw( InputStream::ReadFailedException ) ;


				/**
				 * Tells whether there is data available on input.
				 *
				 */
				virtual bool hasAvailableData() const throw() ;
				
			
				
				/**
				 * Writes message to this file.
				 *
				 * @param message the message to write to this file.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to the size of the string or lower.
				 *
				 * @throw WriteFailed if a write error occurred.
				 *
				 */
				virtual Size write( const std::string & message ) 
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
				virtual Size write( const Ceylan::Byte * buffer, 
						Size maxLength ) 
					throw( OutputStream::WriteFailedException ) ;



				// Interface implementation.


				/**
				 * Returns this file's ID.
				 *
				 * @return the StreamID, generated from the value of this.
				 *
				 */
				virtual StreamID getStreamID() const throw() ;


				/**
				 * Returns this file descriptor for this file, or -1 if 
				 * the file descriptor feature is not available.
				 *
				 */
				virtual StreamID getInputStreamID() const throw() ;


				/**
				 * Returns this file descriptor for this file, or -1 if 
				 * the file descriptor feature is not available.
				 *
				 */
				virtual StreamID getOutputStreamID() const throw() ;


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


				/// The total size of the internal buffer.
				Size _size ;
				
				/// The current index in the internal buffer.
				Size _index ;
				
				/// The internal buffer.
				Ceylan::Byte * _buffer ;
				
				

			private:



				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				MemoryStream( const MemoryStream & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				MemoryStream & operator = ( const MemoryStream & source )
					throw() ;



		} ;
		
	}

}


#endif // CEYLAN_MEMORY_STREAM_H_
