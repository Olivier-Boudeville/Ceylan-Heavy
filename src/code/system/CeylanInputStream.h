#ifndef CEYLAN_INPUT_STREAM_H_
#define CEYLAN_INPUT_STREAM_H_


#include "CeylanStream.h"     // for inheritance
#include "CeylanTypes.h"      // for Ceylan::Byte, etc.


#include <list>
#include <string>



namespace Ceylan 
{


	namespace System 
	{
	
	
		/**
 		 * Abstract input stream class used in IPC.
 		 *
 		 * @see Socket, Pipe, File, AnonymousInputStream.
 		 *
		 * @note Select operations only available if the file descriptor
		 * feature is itself available.
		 *
		 * @note The virtual inheritance has been set for classes such as 
		 * System::File which are both input and output streams : had the 
		 * Stream class a data member, it would not be duplicated in a 
		 * System::File instance (it would be allocated only once).
		 *
 		 */
		class InputStream : public virtual Stream
		{


			public:
	
	
	
				/**
				 * Exception thrown when an operation on an InputStream 
				 * failed.
				 *
				 */
				class InputStreamException : public StreamException
				{
					public: 
					
						explicit InputStreamException( 
								const std::string & reason ) throw() : 
							StreamException( reason )
						{
						
						}
						
				} ;
	
	
				
				/// Exception thrown when select() fails.
				class SelectFailedException : public InputStreamException
				{
					public: 
					
						explicit SelectFailedException( 
								const std::string & reason ) throw() : 
							InputStreamException( reason )
						{
						
						}
						
				} ;
	
	
	
				/// Exception thrown when a read operation failed.
				class ReadFailedException: public InputStreamException
				{ 
					public: 
					
						explicit ReadFailedException( 
								const std::string & reason ) throw() : 
							InputStreamException( reason )
						{
						
						}
								
				} ;
				
				
	
				/**
				 * Exception thrown when an unexpected End-Of-File occured,
				 * i.e. whenever the stream has to few pieces of data
				 * available.
				 *
				 */
				class EOFException: public InputStreamException
				{ 
					public: 
					
						explicit EOFException( 
								const std::string & reason ) throw() : 
							InputStreamException( reason )
						{
						
						}
								
				} ;
	
	
	
	
				/// Basic constructor for InputStream, created not selected.
				InputStream() throw() ;
		
		
				/// Basic virtual destructor.
				virtual ~InputStream() throw() ;
		
		
				/// Tells if the stream has data to read.
				bool isSelected() const throw() ;
		
		
				/// Returns the stream's unique ID.
				virtual StreamID getInputStreamID() const = 0 ;



				// Read section.
				
								
				
				/**
				 * Reads up to maxLength bytes from this InputStream to
				 * specified buffer.
				 *
				 * @param buffer the buffer where to store read bytes. 
				 * Its size must be at least 'length' bytes.
				 *
				 * @param length the maximum number of bytes that should 
				 * be read.
				 *
				 * @return The number of bytes actually read, which should
				 * be equal to 'length' or lower.
				 *
				 * @throw ReadFailed if a read error occurred, or if this
				 * default implementation has not been overriden.
				 *
				 * @note This method is not pure virtual so that other methods
				 * using it can be defined here.
				 *
				 */
		 		virtual Size read( Ceylan::Byte * buffer, Size length ) 
					throw( ReadFailedException ) ;


				/**
				 * Tells whether there is data available on input.
				 *
				 */
				virtual bool hasAvailableData() const throw() = 0 ;
		


				// Read integer types subsection.
		
		
		
				/**
				 * Returns a Ceylan::Sint8 read from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual Ceylan::Sint8 readSint8() 
					throw( ReadFailedException, EOFException ) ;

		
				/**
				 * Returns a Ceylan::Uint8 read from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual Ceylan::Uint8 readUint8() 
					throw( ReadFailedException, EOFException ) ;

		
		
				/**
				 * Returns a Ceylan::Sint16 read from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual Ceylan::Sint16 readSint16() 
					throw( ReadFailedException, EOFException ) ;

		
				/**
				 * Returns a Ceylan::Uint16 read from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual Ceylan::Uint16 readUint16() 
					throw( ReadFailedException, EOFException ) ;



				/**
				 * Returns a Ceylan::Sint32 read from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual Ceylan::Sint32 readSint32() 
					throw( ReadFailedException, EOFException ) ;


				/**
				 * Returns a Ceylan::Uint32 read from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual Ceylan::Uint32 readUint32() 
					throw( ReadFailedException, EOFException ) ;




				// Read floating-point types subsection.
				
				
				/**
				 * Returns a Ceylan::Float32 read from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual Ceylan::Float32 readFloat32() 
					throw( ReadFailedException, EOFException ) ;


				/**
				 * Returns a Ceylan::Float64 read from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual Ceylan::Float64 readFloat64() 
					throw( ReadFailedException, EOFException ) ;



				/**
				 * Reads a string from this input stream, and stores it in 
				 * the specified string.
				 *
				 * @note Read strings can have no more than 65535 characters.
				 *
				 * @param result the string to fill from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual void readString( std::string & result ) 
					throw( ReadFailedException, EOFException ) ;




				// Static section.


				
				/**
				 * Blocks the calling thread until bytes become available
				 * on one or more streams in <b>is</b>.
				 *
				 * To see which are selected, use the <code>isSelected</code>
				 * method.
				 *
				 * @return the number of selected streams.
				 *
				 * @see the non-blocking version, Test.
				 *
				 * @throw SelectFailedException if the operation failed, for 
				 * example if the file descriptor feature is not available
				 * on this platform.
				 * 				 
				 */
				static Ceylan::Uint16 Select( std::list<InputStream*> & is ) 
					throw ( SelectFailedException ) ;
		
		
				/**
				 * Checks whether bytes become available on one or more 
				 * streams in <b>is</b>.
				 *
				 * To see which are selected, use the <code>isSelected</code>
				 * method.
				 *
				 * This method returns always immediatly. 
				 *
				 * @return the number of selected streams.
				 *
				 * @see the blocking version, Select. 
				 *
				 * @throw SelectFailedException if the operation failed, for 
				 * example if the file descriptor feature is not available
				 * on this platform.
				 *
				 */
				static Ceylan::Uint16 Test( std::list<InputStream*> & is )
					throw( SelectFailedException ) ;

	
	
			protected:
	
	
				/// Used to set the selection status.
				virtual void setSelected( bool newStatus ) throw() ;
		
	
			private:


				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				InputStream( const InputStream & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				InputStream & operator = ( const InputStream & source ) 
					throw() ;
	
	
				/// Stores the selected status.
				bool _isSelected ;
	
		} ;
					
	}
	
}


	

#endif // CEYLAN_INPUT_STREAM_H_
