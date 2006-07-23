#ifndef CEYLAN_OUTPUT_STREAM_H_
#define CEYLAN_OUTPUT_STREAM_H_


#include "CeylanSystem.h"     // for IOException
#include "CeylanStream.h"     // for inheritance
#include "CeylanTypes.h"      // for Ceylan::Byte, etc.


namespace Ceylan
{


	namespace System
	{
	
	
		/**
		 * Abstract output stream class used in IPC.
		 *
		 * @see Socket, Pipe, File, AnonymousInputStream, InputStream
		 *
		 * @note The virtual inheritance has been set for classes such as 
		 * System::File which are both input and output streams : had the 
		 * Stream class a data member, it would not be duplicated in a 
		 * System::File instance (it would be allocated only once).
		 *
		 */
		class OutputStream : public virtual Stream
		{


			public:
					
					
						
				/// Exception thrown when a write operation failed.
				class WriteFailedException: public IOException
				{ 
					public: 
					
						explicit WriteFailedException( 
								const std::string & reason ) throw() : 
							IOException( reason )
						{
						
						}
								
				} ;
				
				
				/// Basic constructor.
				OutputStream() throw() ;
	
				/// Basic destructor.
				virtual ~OutputStream() throw() ;
	
	
				/// Returns the output stream's unique ID.
				virtual StreamID getOutputStreamID() const throw() = 0 ;



				// Write section.
				


				/**
				 * Writes message to this OutputStream.
				 *
				 * @param message the message to write.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to the size of the string or lower.
				 *
				 * @throw WriteFailed if a write error occurred.
				 *
				 * @note This method is not pure virtual so that other methods
				 * using it can be defined here. However its OutputStream
				 * implementation just throws a WriteFailedException to 
				 * remember it has to be overloaded.
				 *
				 */
				virtual Size write( const std::string & message ) 
					throw( WriteFailedException ) ;


				/**
				 * Writes up to maxLength bytes from the specified buffer
				 * to this OutputStream.
				 *
				 * @param buffer the buffer where to find bytes that must
				 * be written. Its size must be at least 'length' bytes.
				 *
				 * @param length the maximum number of bytes that should 
				 * be read.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to 'length'.
				 *
				 * @throw WriteFailed if a write error occurred.
				 *
				 * @note This method is not pure virtual so that other methods
				 * using it can be defined here. However its OutputStream
				 * implementation just throws a WriteFailedException to 
				 * remember it has to be overloaded.
				 *
				 */
				virtual Size write( const Ceylan::Byte * buffer, 
					Size length ) throw( WriteFailedException ) ;




				// Write integer types subsection.


				/**
				 * Writes a Ceylan::Sint8 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeSint8( Ceylan::Sint8 toWrite ) 
					throw( WriteFailedException ) ;


				/**
				 * Writes a Ceylan::Uint8 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeUint8( Ceylan::Uint8 toWrite ) 
					throw( WriteFailedException ) ;



				/**
				 * Writes a Ceylan::Sint16 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeSint16( Ceylan::Sint16 toWrite ) 
					throw( WriteFailedException ) ;


				/**
				 * Writes a Ceylan::Uint16 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeUint16( Ceylan::Uint16 toWrite ) 
					throw( WriteFailedException ) ;



				/**
				 * Writes a Ceylan::Sint32 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeSint32( Ceylan::Sint32 toWrite ) 
					throw( WriteFailedException ) ;


				/**
				 * Writes a Ceylan::Uint32 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeUint32( Ceylan::Uint32 toWrite ) 
					throw( WriteFailedException ) ;




				// Write floating-point types subsection.
				
				
				/**
				 * Writes a Ceylan::Uint32 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeFloat32( Ceylan::Float32 toWrite ) 
					throw( WriteFailedException ) ;


				/**
				 * Writes a Ceylan::Uint32 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeFloat64( Ceylan::Float64 toWrite ) 
					throw( WriteFailedException ) ;



				/**
				 * Writes a string to this output stream.
				 *
				 * @note Written strings can have no more than 65535 
				 * characters.
				 *
				 * @param result the string to fill from this input stream.
				 *
				 * @throw ReadFailedException in case a system error occured,
				 * or EOFException is a protocol error occured, with fewer
				 * bytes available than expected.
				 *
				 */
				virtual void writeString( std::string & toWrite ) 
					throw( WriteFailedException ) ;



			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				OutputStream( const OutputStream & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				OutputStream & operator = ( const OutputStream & source )
					throw() ;
	
	
		} ;
				
	}	
		
}


#endif // CEYLAN_OUTPUT_STREAM_H_
