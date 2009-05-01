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


#ifndef CEYLAN_OUTPUT_STREAM_H_
#define CEYLAN_OUTPUT_STREAM_H_


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
		 * System::File which are both input and output streams: had the 
		 * Stream class a data member, it would not be duplicated in a 
		 * System::File instance (it would be allocated only once).
		 *
		 */
		class CEYLAN_DLL OutputStream : public virtual Stream
		{


			public:
					
					
	
				/**
				 * Exception thrown when an operation on an OutputStream 
				 * failed.
				 *
				 */
				class OutputStreamException : public StreamException
				{
					public: 
					
						explicit OutputStreamException( 
								const std::string & reason ) : 
							StreamException( reason )
						{
						
						}
						
				} ;
					
						
						
				/// Exception thrown when a write operation failed.
				class WriteFailedException: public OutputStreamException
				{ 
					public: 
					
						explicit WriteFailedException( 
								const std::string & reason ) : 
							OutputStreamException( reason )
						{
						
						}
								
				} ;
				
				
				
				/**
				 * Basic constructor for OutputStream.
				 *
				 * @param blocking tells whether this output stream should be
				 * created in blocking mode (the default) or in non-blocking
				 * mode (if supported).
				 *
				 */
				explicit OutputStream( bool blocking = true ) ;
	
	
	
				/// Basic destructor.
				virtual ~OutputStream() throw() ;
	
	
	
				/**
				 * Returns the output stream's unique ID.
				 *
				 * @throw OutputStreamException if the operation failed, for 
				 * example if this output stream has not received a valid
				 * identifier yet.
				 *
				 */
				virtual StreamID getOutputStreamID() const = 0 ;




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
				virtual Size write( const std::string & message ) ;



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
				 * @throw WriteFailedException if a write error occurred.
				 *
				 * @note This method is not pure virtual so that other methods
				 * using it can be defined here. However its OutputStream
				 * implementation just throws a WriteFailedException to 
				 * remember it has to be overloaded.
				 *
				 */
				virtual Size write( const Ceylan::Byte * buffer, Size length ) ;




				// Write integer types subsection.


				/**
				 * Writes a Ceylan::Sint8 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeSint8( Ceylan::Sint8 toWrite ) ;



				/**
				 * Writes a Ceylan::Uint8 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeUint8( Ceylan::Uint8 toWrite ) ;



				/**
				 * Writes a Ceylan::Sint16 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeSint16( Ceylan::Sint16 toWrite ) ;



				/**
				 * Writes a Ceylan::Uint16 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeUint16( Ceylan::Uint16 toWrite ) ;



				/**
				 * Writes a Ceylan::Sint32 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeSint32( Ceylan::Sint32 toWrite ) ;



				/**
				 * Writes a Ceylan::Uint32 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeUint32( Ceylan::Uint32 toWrite ) ;





				// Write floating-point types subsection.
				
				
				
				/**
				 * Writes a Ceylan::Uint32 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeFloat32( Ceylan::Float32 toWrite ) ;



				/**
				 * Writes a Ceylan::Uint32 to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeFloat64( Ceylan::Float64 toWrite ) ;



				/**
				 * Writes a string to this output stream.
				 *
				 * @note Written strings cannot have more than 65535 
				 * characters.
				 *
				 * @param toWrite the string to write to this output stream.
				 *
				 * @throw WriteFailedException in case a system error occured.
				 *
				 */
				virtual void writeString( const std::string & toWrite ) ;



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
				OutputStream( const OutputStream & source ) ;
			
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				OutputStream & operator = ( const OutputStream & source ) ;
	
	
	
		} ;
				
	}	
		
}



#endif // CEYLAN_OUTPUT_STREAM_H_

