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
		class CEYLAN_DLL MemoryStream: public InputOutputStream
		{
		

			public:



				/// Index in the internal buffer.
				typedef Size Index ;
				

				/// Mother class for all memory-related exceptions.
				class MemoryStreamException: public SystemException
				{ 
					public: 
					
						explicit MemoryStreamException( 
							const std::string & reason ) ;
						
						virtual ~MemoryStreamException() throw() ; 
							
				} ;



				/**
				 * Constructs a memory stream.
				 *
				 * @param bufferSize the size in bytes of the buffer.
				 *
				 */
				explicit MemoryStream( Size bufferSize ) ;



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
				 * Blanks the buffer so that it is emptied.
				 *
				 */
				virtual void blank() ;
				 
				 

				/**
				 * Closes the stream for read/write actions.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed, 
				 * including if the file was not already opened.
				 *
				 */
				virtual bool close() ;




				/**
				 * Returns the buffer total size, in bytes.
				 *
				 * @see getSize
				 *
				 */
				virtual Size getSize() const ;


				
				/**
				 * Reads up to maxLength bytes from this stream to specified
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
		 		virtual Size read( Ceylan::Byte * buffer, Size maxLength )  ;



				/**
				 * Tells whether there is data available on input.
				 *
				 */
				virtual bool hasAvailableData() const ;
				
			
				
				/**
				 * Writes message to this stream.
				 *
				 * @param message the message to write to this stream.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to the size of the string or lower.
				 *
				 * @throw WriteFailedException if a write error occurred.
				 *
				 */
				virtual Size write( const std::string & message ) ;



				/**
				 * Writes up to maxLength bytes from the specified buffer
				 * to this stream.
				 *
				 * @param buffer the buffer where to find bytes that must
				 * be written to this file.
				 * Its size must be at least maxLength bytes.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to maxLength.
				 *
				 * @throw WriteFailedException if a write error occurred.
				 *
				 */
				virtual Size write( const Ceylan::Byte * buffer, 
					Size maxLength ) ;

				
				
				
				// Lower-level access section.



				/**
				 * Returns the index of the next free chunk in the buffer 
				 * that is to be filled by read data.
				 *
				 * It corresponds to the current end of filled buffer.
				 *
				 * @see getSize
				 *
				 */
				virtual Index getIndexOfNextFreeChunk() const ;
				
				
				
				/**
				 * Returns the index of the next free chunk in the buffer 
				 * that is to be filled by read data .
				 *
				 * It corresponds to the current end of filled buffer.
				 *
				 * @see getSize
				 *
				 */
				virtual Byte * getAddressOfNextFreeChunk() const ;
				
				
				
				/**
				 * Returns the size of the biggest free chunk that can be found
				 * starting at the first free location in the buffer.
				 *
				 * It corresponds to the size, in bytes, between the current 
				 * end of filled buffer and the end of the overall buffer.
				 *
				 * @see getSize
				 *
				 */
				virtual Size getSizeOfNextFreeChunk() const ;
				
				

				/// Returns the current index of filled block in buffer.
				virtual Index getBlockIndex() const ;



				/// Returns the current length of filled block in buffer.
				virtual Size getBlockLength() const ;



				/**
				 * Returns the element at specified index in buffer.
				 *
				 * @param targetIndex the index of the requested element.
				 * It will wrapped around (modulo) if necessary, so that it
				 * is in [0;getSize()[
				 *
				 */
				virtual Ceylan::Byte getElementAt( Index targetIndex ) const ;
					
					
					
				/**
				 * Declares that specified size should be added to the 
				 * current size of the buffer.
				 *
				 * It is used whenever data is directly written to the 
				 * internal buffer for better performance.
				 *
				 * @param bytesAdded the number of bytes added to the filled
				 * block in buffer. It must of course not go past the end of 
				 * the internal buffer.
				 *
				 * @throw MemoryStreamException if the specified size is too
				 * big for the remaining space. 
				 *
				 */
				virtual void increaseFilledBlockOf( Size bytesAdded ) ;



				/**
				 * Translates the filled block in buffer so that the beginning
				 * of block is at the beginning of buffer.
				 *
				 * This way, the free space of the buffer goes from the end
				 * of filled block to the end of buffer, and if more data is
				 * read, it will still make one block in buffer.
				 *
				 */
				virtual void moveFilledBlockToBufferStart() ;





				// Interface implementation.


				/**
				 * Returns this file's ID.
				 *
				 * @return the StreamID, generated from the value of this.
				 *
				 */
				virtual StreamID getStreamID() const ;



				/**
				 * Returns this file descriptor for this file, or -1 if 
				 * the file descriptor feature is not available.
				 *
				 */
				virtual StreamID getInputStreamID() const ;



				/**
				 * Returns this file descriptor for this file, or -1 if 
				 * the file descriptor feature is not available.
				 *
				 */
				virtual StreamID getOutputStreamID() const ;



            	/**
            	 * Returns an user-friendly description of the state of
				 * this object.
            	 *
				 * If the highest level of detail is requested, the full buffer
				 * will be dumped.
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





			protected:



				/// The total size of the internal buffer.
				Size _size ;
				
				
				/**
				 * The current index of the beginning of the filled block
				 * in the buffer.
				 *
				 */
				Size _index ;
				
				
				/**
				 * The current length of the filled block in the buffer.
				 *
				 */
				Size _len ;
				
				
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
				MemoryStream( const MemoryStream & source ) ;



				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				MemoryStream & operator = ( const MemoryStream & source ) ;



		} ;
		
	}
	

}



#endif // CEYLAN_MEMORY_STREAM_H_

