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


#ifndef CEYLAN_LIBFAT_FILE_H_
#define CEYLAN_LIBFAT_FILE_H_


#include "CeylanFile.h"              // for inheritance
#include "CeylanFileSystemCommon.h"  // for FileException


#include <ctime>                 // for time_t
#include <string>




namespace Ceylan
{


	namespace System
	{



		/// Mother class for all exceptions related to libfat files.
		class CEYLAN_DLL LibfatFileException: public FileException
		{ 

			public: 
	       
				explicit LibfatFileException( const std::string & reason ) ;
	    	   
				virtual ~LibfatFileException() throw() ; 
	    		   
		} ;
	


		/**
		 * Encapsulates libfat-based files, as provided by the libfat DS 
		 * library.
		 *
		 * Actual files should be created and opened with respectively the
		 * File::Create and File::Open factories, that allow the
		 * user program to be cross-platform by hiding each filesystem-related
		 * per-platform specificity.
		 *
		 * @note Not all primitives are available, which results in
		 * FileException being raised whenever they are called. The reason for
		 * that is either the underlying platform is unable to provide these
		 * features, or the Ceylan porting effort did not manage them for the
		 * moment.
		 *
		 */
		class CEYLAN_DLL LibfatFile: public File
		{
		


			public:


				/**
				 * Destroys the libfat file reference object, does not remove
				 * the file itself.
				 *
				 * @note Will close automatically the file if needed.
				 *
				 * To remove the file from disk, use remove().
				 *
				 * @see remove
				 *
				 */
				virtual ~LibfatFile() throw() ;




				// Implementation of instance methods inherited from File.



				/**
				 * Returns true iff this file is open.
				 *
				 */
				virtual bool isOpen() const ;



				/**
				 * Closes the file for read/write actions.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw Stream::CloseException if the close operation failed.
				 *
				 */
				virtual bool close() ;



				/**
				 * Saves the file under a new name.
				 *
				 * @param newName the name of the newly copied file.
				 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual void saveAs( const std::string & newName ) ;




				/*
				 * Locking section is fully inherited from File.
				 *
				 * Locking or unlocking will always fail with an exception
				 * (ex: FileReadLockingFailed), as it is not
				 * supported with the libfat back-end.
				 *
				 */



				// size method inherited from File.


				 
				/**
				 * Returns the latest change time of this libfat file.
				 *
				 * @throw FileLastChangeTimeRequestFailed if the 
				 * operation failed.
				 *
				 */
				virtual time_t getLastChangeTime() const ;



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
				 * @throw InputStream::ReadFailedException if a read error
				 * occurred. Note that this is not a child class of 
				 * FileException, as it comes from an inherited interface.
				 *
				 * @note May be unable to read the full content of a file 
				 * if the file was open without the 'Binary' flag (hence
				 * in text mode) and if in the file content it occurs
				 * that accidentally some bytes form an 'end of file'
				 * marker (despite some bytes remain to be read past
				 * this marker).
				 *
				 */
		 		virtual Size read( Ceylan::Byte * buffer, Size maxLength )  ;

				
				
				// readExactLength inherited.
						
								
				// hasAvailableData inherited.
				
				
				
				/**
				 * Writes message to this file.
				 *
				 * @param message the message to write to this file.
				 *
				 * @return The number of bytes actually written, which 
				 * should be equal to the size of the string or lower.
				 *
				 * @throw OutputStream::WriteFailedException if a write error
				 * occurred. Note that this is not a child class of 
				 * FileException, as it comes from an inherited interface.
				 *
				 */
				virtual Size write( const std::string & message ) ;



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
				 * @throw OutputStream::WriteFailedException if a write error
				 * occurred. Note that this is not a child class of 
				 * FileException, as it comes from an inherited interface.
				 *
				 */
				virtual Size write( const Ceylan::Byte * buffer, 
					Size maxLength ) ;



				/**
				 * Determines current position within this file.
				 *
				 * @return offset in bytes from start of file.
                 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual Position tell() ;



				/**
				 * Seeks to specified position within this file.
				 *
				 * @param targetPosition this position corresponds to the
                 * number of bytes from start of file to seek to.
                 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual void seek( Position targetPosition ) ;



				// open and remove inherited from File.



				// LibfatFile-specific methods.
				
				
				/**
				 * Sends the file content to the <b>fd</b> file descriptor
				 * stream.
				 *
				 * @throw LibfatFileException if the operation failed.
				 *		 
				 */
				virtual void serialize( FileDescriptor fd ) const ;
				
				
				
				/**
				 * Returns the stream id, its file descriptor.
				 *
				 * @throw LibfatFileException if the operation failed.
				 *
				 */
				FileDescriptor getFileDescriptor() const ;





				// Interface implementation.


				/**
				 * Returns this file's ID.
				 *
				 * @return the StreamID.
				 *
				 */
				virtual StreamID getStreamID() const ;



				// getInputStreamID inherited.
				
				// getOutputStreamID inherited.
				
				
				
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

								


				/*
				 * Helper section.
				 *
				 * Factories still have to be public, to allow to create on
				 * specific cases (ex: process redirection) specifically
				 * libfat files, not only files.
				 *
				 */
				
		
				/**
				 * Returns a LibfatFile reference on a newly created file.
				 *
				 * By default, it creates a new file on disk. If the name
				 * corresponds to an already-existing file, it will be
				 * truncated and overwritten.
				 *
				 * @param filename the name of the file to be created.
				 *
				 * @param createFlag the flag describing the creation mode.
				 *
				 * @note No permission flag, as permissions are not supported.
				 * Always deemed OwnerReadWrite.
				 *
				 * @see OpeningFlag, PermissionFlag
				 *
				 * @note This LibfatFile factory is only a helper method.
				 * Ceylan users should only use File::Create instead.
				 *
				 * @throw FileException, including FileCreationFailed if the
				 * operation failed.
				 *
				 */
				static LibfatFile & Create( const std::string & filename, 
					OpeningFlag createFlag = CreateToWriteBinary ) ;

				
				
				/**
				 * Returns a LibfatFile reference on specified
				 * already-existing file, which will be opened with specified
				 * settings.
				 *
				 * @param filename the name of the file to open.
				 *
				 * @param openFlag the flag describing the opening mode.
				 *
				 * @see OpeningFlag
				 *
				 * @note This LibfatFile factory is only a helper method.
				 * Ceylan users should only use File::Open instead.
				 *
				 * @throw FileException, including FileOpeningFailed if the
				 * operation failed.
				 *
				 */
				static LibfatFile & Open( const std::string & filename, 
					OpeningFlag openFlag = OpenToReadBinary ) ;
				
		
		
				
			protected:



				/**
				 * Constructs a libfat file reference object.
				 * By default, it creates a new file on card, if the name
				 * corresponds to an already-existing file, it will be
				 * truncated and overwritten.
				 *
				 * @param name the name of the file.
				 *
				 * @param openFlag the flag describing the opening mode.
				 *
				 * @note No permission flag, as permissions are not supported.
				 * Always deemed OwnerReadWrite.
				 *
				 * @see OpeningFlag, PermissionFlag
				 *
				 * @note If not specifically set, the file is open in text 
				 * mode: one should not forget to add the Binary flag. 
				 * The mistake can be detected when basic read() returns less
				 * than the requested size, or when readExactLength() never
				 * terminates.
				 *
				 * @note This constructor should not be called directly, the
				 * File factories (File::Create and File::Open) should be 
				 * used instead, as they allow to write code really independant
				 * from the running platform, not having to choose between the
				 * per-platform constructors.
				 *
				 * @throw FileException if the operation failed.
				 *
				 */
				explicit LibfatFile( const std::string & name, 
					OpeningFlag openFlag = CreateToWriteBinary ) ;



				/**
				 * Constructs a libfat file with size <b>length</b> from file
				 * descriptor <b>fd</b>.
				 *
				 * @param name the name of the file.
				 *
				 * @param length the file size.
				 *
				 * @param fd the file descriptor to use.
				 *
				 * @note No permission flag, as permissions are not supported.
				 * Always deemed OwnerReadWrite.
				 *
				 * @note Very useful to copy files from streams: socket, 
				 * file, pipe.
				 *
				 * @throw FileException if the operation failed.
				 *
				 * @note This constructor should not be called directly, the
				 * File factories (File::Create and File::Open) should be 
				 * used instead, as they allow to write code really independant
				 * from the running platform, not having to choose between the
				 * per-platform constructors.
				 *
				 */
				LibfatFile( const std::string & name, Size length, 
					FileDescriptor fd ) ;

								 


				// Implementations of inherited methods.
				
				
				
				/**
				 * Returns the libfat filesystem manager.
				 *
				 * @throw FileDelegatingException if the operation failed.
				 *
				 */
				virtual FileSystemManager & getCorrespondingFileSystemManager()
					const ;



				/**
				 * Tries to reopen file.
				 *
				 * @throw FileOpeningFailed if the operation failed.
				 *
				 */
				virtual void reopen() ;



				/// Interprets the current state of this file.
				std::string interpretState() const ;
				

				
				
				// Conversion helper subsection.


				/**
				 * Converts specified Ceylan opening flag into a valid 
				 * opening flag to be used with file descriptor 
				 * functions.
				 *
				 * @param openFlag the Ceylan opening flag to convert 
				 * to lower-level flag.
				 *
				 * @return the converted opening flag.
				 *
				 * @throw ConversionFailed if the mapping failed.
				 *
				 */
				static int ConvertToFileDescriptorOpenFlag( 
					OpeningFlag openFlag ) ;  
				



			private:



				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				LibfatFile( const LibfatFile & source ) ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				LibfatFile & operator = ( const LibfatFile & source ) ;



				/**
				 * Transfers bytes from a file descriptor to another.
				 *
				 * @param from the source file descriptor.
				 *
				 * @param to the target file descriptor.
				 *
				 * @param length the length of the transfer, in bytes.
				 *
				 * @throw LibfatFileException if the operation failed.
				 *
				 */
				static void FromFDtoFD( FileDescriptor from, 
					FileDescriptor to, Size length ) ;



				/**
				 * Internal file descriptor.
				 *
				 */
				FileDescriptor _fdes ;


		} ;
		
		
	}

}



#endif // CEYLAN_LIBFAT_FILE_H_

