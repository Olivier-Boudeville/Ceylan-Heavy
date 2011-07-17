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


#ifndef CEYLAN_STANDARD_FILE_H_
#define CEYLAN_STANDARD_FILE_H_


#include "CeylanFile.h"              // for inheritance
#include "CeylanFileSystemCommon.h"  // for FileException


/**
 * Fallback file implementation, always included to avoid dependency on a
 * config.h-style header.
 *
 * #include <iosfwd> cannot be used since we need real (not forward)
 * declarations.
 *
 */
#include <fstream>               // for std::fstream, ios_base
#include <ctime>                 // for time_t
#include <string>




namespace Ceylan
{


	namespace System
	{



		/// Mother class for all exceptions related to standard files.
		class CEYLAN_DLL StandardFileException : public FileException
		{

			public:

				explicit StandardFileException( const std::string & reason ) ;

				virtual ~StandardFileException() throw() ;

		} ;




		/**
		 * Encapsulates standard files, as provided by usual operating systems,
		 * i.e. based on file descriptors on the UNIX platforms, otherwise, for
		 * example on Windows, using the standard C++ library (std::fstream and
		 * al).
		 *
		 * Actual files should be created and opened with respectively the
		 * File::Create and File::Open factories, that allow the user program to
		 * be cross-platform by hiding each filesystem-related per-platform
		 * specificity.
		 *
		 * @note Depending on the platform support, some primitives may not be
		 * available, which results in FileException being raised whenever
		 * called. The reason for that is either the underlying platform is
		 * unable to provide these features, or the Ceylan porting effort did
		 * not manage them for the moment.
		 *
		 * @see following feature symbols to spot the actual support beforehand:
		 *
		 *   - Features::areAdvancedFileAttributesSupported
		 *   - Features::areSymbolicLinksSupported
		 *   - Features::areFileDescriptorsSupported
		 *   - Features::areFileLocksSupported
		 *
		 * @note This class has restricted capabilities if the file descriptor
		 * feature is not available.
		 *
		 */
		class CEYLAN_DLL StandardFile : public File
		{


			/**
			 * Opaque handle for forward-declared but undefined struct pointer
			 * to system permission flag, used to avoid including
			 * system-specific headers which define for example mode_t.
			 *
			 * Otherwise the API exposed by Ceylan would depend on these
			 * headers, then on a config.h that should then be installed but may
			 * clash with others, and so on.
			 *
			 */
			struct SystemSpecificPermissionFlag ;



			public:


				/**
				 * Destroys the standard file reference object, does not remove
				 * the file itself.
				 *
				 * @note Will close automatically the file if needed.
				 *
				 * To remove the file from disk, use remove().
				 *
				 * @see remove
				 *
				 */
				virtual ~StandardFile() throw() ;




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




				// Locking section.



				/**
				 * Locks the file for reading.
				 *
				 * @throw FileReadLockingFailed if the operation failed or if
				 * the file lock feature is not available.
				 *
				 */
				virtual void lockForReading() const ;



				/**
				 * Unlocks the file for reading.
				 *
				 * @throw FileReadUnlockingFailed if the operation failed or if
				 * the file lock feature is not available.
				 *
				 */
				virtual void unlockForReading() const ;



				/**
				 * Locks the file for writing.
				 *
				 * @throw FileWriteLockingFailed if the operation failed or if
				 * the file lock feature is not available.
				 *
				 */
				virtual void lockForWriting() const ;



				/**
				 * Unlocks the file for writing.
				 *
				 * @throw FileWriteUnlockingFailed if the operation failed or if
				 * the file lock feature is not available.
				 *
				 */
				virtual void unlockForWriting() const ;



				/**
				 * Tells whether the file is locked.
				 *
				 * @return true if the lock feature is available and the file is
				 * locked, otherwise returns false, i.e. if the file is locked
				 * or if the lock feature is not available.
				 *
				 */
				virtual bool isLocked() const ;



				// size method inherited from File.



				/**
				 * Returns the latest change time of this standard file.
				 *
				 * @throw FileLastChangeTimeRequestFailed if the operation
				 * failed, or is not supported.
				 *
				 */
				virtual time_t getLastChangeTime() const ;



				/**
				 * Reads up to maxLength bytes from this file to specified
				 * buffer.
				 *
				 * @param buffer the buffer where to store read bytes. Its size
				 * must be at least maxLength bytes.
				 *
				 * @param maxLength the maximum number of bytes that should be
				 * read.
				 *
				 * @return The number of bytes actually read, which should be
				 * maxLength or lower.
				 *
				 * @throw InputStream::ReadFailedException if a read error
				 * occurred. Note that this is not a child class of
				 * FileException, as it comes from an inherited interface.
				 *
				 * @note May be unable to read the full content of a file if the
				 * file was open without the 'Binary' flag (hence in text mode)
				 * and if in the file content it occurs that accidentally some
				 * bytes form an 'end of file' marker (despite some bytes remain
				 * to be read past this marker).
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
				 * @return The number of bytes actually written, which should be
				 * equal to the size of the string or lower.
				 *
				 * @throw OutputStream::WriteFailedException if a write error
				 * occurred. Note that this is not a child class of
				 * FileException, as it comes from an inherited interface.
				 *
				 */
				virtual Size write( const std::string & message ) ;



				/**
				 * Writes up to maxLength bytes from the specified buffer to
				 * this file.
				 *
				 * @param buffer the buffer where to find bytes that must be
				 * written to this file. Its size must be at least maxLength
				 * bytes.
				 *
				 * @return The number of bytes actually written, which should be
				 * equal to maxLength.
				 *
				 * @throw OutputStream::WriteFailedException if a write error
				 * occurred. Note that this is not a child class of
				 * FileException, as it comes from an inherited interface.
				 *
				 */
				virtual Size write( const Ceylan::Byte * buffer,
					Size maxLength ) ;



				/**
				 * Determines current position within this standard file.
				 *
				 * @return offset in bytes from start of file.
				 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual Position tell() ;



				/**
				 * Seeks to specified position within this standard file.
				 *
				 * @param targetPosition this position corresponds to the number
				 * of bytes from start of file to seek to.
				 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual void seek( Position targetPosition ) ;



				// open and remove inherited.




				// StandardFile-specific methods.


				/**
				 * Sends the file content to the <b>fd</b> file descriptor
				 * stream.
				 *
				 * @throw StandardFileException if the operation failed or if
				 * the file descriptor feature is not available.
				 *
				 */
				virtual void serialize( FileDescriptor fd ) const ;



				/**
				 * Returns the stream id, its file descriptor.
				 *
				 * @throw StandardFileException if the operation failed or if
				 * the file descriptor feature is not available.
				 *
				 */
				FileDescriptor getFileDescriptor() const ;





				// Interface implementation.



				/**
				 * Returns this file's ID.
				 *
				 * @return the StreamID or -1 if nothing appropriate can be
				 * returned with the available features.
				 *
				 */
				virtual StreamID getStreamID() const ;




				// getInputStreamID inherited.

				// getOutputStreamID inherited.



				/**
				 * Returns an user-friendly description of the state of this
				 * object.
				 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				virtual const std::string toString(
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;



				/**
				 * Interprets the current state of the specified ifstream.
				 *
				 */
				static std::string InterpretState(
					const std::ifstream & inputFile ) ;



				/**
				 * Interprets the current state of the specified fstream.
				 *
				 * @note Despite a fstream should be a ifstream, since it is
				 * supposed to be one of its child classes, the InterpretState
				 * for ifstream cannot be used with fstream instances.
				 *
				 */
				static std::string InterpretState(
					const std::fstream & inputFile ) ;




				/*
				 * Helper section.
				 *
				 * Factories still have to be public, to allow to create on
				 * specific cases (ex: process redirection) specifically
				 * standard files, not only files.
				 *
				 */



				/**
				 * Returns a StandardFile reference on a newly created file.
				 *
				 * By default, it creates a new file on disk. If the name
				 * corresponds to an already-existing file, it will be truncated
				 * and overwritten.
				 *
				 * @param filename the name of the file to be created.
				 *
				 * @param createFlag the flag describing the creation mode.
				 *
				 * @param permissionFlag the flag describing the requested
				 * permissions.
				 *
				 * @see OpeningFlag, PermissionFlag
				 *
				 * @note This StandardFile factory is only a helper method.
				 * Ceylan users should only use File::Create instead.
				 *
				 * @throw FileException, including FileCreationFailed if the
				 * operation failed or is not supported on this platform.
				 *
				 */
				static StandardFile & Create( const std::string & filename,
					OpeningFlag createFlag = CreateToWriteBinary,
					PermissionFlag permissionFlag = OwnerReadWrite ) ;



				/**
				 * Returns a StandardFile reference on specified
				 * already-existing file, which will be opened with specified
				 * settings.
				 *
				 * @param filename the name of the file to open.
				 *
				 * @param openFlag the flag describing the opening mode.
				 *
				 * @see OpeningFlag
				 *
				 * @note This StandardFile factory is only a helper method.
				 * Ceylan users should only use File::Open instead.
				 *
				 * @throw FileException, including FileOpeningFailed if the
				 * operation failed or is not supported on this platform.
				 *
				 */
				static StandardFile & Open( const std::string & filename,
						OpeningFlag openFlag = OpenToReadBinary ) ;




			protected:



				/**
				 * Constructs a standard file reference object.
				 *
				 * By default, it creates a new file on disk, if the name
				 * corresponds to an already-existing file, it will be truncated
				 * and overwritten.
				 *
				 * @param name the name of the file.
				 *
				 * @param openFlag the flag describing the opening mode.
				 *
				 * @param permissionFlag the flag describing the requested
				 * permissions, if this file is to be created.  Otherwise (if
				 * the file already exists), this parameter is ignored.
				 *
				 * @see OpeningFlag, PermissionFlag
				 *
				 * @note If not specifically set, the file is open in text mode:
				 * one should not forget to add the Binary flag. The mistake can
				 * be detected when basic read() returns less than the requested
				 * size, or when readExactLength() never terminates.
				 *
				 * @note This constructor should not be called directly, the
				 * File factories (File::Create and File::Open) should be used
				 * instead, as they allow to write code really independant from
				 * the running platform, not having to choose between the
				 * per-platform constructors.
				 *
				 * @throw FileException if the operation failed.
				 *
				 */
				explicit StandardFile( const std::string & name,
						OpeningFlag openFlag = CreateToWriteBinary,
						PermissionFlag permissionFlag = OwnerReadWrite ) ;



				/**
				 * Constructs a standard file with size <b>length</b> from file
				 * descriptor <b>fd</b>.
				 *
				 * @param name the name of the file.
				 *
				 * @param length the file size.
				 *
				 * @param fd the file descriptor to use.
				 *
				 * @param permissionFlag the flag describing the requested
				 * permissions, if this file is to be created.
				 *
				 * @note Very useful to copy files from streams: socket, file,
				 * pipe.
				 *
				 * @throw FileException if the operation failed.
				 *
				 * @note This constructor should not be called directly, the
				 * File factories (File::Create and File::Open) should be used
				 * instead, as they allow to write code really independent from
				 * the running platform, not having to choose between the
				 * per-platform constructors.
				 *
				 */
				StandardFile(
					const std::string & name,
					Size length,
					FileDescriptor fd,
					PermissionFlag permissionFlag = OwnerReadWrite ) ;





				// Implementations of inherited methods.



				/**
				 * Returns the standard filesystem manager.
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
				 * Converts specified Ceylan opening flag into a valid opening
				 * flag to be used with file descriptor functions.
				 *
				 * @param openFlag the Ceylan opening flag to convert to
				 * lower-level flag.
				 *
				 * @return the converted opening flag.
				 *
				 * @throw ConversionFailed if the mapping failed, including if
				 * the file descriptor feature is not available.
				 *
				 */
				static int ConvertToFileDescriptorOpenFlag(
					OpeningFlag openFlag ) ;



				/**
				 * Converts specified Ceylan permission flag into a valid
				 * permission flag to be used with file descriptor
				 * functions.
				 *
				 * @param permissionFlag the Ceylan permission flag to convert
				 * to lower-level flag.
				 *
				 * @param returned the structure that will be filled by this
				 * method.
				 *
				 * @return nothing, but the effective result is in the parameter
				 * 'returned'.
				 *
				 * @throw ConversionFailed if the mapping failed, or if the file
				 * descriptor feature is not available.
				 *
				 */
				static void ConvertToFileDescriptorPermissionFlag(
					PermissionFlag permissionFlag,
					struct SystemSpecificPermissionFlag & returned ) ;




				/**
				 * Converts specified Ceylan opening flag into a valid opening
				 * flag to be used with C++ streams.
				 *
				 * @param openFlag the Ceylan opening flag to convert to
				 * lower-level flag.
				 *
				 * @param filename the filename these flags applied to, since it
				 * might be needed to check whether the file already exists, in
				 * the case no special creation is requested.
				 *
				 * @return the converted opening flag.
				 *
				 * @throw ConversionFailed if the mapping failed.
				 *
				 */
				static std::ios_base::openmode ConvertToStreamOpenFlag(
					OpeningFlag openFlag, const std::string & filename ) ;



				/**
				 * Converts specified Ceylan permission flag into a valid
				 * permission flag to be used with C++ streams.
				 *
				 * @param permissionFlag the Ceylan permission flag to convert
				 * to lower-level flag.
				 *
				 * @return the converted permission flag.
				 *
				 * @throw ConversionFailed if the mapping failed.
				 *
				 * @note Currently disabled since permission specifications are
				 * not supported by the underlying C++ stream layer.
				 *
				static FIXME ConvertToStreamPermissionFlag(
						PermissionFlag permissionFlag ) ;
				 */



			private:



				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				StandardFile( const StandardFile & source ) ;



				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				StandardFile & operator = ( const StandardFile & source ) ;



				/**
				 * Transfers bytes from a file descriptor to another.
				 *
				 * @param from the source file descriptor.
				 *
				 * @param to the target file descriptor.
				 *
				 * @param length the length of the transfer, in bytes.
				 *
				 * @throw StandardFileException if the operation failed or is
				 * not supported.
				 *
				 */
				static void FromFDtoFD( FileDescriptor from,
						FileDescriptor to, Size length ) ;



				/**
				 * Internal file descriptor, used if this feature is available.
				 *
				 */
				FileDescriptor _fdes ;



				/**
				 * Internal file input/output stream, used in case the file
				 * descriptor feature is not available.
				 *
				 */
				std::fstream _fstream ;


		} ;


	}
	

}



#endif // CEYLAN_STANDARD_FILE_H_
