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


#ifndef CEYLAN_FILE_H_
#define CEYLAN_FILE_H_


#include "CeylanFileSystemCommon.h"    // for FileException and al
#include "CeylanSystem.h"              // for Size, SignedSize, etc.
#include "CeylanTypes.h"               // for Ceylan::Byte
#include "CeylanInputOutputStream.h"   // for inheritance

#include <string>




namespace Ceylan
{


	namespace System
	{

		
		/*
		 * Each File child class is linked to a corresponding filesystem
		 * manager child class.
		 *
		 */
		class FileSystemManager ;
		
		
		
		/**
		 * File opening openFlags, they can be OR'ed.
		 *
		 * At least one of the { Read ; Write } pair must be set.
		 *
		 */
		typedef Ceylan::Uint16 OpeningFlag ;



		/**
		 * File creation abstract permissions, to be used in case a new 
		 * file is created.
		 *
		 * Permissions can be OR'ed.
		 *
		 * Actual permissions can be modified by the umask of the process,
		 * according to (actualPermissionFlag & ~umask).
		 *
		 * @note When the file descriptor and/or the advanced file attribute
		 * features are not enabled, the permissions which are not managed
		 * are ignored.
		 *
		 */
		typedef Ceylan::Uint16 PermissionFlag ;


		/*
		 * As explained in CeylanFileSystemCommon.h, some file exceptions have
		 * to be declared in that file and have to have a name starting with
		 * 'File'. For the sake of homogeneity, other file exception declared
		 * here also have a name like 'FileX', instead of being
		 * declared as inner classes (ex: File::X').
		 *
		 * Note that exceptions associated to inherited methods
		 * (InputOutputStream) cannot follow this rule.
		 *
		 */
		 
		 

		class CEYLAN_DLL FileReadLockingFailed: public FileException
		{ 
			public: 
			
				explicit FileReadLockingFailed( const std::string & reason ) ; 
		} ;



		class CEYLAN_DLL FileReadUnlockingFailed: public FileException
		{ 
			public: 
			
				explicit FileReadUnlockingFailed( const std::string & reason ) ;
		} ;



		class CEYLAN_DLL FileWriteLockingFailed: public FileException
		{ 
			public: 
			
				explicit FileWriteLockingFailed( const std::string & reason ) ; 
		} ;



		class CEYLAN_DLL FileWriteUnlockingFailed: public FileException
		{ 
			public: 
			
				explicit FileWriteUnlockingFailed( 
					const std::string & reason ) ; 
		} ;



		/**
		 * Thrown when file operations failed because of underlying
		 * filesystem manager: the corresponding backend could not 
		 * be retrieved as expected.
		 *
		 */
		class CEYLAN_DLL FileDelegatingException: public FileException
		{ 
		
			public: 
			
				explicit FileDelegatingException( const std::string & reason ) ;
					
		} ;
		



		/**
		 * Abstract file mother class, so that programs can always manipulate
		 * Ceylan::File instances, whereas per-platform specialized classes are
		 * actually used by the system.
		 *
		 * Actual files should be created and opened with respectively the
		 * File::Create and File::Open factories.
		 * If it allows to fully hide the actual child class being used, an
		 * abstract File class hinders from using such File instances as 
		 * automatic variables: 'File myFile(...)' cannot exist, thus 
		 * 'File & myFile = File::Create(...)' as to be used instead. 
		 * The drawback of this approach is that the life-cycle of the instance
		 * has to be managed explicitly, with for example a 'delete &myFile'
		 * to be placed before each return or throw statement (no automatic
		 * variable can be used directly for an abstract file).
		 * To restore file handling through automatic variable (which are
		 * automatically deallocated in the relevant cases), the Holder
		 * class is available.
		 * It should be used that way:
		 * 'Holder<File> myFileHolder( File::Open(...) ) ;', then
		 * 'myFileHolder.get().lockForWriting()' or, preferably,
		 * 'myFileHolder->lockForWriting()' can be used.
		 *
		 * @see Directory, FileSystemManager for other file-related operations.
		 *
		 * @note Depending on the platform support, some primitives may not
		 * be available, which results in FileException being raised
		 * whenever called. The reason for that is either the underlying
		 * platform is unable to provide these features, or the Ceylan
		 * porting effort did not manage them for the moment.
		 *
		 * @see following feature symbols to spot the actual support 
		 * beforehand:
		 *   - Features::areAdvancedFileAttributesSupported
		 *   - Features::areSymbolicLinksSupported
		 *   - Features::areFileLocksSupported
		 * 
		 * @note To specify all exceptions that may be thrown by file
		 * manipulation, use System::SystemException, the closest mother class
		 * of FileException, InputStream::ReadFailedException,
		 * OutputStream::WriteFailedException, Stream::CloseException. 
		 * 
		 */
		class CEYLAN_DLL File: public InputOutputStream
		{
	

			public:



				// Allows read operations on the opened file. 
				static const OpeningFlag Read ;
				
				
				// Allows write operations on the opened file.
				static const OpeningFlag Write ;
				
				
				
				/**
				 * Creates the file, if it does not exist.
				 *
				 * Could not be named 'Create' (static method of the same
				 * name exists already), thus CreateFile is used.
				 *
				 */
				static const OpeningFlag CreateFile ;
				
				
				// Truncates file to length zero when opening.
				static const OpeningFlag TruncateFile ;
				
				
				// Seeks to the end of the file before each write operation.
				static const OpeningFlag AppendFile ;
				
				
								
				// Opens the file in binary rather than in text mode.
				static const OpeningFlag Binary ;
				
				
				// Does not open the underlying file object.
				static const OpeningFlag DoNotOpen ;
				
				
				
				/*
				 * Creates the file for writing, it is a convenience 
				 * shortcut equal to the very common combination:
				 * 'Read | Write | CreateFile | TruncateFile | Binary'.
				 *
				 */
				static const OpeningFlag CreateToWriteBinary ;
				
				
				/*
				 * Opens the file for reading, it is a convenience 
				 * shortcut equal to the very common combination:
				 * 'Read | Binary'.
				 *
				 */
				static const OpeningFlag OpenToReadBinary ;
				
								
				
				/**
				 * Opens the file in non-blocking mode, when possible.
				 *
				 * Neither the opening nor any subsequent operations on
				 * this file will cause the calling process to wait.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const OpeningFlag NonBlocking ;
				
				
				
				/**
				 * Opens the file in synchronous mode.
				 *
				 * Any write operation on this file will block the
				 * calling process until the data has been physically 
				 * written to the underlying hardware.
				 *
				 * @note Mostly taken into account if the advanced file 
				 * attribute feature is enabled, even though our default C++
				 * file stream-base implementation tries to manage it as well.
				 *
				 */
				static const OpeningFlag Synchronous ;
				
				


				/*
				 * Open flags for 'Owner'.
				 *
				 */


				/// Allows its owner to read the created file.
				static const PermissionFlag OwnerRead ;
				
				
				
				/// Allows its owner to write to the created file.
				static const PermissionFlag OwnerWrite ;



				/// Allows its owner to execute the created file.
				static const PermissionFlag OwnerExec ;
				
				
				
				/**
				 * Provided for convenience: 
				 * OwnerReadWrite = OwnerRead | OwnerWrite.
				 *
				 */
				static const PermissionFlag OwnerReadWrite ;
				
				

				/**
				 * Provided for convenience: 
				 * OwnerReadWriteExec = OwnerReadWrite | OwnerExec.
				 *
				 */
				static const PermissionFlag OwnerReadWriteExec ;
				



				/*
				 * Open flags for 'Group'.
				 *
				 * @note They are only taken into account if the advanced 
				 * file attribute feature is enabled.
				 *
				 */


				/**
				 * Allows the group to read the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupRead ;
				
				
				
				/**
				 * Allows the group to write the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupWrite ;


				/**
				 * Allows the group to execute the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupExec ;
				
				
				
				/**
				 * Provided for convenience: 
				 * GroupReadWrite = GroupRead | GroupWrite.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupReadWrite ;
				
				
				
				/**
				 * Provided for convenience: 
				 * GroupReadWriteExec = GroupReadWrite | GroupExec.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag GroupReadWriteExec ;
				


				/*
				 * Open flags for 'Others'.
				 *
				 * @note They are only taken into account if the advanced 
				 * file attribute feature is enabled.
				 *
				 */



				/**
				 * Allows the others (not owner, not group) to read the 
				 * created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersRead ;
				
				
				
				/**
				 * Allows the others (not owner, not group) to write
				 * to the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersWrite ;



				/**
				 * Allows the others (not owner, not group) to execute
				 * the created file.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersExec ;
				
				
				
				/**
				 * Provided for convenience: 
				 * OthersReadWrite = OthersRead | OthersWrite.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersReadWrite ;
				
				
				
				/**
				 * Provided for convenience: 
				 * OthersReadWriteExec = OthersReadWrite | OthersExec.
				 *
				 * @note Only taken into account if the advanced file 
				 * attribute feature is enabled.
				 *
				 */
				static const PermissionFlag OthersReadWriteExec ;
				
							
				/*
				 * Static section.
				 *
				 * These methods encapsulate calls to the actual filesystem
				 * manager, so that the developer does not need to mess with
				 * platform-specific details and/or with the management of
				 * specialized filesystem managers. 
				 *
				 * Should a delegated operation fail, a FileException is
				 * thrown, including if the filesystem manager backend could 
				 * not be retrieved. In that case a more specialized exception
				 * is thrown, a FileDelegatingException.
				 *
				 */


				/**
				 * Tells whether the regular file or symbolic link 
				 * <b>filename</b> exists (and is not a directory).
				 *
				 * @param filename the filename to look-up.
				 *
				 * This method will work as expected whether the 
				 * symbolic link feature is enabled or not.
				 *
				 * @throw FileException, including FileLookupFailed if the
				 * operation failed (existence test failed with no answer) or
				 * is not supported on this platform, or FileDelegatingException
				 * if the relevant filesystem manager could not be retrieved.
				 *
				 */
				static bool ExistsAsFileOrSymbolicLink( 
					const std::string & filename ) ;



				/**
				 * Tells whether the regular file or symbolic link 
				 * <b>filename</b> exists (and is not a directory).
				 *
				 * @param filename the filename to look-up.
				 *
				 * This method will work as expected whether the 
				 * symbolic link feature is enabled or not.
				 *
				 * @throw FileException, including FileLookupFailed if the
				 * operation failed (existence test failed with no answer) or
				 * is not supported on this platform, or FileDelegatingException
				 * if the relevant filesystem manager could not be retrieved.
				 *
				 * @note This method is an alias for ExistsAsFileOrSymbolicLink.
				 *
				 */
				static bool Exists(	const std::string & filename ) ;



				/**
				 * Removes the file or symbolic link from the filesystem.
				 *
				 * @param filename the filename to remove.
				 *
				 * @throw FileException, including FileRemoveFailed if the
				 * operation failed or is not supported on this platform, or
				 * FileDelegatingException if the relevant filesystem manager
				 * could not be retrieved.
				 *
				 */
				static void Remove( const std::string & filename ) ;
				
				
				
				/**
				 * Moves specified file on filesystem.
				 *
				 * A special case of file moving is file renaming.
				 *
				 * @param sourceFilename the filename of the file to be moved.
				 *
				 * @param targetFilename the target filename of the moved file.
				 *
				 * @throw FileException, including FileMoveFailed if the
				 * operation failed or is not supported on this platform, or
				 * FileDelegatingException if the relevant filesystem manager
				 * could not be retrieved.
				 *
				 */
				static void Move( const std::string & sourceFilename,
						const std::string & targetFilename ) ;



				/**
				 * Copies the file on filesystem.
				 *
				 * @param sourceFilename the filename of the file to be copied.
				 *
				 * @param targetFilename the new filename of the copied file.
				 *
				 * @throw CopyFailed if the operation failed or is not
				 * supported on this platform, or FileDelegatingException if 
				 * the relevant filesystem manager could not be retrieved.
				 *
				 */
				static void Copy( const std::string & sourceFilename,
						const std::string & targetFilename ) ;



				/**
				 * Returns the size, in bytes, of the specified file.
				 *
				 * @param filename the filename whose size is searched.
				 *
				 * @throw FileSizeRequestFailed if the operation failed or is
				 * not supported on this platform, or FileDelegatingException 
				 * if the relevant filesystem manager could not be retrieved.
				 *
				 */
				static Size GetSize( const std::string & filename ) ;



				/**
				 * Returns the last change time of the specified file.
				 *
				 * @param filename the filename whose last change time is
				 * searched.
				 *
				 * @throw FileLastChangeTimeRequestFailed if the operation
				 * failed or is not supported on this platform, or
				 * FileDelegatingException if the relevant filesystem manager
				 * could not be retrieved.
				 *
				 */
				static time_t GetLastChangeTime( 
					const std::string & filename ) ;



				/**
				 * Takes specified <b>rawFilename</b> and tries to transform it 
				 * so that the result should be a valid name, from the
				 * filesystem's point of view.
				 *
				 * @param rawFilename the filename to convert
				 *
				 * @return the converted filename
				 *
				 */
				static std::string TransformIntoValidFilename( 
					const std::string & rawFilename ) ;



				/**
				 * Updates the last access and modification times of 
				 * specified file.
				 *
				 * This is not expected to work for directories.
				 *
				 * @param filename the filename of the file whose times must
				 * be updated.
				 *
				 * @note On contrary to the UNIX command touch, if the
				 * specified file does not exist, it will not be created.
				 * A TouchFailed exception would be raised instead.
				 *
				 * @see File::Create to create empty files.
				 *
				 * @throw TouchFailed if the operation failed or is not
				 * supported on this platform, or FileDelegatingException if 
				 * the relevant filesystem manager could not be retrieved.
				 *
				 */
				static void Touch( const std::string & filename ) ;



				/**
				 * Tells whether the two specified files have exactly the same
				 * content (byte-wise).
				 *
				 * @param firstFilename the filename of the first file to
				 * compare.
				 *
				 * @param secondFilename the filename of the second file to
				 * compare.
				 *
				 * @return true iff these files exists and have exactly the 
				 * same content.
				 *
				 * @throw DiffFailed if the operation failed or is not
				 * supported on this platform, or FileDelegatingException if 
				 * the relevant filesystem manager could not be retrieved.
				 *
				 */
				static bool Diff( const std::string & firstFilename,
						const std::string & secondFilename ) ;


				
				
				
				// Factory section.
				
		
				/**
				 * Returns a File reference on a newly created file.
				 *
				 * By default, it creates a new file on disk. If the name
				 * corresponds to an already-existing file, it will be
				 * truncated and overwritten.
				 *
				 * @param filename the name of the file.
				 *
				 * @param createFlag the flag describing the creation mode.
				 *
				 * @param permissionFlag the flag describing the requested
				 * permissions. On platforms that do not manage permissions,
				 * this parameter will be ignored.
				 *
				 * @see OpeningFlag, PermissionFlag
				 *
				 * File factory, to be used instead of a specific File subclass
				 * constructor, so that it can return a File instance that is
				 * actually a specialized one (ex: a StandardFile, not an
				 * abstract File).
				 *
				 * @throw FileException, including FileCreationFailed if the
				 * operation failed or is not supported on this platform, or
				 * FileDelegatingException if the relevant filesystem manager
				 * could not be retrieved.
				 *
				 * @example:
				 * <pre>
				 * File & myFile = File::Create( "myfilename" ) ;
				 * ...
				 * myFile.write( "Hello Ceylan!" ) ;
				 * ...
                 * delete & myFile ;
				 * </pre>
				 *
				 * @note Ceylan::Holder can be used as well to simplify the
				 * management of the lifecycle of File instances.
				 *
				 */
				static File & Create( const std::string & filename, 
					OpeningFlag createFlag = CreateToWriteBinary,
					PermissionFlag permissionFlag = OwnerReadWrite ) ;

				
				
				/**
				 * Returns a File reference on specified already-existing file,
				 * which will be opened with specified settings.
				 *
				 * @param filename the name of the file.
				 *
				 * @param openFlag the flag describing the opening mode.
				 *
				 * @see OpeningFlag
				 *
				 * File factory, to be used instead of a specific File subclass
				 * constructor, so that it can return a File instance that is
				 * actually a specialized one (ex: a StandardFile, not an
				 * abstract File).
				 *
				 * @throw FileException, including FileOpeningFailed if the
				 * operation failed or is not supported on this platform, or
				 * FileDelegatingException if the relevant filesystem manager
				 * could not be retrieved.
				 *
				 * @example:
				 * <pre>
				 * File & myFile = File::Open( "myfilename.dat" ) ;
				 * ...
				 * myFile.read( buf, 100 ) ;
				 * ...
				 * </pre>
				 *
				 * @note Ceylan::Holder can be used as well to simplify the
				 * management of the lifecycle of File instances.
				 *
				 */
				static File & Open( const std::string & filename, 
					OpeningFlag openFlag = OpenToReadBinary ) ;


				
				/**
				 * Destroys the file reference object, does not remove the
				 * file itself.
				 *
				 * @note Will close automatically the file if needed.
				 *
				 * To remove the file itself (from disk), use Remove.
				 *
				 * @see Remove
				 *
				 * This destructor has to be public, so that instances created
				 * from factories can be deleted by the caller.
				 *
				 */
				virtual ~File() throw() ;




				// Instance methods.


				/// Returns this file's name.
				const std::string & getName() const ;

				
				
				/**
				 * Returns true iff this file is open.
				 *
				 */
				virtual bool isOpen() const = 0 ;
				
				
				
				/**
				 * Closes the file for read/write actions.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw Stream::CloseException if the close operation failed.
				 *
				 */
				virtual bool close() = 0 ;



				/**
				 * Saves the file under a new name.
				 *
				 * @param newName the name of the newly copied file.
				 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual void saveAs( const std::string & newName ) = 0 ;





				// Locking section.
				
				 
				 
				/**
				 * Locks the file for reading.
				 *
				 * @throw FileReadLockingFailed if the operation failed or 
				 * if the file lock feature is not available.
				 *		 
				 * This default implementation, meant to be overriden, throws
				 * this exception if called.
				 *
				 */
				virtual void lockForReading() const ;



				/**
				 * Unlocks the file for reading.
				 *
				 * @throw FileReadUnlockingFailed if the operation failed or 
				 * if the file lock feature is not available.
				 *		 
				 * This default implementation, meant to be overriden, throws
				 * this exception if called.
				 *
				 */
				virtual void unlockForReading() const ;



				/**
				 * Locks the file for writing.
				 *
				 * @throw FileWriteLockingFailed if the operation failed or 
				 * if the file lock feature is not available.
				 *		 
				 * This default implementation, meant to be overriden, throws
				 * this exception if called.
				 *
				 */
				virtual void lockForWriting() const ;



				/**
				 * Unlocks the file for writing.
				 *
				 * @throw FileWriteUnlockingFailed if the operation failed or 
				 * if the file lock feature is not available.
				 *		 
				 * This default implementation, meant to be overriden, throws
				 * this exception if called.
				 *
				 */
				virtual void unlockForWriting() const ;




				/**
				 * Tells whether the file is locked.
				 *
				 * @return true if the lock feature is available and the 
				 * file is locked, otherwise returns false, i.e. if the file
				 * is locked or if the lock feature is not available.
				 *		 
				 * This default implementation, meant to be overriden, returns
				 * always false.
				 *
				 */
				virtual bool isLocked() const ;




				/**
				 * Returns the file size, in bytes.
				 *
				 * @see GetSize
				 *
				 * @throw FileException, including FileLookupFailed if the file
				 * metadata could not be accessed or if the operation is not
				 * supported on this platform, and FileDelegatingException if
				 * the corresponding filesystem manager could not be used as
				 * expected.
				 *
				 */
				virtual Size size() const ;



				/**
				 * Returns the latest change time of this standard file.
				 *
				 * @throw FileLastChangeTimeRequestFailed if the 
				 * operation failed, or is not supported.
				 *
				 */
				virtual time_t getLastChangeTime() const = 0 ;



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
		 		virtual Size read( Ceylan::Byte * buffer, Size maxLength ) = 0 ;



				/**
				 * Reads exactly exactLength bytes from this file to specified
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
				 * @note May never terminate if the file was open without
				 * the 'Binary' flag (hence in text mode) and if in the 
				 * file content it occurs that accidentally some bytes 
				 * form an 'end of file' marker (despite some bytes remain
				 * to be read past this marker).
				 *
				 */
		 		virtual void readExactLength( Ceylan::Byte * buffer, 
						Size exactLength ) ;



				/**
				 * Tells whether there is data available on input.
				 *
				 * This methods returns always true for files.
				 *
				 */
				virtual bool hasAvailableData() const ;
				
				
				
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
				virtual Size write( const std::string & message ) = 0 ;



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
					Size maxLength ) = 0 ;



				/**
				 * Determines current position within this file.
				 *
				 * @return offset in bytes from start of file.
                 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual Position tell() = 0 ;



				/**
				 * Seeks to specified position within this file.
				 *
				 * @param targetPosition this position corresponds to the
                 * number of bytes from start of file to seek to.
                 *
				 * @throw FileException if the operation failed.
				 *
				 */
				virtual void seek( Position targetPosition ) = 0 ;



				/**
				 * Tries to open file, useful if it was created with the
				 * DoNotOpen open flag.
				 *
				 * @throw FileException, including FileAlreadyOpened if file was
				 * already opened, and FileOpeningFailed if an error occurred, 
				 * if the operation failed or is not supported.
				 */
				virtual void open( OpeningFlag openFlag = CreateToWriteBinary, 
					PermissionFlag permissionFlag = OwnerReadWrite ) ;



				/**
				 * Removes this file from disk.
				 *
				 * Closes it if necessary. No other operation should be 
				 * performed afterwards on that file.
				 *
				 * @throw FileRemoveFailed if the operation failed or is not
				 * supported on this platform, and FileDelegatingException
				 * if the corresponding filesystem manager could not be used.
				 *
				 */
				virtual void remove() ;





				// Interface implementation.


				/**
				 * Returns this file's ID.
				 *
				 * @return the StreamID or -1 if nothing appropriate can
				 * be returned with the available features.
				 *
				 */
				virtual StreamID getStreamID() const = 0 ;



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



				/// Describes buffer size for usual I/O operations.
				static const Size UsualBufferSize ;
				
				
				/// Describes buffer size for significant I/O operations.
				static const Size BigBufferSize ;
				



			protected:



				/**
				 * Constructs a file reference object.
				 *
				 * @note Not to be called directly, use factories (File:Create,
				 * File::Open instead).
				 *
				 * @param name the name of the file.
				 *
				 * @param openFlag the flag describing the opening mode.
				 *
				 * @param permissionFlag the flag describing the requested
				 * permissions, if this file is to be created.
				 * Otherwise (if the file already exists), this parameter
				 * is ignored.
				 *
				 * @see OpeningFlag, PermissionFlag
				 *
				 * @note If not specifically set, the file is opened in 
				 * text mode: one should not forget to add the Binary flag. 
				 * The mistake can be detected when basic read() returns less
				 * than the requested size, or when readExactLength() never
				 * terminates.
				 *
				 * @throw FileException if the operation failed.
				 *
				 */
				explicit File( const std::string & name, 
					OpeningFlag openFlag = CreateToWriteBinary,
					PermissionFlag permissionFlag = OwnerReadWrite ) ;



				/**
				 * Returns the filesystem manager that corresponds to the 
				 * actual File child class.
				 *
				 * @throw FileDelegatingException if the operation failed.
				 *
				 */
				virtual FileSystemManager & getCorrespondingFileSystemManager()
					const = 0 ;
					


				/**
				 * Tries to reopen file.
				 *
				 * @throw FileOpeningFailed if the operation failed.
				 *
				 */
				virtual void reopen() = 0 ;


				/// Interprets the current state of this file.
				virtual std::string interpretState() const = 0 ;



				/// Name of the file.
				std::string _name ;



				/// Flags used for opening.
				OpeningFlag _openFlag ;



				/// Permissions used for opening.
				PermissionFlag _permissions ;


				/**
				 * Bit field for access locks.
				 *
				 * Not used by all implementations.
				 *
				 */
				bool _lockedForReading: 1, _lockedForWriting: 1 ;

								

				/**
				 * Returns the filesystem manager that should be used by 
				 * File static methods, which is the default manager.
				 *
				 * @return the default filesystem manager.
				 *
				 * @throw FileDelegatingException if the operation failed.
				 *
				 */
				static FileSystemManager & GetCorrespondingFileSystemManager() ;




			private:



				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				File( const File & source ) ;



				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				File & operator = ( const File & source ) ;

				

		} ;
		
		
	}

}



#endif // CEYLAN_FILE_H_

