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


#ifndef CEYLAN_FILE_SYSTEM_MANAGER_H_
#define CEYLAN_FILE_SYSTEM_MANAGER_H_



#include "CeylanSystem.h"           // for SystemException
#include "CeylanStringUtils.h"      // for Latin1Char
#include "CeylanFileSystemCommon.h" // for the various shared exceptions
#include "CeylanFile.h"             // for OpeningFlag, PermissionFlag, etc.


#include <string>




namespace Ceylan
{



	namespace System
	{



		// A filesystem manager can operate on directories.
		class Directory ;


		/// Thrown when filesystem manager operations failed.
		class CEYLAN_DLL FileSystemManagerException : public FileSystemException
		{

			public:

				explicit FileSystemManagerException(
						const std::string & reason ) :
					FileSystemException( reason )
				{

				}

		} ;




		/*
		 * List here all the FileSystemManager exceptions not directly seen by
		 * File and Directory headers.
		 *
		 */


		class CEYLAN_DLL EntryLookupFailed : public FileSystemManagerException
		{

			public:

				explicit EntryLookupFailed( const std::string & reason ) :
					FileSystemManagerException( reason )
				{

				}

		} ;



		class CEYLAN_DLL SymlinkFailed : public FileSystemManagerException
		{

			public:

				explicit SymlinkFailed( const std::string & reason ) :
					FileSystemManagerException( reason )
				{

				}

		} ;



		class CEYLAN_DLL GetChangeTimeFailed : public FileSystemManagerException
		{

			public:

				explicit GetChangeTimeFailed( const std::string & reason ) :
					FileSystemManagerException( reason )
				{

				}

		} ;



		class CEYLAN_DLL FileSystemManagerDelegatingException :
			public FileSystemManagerException
		{

			public:

				explicit FileSystemManagerDelegatingException(
						const std::string & reason ) :
					FileSystemManagerException( reason )
				{

				}

		} ;





		/**
		 * Interface (abstract class) that allows to manage a specific
		 * filesystem. More than one filesystem can be used at once, and two
		 * different filesystems may have to be managed according to very
		 * different ways (ex: they have to be initialized differently).
		 *
		 * Thus files and directories may be manipulated from various
		 * filesystems simultaneously.
		 *
		 *
		 */
		class CEYLAN_DLL FileSystemManager : public TextDisplayable
		{


			public:



				/*
				 * FileSystemManager-specific section.
				 *
				 * These methods are not directly linked to File or Directory.
				 * Hence there actual implementation methods are declared here
				 * (ex: existsAsEntry), as well as their static counterpart (ex:
				 * ExistsAsEntry) that will select the right implementation
				 * under the hood.
				 *
				 * All these static methods use any default filesystem manager,
				 * or create a platform-default one should none be set already.
				 *
				 */



				// First, the static methods directly exposed to the user.


				/**
				 * Tells whether the filesystem entry <b>entryPath</b> exists,
				 * be it a file, a symbolic link, a directory, a character or
				 * block device, a FIFO, a socket, etc.
				 *
				 * @note Uses the default filesystem manager.
				 *
				 * @param entryPath the path of the entry to look-up.
				 *
				 * @throw FileSystemManagerException, which includes
				 * EntryLookupFailed if the operation failed or is not
				 * supported, and FileSystemManagerDelegatingException if the
				 * underlying specialized FileSystemManager instance could not
				 * be retrieved.
				 *
				 */
				static bool ExistsAsEntry( const std::string & entryPath ) ;



				/**
				 * Creates a symbolic link on filesystem.
				 *
				 * @note Uses the default filesystem manager.
				 *
				 * @param linkTarget the full path of the entry the new link
				 * should point to.
				 *
				 * @param linkName the filename of the link to create.
				 *
				 * @throw FileSystemManagerException, which includes
				 * SymlinkFailed if the creation failed or is not supported, and
				 * FileSystemManagerDelegatingException if the underlying
				 * specialized FileSystemManager instance could not be
				 * retrieved.
				 *
				 */
				static void CreateSymbolicLink( const std::string & linkTarget,
					const std::string & linkName ) ;



				/**
				 * Returns the change time time of the entry <b>entryPath</b>,
				 * be it a file, a directory, etc.
				 *
				 * @note Uses the default filesystem manager.
				 *
				 * @param entryPath the path of the entry.
				 *
				 * @throw FileSystemManagerException, which includes
				 * GetChangeTimeFailed if the operation failed or is not
				 * supported, and FileSystemManagerDelegatingException if the
				 * underlying specialized FileSystemManager instance could not
				 * be retrieved.
				 *
				 */
				static time_t GetEntryChangeTime(
					const std::string & entryPath ) ;




				// Accessors to FilesystemManager constants.


				/**
				 * Returns the root directory prefix.
				 *
				 * @note Uses the default filesystem manager.
				 *
				 * @example "" on Unix, "c:" on Windows.
				 *
				 * @throw FileSystemManagerDelegatingException if the underlying
				 * specialized FileSystemManager instance could not be
				 * retrieved.
				 *
				 */
				static const std::string & GetRootDirectoryPrefix() ;



				/**
				 * Returns the directory separator, a Latin-1 character.
				 *
				 * @note Uses the default filesystem manager.
				 *
				 * @example Slash or backslash, i.e. '/' or '\'.
				 *
				 * @throw FileSystemManagerDelegatingException if the underlying
				 * specialized FileSystemManager instance could not be
				 * retrieved.
				 *
				 */
				static Ceylan::Latin1Char GetSeparator() ;



				/**
				 * Returns the directory separator, in the form of a string.
				 *
				 * @note Uses the default filesystem manager.
				 *
				 * More convenient for some operations than a character.
				 *
				 * @example Slash or backslash, i.e. '/' or '\'.
				 *
				 * @throw FileSystemManagerDelegatingException if the underlying
				 * specialized FileSystemManager instance could not be
				 * retrieved.
				 *
				 */
				static std::string GetSeparatorAsString() ;



				/**
				 * Returns the alias for the working directory.
				 *
				 * @note Uses the default filesystem manager.
				 *
				 * @example Typically it is ".", it is the value returned by
				 * this default implementation.
				 *
				 * @throw FileSystemManagerDelegatingException if the underlying
				 * specialized FileSystemManager instance could not be
				 * retrieved.
				 *
				 */
				static const std::string & GetAliasForCurrentDirectory() ;



				/**
				 * Returns the alias for the upper (parent) directory.
				 *
				 * @note Uses the default filesystem manager.
				 *
				 * @example Typically it is "..", it is the value returned by
				 * this default implementation.
				 *
				 * @throw FileSystemManagerDelegatingException if the underlying
				 * specialized FileSystemManager instance could not be
				 * retrieved.
				 *
				 */
				static const std::string & GetAliasForParentDirectory() ;





				// Second, the overridable implementation methods.



				/**
				 * Tells whether the filesystem entry <b>entryPath</b> exists,
				 * be it a file, a symbolic link, a directory, a character or
				 * block device, a FIFO, a socket, etc.
				 *
				 * @param entryPath the path of the entry to look-up.
				 *
				 * @throw EntryLookupFailed if the operation failed (existence
				 * test failed with no answer) or is not supported on this
				 * platform.
				 *
				 */
				virtual bool existsAsEntry( const std::string & entryPath )
					const = 0 ;



				/**
				 * Creates a symbolic link on filesystem.
				 *
				 * @param linkTarget the full path of the entry the new link
				 * should point to.
				 *
				 * @param linkName the filename of the link to create.
				 *
				 * @throw SymlinkFailed if the creation failed, or if the
				 * symbolic link feature is not supported.
				 *
				 */
				virtual void createSymbolicLink( const std::string & linkTarget,
					const std::string & linkName ) = 0 ;



				/**
				 * Returns the change time time of the entry <b>entryPath</b>,
				 * be it a file, a directory, etc.
				 *
				 * @param entryPath the path of the entry.
				 *
				 * @throw GetChangeTimeFailed if the operation failed or is not
				 * supported.
				 *
				 */
				virtual time_t getEntryChangeTime(
					const std::string & entryPath ) = 0 ;




				// Accessors to FilesystemManager constants.


				/**
				 * Returns the root directory prefix.
				 *
				 * @example "" on Unix, "c:" on Windows.
				 *
				 */
				virtual const std::string & getRootDirectoryPrefix()
					const = 0 ;



				/**
				 * Returns the directory separator, a Latin-1 character.
				 *
				 * @example Slash or backslash, i.e. '/' or '\'.
				 *
				 */
				virtual Ceylan::Latin1Char getSeparator() const  = 0 ;



				/**
				 * Returns the directory separator, in the form of a string.
				 *
				 * More convenient for some operations than a character.
				 *
				 * @example Slash or backslash, i.e. '/' or '\'.
				 *
				 */
				virtual std::string getSeparatorAsString() const ;



				/**
				 * Returns the alias for the working directory.
				 *
				 * @example Typically it is ".", it is the value returned by
				 * this default implementation.
				 *
				 */
				virtual const std::string & getAliasForCurrentDirectory()
					const ;



				/**
				 * Returns the alias for the upper (parent) directory.
				 *
				 * @example Typically it is "..", it is the value returned by
				 * this default implementation.
				 *
				 */
				virtual const std::string & getAliasForParentDirectory()
					const ;





				// File-related section.



				/**
				 * Returns a File reference on a newly created file.
				 *
				 * By default, it creates a new file on disk. If the name
				 * corresponds to an already-existing file, it will be truncated
				 * and overwritten.
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
				 * @return A reference to a new file instance, whose ownership
				 * is transferred to the caller.
				 *
				 * @throw FileException, including FileCreationFailed if the
				 * file creation failed or is not supported on this platform.
				 *
				 */
				virtual File & createFile( const std::string & filename,
					OpeningFlag createFlag = File::CreateToWriteBinary,
					PermissionFlag permissionFlag = File::OwnerReadWrite ) = 0 ;



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
				 * @return A reference to a new file instance, whose ownership
				 * is transferred to the caller.
				 *
				 * @throw FileException, including FileOpeningFailed if the file
				 * opening failed.
				 *
				 */
				virtual File & openFile( const std::string & filename,
					OpeningFlag openFlag = File::OpenToReadBinary ) = 0 ;



				/**
				 * Tells whether the regular file or symbolic link
				 * <b>filename</b> exists (and is not a directory).
				 *
				 * @param filename the filename to look-up.
				 *
				 * This method will work as expected whether the
				 * symbolic link feature is enabled or not.
				 *
				 * @throw FileLookupFailed if the operation failed (existence
				 * test failed with no answer) or is not supported on this
				 * platform.
				 *
				 */
				virtual bool existsAsFileOrSymbolicLink(
					const std::string & filename ) const = 0 ;



				/**
				 * Removes the file or symbolic link from the filesystem.
				 *
				 * @param filename the filename to remove.
				 *
				 * @throw FileRemoveFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void removeFile( const std::string & filename ) = 0 ;



				/**
				 * Moves the file on filesystem.
				 *
				 * A special case of file moving is file renaming.
				 *
				 * @param sourceFilename the filename of the file to be moved.
				 *
				 * @param targetFilename the target filename of the moved file.
				 *
				 * @throw FileMoveFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void moveFile( const std::string & sourceFilename,
					const std::string & targetFilename ) = 0 ;



				/**
				 * Copies the file on filesystem.
				 *
				 * @param sourceFilename the filename of the file to be copied.
				 *
				 * @param targetFilename the new filename of the copied file.
				 *
				 * @throw FileCopyFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void copyFile( const std::string & sourceFilename,
					const std::string & targetFilename ) = 0 ;



				/**
				 * Returns the size, in bytes, of the specified file.
				 *
				 * @param filename the filename whose size is searched.
				 *
				 * @throw FileSizeRequestFailed if the operation failed (ex:
				 * file not found) or is not supported on this platform.
				 *
				 */
				virtual Size getSize( const std::string & filename ) = 0 ;



				/**
				 * Returns the last change time of the specified file.
				 *
				 * @param filename the filename whose last change time is
				 * searched.
				 *
				 * @throw FileLastChangeTimeRequestFailed if the operation
				 * failed or is not supported on this platform.
				 *
				 */
				virtual time_t getLastChangeTimeFile(
					const std::string & filename ) = 0 ;



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
				virtual std::string transformIntoValidFilename(
					const std::string & rawFilename ) ;



				/**
				 * Updates the last access and modification times of specified
				 * file.
				 *
				 * This is not expected to work for directories.
				 *
				 * @param filename the filename of the file whose times must be
				 * updated.
				 *
				 * @note On contrary to the UNIX command touch, if the specified
				 * file does not exist, it will not be created. A
				 * FileTouchFailed exception would be raised instead.
				 *
				 * @see File::Create to create empty files.
				 *
				 * @throw FileTouchFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void touch( const std::string & filename ) = 0 ;



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
				 * @return true iff these files exists and have exactly the same
				 * content.
				 *
				 * @throw FileDiffFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual bool diff( const std::string & firstFilename,
					const std::string & secondFilename ) ;





				// Directory-related section.


				// Factory-related subsection.



				/**
				 * Returns a Directory reference on a directory newly created on
				 * filesystem.
				 *
				 * @param newDirectoryName the name of the directory to create.
				 *
				 * Directory factory, to be used instead of a specific Directory
				 * subclass constructor, so that it can return a Directory
				 * instance that is actually a specialized one (ex: a
				 * StandardDirectory, not an abstract Directory).
				 *
				 * @throw DirectoryException, including DirectoryCreationFailed
				 * if the directory creation failed.
				 *
				 */
				virtual Directory & createDirectory(
					const std::string & newDirectoryName ) = 0 ;



				/**
				 * Returns a Directory reference on specified already-existing
				 * directory, which will be "opened" (i.e. referred to).
				 *
				 * @param directoryName the name of the directory. If not
				 * specified (the string is empty), returns a reference to the
				 * current working directory.
				 *
				 * Directory factory, to be used instead of a specific Directory
				 * subclass constructor, so that it can return a Directory
				 * instance that is actually a specialized one (ex: a
				 * StandardDirectory, not an abstract Directory).
				 *
				 * @throw DirectoryException, including DirectoryOpeningFailed
				 * if the directory opening failed.
				 *
				 */
				virtual Directory & openDirectory(
					const std::string & directoryName = "" ) = 0 ;



				/**
				 * Tells whether the directory <b>directoryPath</b> exists and
				 * is a directory indeed.
				 *
				 * @param directoryPath the directory path to look-up.
				 *
				 * @note On Windows, files and directories are case-insensitive,
				 * and 'c:' is not a directory (it is seen as a drive), whereas
				 * 'c:\' is a directory.
				 *
				 * @throw DirectoryLookupFailed if the operation failed
				 * (existence test failed with no answer) or is not supported on
				 * this platform.
				 *
				 */
				virtual bool existsAsDirectory(
					const std::string & directoryPath ) const = 0 ;



				/**
				 * Removes the directory from filesystem.
				 *
				 * @param directoryPath the path of the target directory.
				 *
				 * @param recursive if false, the specified directory is
				 * expected to be empty, and it will be removed. If true, then
				 * the full directory content (including all files and possible
				 * subdirectories) and this directory itself will be removed.
				 *
				 * @throw DirectoryRemoveFailed if the operation failed or is
				 * not supported.
				 *
				 */
				virtual void removeDirectory( const std::string & directoryPath,
					bool recursive = false ) = 0 ;



				/**
				 * Moves the directory on filesystem.
				 *
				 * A special case of directory moving is directory renaming.
				 *
				 * @param sourceDirectoryPath the path of the directory to be
				 * moved.
				 *
				 * @param targetDirectoryPath the path of the target directory.
				 *
				 * @throw DirectoryMoveFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void moveDirectory(
					const std::string & sourceDirectoryPath,
					const std::string & targetDirectoryPath ) = 0 ;



				/**
				 * Copies the file on filesystem.
				 *
				 * @param sourceDirectoryPath the path of the directory to be
				 * copied.
				 *
				 * @param targetDirectoryPath the path of the target directory.
				 *
				 * @throw DirectoryCopyFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void copyDirectory(
					const std::string & sourceDirectoryPath,
					const std::string & targetDirectoryPath ) = 0 ;



				/**
				 * Returns the last change time of the specified directory.
				 *
				 * @param directoryPath the path of the directory whose last
				 * change time is searched.
				 *
				 * @throw DirectoryLastChangeTimeRequestFailed if the operation
				 * failed or is not supported on this platform.
				 *
				 */
				virtual time_t getLastChangeTimeDirectory(
					const std::string & directoryPath ) = 0 ;



				/**
				 * Returns whether specified string is a valid directory path.
				 * (i.e. checks the name can be used, does not look-up any real
				 * filesystem entry).
				 *
				 * @param directoryString the directory string to examine.
				 *
				 * @note If no regular expression support is available, then the
				 * path will be deemed always correct.
				 *
				 */
				virtual bool isAValidDirectoryPath(
					const std::string & directoryString ) = 0 ;



				/**
				 * Removes the leading separator, if any, in specified
				 * directory's path.
				 *
				 * @param path the path that will be modified.
				 *
				 */
				virtual void removeLeadingSeparator( std::string & path ) ;



				/**
				 * Tells whether specified path is an absolute path.
				 *
				 * @param path the path that may be absolute.
				 *
				 */
				virtual bool isAbsolutePath( const std::string & path ) = 0 ;



				/**
				 * Returns the current working directory path.
				 *
				 * @throw DirectoryGetCurrentFailed if the operation failed or
				 * is not supported on the target platform.
				 *
				 */
				virtual std::string getCurrentWorkingDirectoryPath() = 0 ;



				/**
				 * Changes current working directory to
				 * <b>newWorkingDirectory</b>.
				 *
				 * @param newWorkingDirectory the target working directory.
				 *
				 * @throw DirectoryChangeFailed if the operation failed or is
				 * not supported on the target platform.
				 *
				 */
				virtual void changeWorkingDirectory(
					const std::string & newWorkingDirectory )= 0 ;





				// Path manipulation section, filesystem-independant.



				/**
				 * Splits up <b>path</b> into the list of its sub-elements (set
				 * of directory/file names).
				 *
				 * @param path the path to split.
				 *
				 * @example splitPath( "/mnt/raid/md0/LOANI-0.3" ) returns on
				 * UNIX: [ "", "mnt", "raid", "md0", "LOANI-0.3" ].
				 *
				 * @see joinPath, Directory::JoinPath
				 *
				 */
				virtual std::list<std::string> splitPath(
					const std::string & path ) ;



				/**
				 * Joins the specified path elements with the relevant directory
				 * separator.
				 *
				 * @param pathElements the path elements to join in a path.
				 *
				 * @example joinPath([ "", "mnt", "raid", "md0", "LOANI-0.3" ])
				 * returns on UNIX: "/mnt/raid/md0/LOANI-0.3".
				 *
				 * @see splitPath
				 *
				 * @note Using Directory::JoinPath is to be preferred.
				 *
				 */
				virtual std::string joinPath(
					const std::list<std::string> & pathElements ) ;



				/**
				 * Joins the two specified path elements with the relevant
				 * directory separator.
				 *
				 * @param firstPath the first part of the final path.
				 *
				 * @param secondPath the second part of the final path.
				 *
				 * @example joinPath( "/mnt/raid", "md0/LOANI-0.3" ) returns on
				 * UNIX: "/mnt/raid/md0/LOANI-0.3".
				 *
				 * @see splitPath
				 *
				 * @note Using Directory::JoinPath is to be preferred.
				 *
				 */
				virtual std::string joinPath( const std::string & firstPath,
					const std::string & secondPath ) ;



				/**
				 * Separates the full pathname <b>path</b> into a basename
				 * <b>base</b> and file name <b>file</b>.
				 *
				 * For example, this method applied to "/mnt/raid/md0/LOANI-0.3"
				 * returns respectively "/mnt/raid/md0" and "LOANI-0.3", when
				 * the separator is '/'.
				 *
				 * @param path the path which is to be stripped.
				 *
				 * @param base if non null, must be a pointer to an already
				 * allocated string where the basename will be stored. If not
				 * interested in the basename, specify a null (0) pointer
				 * instead: this method will act as UNIX "basename".
				 *
				 * @param file if non null, must be a pointer to an already
				 * allocated string where the filename will be stored. If not
				 * interested in the filename, specify a null (0) pointer
				 * instead: this method will act as UNIX "dirname".
				 *
				 */
				virtual void stripFilename( const std::string & path,
					std::string * base, std::string * file = 0 ) ;




				// FileSystemManager own section.


				/**
				 * Returns whether there is currently at least one opened file
				 * through this file system manager.
				 *
				 */
				bool hasOpenFiles() const ;


				/**
				 * Returns the list of files currently opened through this file
				 * manager.
				 *
				 */
				const std::list<std::string> & getOpenFileList() const ;


				/**
				 * Returns a user-friendly description of the state of this
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






				// Static section to handle default filesystem manager.



				/*
				 * Default filesystem manager subsection.
				 *
				 * It is the filesystem manager instance used when none is
				 * specified, not necessarily a manager deemed the default
				 * choice for a given platform (there are two different notions
				 * here).
				 *
				 */



				/**
				 * Returns whether the default filesystem manager is set.
				 *
				 * @return true iff a default filesystem manager is currently
				 * available.
				 *
				 */
				static bool IsDefaultFileSystemManagerSet() ;



				/**
				 * Sets the default filesystem manager, the one that will be
				 * used if no filesystem manager is specified otherwise.
				 *
				 * @param deallocatePreviousIfAny if true, any previously
				 * existing manager will be deallocated. If false, it would be
				 * replaced but not deleted.
				 *
				 */
				static void SetDefaultFileSystemManager(
					FileSystemManager & newDefaultFileSystemManager,
					bool deallocatePreviousIfAny = true ) ;



				/**
				 * Sets the default filesystem manager to the platform default,
				 * i.e. to the manager that is the more natural for the target
				 * platform.
				 *
				 * On computers (be they UNIX or Windows), the default manager
				 * is by default the standard one (StandardFileSystemManager).
				 *
				 * On the Nintendo DS, it is the libfat one
				 * (LibfatFileSystemManager).
				 *
				 * Any previously existing manager will be deallocated first.
				 *
				 * @throw FileSystemManagerException if the operation failed or
				 * is not supported.
				 *
				 */
				static void SetDefaultFileSystemManagerToPlatformDefault() ;



				/**
				 * Returns a reference to the unique default filesystem manager,
				 * that is expected to exist already.
				 *
				 * @throw FileSystemManagerException if the operation failed,
				 * including if there is no manager available.
				 *
				 * @see GetAnyDefaultFileSystemManager
				 *
				 */
				static FileSystemManager &
					GetExistingDefaultFileSystemManager() ;



				/**
				 * Returns a reference to the unique default filesystem manager,
				 * that is created from platform defaults if needed.
				 *
				 * Ensures that the unique default filesystem manager is set,
				 * creates it if needed, ensures it remains a singleton.
				 *
				 * If a manager is already available, returns it. Otherwise, it
				 * means the user did not wanted to specify a specific one,
				 * hence one is created from platform defaults.
				 *
				 * @return the current default filesystem manager, either
				 * already existing or created from platform defaults.
				 *
				 * @throw FileSystemManagerException if the operation failed.
				 *
				 * @note This is a helper method to ensure that File and
				 * Directory static method can rely in all cases on a filesystem
				 * manager.
				 *
				 * @see GetExistingDefaultFileSystemManager
				 *
				 */
				static FileSystemManager & GetAnyDefaultFileSystemManager() ;



				/**
				 * Removes the current default filesystem manager, if any.
				 *
				 */
				static void RemoveDefaultFileSystemManager() ;




			protected:



				/**
				 * Constructs a reference to a filesystem, initializes
				 * accordingly any needed subsystem.
				 *
				 * Cannot be private as has to be subclassed.
				 *
				 * @param trackOpenedFiles tells whether opened files should be
				 * tracked.
				 *
				 * @throw FileSystemManagerException if the operation failed.
				 *
				 */
				explicit FileSystemManager( bool trackOpenedFiles = false ) ;



				/**
				 * Destroys the Ceylan filesystem reference, not the filesystem
				 * itself.
				 *
				 * Cannot be private as has to be subclassed.
				 *
				 */
				virtual ~FileSystemManager() throw() ;


				/**
				 * Allows this filesystem manager to keep track of the files it
				 * opened.
				 *
				 */
				void declareFileOpening( const Ceylan::System::File & file ) ;


				/**
				 * Allows this filesystem manager to keep track of the files it
				 * opened.
				 *
				 */
				void declareFileClosing( const Ceylan::System::File & file ) ;


				/**
				 * Returns a textual description of the files currently opened
				 * through this filesystem manager.
				 *
				 */
				std::string listOpenFiles() const ;


				/**
				 * Pointer to the default filesystem manager (if any).
				 *
				 * This default manager is, unless specified otherwise, the one
				 * that is deemed the most natural for the target running
				 * platform: standard one for computers (either UNIX or
				 * Windows), libfat-based one for the Nintendo DS.
				 *
				 * @note It means it will be the filesystem manager that will be
				 * used by default (should no specific manager be specified),
				 * which is not necessarily the most usual one for the target
				 * platform.
				 *
				 * Must be static to allow child classes to unsubscribe their
				 * manager if needed.
				 *
				 */
				static FileSystemManager * _CurrentDefaultFileSystemManager ;


				/**
				 * Tells whether this filesystem manager should monitor the
				 * files that were opened through it (and have been not closed
				 * yet).
				 *
				 */
				bool _trackOpenFiles ;


				/**
				 * Stores the list of currently opened files (if any, and if
				 * enabled).
				 *
				 * @note Protected, as some specific filesystem managers may
				 * have to transform that list (ex: cyphered filenames).
				 *
				 */
				std::list<std::string> _openFiles ;




			private:



				/**
				 * Default alias for the working directory (current directory).
				 *
				 * @example Typically it is ".".
				 *
				 */
				static const std::string DefaultAliasForCurrentDirectory ;


				/**
				 * Default alias for the upper (parent) directory.
				 *
				 * @example Typically it is "..".
				 *
				 */
				static const std::string DefaultAliasForParentDirectory ;



				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				FileSystemManager( const FileSystemManager & source ) ;



				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				FileSystemManager & operator = (
					const FileSystemManager & source ) ;



		} ;


	}


}



#endif // CEYLAN_FILE_SYSTEM_MANAGER_H_
