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


#ifndef CEYLAN_STANDARD_FILE_SYSTEM_MANAGER_H_
#define CEYLAN_STANDARD_FILE_SYSTEM_MANAGER_H_



#include "CeylanFileSystemManager.h"  // for inheritance
#include "CeylanFileSystemCommon.h"   // for FileSystemManagerException


#include <string>



namespace Ceylan
{



	namespace System
	{



		class CEYLAN_DLL StandardFileSystemManagerException: 
			public FileSystemManagerException
		{ 
		
			public: 
			
				explicit StandardFileSystemManagerException( 
						const std::string & reason ) throw():
					FileSystemManagerException( reason )
				{
				
				}								
					
		} ;



		/**
		 * Allows to interact with the filesystems supported by any underlying
		 * operation system.
		 *
		 * Standard (POSIX and all) calls are issued by this class.
		 *
         * Use GetStandardFileSystemManager to retrieve this manager.
         *
		 */
		class CEYLAN_DLL StandardFileSystemManager: public FileSystemManager
		{


			public:


				/*
				 * No static methods exposed: the user is expected to call  
				 * them from the abstract mother classes: File, Directory and
				 * FileSystemManager.
				 *
				 * So only the pure virtual methods of FileSystemManager have
				 * to be defined in this child class, that must not be abstract.
				 *
				 */
				
				
				
				// FileSystemManager-specific section.
						
				
				/**
				 * Tells whether the filesystem entry <b>entryPath</b> exists,
				 * be it a file, a symbolic link, a directory, a character
				 * or block device, a FIFO, a socket, etc.
				 *
				 * @param entryPath the path of the entry to look-up.
				 *
				 * @throw EntryLookupFailed if the operation failed (existence
				 * test failed with no answer) or is not supported on this 
				 * platform.
				 *
				 */
				virtual bool existsAsEntry( const std::string & entryPath ) 
					const throw( EntryLookupFailed )  ;


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
					const std::string & linkName ) throw( SymlinkFailed ) ;


				/**
				 * Returns the change time time of the entry <b>entryPath</b>,
				 * be it a file, a directory, etc.
				 *
				 * @param entryPath the path of the entry.
				 *
				 * @throw GetChangeTimeFailed if the operation failed or
				 * is not supported.
				 *
				 */
				virtual time_t getEntryChangeTime( 
						const std::string & entryPath )
					throw( GetChangeTimeFailed ) ;




				// Accessors to FilesystemManager constants.

				
				/**
				 * Returns the root directory prefix.
				 *
				 * @example "" on Unix, "c:" on Windows.
				 *
				 */
				virtual const std::string & getRootDirectoryPrefix()
					const throw() ;
	
	
				/**
				 * Returns the directory separator, as a Latin-1 character.
				 *
				 * @example Slash or backslash, i.e. '/' or '\'.
				 *
				 */
				virtual Ceylan::Latin1Char getSeparator() const throw() ;
	
	
				/*
				 * Already available from FileSystemManager:
				 *   - getSeparatorAsString()
				 *   - getAliasForCurrentDirectory()
				 *   - getAliasForParentDirectory()
				 *
				 */
	
	



				// File-related section.
				
				
				/**
				 * Returns a StandardFile reference on a newly created file.
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
				 * @note StandardFile helper factory, has to be public but the
				 * user should not use it: File::Create is expected to be used
				 * instead, as it is the cross-platform factory intended for
				 * use.
				 *
				 * @throw FileException, including FileCreationFailed if the
				 * operation failed or is not supported on this platform.
				 *
				 */
				virtual File & createFile( const std::string & filename, 
						OpeningFlag createFlag = File::CreateToWriteBinary,
						PermissionFlag permissionFlag = File::OwnerReadWrite ) 
					throw( FileException ) ;

				
				
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
				 * @note StandardFile helper factory, has to be public but the
				 * user should not use it: File::Open is expected to be used
				 * instead, as it is the cross-platform factory intended for
				 * use.
				 *
				 * @throw FileException, including FileOpeningFailed if the
				 * operation failed or is not supported on this platform.
				 *
				 */
				virtual File & openFile( const std::string & filename, 
						OpeningFlag openFlag = File::OpenToReadBinary ) 
					throw( FileException ) ;
				
					
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
						const std::string & filename ) const 
					throw( FileLookupFailed ) ;


				/**
				 * Removes the file or symbolic link from the filesystem.
				 *
				 * @param filename the filename to remove.
				 *
				 * @throw FileRemoveFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void removeFile( const std::string & filename ) 
					throw( FileRemoveFailed ) ;


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
						const std::string & targetFilename ) 
					throw( FileMoveFailed ) ;


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
						const std::string & targetFilename ) 
					throw( FileCopyFailed ) ;



				/**
				 * Returns the size, in bytes, of the specified file.
				 *
				 * @param filename the filename whose size is searched.
				 *
				 * @throw FileSizeRequestFailed if the operation failed (ex:
				 * file not found) or is not supported on this platform.
				 *
				 */
				virtual Size getSize( const std::string & filename ) 
					throw( FileSizeRequestFailed ) ;


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
						const std::string & filename ) 
					throw( FileLastChangeTimeRequestFailed ) ;


				// transformIntoValidFilename inherited from File.


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
				 * A FileTouchFailed exception would be raised instead.
				 *
				 * @see File::Create to create empty files.
				 *
				 * @throw FileTouchFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void touch( const std::string & filename ) 
					throw( FileTouchFailed ) ;


				// diff directly inherited from File.



			
				// Directory-related section.
			

				// Factory-related subsection.

				
				/**
				 * Returns a Directory reference on a directory newly created
				 * on filesystem.
				 *
				 * @param newDirectoryName the name of the directory to create.
				 *
				 * @note StandardDirectory helper factory, has to be public 
				 * but the user should not use it: Directory::Create is 
				 * expected to be used instead, as it is the cross-platform
				 * factory intended for use.
				 *
				 * @throw DirectoryException, including DirectoryCreationFailed
				 * if the directory creation failed.
				 *
				 */
				virtual Directory & createDirectory( 
						const std::string & newDirectoryName ) 
					throw( DirectoryException ) ;

				
				/**
				 * Returns a Directory reference on specified already-existing
				 * directory, which will be "opened" (i.e. referred to).
				 *
				 * @param directoryName the name of the directory. If not
				 * specified (the string is empty), returns a reference to the
				 * current working directory.
				 *
				 * @note StandardDirectory helper factory, has to be public 
				 * but the user should not use it: Directory::Open is 
				 * expected to be used instead, as it is the cross-platform
				 * factory intended for use.
				 *
				 * @throw DirectoryException, including DirectoryOpeningFailed
				 * if the directory opening failed.
				 *
				 */
				virtual Directory & openDirectory( 
						const std::string & directoryName = "" ) 
					throw( DirectoryException ) ;
					


				/**
				 * Tells whether the directory <b>directoryPath</b> exists 
				 * and is a directory indeed.
				 *
				 * @param directoryPath the directory path to look-up.
				 *
				 * @note On Windows, files and directories are 
				 * case-insensitive, and 'c:' is not a directory 
				 * (it is seen as a drive), whereas 'c:\' is a directory.
				 *
				 * @throw DirectoryLookupFailed if the operation failed
				 * (existence test failed with no answer) or is not supported 
				 * on this platform.
				 *
				 */
				virtual bool existsAsDirectory( 
						const std::string & directoryPath ) const
					throw( DirectoryLookupFailed ) ;


				/**
				 * Removes the directory from filesystem.
				 *
				 * @param directoryPath the path of the target directory.
				 *
				 * @param recursive if false, the specified directory is 
				 * expected to be empty, and it will be removed. If true,
				 * then the full directory content (including all files and
				 * possible subdirectories) and this directory itself will be
				 * removed.
				 *
				 * @throw DirectoryRemoveFailed if the operation failed or 
				 * is not supported.
				 *
				 */
				virtual void removeDirectory( 
						const std::string & directoryPath, 
						bool recursive = false ) 
					throw( DirectoryRemoveFailed ) ;


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
						const std::string & targetDirectoryPath ) 
					throw( DirectoryMoveFailed ) ;


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
						const std::string & targetDirectoryPath ) 
					throw( DirectoryCopyFailed ) ;


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
						const std::string & directoryPath ) 
					throw( DirectoryLastChangeTimeRequestFailed ) ;
					
					
				/**
				 * Returns whether specified string is a valid directory path.
				 * (i.e. checks the name can be used, does not look-up any
				 * real filesystem entry).
				 *
				 * @param directoryString the directory string to examine.
				 *
				 * @note If no regular expression support is available, 
				 * then the path will be deemed always correct.
				 *
				 */
				virtual bool isAValidDirectoryPath( 
					const std::string & directoryString ) throw() ;
			
			
			
				// removeLeadingSeparator inherited.
			
			
			
				/**
				 * Tells whether specified path is an absolute path.
				 *
				 * @param path the path that may be absolute.
				 *
				 */
				virtual bool isAbsolutePath( const std::string & path ) 
					throw() ;
			
			
				/**
				 * Returns the current working directory path.
				 *
				 * @throw DirectoryGetCurrentFailed if the operation failed 
				 * or is not supported on the target platform.
				 *
				 */
				virtual std::string getCurrentWorkingDirectoryPath()	
					throw( DirectoryGetCurrentFailed ) ;


				/**
				 * Changes current working directory to
				 * <b>newWorkingDirectory</b>.
				 *
				 * @param newWorkingDirectory the target working directory.
				 *
				 * @throw DirectoryChangeFailed if the operation failed 
				 * or is not supported on the target platform.
				 *
				 */
				virtual void changeWorkingDirectory( 
						const std::string & newWorkingDirectory )
					throw( DirectoryChangeFailed ) ;


				/*
				 * Already available from FileSystemManager:
				 *   - splitPath
				 *   - joinPath (both)
				 *   - stripFilename
				 *
				 */



				// StandardFileSystemManager own section.
				

				/**
				 * Returns a user-friendly description of the state of 
				 * this object.
				 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
				 */
	            virtual const std::string toString( 
						Ceylan::VerbosityLevels level = Ceylan::high )
					const throw() ;
	





				// Static section to handle standard filesystem manager.
			


				/**
				 * Returns a reference to the unique standard filesystem 
				 * manager. 
				 *
				 * Does not set this filesystem manager as the default one.
				 *
				 * Creates it if needed: this method ensures it remains a
				 * singleton.
				 *
				 * Must be public, as ancestor has to be able to call it.
				 *
				 * @throw StandardFileSystemManagerException if the operation
				 * failed.
				 *
				 */
				static StandardFileSystemManager & 
						GetStandardFileSystemManager() 
					throw( StandardFileSystemManagerException ) ;


				/**
				 * Removes the current standard filesystem manager, if any.
				 *
				 * Must be public, as ancestor has to be able to call it.
				 *
				 * @note Removing such manager, if it was set as the default 
				 * one, will remove it as well.
				 *
				 */
				static void RemoveStandardFileSystemManager() throw() ;
			
			
			
				/**
				 * Constructs a reference to a standard filesystem, initializes 
				 * accordingly any needed subsystem.
				 *
				 * Cannot be private, as has to be subclassed.
				 * Cannot be protected, as has to be instanciated by factories
				 * from the mother class.
				 *
				 * @note Not to be called by the user.
				 *
				 * @see GetStandardFileSystemManager instead.
				 *
				 * @throw StandardFileSystemManagerException if the operation
				 * failed.
				 *
				 */
				StandardFileSystemManager() 
					throw( StandardFileSystemManagerException ) ;


				/**
				 * Destroys the Ceylan standard filesystem reference, not the 
				 * filesystem itself.
				 *
				 * Cannot be private as has to be subclassed.
				 *
				 */
				virtual ~StandardFileSystemManager() throw() ;


				/**
				 * Duplicates the file descriptor.
				 *
				 * @param fd the file descriptor to duplicate.
				 *
				 * @throw DuplicateFailed if the operation failed or
				 * if the file descriptor feature is not available.
				 *
				 */
				static FileDescriptor Duplicate( FileDescriptor fd ) 
					throw( DuplicateFailed ) ;



			private:
				
				
				
				// Directory constants.
				
				
				/**
				 * Root directory prefix.
				 * @example "" on Unix, "c:" on Windows.
				 *
				 */
				static const std::string RootDirectoryPrefix ;


				/**
				 * Directory separator.
				 *
				 * @example Slash or backslash.
				 *
				 */
				static const Ceylan::Latin1Char Separator ;
				
						
						
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				StandardFileSystemManager( 
					const StandardFileSystemManager & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				StandardFileSystemManager & operator = ( 
					const StandardFileSystemManager & source ) throw() ;
				
				
					
					
				/// Pointer to the current standard filesystem manager (if any).
				static StandardFileSystemManager * _StandardFileSystemManager ;
				

		} ;

	}

}


#endif // CEYLAN_STANDARD_FILE_SYSTEM_MANAGER_H_
