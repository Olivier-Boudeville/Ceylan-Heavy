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


#ifndef CEYLAN_DIRECTORY_H_
#define CEYLAN_DIRECTORY_H_


#include "CeylanFileSystemCommon.h"    // for DirectoryException and al
#include "CeylanStringUtils.h"         // for Latin1Char


#include <list>
#include <string>




namespace Ceylan
{



	namespace System
	{



		/*
		 * Each Directory child class is linked to a corresponding filesystem
		 * manager child class.
		 *
		 */
		class FileSystemManager ;
		
		

		/**
		 * Thrown when file operations failed because of underlying
		 * filesystem manager: the corresponding backend could not 
		 * be retrieved as expected.
		 *
		 */
		class CEYLAN_DLL DirectoryDelegatingException: public DirectoryException
		{ 
		
			public: 
			
				explicit DirectoryDelegatingException( 
					const std::string & reason ) ; 
					
		} ;
		



		
		/**
		 * Abstract directory mother class, so that programs can always
		 * manipulate Ceylan::Directory instances, whereas per-platform
		 * specialized classes are actually used by the system.
		 *
		 * Examples of convenient cross-platform Directory methods whose use 
		 * is encouraged: Create, Open, Remove, ExistsAsDirectory.
		 *
		 * @see File, FileSystemManager for other file-related operations.
		 *
		 */
		class CEYLAN_DLL Directory: public TextDisplayable
		{


			public:



				/*
				 * Static section.
				 *
				 * These methods encapsulate calls to the actual filesystem
				 * manager, so that the developer does not need to mess with
				 * platform-specific details and/or with the management of
				 * specialized filesystem managers. 
				 *
				 * Should a delegated operation fail, a DirectoryException is
				 * thrown, including if the filesystem manager backend could 
				 * not be retrieved. In that case a more specialized exception
				 * is thrown, a DirectoryDelegatingException.
				 * 
				 * The filesystem being used is the default one: either one is
				 * already defined, and it will be used, or none is available
				 * and the platform-specific default one will be created (thus
				 * it has a side-effect).
				 *
				 * @see FileSystemManager::GetAnyDefaultFileSystemManager
				 *
				 * @note Non-static methods uses their linked filesystem
				 * manager, which may or may not be the default one: depending
				 * on a static or non-static method being called, different
				 * filesystem managers may be called.
				 *
				 */
				

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
				 * @throw DirectoryException, including DirectoryLookupFailed 
				 * if the operation failed (existence test failed with no
				 * answer) or is not supported on this platform, or
				 * DirectoryDelegatingException should the underlying 
				 * filesystem manager not be retrieved as expected. 
				 *
				 */
				static bool Exists( const std::string & directoryPath ) ;
				


				/**
				 * Removes the directory from disk.
				 *
				 * @param directoryPath the path of the target directory.
				 *
				 * @param recursive if false, the specified directory is 
				 * expected to be empty, and it will be removed. If true,
				 * then the full directory content (including all files and
				 * possible subdirectories) and this directory itself will be
				 * removed.
				 *
				 * @throw DirectoryException, including DirectoryRemoveFailed 
				 * if the operation failed or is not supported on this platform,
				 * or DirectoryDelegatingException should the underlying 
				 * filesystem manager not be retrieved as expected. 
				 *
				 */
				static void Remove( const std::string & directoryPath, 
					bool recursive = false ) ;
				
				
				
				/**
				 * Moves specified directory on filesystem.
				 *
				 * A special case of directory moving is directory renaming.
				 *
				 * @param sourceDirectoryname the path of the directory to be
				 * moved.
				 *
				 * @param targetDirectoryname the path of the target directory.
				 *
				 * @throw DirectoryException, including DirectoryMoveFailed 
				 * if the operation failed or is not supported on this platform,
				 * or DirectoryDelegatingException should the underlying 
				 * filesystem manager not be retrieved as expected. 
				 *
				 */
				static void Move( const std::string & sourceDirectoryname,
					const std::string & targetDirectoryname ) ;
				
				
				
				/**
				 * Copies the directory on filesystem.
				 *
				 * @param sourceDirectoryname the path of the directory to be
				 * copied.
				 *
				 * @param targetDirectoryname the path of the target directory.
				 *
				 * @throw DirectoryException, including DirectoryCopyFailed 
				 * if the operation failed or is not supported on this platform,
				 * or DirectoryDelegatingException should the underlying 
				 * filesystem manager not be retrieved as expected. 
				 *
				 */
				static void Copy( const std::string & sourceDirectoryname,
					const std::string & targetDirectoryname ) ;



				/**
				 * Returns the last change time of the specified directory.
				 *
				 * @param directoryPath the path of the directory whose last
				 * change time is searched.
				 *
				 * @throw DirectoryLastChangeTimeRequestFailed if the operation
				 * failed or is not supported on this platform, or
				 * DirectoryDelegatingException if the relevant filesystem
				 * manager could not be retrieved.
				 *
				 */
				static time_t GetLastChangeTime( 
					const std::string & directoryPath ) ;



				/**
				 * Returns whether specified string is a valid directory path.
				 *
				 * @param directoryString the directory string.
				 *
				 * @note If no regular expression support is available, 
				 * then the name will be deemed always correct.
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static bool IsAValidDirectoryPath( 
					const std::string & directoryString ) ;



				/**
				 * Removes the leading separator, if any, in specified
				 * directory's path.
				 *
				 * @param path the path that will be modified.
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static void RemoveLeadingSeparator( std::string & path ) ;

			
			
				/**
				 * Tells whether specified path is an absolute path.
				 *
				 * @param path the path that may be absolute.
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static bool IsAbsolutePath( const std::string & path ) ;



				/**
				 * Returns the current working directory name.
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static std::string GetCurrentWorkingDirectoryPath()	;



				/**
				 * Changes current working directory to
				 * <b>newWorkingDirectory</b>.
				 *
				 * @param newWorkingDirectory the target working directory.
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static void ChangeWorkingDirectory( 
						const std::string & newWorkingDirectory ) ;



				/**
				 * Splits up <b>path</b> into the list of its sub-elements
				 * (set of directory/file names).
				 *
				 * @param path the path to split.
				 *
				 * @example SplitPath( "/mnt/raid/md0/LOANI-0.3" ) returns 
				 * on UNIX:
				 * [ "", "mnt", "raid", "md0", "LOANI-0.3" ].
				 *
				 * @see JoinPath
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static std::list<std::string> SplitPath( 
					const std::string & path ) ;



				/**
				 * Joins the specified path elements with the relevant 
				 * directory separator.
				 *
				 * @param pathElements the path elements to join in a path.
				 *
				 * @example JoinPath([ "", "mnt", "raid", "md0", "LOANI-0.3" ])
				 * returns on UNIX: "/mnt/raid/md0/LOANI-0.3".
				 *
				 * @see SplitPath
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static std::string JoinPath( 
						const std::list<std::string> & pathElements ) ;
					
					

				/**
				 * Joins the two specified path elements with the relevant
				 * directory separator.
				 *
				 * @param firstPath the first part of the final path.
				 *
				 * @param secondPath the second part of the final path.
				 *
				 * @example JoinPath( "/mnt/raid", "md0/LOANI-0.3" ) 
				 * returns on UNIX: "/mnt/raid/md0/LOANI-0.3".
				 *
				 * @see SplitPath
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static std::string JoinPath( const std::string & firstPath,
					const std::string & secondPath ) ;
					
					
					
				/**
				 * Separates the full pathname <b>path</b> into a basename
				 * <b>base</b> and file name <b>file</b>.
				 *
				 * For example, this method applied to 
				 * "/mnt/raid/md0/LOANI-0.3" returns respectively
				 * "/mnt/raid/md0" and "LOANI-0.3", when the separator is '/'.
				 *
				 * @param path the path which is to be stripped.
				 *
				 * @param base if non null, must be a pointer to an 
				 * already allocated string where the basename will be stored.
				 * If not interested in the basename, specify a null (0)
				 * pointer instead: this method will act as UNIX "basename".
				 *
				 * @param file if non null, must be a pointer to an already
				 * allocated string where the filename will be stored. 
				 * If not interested in the filename, specify a null (0) 
				 * pointer instead: this method will act as UNIX "dirname".
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static void StripFilename( const std::string & path,
						std::string * base, std::string * file = 0 ) ;
					
					
										
				/**
				 * Returns the directory separator, a Latin-1 character.
				 *
				 * @example Slash or backslash, i.e. '/' or '\'.
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static Ceylan::Latin1Char GetSeparator() ;



				/**
				 * Returns the directory separator, in the form of a string.
				 *
				 * More convenient for some operations than a character.
				 *
				 * @example Slash or backslash, i.e. '/' or "\".
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported on this platform. This includes throwing a more
				 * specific DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static std::string GetSeparatorAsString() ;
				
				

				
				// Factory subsection.



				/**
				 * Returns a Directory reference on a directory newly created
				 * on disk.
				 *
				 * @param newDirectoryName the name of the directory to create.
				 *
				 * Directory factory, to be used instead of a specific Directory
				 * subclass constructor, so that it can return a Directory
				 * instance that is actually a specialized one (ex: a
				 * StandardDirectory, not an abstract Directory).
				 *
				 * @throw DirectoryException, including DirectoryCreationFailed
				 * if the operation failed or is not supported on this platform,
				 * or DirectoryDelegatingException if the relevant filesystem
				 * manager could not be retrieved.
				 *
				 */
				static Directory & Create( 
					const std::string & newDirectoryName ) ;


				
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
				 * if the operation failed or is not supported on this platform,
				 * or DirectoryDelegatingException if the relevant filesystem
				 * manager could not be retrieved.
				 *
				 */
				static Directory & Open( 
					const std::string & directoryName = "" ) ;
					


				/**
				 * Destroys the directory reference, not the directory itself.
				 *
				 * @see Remove
				 *
				 * The destructor must be public as instances created by 
				 * factories have to be deallocated by the user.
				 *
				 */
				virtual ~Directory() throw() ;




				// Instance methods.
				
				
				
				// Directory content subsection.
				
				
				
				/**
				 * Tells whether the directory has a direct subdirectory named 
				 * <b>subdirectoryName</b>.
				 *
				 * @param subdirectoryName the name of the directory entry to
				 * look-up. Alias for current directory (ex: '.') and parent
				 * one (ex: '..') are always deemed existing.
				 *
				 * @throw DirectoryLookupFailed is the operation failed or is
				 * not supported.
				 *
				 */
				virtual bool hasDirectory( 
					const std::string & subdirectoryName ) const = 0 ;



				/**
				 * Tells whether the directory has a direct file or symbolic
				 * link named <b>fileName</b>.
				 *
				 * @param fileName the name of the file to look-up.
				 *
				 * @throw DirectoryLookupFailed is the operation failed or is
				 * not supported.
				 *
				 */
				virtual bool hasFile( const std::string & fileName ) const = 0 ;

				
				
				/**
				 * Tells whether the directory has a direct entry named 
				 * <b>entryName</b>.
				 *
				 * @param entryName the name of the entry to look-up.
				 *
				 * @throw DirectoryLookupFailed is the operation failed or is
				 * not supported.
				 *
				 */
				virtual bool hasEntry( const std::string & entryName ) const 
					= 0 ;



				/**
				 * Returns the names of all direct subdirectories of this
				 * directory, in the specified list.
				 *
				 * @param subDirectories the caller-provided list in which
				 * subdirectories will be added.
				 *
				 * @throw DirectoryLookupFailed if an error occured.
				 *
				 * @note Aliases for current and parent directories (ex: '.'
				 * and '..') will be filtered out.
				 *
				 */
				virtual void getSubdirectories( 
					std::list<std::string> & subDirectories ) const = 0 ;



				/**
				 * Returns the names of all files of this directory, in the
				 * specified list.
				 *
				 * @param files the caller-provided list in which
				 * subdirectories will be added.
				 *
				 * @throw DirectoryLookupFailed if an error occured.
				 *
				 */
				virtual void getFiles( std::list<std::string> & files )
					const = 0 ;



				/**
				 * Returns the names of all direct entries of any type of 
				 * this directory (including files and directories), in the
				 * specified list.
				 *
				 * @param entries the caller-provided list in which 
				 * entries will be added.
				 *
				 * @throw DirectoryLookupFailed if the operation failed or
				 * is not supported.
				 *
				 * @note Aliases for current and parent directories (ex: '.'
				 * and '..') will be filtered out.
				 *
				 * @note Aliases for current and parent directories (ex: '.'
				 * and '..') will be filtered out.
				 *
				 * @throw DirectoryLookupFailed if the operation failed or
				 * is not supported.
				 *
				 */
				virtual void getEntries( std::list<std::string> & entries )
					const = 0 ;



				/**
				 * Returns the names of all direct entries of any type of 
				 * this directory (including files and directories), in the
				 * corresponding specified list.
				 *
				 * @param subDirectories the caller-provided list in which 
				 * subDirectories of this directory will be added.
				 *
				 * @param files the caller-provided list in which 
				 * files of this directory will be added.
				 *
				 * @param otherEntries the caller-provided list in which 
				 * other entries (named FIFO, sockets, block or character
				 * device, etc.) of this directory will be added.
				 *
				 * @throw DirectoryLookupFailed if the operation failed or
				 * is not supported.
				 *
				 * @note Aliases for current and parent directories (ex: '.'
				 * and '..') will be filtered out.
				 *
				 */
				virtual void getSortedEntries( 
					std::list<std::string> & subDirectories,
					std::list<std::string> & files,
					std::list<std::string> & otherEntries ) const = 0 ;




				// Other instance methods.
				

				/**
				 * Changes directory to one of its direct subdirectories.
				 *
				 * @param subdirectoryName the name of the subdirectory of
				 * this directory to go to. It should not have any separator
				 * in it.
				 *
				 * @example myDir.goDown( "data" )
				 *
				 * @throw DirectoryChangeFailed if the operation failed or is
				 * not supported.
				 *
				 */
				virtual void goDown( const std::string & subdirectoryName ) ;



				/**
				 * Tells whether the reference on the directory is valid.
				 *
				 * Checks that corresponding path exists, and that it is a
				 * directory.
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported.
				 *
				 */
				virtual bool isValid() const ;



				/**
				 * Returns the path of the directory which is referred to.
				 *
				 * @return the path, with no leading separator.
				 *
				 */
				virtual const std::string & getPath() const ;



				/**
				 * Removes the leading separator, if any, in the path of this
				 * directory path.
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported.
				 *
				 */
				virtual void removeLeadingSeparator() ;



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
					Ceylan::VerbosityLevels level = Ceylan::high ) const  ;



				

			protected:



				/**
				 * Constructs a reference to the directory <b>directoryName</b>.
				 *
				 */
				explicit Directory( const std::string & directoryName )	;



				/**
				 * Returns the filesystem manager that corresponds to the 
				 * actual Directory child class.
				 *
				 * @throw DirectoryDelegatingException if the operation failed.
				 *
				 */
				virtual FileSystemManager & getCorrespondingFileSystemManager()
					const = 0 ;



				/**
				 * The path a directory instance refers to.
				 *
				 * @note All stored path have any leading separator removed.
				 *
				 */
				std::string _path ;



				/**
				 * Returns the filesystem manager that should be used by 
				 * Directory static methods, which is the default manager.
				 *
				 * @return the default filesystem manager.
				 *
				 * @throw DirectoryDelegatingException if the operation failed.
				 *
				 */
				static FileSystemManager & GetCorrespondingFileSystemManager() ;




			private:



				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				Directory( const Directory & source ) ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 *
				 */
				Directory & operator = ( const Directory & source ) ;
			

		} ;
		

	}

}



#endif // CEYLAN_DIRECTORY_H_

