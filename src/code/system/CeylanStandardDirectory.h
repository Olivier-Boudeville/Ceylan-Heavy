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


#ifndef CEYLAN_STANDARD_DIRECTORY_H_
#define CEYLAN_STANDARD_DIRECTORY_H_



#include "CeylanDirectory.h"   // for inheritance


#include <list>
#include <string>




namespace Ceylan
{



	namespace System
	{


		/*
		 * Each Directory child class is linked to a corresponding filesystem
		 * manager child class, here it is the standard one.
		 *
		 */
		class StandardFileSystemManager ;
		

		
		/**
		 * Encapsulates standard directories, as provided by usual operating
		 * systems.
		 *
		 * Actual directories should be created and opened with respectively the
		 * Directory::Create and Directory::Open factories, that allow the
		 * user program to be cross-platform by hiding each filesystem-related
		 * per-platform specificity.
		 *
		 * @see File, FileSystemManager for other file-related operations.
		 *
		 */
		class CEYLAN_DLL StandardDirectory: public Directory
		{


			public:


				/**
				 * Destroys the directory reference, not the directory itself.
				 *
				 * @see Directory::Remove
				 *
				 * The destructor must be public as instances created by 
				 * factories have to be deallocated by the user.
				 *
				 */
				virtual ~StandardDirectory() throw() ;




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
						const std::string & subdirectoryName ) const
					throw( DirectoryLookupFailed ) ;


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
				virtual bool hasFile( const std::string & fileName ) const
					throw( DirectoryLookupFailed ) ;


				/**
				 * Tells whether the directory has a direct entry named 
				 * <b>entryName</b>, whatever the entry is (file, directory,
				 * named FIFO, socket, etc.), except aliases for current
				 * directory (ex: '.') and parent one (ex: '..') that are
				 * ignored (false is returned for them).
				 *
				 * @param entryName the name of the entry to look-up.
				 *
				 * @throw DirectoryLookupFailed is the operation failed or is
				 * not supported.
				 *
				 */
				virtual bool hasEntry( const std::string & entryName ) const
					throw( DirectoryLookupFailed ) ;



				/**
				 * Returns the names of all direct subdirectories of this
				 * directory, in the specified list.
				 *
				 * @param subDirectories the caller-provided list in which
				 * subdirectories will be added.
				 *
				 * @throw DirectoryLookupFailed if the operation failed or
				 * is not supported.
				 *
				 * @note Aliases for current and parent directories (ex: '.'
				 * and '..') will be filtered out.
				 *
				 */
				virtual void getSubdirectories( 
						std::list<std::string> & subDirectories )
					const throw( DirectoryLookupFailed ) ;


				/**
				 * Returns the names of all regular files and symbolic links 
				 * of this directory, in the specified list.
				 *
				 * @param files the caller-provided list in which
				 * regular files and symbolic links will be added.
				 *
				 * @throw DirectoryLookupFailed if the operation failed or
				 * is not supported.
				 *
				 */
				virtual void getFiles( std::list<std::string> & files )
					const throw( DirectoryLookupFailed ) ;


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
				 */
				virtual void getEntries( std::list<std::string> & entries )
					const throw( DirectoryLookupFailed ) ;


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
				 * other entries (named FIFO, sockets, etc.) of this
				 * directory will be added.
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
						std::list<std::string> & otherEntries )
					const throw( DirectoryLookupFailed ) ;




				// Other instance methods.
				

				// goDown, isValid, getPath, removeLeadingSeparator inherited.


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




				// Factory section.


				/**
				 * Returns a StandardDirectory reference on a directory newly
				 * created on disk, thanks to the operating system facilities.
				 *
				 * @param newDirectoryName the name of the directory to create.
				 *
				 * This is a StandardDirectory helper factory, user code should
				 * rely on the higher level and cross-platform 
				 * Directory::Create instead.
				 *
				 * @throw DirectoryException, including DirectoryCreationFailed
				 * if the operation failed or is not supported on this platform,
				 * or DirectoryDelegatingException if the relevant filesystem
				 * manager could not be retrieved.
				 *
				 */
				static StandardDirectory & Create( 
						const std::string & newDirectoryName ) 
					throw( DirectoryException ) ;

				
				/**
				 * Returns a StandardDirectory reference on specified
				 * already-existing directory, which will be "opened" (i.e.
				 * referred to), thanks to the operating system facilities.
				 *
				 * @param directoryName the name of the directory. If not
				 * specified (the string is empty), returns a reference to the
				 * current working directory.
				 *
				 * This is a StandardDirectory helper factory, user code should
				 * rely on the higher level and cross-platform 
				 * Directory::Open instead.
				 *
				 * @throw DirectoryException, including DirectoryOpeningFailed
				 * if the operation failed or is not supported on this platform,
				 * or DirectoryDelegatingException if the relevant filesystem
				 * manager could not be retrieved.
				 *
				 */
				static StandardDirectory & Open( 
						const std::string & directoryName = "" ) 
					throw( DirectoryException ) ;
					
					

				/**
				 * Creates a StandardDirectory reference on a directory either
				 * already existing, or to be created on disk thanks to the
				 * operating system facilities.
				 *
				 * @param directoryName the name of the directory to create.
				 * If this string is empty, then the current directory of the
				 * process will be retained.
				 *
				 * This is a StandardDirectory helper constructor, user code
				 * should rely on the higher level and cross-platform factories,
				 * Directory::Create and Directory::Open instead.
				 *
				 * @throw DirectoryException, including DirectoryCreationFailed
				 * if the operation failed or is not supported on this platform,
				 * or DirectoryDelegatingException if the relevant filesystem
				 * manager could not be retrieved.
				 *
				 */
				explicit StandardDirectory( 
						const std::string & directoryName = "",
						bool create = true ) 
					throw( DirectoryException ) ;

				
					
			protected:



				/**
				 * Returns the standard filesystem manager.
				 *
				 * @note It is not necessarily the current default one.
				 *
				 * @throw DirectoryDelegatingException if the operation failed.
				 *
				 */
				virtual FileSystemManager & getCorrespondingFileSystemManager()
					const throw( DirectoryDelegatingException ) ;



				/**
				 * Root directory prefix.
				 * @example "" on Unix, "c:" on Windows.
				 *
				 */
				static const std::string RootDirectoryPrefix ;


				/**
				 * Current directory name alias.
				 *
				 * @example Typically it is ".".
				 *
				 */
				static const std::string CurrentDirectoryAlias ;


				/**
				 * Upper directory alias.
				 *
				 * @example Typically it is "..".
				 *
				 */
				static const std::string UpperDirectoryAlias ;


				/**
				 * Directory separator.
				 * @example Slash or backslash.
				 */
				static const char Separator ;



			private:


				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				StandardDirectory( const StandardDirectory & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 *
				 */
				StandardDirectory & operator = ( 
					const StandardDirectory & source ) throw() ;



		} ;


	}

}


#endif // CEYLAN_STANDARD_DIRECTORY_H_

