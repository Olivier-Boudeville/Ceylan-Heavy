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


#ifndef CEYLAN_FILE_LOCATOR_H_
#define CEYLAN_FILE_LOCATOR_H_


#include "CeylanTextDisplayable.h"   // for inheritance
#include "CeylanSystem.h"            // for SystemException


#include <string>
#include <list>




namespace Ceylan
{


	namespace System
	{



		/// Exception raised when file locator could not fulfill a request.
		class FileLocatorException : public SystemException
		{

			public:

				FileLocatorException( const std::string & message ) ;
				virtual ~FileLocatorException() throw() ;

		} ;




		/**
		 * File locators allow to find files in a set of directories, as Unix
		 * PATH variables do.
		 *
		 * Directories are searched in turn from the first registered one to the
		 * last.
		 *
		 * The user may register non-existing directories in the locator.
		 * A given directory can be referenced at most one time in the locator.
		 *
		 */
		class CEYLAN_DLL FileLocator : public TextDisplayable
		{


			public:



				/// Creates a new empty file locator.
				FileLocator() ;



				/**
				 * Creates a new file locator, whose first registered paths come
				 * from specified environment variable, parsed according to the
				 * chosen separator.
				 *
				 * @see addPathsFromEnvironmentVariable
				 *
				 */
				explicit FileLocator( const std::string & variableName,
					char separator = ':' ) ;



				/// Virtual destructor.
				virtual ~FileLocator() throw() ;



				/**
				 * Adds a new path to the set of directories gathered by the
				 * file locator.
				 *
				 * @param newPath a new path to add.
				 *
				 * @return true iff the added path was not already listed.
				 *
				 */
				virtual bool addPath( const std::string & newPath ) ;



				/**
				 * Adds all paths in specified list to the set of directories
				 * gathered by the file locator.
				 *
				 * @param paths the list of paths to add.
				 *
				 * @return true iff at least one path was not already listed.
				 *
				 */
				virtual bool addPaths( const std::list<std::string> & paths ) ;



				/**
				 * Adds the set of directories specified by an environment
				 * variable, which are aggregated thanks to the specified
				 * separator.
				 *
				 * For example, addPathsFromEnvironmentVariable( "PATH" ), with
				 * PATH = "/bin:/usr/bin:/usr/local/bin:/usr/local" adds the
				 * corresponding four directories to this file locator.
				 *
				 * @return true iff at least one directory has been added thanks
				 * to this call.
				 *
				 */
				virtual bool addPathsFromEnvironmentVariable(
					const std::string & variableName,
					char separator = ':' ) ;



				/**
				 * Removes specified path from the set of directories of the
				 * file locator, if present.
				 *
				 * @param pathToRemove the path to remove.
				 *
				 * @return true iff the specified path has been found, and
				 * therefore has been removed.
				 *
				 */
				virtual bool removePath( const std::string & pathToRemove ) ;



				/**
				 * Finds the specified file in directories listed by the
				 * locator, and returns the full path of the first match in the
				 * list.
				 *
				 * @param filename the name of the file to look for, be it a
				 * regular file or a symbolic link.
				 *
				 * @return the full path of the first directory stored in this
				 * Locator that contains specified file.
				 *
				 * @throw FileLocatorException if nothing matches, i.e. if this
				 * file is not found through recorded directories.
				 *
				 */
				virtual std::string find( const std::string & filename )
					const ;



				/// Returns the internal list of paths managed by the locator.
				virtual const std::list<std::string> & getPaths() const ;



				/**
				 * Returns an user-friendly description of the state of this
				 * object.
				 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see Ceylan::TextDisplayable
				 *
				 */
				virtual const std::string toString(
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;




			protected:


/*
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt to understand
 * it, and to be aware of the associated risks.
 *
 */
#pragma warning( push )
#pragma warning( disable : 4251 )

				/// The internal list of paths of the locator.
				std::list<std::string> _paths ;

#pragma warning( pop )




			private:



				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 *
				 */
				FileLocator( const FileLocator & source ) ;



				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				FileLocator & operator = ( const FileLocator & source )  ;



		} ;


	}


}



#endif // CEYLAN_FILE_LOCATOR_H_
