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
			
				FileLocatorException( const std::string & message ) throw() ;
				virtual ~FileLocatorException() throw() ;
			
		} ;
		
		
	
		/**
		 * File locators allow to find files in a set of directories, as 
		 * Unix PATH variables do.
		 *
		 * Directories are searched in turn from the first registered one 
		 * to the last.
		 *
		 * The user may register non-existing directories in the locator.
		 * A given directory can be referenced at most one time in the 
		 * locator. 
		 * 
		 */
		class FileLocator : public TextDisplayable
		{
		
		
			public:
		
		
				/// Creates a new empty file locator.
				FileLocator() throw() ;
				
				
				/**
				 * Creates a new file locator, whose first registered paths 
				 * come from specified environment variable, parsed 
				 * according to the chosen separator.
				 *
				 * @see addPathsFromEnvironmentVariable
				 *
				 */
				explicit FileLocator( const std::string & variableName, 
					char separator = ':' ) throw() ;
				
				
				/// Virtual destructor.
				virtual ~FileLocator() throw() ;
				
				
				/**
				 * Adds a new path to the set of directories gathered by 
				 * the file locator.
				 *
				 * @param newPath a new path to add.
				 *
				 * @return true iff the added path was not already listed.
				 *
				 */
				virtual bool addPath( const std::string & newPath ) throw() ;
				
				
				/**
				 * Adds all paths in specified list to the set of directories
				 * gathered by the file locator.
				 *
				 * @param paths the list of paths to add.
				 *
				 * @return true iff at least one path was not already listed.
				 *
				 */
				virtual bool addPaths( const std::list<std::string> & paths )
					throw() ;
		
		
				/**
				 * Adds the set of directories specified by an environment
				 * variable, which are aggregated thanks to the specified
				 * separator.
				 *
				 * For example, addPathsFromEnvironmentVariable( "PATH" ), with 
				 * PATH = "/bin:/usr/bin:/usr/local/bin:/usr/local" adds the
				 * corresponding four directories to this file locator.
				 *
				 * @return true iff at least one directory has been added 
				 * thanks to this call.
				 *
				 */
				virtual bool addPathsFromEnvironmentVariable( 
					const std::string & variableName,
					char separator = ':' ) throw() ;
					
				 
				/**
				 * Removes specified path from the set of directories of the
				 * file locator, if present.
				 *
				 * @param pathToRemove the path to remove.
				 *
				 * @return true iff the specified path has been found, 
				 * and therefore has been removed.
				 *
				 */
				virtual bool removePath( const std::string & pathToRemove )
					throw() ;
				
				
				/**
				 * Finds the specified file in directories listed by the
				 * locator, and returns the full path of the first match
				 * in the list.
				 *
				 * @param filename the name of the file to look for, be it
				 * a regular file or a symbolic link.
				 *
				 * @return the full path of the first directory stored in 
				 * this Locator that contains specified file.
				 *
				 * @throw FileLocatorException if nothing matches, i.e. if 
				 * this file is not found through recorded directories.
				 *
				 */
				virtual std::string find( const std::string & filename ) const 
					throw( FileLocatorException ) ;
				
				
				/// Returns the internal list of paths managed by the locator.
				virtual const std::list<std::string> & getPaths() const 
					throw() ;
					
					 
				/**
	             * Returns an user-friendly description of the state of 
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see Ceylan::TextDisplayable
	             *
	             */
		 		virtual const std::string toString( 
						Ceylan::VerbosityLevels level = Ceylan::high ) 
					const throw() ;
			
			
			
			protected:
			
				

				/// The internal list of paths of the locator.
				std::list<std::string> _paths ;
			
			
			
			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 *
				 */			 
				FileLocator( const FileLocator & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */			 
				FileLocator & operator = ( const FileLocator & source ) 
					throw() ;
				
				
		} ;
	
	
	}


}

 
#endif // CEYLAN_FILE_LOCATOR_H_
