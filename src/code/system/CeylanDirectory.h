#ifndef CEYLAN_DIRECTORY_H_
#define CEYLAN_DIRECTORY_H_



#include "CeylanSystem.h"


#include <list>
#include <string>




namespace Ceylan
{



	namespace System
	{


		/**
		 * Simple directory management class.
		 *
		 * @note All internal paths kept in directory references (this object)
		 * should be absolute paths.
		 *
		 * @todo check conformity with other platforms path conventions 
		 * (namely, Windows).
		 *
		 * @see File
		 *
		 */
		class CEYLAN_DLL Directory : public TextDisplayable
		{

			public:


				/// Mother class of all directory-related exceptions.
				class DirectoryException: public SystemException
				{
					public:

						explicit DirectoryException( 
								const std::string & reason ) :
							SystemException( reason )
						{
						
						}

				} ;


				class CouldNotCreate: public DirectoryException
				{
					public:
					
						explicit CouldNotCreate( const std::string & name ):
							DirectoryException( 
								"Could not create directory " + name )
						{
						
						}
						
				} ;


				class CouldNotRemove: public DirectoryException
				{
					public:
					
						explicit CouldNotRemove( const std::string & name ):
							DirectoryException( 
								"Could not remove directory " + name )
						{
						
						}
						
				} ;


				class ChangeDirectoryFailed: public DirectoryException
				{
					public:
					
						explicit ChangeDirectoryFailed( 
								const std::string & name ):
							DirectoryException( 
								"Could not change to directory " + name )
						{
						
						}
						
				} ;


				class InvalidPathException: public DirectoryException
				{
					public:
					
						InvalidPathException( const std::string & message ):
							DirectoryException( message )
						{
						
						}
						
				} ;


				/// Constructs a reference on the current working directory.
				Directory() throw() ;


				/**
				 * Constructs a reference to the directory <b>name</b>.
				 *
				 * @note If the directory does not exist and <b>create</b>
				 * is set, tries to create a new directory.
				 *
				 */
				explicit Directory( const std::string & name, 
						bool createDirectory = true )
					throw( DirectoryException ) ;


				/**
				 * Destroys the directory reference, not the directory itself.
				 *
				 * @see remove
				 *
				 */
				virtual ~Directory() throw() ;


				/// Tells whether the directory has an entry named <b>name</b>.
				virtual bool hasEntry( const std::string & name ) const
					throw() ;


				/**
				 * Returns the list of all direct subdirectories of this
				 * directory, in the specified list.
				 *
				 * @param entries the caller-provided list in which
				 * subdirectories will be put.
				 *
				 * @throw DirectoryException if an error occured.
				 *
				 */
				virtual void getSubdirectories( 
						std::list<std::string> & subDirectories )
					const throw( DirectoryException ) ;


				/**
				 * Returns the list of all direct entries of any type of 
				 * this directory, in the specified list.
				 *
				 * @param entries the caller-provided list in which 
				 * entries will be put.
				 *
				 * @throw DirectoryException if the operation failed or
				 * is not supported.
				 *
				 */
				virtual void getEntries( std::list<std::string> & entries )
					const throw( DirectoryException ) ;


				/**
				 * Changes directory to one of its direct subdirectories,
				 * which must be specified relatively (no absolute path).
				 *
				 */
				virtual void goDown( const std::string & path )
					throw( ChangeDirectoryFailed ) ;


				/**
				 * Tells whether the reference on the directory is valid.
				 * Checks that corresponding path exists,
				 * and that it is a directory.
				 *
				 * @see Its static counterpart, <code>exists</code>.
				 *
				 */

				virtual bool isValid() const throw() ;


				/// Returns the path of the directory which is referred to.
				inline const std::string & getPath() const throw() ;


				/**
				 * Removes the leading separator, if any, in this
				 * directory's path.
				 *
				 */
				virtual void removeLeadingSeparator() throw() ;


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



				// Static methods start here.


				/**
				 * Returns whether specified string is a valid directory name.
				 *
				 * @note If no regular expression support is available, 
				 * then the name will be deemed always correct.
				 *
				 */
				static bool IsAValidDirectoryName( 
					const std::string & directoryString ) throw() ;


				/**
				 * Removes the leading separator, if any, in specified
				 * directory's path.
				 *
				 */
				static void RemoveLeadingSeparator( std::string & path ) 
					throw() ;


				/// Tells whether specified path is an absolute path.
				static bool IsAbsolutePath( const std::string & path ) throw() ;


				/// Returns the current working directory name.
				static std::string GetCurrentWorkingDirectoryName()	
					throw( DirectoryException ) ;


				/**
				 * Changes current working directory to
				 * <b>newWorkingDirectory</b>.
				 *
				 */
				static void ChangeWorkingDirectory( 
						const std::string & newWorkingDirectory )
					throw( DirectoryException ) ;


				/**
				 * Splits up <b>path</b> into the list of its sub-elements
				 * (set of directory names).
				 *
				 * @example SplitPath( '/mnt/raid/md0/LOANI-0.3' ) returns 
				 * on UNIX :
				 * [ '', 'mnt', 'raid', 'md0', 'LOANI-0.3' ].
				 *
				 * @see JoinPath
				 *
				 */
				static std::list<std::string> SplitPath( 
					const std::string & path ) throw() ;


				/**
				 * Joins the specified path elements with the relevant 
				 * directory separator.
				 *
				 * @example JoinPath([ '', 'mnt', 'raid', 'md0', 'LOANI-0.3' ])
				 * returns on UNIX : '/mnt/raid/md0/LOANI-0.3'.
				 *
				 * @see SplitPath
				 *
				 */
				static std::string JoinPath( 
					const std::list<std::string> & pathElements ) throw() ;


				/**
				 * Joins the two specified path elements with the relevant
				 * directory separator.
				 *
				 * @param firstPath the first part of the final path.
				 *
				 * @param secondPath the second part of the final path.
				 *
				 * @example JoinPath( '/mnt/raid', 'md0/LOANI-0.3' ) 
				 * returns on UNIX : '/mnt/raid/md0/LOANI-0.3'.
				 *
				 * @see SplitPath
				 *
				 */
				static std::string JoinPath( const std::string & firstPath,
					const std::string & secondPath ) throw() ;


				/**
				 * Separates the full pathname <b>path</b> into a basename
				 * <b>base</b> and file name <b>file</b>.
				 *
				 * For example, this method applied to 
				 * '/mnt/raid/md0/LOANI-0.3' returns respectively
				 * '/mnt/raid/md0' and 'LOANI-0.3', when the separator is '/'.
				 *
				 * @param path the path which is to be stripped.
				 *
				 * @param base if non null, must be a pointer to an 
				 * already allocated string where the basename will be stored.
				 * If not interested in the basename, specify a null (0)
				 * pointer instead : this method will act as UNIX 'basename'.
				 *
				 * @param file if non null, must be a pointer to an already
				 * allocated string where the filename will be stored. 
				 * If not interested in the filename, specify a null (0) 
				 * pointer instead : this method will act as UNIX 'dirname'.
				 *
				 */
				static void StripFilename( const std::string & path,
					std::string * base, std::string * file = 0 ) throw() ;


				/**
				 * Tells whether the directory <b>path</b> exists and is 
				 * a directory indeed.
				 *
				 * @see Directory member counterpart, isValid
				 *
				 * @note On Windows, files and directories are case-insensitive, and
				 * 'c:' is not a directory (it is seen as a drive), whereas 'c:\' is a
				 * directory.
				 *
				 * @throw DirectoryException if the operation failed or
				 * is not supported.
				 *
				 */
				static bool Exists( const std::string & path ) 
						throw( DirectoryException ) ;


				/// Creates a new directory on disk.
				static void Create( const std::string & name ) 
					throw( CouldNotCreate ) ;


				/// Removes the directory from disk.
				static void Remove( const std::string & name, 
					bool recursive = false ) throw( CouldNotRemove ) ;


				/// Returns the change time time of the entry <b>name</b>.
				static time_t GetEntryChangeTime( const std::string & name )
					throw( DirectoryException ) ;


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


				/**
				 * Introduced for readibility only in constructors.
				 *
				 * @example :
				 * <pre>
				 * Directory root( "/", Directory::DoNotCreate ) ;
				 * </pre>
				 */
				enum CreateFlags
				{
					DoNotCreate = 0
				} ;




			private:


				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				Directory( const Directory & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 *
				 */
				Directory & operator = ( const Directory & source ) throw() ;


				/// The path a directory instance refers to.
				std::string _path ;

		} ;


		const std::string & Directory::getPath() const throw()
		{
			return _path ;
		}

	}

}


#endif // CEYLAN_DIRECTORY_H_
