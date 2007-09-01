#ifndef CEYLAN_DIRECTORY_H_
#define CEYLAN_DIRECTORY_H_


#include "CeylanFileSystemCommon.h"    // for FileManagementException and al
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
		 * Abstract directory mother class, so that programs can always
		 * manipulate Ceylan::Directory instances, whereas per-platform
		 * specialized classes are actually used by the system.
		 *
		 * Examples of convenient cross-platform Directory methods whose use 
		 * is encouraged: Create, Open, Remove, ExistsAsDirectory.
		 *
		 * @see File
		 *
		 */
		class CEYLAN_DLL Directory: public TextDisplayable
		{


			public:


				/// Mother class of all directory-related exceptions.
				class CEYLAN_DLL DirectoryException: 
					public FileManagementException
				{
				
					public:

						explicit DirectoryException( 
								const std::string & reason ) throw() :
							FileManagementException( reason )
						{
						
						}
						
						
						virtual ~DirectoryException() throw()
						{
						
						}

				} ;



				class CEYLAN_DLL CreateFailed: public DirectoryException
				{
					public:
					
						explicit CreateFailed( const std::string & message )
								throw():
							DirectoryException( message )
						{
						
						}	
						
				} ;


				class CEYLAN_DLL OpenFailed: public DirectoryException
				{
					public:
					
						explicit OpenFailed( const std::string & message )
								throw():
							DirectoryException( message )
						{
						
						}	
						
				} ;


				class CEYLAN_DLL RemoveFailed: public DirectoryException
				{
					public:
					
						explicit RemoveFailed( const std::string & message )
								throw():
							DirectoryException( message )
						{
						
						}	
						
				} ;


				class CEYLAN_DLL DirectoryLookupFailed: public DirectoryException
				{
					public:
					
						explicit DirectoryLookupFailed( 
								const std::string & message )throw():
							DirectoryException( message )
						{
						
						}	
						
				} ;


				class CEYLAN_DLL ChangeDirectoryFailed: 
					public DirectoryException
				{
					public:
					
						explicit ChangeDirectoryFailed( 
								const std::string & message ) throw():
							DirectoryException( message )
						{
						
						}	
						
						
				} ;


				class CEYLAN_DLL InvalidPathException: public DirectoryException
				{
					public:
					
						explicit InvalidPathException( 
								const std::string & message ) throw():
							DirectoryException( message )
						{
						
						}
						
				} ;



				// Static section for user-dedicated cross-platform primitives.
				
				
				
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
				 * @throw CreateFailed if the directory creation failed.
				 *
				 */
				static Directory & Create( 
						const std::string & newDirectoryName ) 
					throw( CreateFailed ) ;

				
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
				 * @throw OpenFailed if the directory opening failed.
				 *
				 */
				static Directory & Open( 
						const std::string & directoryName = "" ) 
					throw( OpenFailed ) ;
					

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
				 * @throw RemoveFailed if the operation failed or is not
				 * supported.
				 *
				 */
				static void Remove( const std::string & directoryPath, 
					bool recursive = false ) throw( RemoveFailed ) ;


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
					throw( DirectoryLookupFailed ) = 0 ;


				/**
				 * Returns the names of all direct subdirectories of this
				 * directory, in the specified list.
				 *
				 * @param subDirectories the caller-provided list in which
				 * subdirectories will be put.
				 *
				 * @throw DirectoryLookupFailed if an error occured.
				 *
				 */
				virtual void getSubdirectories( 
						std::list<std::string> & subDirectories )
					const throw( DirectoryLookupFailed ) = 0 ;


				/**
				 * Returns the names of all files of this directory, in the
				 * specified list.
				 *
				 * @param files the caller-provided list in which
				 * subdirectories will be put.
				 *
				 * @throw DirectoryLookupFailed if an error occured.
				 *
				 */
				virtual void getFiles( std::list<std::string> & files )
					const throw( DirectoryLookupFailed ) = 0 ;


				/**
				 * Returns the names of all direct entries of any type of 
				 * this directory (including files and directories), in the
				 * specified list.
				 *
				 * @param entries the caller-provided list in which 
				 * entries will be put.
				 *
				 * @throw DirectoryLookupFailed if the operation failed or
				 * is not supported.
				 *
				 */
				virtual void getEntries( std::list<std::string> & entries )
					const throw( DirectoryLookupFailed ) = 0 ;


				/**
				 * Returns the names of all direct entries of any type of 
				 * this directory (including files and directories), in the
				 * corresponding specified list.
				 *
				 * @param subDirectories the caller-provided list in which 
				 * subDirectories of this directory will be put.
				 *
				 * @param files the caller-provided list in which 
				 * files of this directory will be put.
				 *
				 * @param otherEntries the caller-provided list in which 
				 * other entries (named FIFO, sockets, etc.) of this
				 * directory will be put.
				 *
				 * @throw DirectoryLookupFailed if the operation failed or
				 * is not supported.
				 *
				 */
				virtual void getSortedEntries( 
						std::list<std::string> & subDirectories,
						std::list<std::string> & files,
						std::list<std::string> & otherEntries )
					const throw( DirectoryLookupFailed ) = 0 ;




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
				 * @throw ChangeDirectoryFailed if the operation failed or is
				 * not supported.
				 *
				 */
				virtual void goDown( const std::string & subdirectoryName )
					throw( ChangeDirectoryFailed ) ;


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
				virtual bool isValid() const throw( DirectoryException ) ;


				/**
				 * Returns the path of the directory which is referred to.
				 *
				 * @return the path, with no leading separator.
				 *
				 */
				virtual const std::string & getPath() const throw() ;


				/**
				 * Removes the leading separator, if any, in the path of this
				 * directory path.
				 *
				 * @throw DirectoryException if the operation failed or is not
				 * supported.
				 *
				 */
				virtual void removeLeadingSeparator()
					throw( DirectoryException ) ;


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
				 * @throw DirectoryLookupFailed if the operation failed
				 * (existence test failed with no answer) or is not supported 
				 * on this platform. This includes throwing a more specific
				 * DirectoryDelegatingException (a child class of
				 * DirectoryException), should the underlying filesystem 
				 * manager not be retrieved as expected. 
				 *
				 */
				static bool Exists(	const std::string & directoryPath ) 
					throw( DirectoryException ) ;


				/**
				 * Returns whether specified string is a valid directory name.
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
				static bool IsAValidDirectoryName( 
						const std::string & directoryString ) 
					throw( DirectoryException ) ;


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
				static void RemoveLeadingSeparator( std::string & path ) 
					throw( DirectoryException ) ;

			
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
				static bool IsAbsolutePath( const std::string & path ) 
					throw( DirectoryException ) ;


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
				static std::string GetCurrentWorkingDirectoryName()	
					throw( DirectoryException ) ;


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
						const std::string & newWorkingDirectory )
					throw( DirectoryException ) ;



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
					const std::string & path ) throw( DirectoryException ) ;


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
						const std::list<std::string> & pathElements ) 
					throw( DirectoryException ) ;
					
					

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
						const std::string & secondPaththrow) 
					throw( DirectoryException ) ;
					
					
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
						std::string * base, std::string * file = 0 ) 
					throw( DirectoryException )	;
					
					
										
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
				static Ceylan::Latin1Char GetSeparator() 
					throw( DirectoryException ) ;


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
				static std::string GetSeparatorAsString() 
					throw( DirectoryException ) ;
				
				
				

			protected:



				/**
				 * Constructs a reference to the directory <b>directoryName</b>.
				 *
				 */
				explicit Directory( const std::string & directoryName )
					throw( DirectoryException ) ;



				/**
				 * Returns the filesystem manager that corresponds to the 
				 * actual Directory child class.
				 *
				 * @throw DirectoryDelegatingException if the operation failed.
				 *
				 */
				virtual FileSystemManager & getCorrespondingFileSystemManager()
					const throw( DirectoryDelegatingException ) = 0 ;



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


				/**
				 * The path a directory instance refers to.
				 *
				 * @note All stored path have any leading separator removed.
				 *
				 */
				std::string _path ;
				

		} ;

	}

}


#endif // CEYLAN_DIRECTORY_H_
