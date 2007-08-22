#ifndef CEYLAN_FILE_SYSTEM_MANAGER_H_
#define CEYLAN_FILE_SYSTEM_MANAGER_H_



#include "CeylanSystem.h"      // for SystemException
#include "CeylanStringUtils.h" // for Latin1Char


#include <string>



namespace Ceylan
{



	namespace System
	{



		/**
		 * Mother class of all file-related exceptions, including filesystem,
		 * files and directories.
		 *
		 */
		class CEYLAN_DLL FileManagementException: public SystemException
		{
				
			public:

				explicit FileManagementException( 
						const std::string & reason ):
					SystemException( reason )
				{
						
				}
						
						
				virtual ~FileManagementException() throw()
				{
						
				}

		} ;
	



		/**
		 * Interface (abstract class) that allows to manage a specific
		 * filesystem.
		 *
		 * Thus files and directories may be manipulated from various 
		 * filesystems simultaneously.
		 *
		 */
		class CEYLAN_DLL FileSystemManager: public TextDisplayable
		{

			public:


				/// Mother class of all filesystem-related exceptions.
				class CEYLAN_DLL FileSystemManagerException: 
					public FileManagementException
				{
				
					public:

						explicit FileSystemManagerException( 
								const std::string & reason ):
							FileManagementException( reason )
						{
						
						}
						
						
						virtual ~FileSystemManagerException() throw()
						{
						
						}

				} ;



				/// If directory operations at the filesystem level failed.
				class CEYLAN_DLL DirectoryOperationFailed: 
					public FileSystemManagerException
				{ 
				
					public: 
					
						explicit DirectoryOperationFailed( 
								const std::string & reason ) throw() ; 
								
				} ;



				class CEYLAN_DLL TouchFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit TouchFailed( 
								const std::string & reason ) throw() ; 
								
				} ;



				class CEYLAN_DLL RemoveFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit RemoveFailed( 
								const std::string & reason ) throw() ; 
				} ;



				class CEYLAN_DLL SymlinkFailed: 
					public FileSystemManagerException
				{ 
				
					public: 
					
						explicit SymlinkFailed( 
								const std::string & reason ) throw() ; 
				} ;



				class CEYLAN_DLL MoveFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit MoveFailed( 
							const std::string & reason ) throw() ;
							 
				} ;



				class CEYLAN_DLL CopyFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit CopyFailed( 
							const std::string & reason ) throw() ; 
							
				} ;



				class CEYLAN_DLL GetChangeTimeFailed: 
					public FileSystemManagerException
				{ 
				
					public: 
					
						explicit GetChangeTimeFailed( 
							const std::string & reason ) throw() ;
							
				} ;



				class CEYLAN_DLL CouldNotDuplicate: 
					public FileSystemManagerException
				{ 
				
					public: 
					
						explicit CouldNotDuplicate( 
							const std::string & reason ) throw() ;
							
				} ;


				class CEYLAN_DLL CouldNotStatEntry: 
					public FileSystemManagerException
				{ 
					public: 
						
						explicit CouldNotStatEntry( 
							const std::string & reason ) throw() ;
							
				} ;


				class CEYLAN_DLL DiffFailed: public FileSystemManagerException
				{ 
					public: 
						
						explicit DiffFailed( 
							const std::string & reason ) throw() ;
							
				} ;
				
				
				
				
				// Existence test section.
				

				/**
				 * Tells whether the regular file or symbolic link 
				 * <b>filename</b> exists (and is not a directory).
				 *
				 * @param filename the filename to look-up.
				 *
				 * This method will work as expected whether the 
				 * symbolic link feature is enabled or not.
				 *
				 * @throw CouldNotStatEntry if the operation failed (existence
				 * test failed with no answer) or is not supported on this 
				 * platform.
				 *
				 */
				virtual bool existsAsFileOrSymbolicLink( 
						const std::string & filename ) const 
					throw( CouldNotStatEntry ) = 0 ;

							

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
				 * @throw CouldNotStatEntry if the operation failed (existence
				 * test failed with no answer) or is not supported on this 
				 * platform.
				 *
				 */
				virtual bool existsAsDirectory( 
						const std::string & directoryPath ) const
					throw( CouldNotStatEntry ) = 0 ;
			
					

				/**
				 * Tells whether the filesystem entry <b>entryPath</b> exists,
				 * be it a file, a symbolic link, a directory, a character
				 * or block device, a FIFO, a socket, etc.
				 *
				 * @param entryPath the path of the entry to look-up.
				 *
				 * @throw CouldNotStatEntry if the operation failed (existence
				 * test failed with no answer) or is not supported on this 
				 * platform.
				 *
				 */
				virtual bool existsAsEntry( const std::string & entryPath ) 
					const throw( CouldNotStatEntry ) = 0 ;




				// File-related section.
				
				
				/**
				 * Creates a symbolic link on disk.
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
					const std::string & linkName ) throw( SymlinkFailed ) = 0 ;
					

				/**
				 * Removes the file or symbolic link from the disk.
				 *
				 * @param filename the filename to remove.
				 *
				 * @throw RemoveFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void remove( const std::string & filename ) 
					throw( RemoveFailed ) = 0 ;


				/**
				 * Moves the file on disk.
				 *
				 * A special case of file moving is file renaming.
				 *
				 * @param sourceFilename the filename of the file to be moved.
				 *
				 * @param targetFilename the target filename of the moved file.
				 *
				 * @throw MoveFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void move( const std::string & sourceFilename,
						const std::string & targetFilename ) 
					throw( MoveFailed ) = 0 ;


				/**
				 * Copies the file on disk.
				 *
				 * @param sourceFilename the filename of the file to be copied.
				 *
				 * @param targetFilename the new filename of the copied file.
				 *
				 * @throw CopyFailed if the operation failed or is not
				 * supported on this platform.
				 *
				 */
				virtual void copy( const std::string & sourceFilename,
						const std::string & targetFilename ) 
					throw( CopyFailed ) = 0 ;



				/**
				 * Returns the size, in bytes, of the specified file.
				 *
				 * @param filename the filename whose size is searched.
				 *
				 * @throw CouldNotStatEntry if the operation failed (ex: file
				 * not found) or is not supported on this platform.
				 *
				 */
				virtual Size getSize( const std::string & filename ) 
					throw( CouldNotStatEntry ) = 0 ;



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
				 * supported on this platform.
				 *
				 */
				virtual void touch( const std::string & filename ) 
					throw( TouchFailed ) = 0 ;



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
				 * supported on this platform.
				 *
				 */
				virtual bool diff( const std::string & firstFilename,
						const std::string & secondFilename ) 
					throw( DiffFailed ) ;



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
			
			
			
			
			
				// Directory-related section.
			
			
			
				/**
				 * Returns whether specified string is a valid directory name.
				 *
				 * @param directoryString the directory string.
				 *
				 * @note If no regular expression support is available, 
				 * then the name will be deemed always correct.
				 *
				 */
				virtual bool isAValidDirectoryName( 
					const std::string & directoryString ) throw() = 0 ;
			
			
				/**
				 * Removes the leading separator, if any, in specified
				 * directory's path.
				 *
				 * @param path the path that will be modified.
				 *
				 */
				virtual void removeLeadingSeparator( std::string & path ) 
					throw() ;
			
			
				/**
				 * Tells whether specified path is an absolute path.
				 *
				 * @param path the path that may be absolute.
				 *
				 */
				virtual bool isAbsolutePath( const std::string & path ) 
					throw() = 0 ;
			
			
				/**
				 * Returns the current working directory name.
				 *
				 * @throw DirectoryOperationFailed if the operation failed 
				 * or is not supported on the target platform.
				 *
				 */
				virtual std::string getCurrentWorkingDirectoryName()	
					throw( DirectoryOperationFailed ) = 0 ;


				/**
				 * Changes current working directory to
				 * <b>newWorkingDirectory</b>.
				 *
				 * @param newWorkingDirectory the target working directory.
				 *
				 * @throw DirectoryOperationFailed if the operation failed 
				 * or is not supported on the target platform.
				 *
				 */
				virtual void changeWorkingDirectory( 
						const std::string & newWorkingDirectory )
					throw( DirectoryOperationFailed ) = 0 ;


				/**
				 * Splits up <b>path</b> into the list of its sub-elements
				 * (set of directory names).
				 *
				 * @param path the path to split.
				 *
				 * @example splitPath( '/mnt/raid/md0/LOANI-0.3' ) returns 
				 * on UNIX:
				 * [ '', 'mnt', 'raid', 'md0', 'LOANI-0.3' ].
				 *
				 * @see joinPath
				 *
				 */
				virtual std::list<std::string> splitPath( 
					const std::string & path ) throw() ;


				/**
				 * Joins the specified path elements with the relevant 
				 * directory separator.
				 *
				 * @param pathElements the path elements to join in a path.
				 *
				 * @example joinPath([ '', 'mnt', 'raid', 'md0', 'LOANI-0.3' ])
				 * returns on UNIX: '/mnt/raid/md0/LOANI-0.3'.
				 *
				 * @see splitPath
				 *
				 */
				virtual std::string joinPath( 
					const std::list<std::string> & pathElements ) throw() ;


				/**
				 * Joins the two specified path elements with the relevant
				 * directory separator.
				 *
				 * @param firstPath the first part of the final path.
				 *
				 * @param secondPath the second part of the final path.
				 *
				 * @example joinPath( '/mnt/raid', 'md0/LOANI-0.3' ) 
				 * returns on UNIX: '/mnt/raid/md0/LOANI-0.3'.
				 *
				 * @see splitPath
				 *
				 */
				virtual std::string joinPath( const std::string & firstPath,
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
				 * pointer instead: this method will act as UNIX 'basename'.
				 *
				 * @param file if non null, must be a pointer to an already
				 * allocated string where the filename will be stored. 
				 * If not interested in the filename, specify a null (0) 
				 * pointer instead: this method will act as UNIX 'dirname'.
				 *
				 */
				virtual void stripFilename( const std::string & path,
					std::string * base, std::string * file = 0 ) throw() ;




				// Filesystem constants.
				
				
				/**
				 * Returns the root directory prefix.
				 *
				 * @example "" on Unix, "c:" on Windows.
				 *
				 */
				virtual const std::string & getRootDirectoryPrefix()
					const throw() = 0 ;
	
	
				/**
				 * Returns the directory separator, a Latin-1 character.
				 *
				 * @example Slash or backslash, i.e. '/' or '\'.
				 *
				 */
				virtual Ceylan::Latin1Char getSeparator() const throw() = 0 ;



				/**
				 * Returns the alias for the working directory.
				 *
				 * @example Typically it is ".", it is the value returned by
				 * this default implementation.
				 *
				 */
				virtual const std::string & getAliasForCurrentDirectory()
					const throw() ;
	

				/**
				 * Returns the alias for the upper (parent) directory.
				 *
				 * @example Typically it is "..", it is the value returned by
				 * this default implementation.
				 *
				 */
				virtual const std::string & getAliasForParentDirectory()
					const throw() ;
	

	
	
				/**
				 * Returns the directory separator, in the form of a string.
				 *
				 * More convenient for some operations than a character.
				 *
				 * @example Slash or backslash, i.e. "/" or "\".
				 *
				 */
				virtual std::string getSeparatorAsString() const 
					throw() ;
	
			
			
			
			
				// Static section.
			
			
				/**
				 * Returns a reference to the unique default filesystem 
				 * manager.
				 *
				 * On computers (be they UNIX or Windows), the default manager
				 * is the standard one (StandardFileSystemManager).
				 *
				 * On the Nintendo DS, it is the libfat one
				 * (LibfatFileSystemManager).
				 *
				 * Creates it if needed, ensures it remains a singleton.
				 *
				 * @throw FileSystemManagerException if the operation failed.
				 *
				 */
				static FileSystemManager & GetDefaultFileSystemManager() 
					throw( FileSystemManagerException ) ;


				/**
				 * Removes the current default filesystem manager, if any.
				 *
				 */
				static void RemoveDefaultFileSystemManager() throw() ;
				
				
			
			
			protected:


				/**
				 * Constructs a reference to a filesystem, initializes 
				 * accordingly any needed subsystem.
				 *
				 * Cannot be private as has to be subclassed.
				 *
				 * @throw FileSystemManagerException if the operation failed.
				 *
				 */
				FileSystemManager() throw( FileSystemManagerException ) ;


				/**
				 * Destroys the Ceylan filesystem reference, not the 
				 * filesystem itself.
				 *
				 * Cannot be private as has to be subclassed.
				 *
				 */
				virtual ~FileSystemManager() throw() ;




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
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				FileSystemManager( const FileSystemManager & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				FileSystemManager & operator = ( 
					const FileSystemManager & source ) throw() ;
				
					
					
				/**
				 * Pointer to the default filesystem manager (if any).
				 *
				 * This default manager is the one that is deemed the most
				 * natural for the target running platform: standard one for
				 * computers (either UNIX or Windows), libfat-based one for
				 * the Nintendo DS.
				 *
				 */
				static FileSystemManager * _DefaultFileSystemManager ;
				

		} ;

	}

}


#endif // CEYLAN_FILE_SYSTEM_MANAGER_H_
