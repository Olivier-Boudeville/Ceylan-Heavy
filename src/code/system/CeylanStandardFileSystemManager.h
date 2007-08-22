#ifndef CEYLAN_STANDARD_FILE_SYSTEM_MANAGER_H_
#define CEYLAN_STANDARD_FILE_SYSTEM_MANAGER_H_



#include "CeylanFileSystemManager.h"  // for inheritance


#include <string>



namespace Ceylan
{



	namespace System
	{



		/**
		 * Allows to manage a specific filesystem.
		 *
		 * Thus files and directories may be manipulated from various 
		 * filesystems simultaneously.
		 *
		 */
		class CEYLAN_DLL StandardFileSystemManager: public FileSystemManager
		{

			public:


				/// Mother class of all standard filesystem-related exceptions.
				class StandardFileSystemManagerException: 
					public FileSystemManagerException
				{
				
					public:

						explicit StandardFileSystemManagerException( 
								const std::string & reason ):
							FileSystemManagerException( reason )
						{
						
						}


						virtual ~StandardFileSystemManagerException() throw()
						{
						
						}
						
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
					throw( CouldNotStatEntry ) ;

							

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
					throw( CouldNotStatEntry ) ;
			
					

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
					const throw( CouldNotStatEntry ) ;



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
					const std::string & linkName ) throw( SymlinkFailed ) ;
					

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
					throw( RemoveFailed ) ;


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
					throw( MoveFailed ) ;


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
					throw( CopyFailed ) ;



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
					throw( CouldNotStatEntry ) ;



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
					throw( TouchFailed ) ;

					

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
					const std::string & directoryString ) throw() ;
			
			
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
					throw() ;
			
			
				/**
				 * Returns the current working directory name.
				 *
				 * @throw DirectoryOperationFailed if the operation failed 
				 * or is not supported on the target platform.
				 *
				 */
				virtual std::string getCurrentWorkingDirectoryName()	
					throw( DirectoryOperationFailed ) ;


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
					throw( DirectoryOperationFailed ) ;




				// Filesystem constants.
				
				
				/**
				 * Returns the root directory prefix.
				 *
				 * @example "" on Unix, "c:" on Windows.
				 *
				 */
				virtual const std::string & getRootDirectoryPrefix()
					const throw() ;
		

				/**
				 * Returns the directory separator, a Latin-1 character.
				 *
				 * @example Slash or backslash, i.e. '/' or '\'.
				 *
				 */
				virtual Ceylan::Latin1Char getSeparator() const throw() ;
	
	


				// Static section.
			

				/**
				 * Returns a reference to the unique standard filesystem 
				 * manager.
				 *
				 * Creates it if needed: this method ensures it remains a
				 * singleton.
				 *
				 * Must be public, as ancester has to be able to call it.
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
				 * Must be public, as ancester has to be able to call it.
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




			protected:



				/**
				 * Duplicates the file descriptor.
				 *
				 * @param fd the file descriptor to duplicate.
				 *
				 * @throw CouldNotDuplicate if the operation failed or
				 * if the file descriptor feature is not available.
				 *
				 */
				static FileDescriptor Duplicate( FileDescriptor fd ) 
					throw( CouldNotDuplicate ) ;



				/**
				 * Takes specified <b>rawFilename</b> and tries to transform it 
				 * so that the result should be a valid name, from the
				 * standard filesystem's point of view.
				 *
				 * @param rawFilename the filename to convert
				 *
				 * @return the converted filename
				 *
				 */
				static const std::string TransformIntoValidFilename( 
					const std::string & rawFilename ) throw() ;



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
				
					
					
				/// Pointer to the standard filesystem manager (if any).
				static StandardFileSystemManager * _StandardFileSystemManager ;
				

		} ;

	}

}


#endif // CEYLAN_STANDARD_FILE_SYSTEM_MANAGER_H_
