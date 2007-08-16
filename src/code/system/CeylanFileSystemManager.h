#ifndef CEYLAN_FILE_SYSTEM_MANAGER_H_
#define CEYLAN_FILE_SYSTEM_MANAGER_H_



#include "CeylanSystem.h"  // for SystemException


#include <string>



namespace Ceylan
{



	namespace System
	{



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
				class FileSystemManagerException: public SystemException
				{
				
					public:

						explicit FileSystemManagerException( 
								const std::string & reason ):
							SystemException( reason )
						{
						
						}
						
						
						virtual ~FileSystemManagerException() throw()
						{
						
						}

				} ;



				class TouchFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit TouchFailed( 
								const std::string & reason ) throw() ; 
								
				} ;



				class RemoveFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit RemoveFailed( 
								const std::string & reason ) throw() ; 
				} ;



				class SymlinkFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit SymlinkFailed( 
								const std::string & reason ) throw() ; 
				} ;



				class MoveFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit MoveFailed( 
							const std::string & reason ) throw() ;
							 
				} ;



				class CopyFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit CopyFailed( 
							const std::string & reason ) throw() ; 
							
				} ;



				class GetChangeTimeFailed: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit GetChangeTimeFailed( 
							const std::string & reason ) throw() ;
							
				} ;



				class CouldNotDuplicate: public FileSystemManagerException
				{ 
				
					public: 
					
						explicit CouldNotDuplicate( 
							const std::string & reason ) throw() ;
							
				} ;


				class CouldNotStatEntry: public FileSystemManagerException
				{ 
					public: 
						
						explicit CouldNotStatEntry( 
							const std::string & reason ) throw() ;
							
				} ;


				class DiffFailed: public FileSystemManagerException
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
				 * Tells whether the filesystem entry <b>entryPath</b> (be it
				 * a file, a symbolic link or a directory) exists.
				 *
				 * @param entryPath the path of the entry to look-up.
				 *
				 * @throw CouldNotStatEntry if the operation failed (existence
				 * test failed with no answer) or is not supported on this 
				 * platform.
				 *
				 */
				virtual bool existsAsEntry( const std::string & entryPath ) 
					throw( CouldNotStatEntry ) = 0 ;



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
					throw( DiffFailed ) = 0 ;



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
				 * accordingly any subsystem.
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
