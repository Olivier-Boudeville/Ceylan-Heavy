#ifndef CEYLAN_FILE_SYSTEM_COMMON_H_
#define CEYLAN_FILE_SYSTEM_COMMON_H_



#include "CeylanSystem.h"      // for SystemException


#include <string>



namespace Ceylan
{



	namespace System
	{

		
		
		/*
		 * Exception declaration section.
		 *
		 * Exceptions could be organized by origin (ex: File, Directory, etc.)
		 * or by theme (ex: creation, opening or removing failed).
		 *
		 * We preferred the theme-based organization, so that there are less
		 * exception conversions to perform when, for example, manipulating
		 * a filesystem from a File method.
		 *
		 * Here are centralized all exceptions that are either at the root
		 * of file management or possibly thrown by FileSystemManager.
		 *
		 * One goal is to avoid having CeylanFile.h and CeylanDirectory.h 
		 * depending on CeylanFileSystemManager.h, because the other way round
		 * is already true (ex: the factories defined in FileSystemManager 
		 * have to know the file open flags).
		 *
		 * Hence CeylanFile.h and CeylanDirectory.h (and
		 * CeylanFileSystemManager.h) depends on this file.
		 *
		 * Some exceptions have to start by the name of their corresponding
		 * class (ex: FileLookupFailed), as LookupFailed would have to be
		 * common to File and Directory, and File::LookupFailed cannot be used
		 * as would be declared in File class but thrown by
		 * CeylanFileSystemManager, thus recreating a dependency cycle.
		 *
		 * As a consequence, we chose to start the names of all file exceptions
		 * with 'File' and to declare them outside the File class, so that they
		 * are all handled the same way.
		 *
		 * The same process has been applied to Directory exceptions.
		 *
		 */
		 
				

		/**
		 * Mother class of all file-related exceptions, including filesystems,
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



		/*
		 * Three specific child classes for FileManagementException: 
		 *   - FileSystemException (defined in FileSystemManager)
		 *   - FileException (defined here to be shared with FileSystemManager)
		 *   - DirectoryException (same thing)
		 *
		 */
		
		
		/// Thrown when filesystem operations failed.
		class CEYLAN_DLL FileSystemException: public FileManagementException
		{ 
		
			public: 
			
				explicit FileSystemException( 
						const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}								
						
		} ;



		/**
		 * Thrown when file operations failed.
		 *
		 * @note Must be here and not in CeylanFile.h, as FileSystemManager 
		 * will throw, in file-specific methods, instances of FileException
		 * child classes. 
		 * This allows to spare some catch/throw pairs (conversions not really
		 * useful).
		 *
		 */
		class CEYLAN_DLL FileException: public FileManagementException
		{ 
		
			public: 
			
				explicit FileException( const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}								
						
		} ;
		



		// FileException child classes.
		
		
		
		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileCreationFailed: public FileException
		{
		
			public:
				
				explicit FileCreationFailed( 
						const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;
		
		
		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileOpeningFailed: public FileException
		{
		
			public:
				
				explicit FileOpeningFailed( 
						const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;
		
		
		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileAlreadyOpened: public FileException
		{
		
			public:
				
				explicit FileAlreadyOpened( 
						const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;
		
		
		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileLookupFailed: public FileException
		{
		
			public:
				
				explicit FileLookupFailed( 
						const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;
		
		
		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileRemoveFailed: public FileException
		{
		
			public:
				
				explicit FileRemoveFailed( 
						const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;
		
		
		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileMoveFailed: public FileException
		{
		
			public:
				
				explicit FileMoveFailed( const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;
	
	
		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileCopyFailed: public FileException
		{
		
			public:
				
				explicit FileCopyFailed( const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;
	
	
		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileSizeRequestFailed: public FileException
		{
		
			public:
				
				explicit FileSizeRequestFailed( 
						const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;


		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileLastChangeTimeRequestFailed: public FileException
		{
		
			public:
				
				explicit FileLastChangeTimeRequestFailed( 
						const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;


		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileTouchFailed: public FileException
		{
		
			public:
				
				explicit FileTouchFailed( const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;


		/// Raised at first by FileSystemManager and file-specific.
		class CEYLAN_DLL FileDiffFailed: public FileException
		{
		
			public:
				
				explicit FileDiffFailed( const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					 
						
		} ;





		// Directory section.
		
		
		/**
		 * Thrown when directory operations failed.
		 *
		 * @note Must be here and not in CeylanDirectory.h, as FileSystemManager
		 * will throw, in file-specific methods, instances of FileException
		 * child classes. 
		 * This allows to spare some catch/throw pairs (conversions not really
		 * useful).
		 *
		 */
		class CEYLAN_DLL DirectoryException: public FileManagementException
		{ 
		
			public: 
			
				explicit DirectoryException( 
						const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}								
						
		} ;




		// DirectoryException child classes.

		
		/// Raised at first by FileSystemManager and directory-specific.
		class CEYLAN_DLL DirectoryCreationFailed: public DirectoryException
		{
		
			public:
				
				explicit DirectoryCreationFailed( 
						const std::string & reason ) throw():
					DirectoryException( reason )
				{
				
				}								
					 
						
		} ;
		
		
		/// Raised at first by FileSystemManager and directory-specific.
		class CEYLAN_DLL DirectoryOpeningFailed: public DirectoryException
		{
		
			public:
				
				explicit DirectoryOpeningFailed( 
						const std::string & reason ) throw():
					DirectoryException( reason )
				{
				
				}								
					 
						
		} ;
		
		
		/// Raised at first by FileSystemManager and directory-specific.
		class CEYLAN_DLL DirectoryLookupFailed: public DirectoryException
		{
		
			public:
				
				explicit DirectoryLookupFailed( 
						const std::string & reason ) throw():
					DirectoryException( reason )
				{
				
				}								
					 
						
		} ;
		
		
		/// Raised at first by FileSystemManager and directory-specific.
		class CEYLAN_DLL DirectoryRemoveFailed: public DirectoryException
		{
		
			public:
				
				explicit DirectoryRemoveFailed( 
						const std::string & reason ) throw():
					DirectoryException( reason )
				{
				
				}								
					 
						
		} ;
		

		
		/// Raised at first by FileSystemManager and directory-specific.
		class CEYLAN_DLL DirectoryMoveFailed: public DirectoryException
		{
		
			public:
				
				explicit DirectoryMoveFailed( const std::string & reason )
						throw():
					DirectoryException( reason )
				{
				
				}								
					 
						
		} ;
	
	
		/// Raised at first by FileSystemManager and directory-specific.
		class CEYLAN_DLL DirectoryCopyFailed: public DirectoryException
		{
		
			public:
				
				explicit DirectoryCopyFailed( const std::string & reason )
						throw():
					DirectoryException( reason )
				{
				
				}								
					 
						
		} ;
		

		/// Raised at first by FileSystemManager and directory-specific.
		class CEYLAN_DLL DirectoryLastChangeTimeRequestFailed: 
			public DirectoryException
		{
		
			public:
				
				explicit DirectoryLastChangeTimeRequestFailed( 
						const std::string & reason ) throw():
					DirectoryException( reason )
				{
				
				}								
					 
						
		} ;

		
		/// Raised at first by FileSystemManager and directory-specific.
		class CEYLAN_DLL DirectoryGetCurrentFailed: public DirectoryException
		{
		
			public:
				
				explicit DirectoryGetCurrentFailed( 
						const std::string & reason ) throw():
					DirectoryException( reason )
				{
				
				}								
					 
						
		} ;
		
		
		/// Raised at first by FileSystemManager and directory-specific.
		class CEYLAN_DLL DirectoryChangeFailed: public DirectoryException
		{
		
			public:
				
				explicit DirectoryChangeFailed( 
						const std::string & reason ) throw():
					DirectoryException( reason )
				{
				
				}								
					 
						
		} ;



	}	

}



#endif // CEYLAN_FILE_SYSTEM_COMMON_H_

