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
		 *   - FileSystemException
		 *   - FileException
		 *   - DirectoryException
		 *
		 * They are defined in their corresponding header files.
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


		/// Thrown when file operations failed.
		class CEYLAN_DLL FileException: public FileManagementException
		{ 
		
			public: 
			
				explicit FileException( 
						const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}								
						
		} ;
		
		
		/**
		 * Thrown when file operations failed because of underlying
		 * filesystem manager : the corresponding backend could not be retrieved
		 * as expected.
		 *
		 */
		class CEYLAN_DLL FileDelegatingException: 
			public FileException
		{ 
		
			public: 
			
				explicit FileDelegatingException( 
						const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
						
		} ;



		/// Thrown when directory operations failed.
		class CEYLAN_DLL DirectoryException: public FileManagementException
		{ 
		
			public: 
			
				explicit DirectoryException( 
						const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}								
						
		} ;


		/**
		 * Thrown when directory operations failed because of underlying
		 * filesystem manager : the corresponding backend could not be retrieved
		 * as expected.
		 *
		 */
		class CEYLAN_DLL DirectoryDelegatingException: 
			public DirectoryException
		{ 
		
			public: 
			
				explicit DirectoryDelegatingException( 
						const std::string & reason ) throw():
					DirectoryException( reason )
				{
				
				}								
						
		} ;




		// Filesystem exceptions child classes.


		/// Thrown when filesystem manager operations failed.
		class CEYLAN_DLL FileSystemManagerException: public FileSystemException
		{ 
		
			public: 
			
				explicit FileSystemManagerException( 
						const std::string & reason ) throw():
					FileSystemException( reason )
				{
				
				}								
						
		} ;


		class CEYLAN_DLL GetChangeTimeFailed: public FileSystemException
		{ 
		
			public: 
			
				explicit GetChangeTimeFailed( const std::string & reason )
						throw():
					FileSystemException( reason )
				{
				
				}								
					
		} ;



		class CEYLAN_DLL DuplicateFailed: public FileSystemException
		{ 
		
			public: 
			
				explicit DuplicateFailed( const std::string & reason ) throw():
					FileSystemException( reason )
				{
				
				}								
					
		} ;


		class CEYLAN_DLL StatEntryFailed: public FileSystemException
		{ 
			public: 
				
				explicit StatEntryFailed( const std::string & reason ) throw():
					FileSystemException( reason )
				{
				
				}	
											
		} ;
					
					
		class CEYLAN_DLL TouchFailed: public FileSystemException
		{ 
		
			public: 
			
				explicit TouchFailed( 
						const std::string & reason ) throw():
					FileSystemException( reason )
				{
				
				}								 
						
		} ;


		class CEYLAN_DLL SymlinkFailed: public FileSystemException
		{ 
		
			public: 
			
				explicit SymlinkFailed( const std::string & reason ) throw():
					FileSystemException( reason )
				{
				
				}	
											
		} ;




		// Other direct child classes of FileManagementException, theme-based.
		
		
		class CEYLAN_DLL CreateFailed: public FileManagementException
		{ 
		
			public: 
			
				explicit CreateFailed( 
						const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}								
					 
		} ;
		

		class CEYLAN_DLL OpenFailed: public FileManagementException
		{ 
		
			public: 

				explicit OpenFailed( const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}								
					 
		} ;
		
		
		class CEYLAN_DLL RemoveFailed: public FileManagementException
		{ 
		
			public: 
			
				explicit RemoveFailed( const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}	
											 
		} ;


		class CEYLAN_DLL MoveFailed: public FileManagementException
		{ 
		
			public: 
			
				explicit MoveFailed( const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}								
					 
		} ;



		class CEYLAN_DLL CopyFailed: public FileManagementException
		{ 
		
			public: 
			
				explicit CopyFailed( const std::string & reason ) throw():
					FileManagementException( reason )
				{
				
				}								
					
		} ;


		
		
		// FileException child classes.
		
		
		class CEYLAN_DLL DiffFailed: public FileException
		{ 
		
			public: 
				
				explicit DiffFailed( const std::string & reason ) throw():
					FileException( reason )
				{
				
				}								
					
		} ;
		

	}	

}



#endif // CEYLAN_FILE_SYSTEM_COMMON_H_
