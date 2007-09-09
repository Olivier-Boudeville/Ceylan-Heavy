#include "CeylanStandardDirectory.h"


#include "CeylanFileSystemManager.h"         // for FileSystemManager
#include "CeylanStandardFileSystemManager.h" // for FileSystemManager

#include "CeylanRegularExpression.h" // for RegExp
#include "CeylanLogLight.h"          // for CEYLAN_LOG
#include "CeylanOperators.h"         // for toString
#include "CeylanStringUtils.h"       // for isLetter



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


// None of them is available in standard include :
extern "C"
{

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_DIRENT_H
#include <dirent.h>
#endif // CEYLAN_USES_DIRENT_H

#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for S_IRWXU
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SYS_STAT_H
#include <sys/stat.h>          // for S_ISDIR, stat, etc.
#endif // CEYLAN_USES_SYS_STAT_H

#ifdef CEYLAN_USES_DIRECT_H
#include <direct.h>            // for _mkdir
#endif // CEYLAN_USES_DIRECT_H

}


#ifndef PATH_MAX
#define PATH_MAX 1024
#endif // PATH_MAX


/*
 * Implementation notes.
 *
 * @note All internal paths kept in directory references (this object)
 * should be absolute paths.
 *
 */
 
using std::string ;
using std::list ;

using namespace Ceylan::System ;




/// Flags used to create new directories :

#if CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

const mode_t basicDirectory = 
	S_IRWXU | S_IRGRP | S_IXGRP	| S_IROTH | S_IXOTH ;
	
#else // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

#ifdef CEYLAN_USES_MKDIR_TWO_ARGS

// Currently not used on MinGW : 
const mode_t basicDirectory = S_IRWXU ;

#endif // CEYLAN_USES_MKDIR_TWO_ARGS

#endif // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES		


/*
 * ::stat is used often here.
 * Maybe check specifically CEYLAN_USES_SYS_STAT_H, short of having
 * 'CEYLAN_USES_STAT'.
 *
 */



StandardDirectory::~StandardDirectory() throw()
{

	// Nothing special here for abstract directories.

}




// StandardDirectory implementation section.


// Instance methods.
				

			
				
// Directory content subsection.



bool StandardDirectory::hasDirectory( const string & subdirectoryName ) const
	throw( DirectoryLookupFailed )
{


#ifdef CEYLAN_USES_STAT

	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"StandardDirectory::hasDirectory failed: " + e.toString() ) ;
			 
	}	


	if ( ( subdirectoryName == fsManager->getAliasForCurrentDirectory() )
			|| ( subdirectoryName == fsManager->getAliasForParentDirectory() ) )
		return true ;
	
	struct stat buf ;
	
	string tmp = fsManager->joinPath( _path, subdirectoryName ) ;
	
	return ( ::stat( tmp.c_str(), & buf ) == 0 ) && S_ISDIR( buf.st_mode ) ;


#else // CEYLAN_USES_STAT


#ifdef CEYLAN_USES__STAT


	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"StandardDirectory::hasDirectory failed: " + e.toString() ) ;
			 
	}	


	if ( ( subdirectoryName == fsManager->getAliasForCurrentDirectory() )
			|| ( subdirectoryName == fsManager->getAliasForParentDirectory() ) )
		return true ;
	
	struct _stat buf ;
	
	string tmp = fsManager->joinPath( _path, subdirectoryName ) ;
	
	return ( ::_stat( tmp.c_str(), & buf ) == 0 ) && S_ISDIR( buf.st_mode ) ;
	
#else // CEYLAN_USES__STAT

	throw DirectoryException( "StandardDirectory::hasDirectory : "
		"not implemented on this platform." ) ;
		
#endif  // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}



bool StandardDirectory::hasFile( const string & fileName ) const
	throw( DirectoryLookupFailed )
{


#ifdef CEYLAN_USES_STAT

	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"StandardDirectory::hasFile failed: " + e.toString() ) ;
			 
	}	


	if ( ( fileName == fsManager->getAliasForCurrentDirectory() )
			|| ( fileName == fsManager->getAliasForParentDirectory() ) )
		return false ;
	
	struct stat buf ;
	
	string tmp = fsManager->joinPath( _path, fileName ) ;
	
	return ( ::stat( tmp.c_str(), & buf ) == 0 ) && 
		( S_ISREG( buf.st_mode ) || S_ISLNK( buf.st_mode ) ) ;


#else // CEYLAN_USES_STAT


#ifdef CEYLAN_USES__STAT


	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"StandardDirectory::hasFile failed: " + e.toString() ) ;
			 
	}	


	if ( ( fileName == fsManager->getAliasForCurrentDirectory() )
			|| ( fileName == fsManager->getAliasForParentDirectory() ) )
		return false ;
	
	struct _stat buf ;
	
	string tmp = fsManager->joinPath( _path, fileName ) ;
	
	return ( ::_stat( tmp.c_str(), & buf ) == 0 ) && S_ISREG( buf.st_mode ) ;
	
#else // CEYLAN_USES__STAT

	throw DirectoryException( "StandardDirectory::hasFile : "
		"not implemented on this platform." ) ;
		
#endif  // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT


}



bool StandardDirectory::hasEntry( const string & entryName ) const
	throw( DirectoryLookupFailed )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;
	
	string tmp ;
	
	try
	{
	
		tmp = getCorrespondingFileSystemManager().joinPath( _path, entryName ) ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( "StandardDirectory::hasEntry:"
			+ e.toString() ) ;
			
	}
	
	return ::stat( tmp.c_str(), & buf ) == 0 ;

#else // CEYLAN_USES_STAT


#ifdef CEYLAN_USES__STAT

	
	struct _stat buf ;
	
	string tmp ;
	
	try
	{
	
		tmp = getCorrespondingFileSystemManager().joinPath( _path, entryName ) ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( "StandardDirectory::hasEntry:"
			+ e.toString() ) ;
			
	}
	
	return ::_stat( tmp.c_str(), & buf ) == 0 ;
	
#else // CEYLAN_USES__STAT

	throw DirectoryException( "StandardDirectory::hasEntry : "
		"not implemented on this platform." ) ;
		
#endif  // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}


			
					
void StandardDirectory::getSubdirectories( list<string> & subDirectories )
	const throw( DirectoryLookupFailed )
{

#ifdef CEYLAN_USES_DIRENT_H

	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"StandardDirectory::getSubdirectories failed: " + e.toString() ) ;
			 
	}	
	
	DIR * d = ::opendir( _path.c_str() ) ;

	if ( d == 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getSubdirectories: open: " + explainError() ) ;

	struct dirent * de = 0 ;

	while( ( de = ::readdir( d ) ) != 0 )
	{
	
		string name = de->d_name ;

		// Selects only real subdirectories :
		if ( hasDirectory( name ) )
			subDirectories.push_back( name ) ;
		
	}
	
	
	/*
	 * errno could be checked here :
	 
	if ( errno != 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getSubdirectories: read: " + explainError() ) ;
	 *
	 */
	 
	if ( ::closedir( d ) == -1 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getSubdirectories: close: " + explainError() ) ;


#else // CEYLAN_USES_DIRENT_H

	/*
	 * @portme Use FindFirstFile(), FindNextFile(), and FindClose() Win32 API
	 * functions.
	 *
	 */

	throw DirectoryLookupFailed( "StandardDirectory::getSubdirectories: "
		"not available on this platform." ) ;
	
#endif // CEYLAN_USES_DIRENT_H

}

					
										
void StandardDirectory::getFiles( list<string> & files )
	const throw( DirectoryLookupFailed )
{

#ifdef CEYLAN_USES_DIRENT_H

	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"StandardDirectory::getFiles failed: " + e.toString() ) ;
			 
	}	
	
	DIR * d = ::opendir( _path.c_str() ) ;

	if ( d == 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getFiles: open: " + explainError() ) ;

	struct dirent * de = 0 ;

	while( ( de = ::readdir( d ) ) != 0 )
	{
	
		string name = de->d_name ;

		// Selects only real files :
		if ( hasFile( name ) )
			files.push_back( name ) ;
	}
	
	
	/*
	 * errno could be checked here :
	 
	if ( errno != 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getFiles: read: " + explainError() ) ;
	 *
	 */
	 
	if ( ::closedir( d ) == -1 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getFiles: close: " + explainError() ) ;


#else // CEYLAN_USES_DIRENT_H

	/*
	 * @portme Use FindFirstFile(), FindNextFile(), and FindClose() Win32 API
	 * functions.
	 *
	 */

	throw DirectoryLookupFailed( "StandardDirectory::getFiles: "
		"not available on this platform." ) ;
	
#endif // CEYLAN_USES_DIRENT_H

}

	
					
void StandardDirectory::getEntries( list<string> & entries )
	const throw( DirectoryLookupFailed )
{


#ifdef CEYLAN_USES_DIRENT_H

	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"StandardDirectory::getEntries failed: " + e.toString() ) ;
			 
	}	
	
	DIR * d = ::opendir( _path.c_str() ) ;

	if ( d == 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getEntries: open: " + explainError() ) ;

	struct dirent * de = 0 ;

	while( ( de = ::readdir( d ) ) != 0 )
	{
	
		string name = de->d_name ;

		// Selects only real files :
		if ( hasEntry( name ) )
			entries.push_back( name ) ;
	}
	
	
	/*
	 * errno could be checked here :
	 
	if ( errno != 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getEntries: read: " + explainError() ) ;
	 *
	 */
	 
	if ( ::closedir( d ) == -1 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getEntries: close: " + explainError() ) ;


#else // CEYLAN_USES_DIRENT_H

	/*
	 * @portme Use FindFirstFile(), FindNextFile(), and FindClose() Win32 API
	 * functions.
	 *
	 */

	throw DirectoryLookupFailed( "StandardDirectory::getEntries: "
		"not available on this platform." ) ;
	
#endif // CEYLAN_USES_DIRENT_H


}


					
void StandardDirectory::getSortedEntries( list<string> & subDirectories,
		list<string> & files, list<string> & otherEntries )
	const throw( DirectoryLookupFailed )
{

#ifdef CEYLAN_USES_DIRENT_H

	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"StandardDirectory::getSortedEntries failed: " + e.toString() ) ;
			 
	}	
	
	DIR * d = ::opendir( _path.c_str() ) ;

	if ( d == 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getSortedEntries: open: " + explainError() ) ;

	struct dirent * de = 0 ;
	struct stat buf ;

	while( ( de = ::readdir( d ) ) != 0 )
	{
	
		string name = de->d_name ;
		string fullname = fsManager->joinPath( _path, name ) ;

		// Selects only real entries :
		if ( name != fsManager->getAliasForCurrentDirectory()
				&& name != fsManager->getAliasForParentDirectory()
				&& ::stat( fullname.c_str(), & buf ) == 0 )
		{		
		
			if ( S_ISDIR( buf.st_mode ) )
				subDirectories.push_back( name ) ;
			else
#ifdef CEYLAN_USES_STAT
				if ( S_ISREG( buf.st_mode ) || S_ISLNK( buf.st_mode ) )
#else // CEYLAN_USES_STAT
				if ( S_ISREG( buf.st_mode ) )
#endif // CEYLAN_USES_STAT				
					files.push_back( name ) ;
				else
					otherEntries.push_back( name ) ;
		
		}	
	}
	
	
	/*
	 * errno could be checked here :
	 
	if ( errno != 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getEntries: read: " + explainError() ) ;
	 *
	 */
	 
	if ( ::closedir( d ) == -1 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getSortedEntries: close: " + explainError() ) ;


#else // CEYLAN_USES_DIRENT_H

	/*
	 * @portme Use FindFirstFile(), FindNextFile(), and FindClose() Win32 API
	 * functions.
	 *
	 */

	throw DirectoryLookupFailed( "StandardDirectory::getSortedEntries: "
		"not available on this platform." ) ;
	
#endif // CEYLAN_USES_DIRENT_H

}
	
	
	
					
// Other instance methods.


void StandardDirectory::goDown( const string & subdirectoryName )
					throw( DirectoryChangeFailed )
{

	if ( hasDirectory( subdirectoryName ) )
	{

		try
		{
	
			_path = getCorrespondingFileSystemManager().joinPath( 
				_path, subdirectoryName ) ;
		
		}
		catch( const DirectoryDelegatingException & e )
		{
	
			throw DirectoryChangeFailed( "StandardDirectory::goDown failed: " 
				+ e.toString() ) ;
			 
		}	
	
		return ;
		
	}
	
	throw DirectoryChangeFailed( "StandardDirectory::goDown failed for "
		+ subdirectoryName ) ;

}

					
bool StandardDirectory::isValid() const throw( DirectoryException )
{

	// Let DirectoryDelegatingException and DirectoryLookupFailed propagate:
	return getCorrespondingFileSystemManager().existsAsDirectory( _path ) ;
	
}


void StandardDirectory::removeLeadingSeparator() throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	return getCorrespondingFileSystemManager().removeLeadingSeparator( _path ) ;
	
}
					
					
const string StandardDirectory::toString( Ceylan::VerbosityLevels level )
	const throw()
{

	return "Standard directory referring to path '" + _path + "'" ;
 
}
					
		
					
					
// Factory section.										


StandardDirectory & StandardDirectory::Create( const string & newDirectoryName )
	throw( DirectoryException )
{

	return * new StandardDirectory( newDirectoryName, /* create */ true ) ;

}


					
StandardDirectory & StandardDirectory::Open( const string & directoryName ) 
	throw( DirectoryException )
{

	return * new StandardDirectory( directoryName, /* create */ false ) ;

}
			
			
StandardDirectory::StandardDirectory( const string & directoryName,
		bool create ) throw( DirectoryException ):
	Directory( directoryName )
{
	
	if ( _path.empty() )
	{
	
		_path = 
		getCorrespondingFileSystemManager().getCurrentWorkingDirectoryName() ;
	
	}
				
	// path should never finish with a separator :
	removeLeadingSeparator() ;
	
	
#ifdef CEYLAN_USES_MKDIR

	CEYLAN_LOG( "StandardDirectory constructor: "
		"creating directory reference for " + _path ) ;
		
	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryCreationFailed( 
			"StandardDirectory constructor failed: " + e.toString() ) ;
			 
	}	

	if ( create && ! isValid() )
	{

		CEYLAN_LOG( "StandardDirectory constructor: directory " + _path 
			+ " does not exist yet and is to be created." ) ;

		/*
		 * Not a valid path, must be created, each intermediate
		 * directory will be created if necessary.
		 *
		 */

		string path ;

#if CEYLAN_ARCH_UNIX

		// Next split will eat leading '/' :
		if ( fsManager->isAbsolutePath( _path ) )
			path = fsManager->getRootDirectoryPrefix() 
				+ fsManager->getSeparator() ;

#endif // CEYLAN_ARCH_UNIX

#if CEYLAN_ARCH_WINDOWS

		/*
		 * For Windows, for example we have to evaluate first 'c:\' 
		 * (not 'c:' which would never be found existing), then 
		 * 'c:\Documents and Settings' 
		 * (not 'c:\Documents and Settings\' which would never be found
		 * existing)
		 *
		 */
		bool firstPathElement = true ;

#endif // CEYLAN_ARCH_WINDOWS

		list<string> nodes = fsManager->splitPath( _path ) ;

		CEYLAN_LOG( "StandardDirectory constructor: creating sub-elements." ) ;

		for ( list<string>::const_iterator it = nodes.begin(); 
			it != nodes.end(); it++ )
		{
		
			path += *it ;
			
#if CEYLAN_ARCH_WINDOWS
			if ( firstPathElement )
			{

				// 'c:' is never a directory, whereas 'C:\' is.
				path += fsManager->getSeparator() ;
				firstPathElement = false ;
				
			}
#endif // CEYLAN_ARCH_WINDOWS


			CEYLAN_LOG( "StandardDirectory constructor: examining " + path ) ;

			if ( ! fsManager->existsAsDirectory( path ) )
			{

				CEYLAN_LOG( "StandardDirectory constructor: creating " 
					+ path ) ;

				// Mingw's mkdir takes only parameter :
								
#ifdef CEYLAN_USES_MKDIR_TWO_ARGS

				if ( ::mkdir( path.c_str(), basicDirectory ) == -1 )
				
#else // CEYLAN_USES_MKDIR_TWO_ARGS

#ifdef CEYLAN_USES__MKDIR

				if ( ::_mkdir( path.c_str() /* no basicDirectory argument */ ) 
					== -1 )

#else // CEYLAN_USES__MKDIR 

				if ( ::mkdir( path.c_str() /* no basicDirectory argument */ ) 
					== -1 )
					
#endif // CEYLAN_USES__MKDIR 

#endif // CEYLAN_USES_MKDIR_TWO_ARGS			
					throw DirectoryCreationFailed( 
						"StandardDirectory constructor failed for path "
						+ path + ": " + explainError() ) ;
			}

			path += fsManager->getSeparator() ;

		} // for it in nodes

	} // if ( create && ! isValid() )


	if ( ! fsManager->isAbsolutePath( _path ) )
		_path = fsManager->getCurrentWorkingDirectoryName() 
			+ fsManager->getSeparator() + _path ;

	CEYLAN_LOG( "Directory reference to " + _path + " done." ) ;

#else // CEYLAN_USES_MKDIR

	throw DirectoryException( "StandardDirectory constructor: operation not "
		"available on this platform." ) ;
		
#endif // CEYLAN_USES_MKDIR

	
}

			
					
// Protected section.


FileSystemManager & StandardDirectory::getCorrespondingFileSystemManager()
	const throw( DirectoryDelegatingException )
{

	try
	{
	
		return StandardFileSystemManager::GetStandardFileSystemManager() ;
	
	} 
	catch ( const StandardFileSystemManagerException & e )
	{
	
		throw DirectoryDelegatingException(
			"StandardDirectory::getCorrespondingFileSystemManager failed: "
			+ e.toString() ) ;
		
	}	
	
}
																					
