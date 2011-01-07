/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "CeylanStandardDirectory.h"


#include "CeylanFileSystemManager.h"         // for FileSystemManager
#include "CeylanStandardFileSystemManager.h" // for StandardFileSystemManager

#include "CeylanLogLight.h"          // for CEYLAN_LOG
#include "CeylanOperators.h"         // for toString



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



// None of them is available in standard include:
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



/*
 * Implementation notes.
 *
 * @note All internal paths kept in directory references (this object)
 * should be absolute paths.
 *
 * @note In a non-static method, no static method should be used, as the former
 * is expected to use the standard filesystem manager, whereas the latter shall
 * use the default filesystem manager, which may or may not be the standard
 * one.
 * 
 */
 
using std::string ;
using std::list ;

using namespace Ceylan::System ;




/// Flags used to create new directories:

#if CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

const mode_t basicDirectory = 
	S_IRWXU | S_IRGRP | S_IXGRP	| S_IROTH | S_IXOTH ;
	
#else // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

#ifdef CEYLAN_USES_MKDIR_TWO_ARGS

// Currently not used on MinGW: 
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

	// Nothing special here for standard directories.

}




// StandardDirectory implementation section.


// Instance methods.
				

			
				
// Directory content subsection.



bool StandardDirectory::hasDirectory( const string & subdirectoryName ) const
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
	
	return ( ::_stat( tmp.c_str(), & buf ) == 0 )
		&& ( buf.st_mode & _S_IFDIR ) ;
	
#else // CEYLAN_USES__STAT

	throw DirectoryException( "StandardDirectory::hasDirectory: "
		"not implemented on this platform." ) ;
		
#endif  // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}



bool StandardDirectory::hasFile( const string & fileName ) const
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
	
	return ( ::_stat( tmp.c_str(), & buf ) == 0 ) 
		&& ( buf.st_mode & _S_IFREG ) ;
	
#else // CEYLAN_USES__STAT

	throw DirectoryException( "StandardDirectory::hasFile: "
		"not implemented on this platform." ) ;
		
#endif  // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}



bool StandardDirectory::hasEntry( const string & entryName ) const
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

	throw DirectoryException( "StandardDirectory::hasEntry: "
		"not implemented on this platform." ) ;
		
#endif  // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}


							
void StandardDirectory::getSubdirectories( list<string> & subDirectories ) const
{

#ifdef CEYLAN_USES_DIRENT_H
	
	DIR * d = ::opendir( _path.c_str() ) ;

	if ( d == 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getSubdirectories: open: " + explainError() ) ;

	struct dirent * de = 0 ;

	while( ( de = ::readdir( d ) ) != 0 )
	{
	
		string name = de->d_name ;

		// Selects only real subdirectories:
		if ( hasDirectory( name ) )
			subDirectories.push_back( name ) ;
		
	}
	
	
	/*
	 * errno could be checked here:
	 
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

					
										
void StandardDirectory::getFiles( list<string> & files ) const
{

#ifdef CEYLAN_USES_DIRENT_H

	
	DIR * d = ::opendir( _path.c_str() ) ;

	if ( d == 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getFiles: open: " + explainError() ) ;

	struct dirent * de = 0 ;

	while( ( de = ::readdir( d ) ) != 0 )
	{
	
		string name = de->d_name ;

		// Selects only real files:
		if ( hasFile( name ) )
			files.push_back( name ) ;
	}
	
	
	/*
	 * errno could be checked here:
	 
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

	
					
void StandardDirectory::getEntries( list<string> & entries ) const
{

#ifdef CEYLAN_USES_DIRENT_H
	
	DIR * d = ::opendir( _path.c_str() ) ;

	if ( d == 0 )
		throw DirectoryLookupFailed( 
			"StandardDirectory::getEntries: open: " + explainError() ) ;

	struct dirent * de = 0 ;

	while( ( de = ::readdir( d ) ) != 0 )
	{
	
		string name = de->d_name ;

		// Selects only real files:
		if ( hasEntry( name ) )
			entries.push_back( name ) ;
			
	}
	
	
	/*
	 * errno could be checked here:
	 
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
		list<string> & files, list<string> & otherEntries ) const
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

		// Selects only real entries:
		if ( name != fsManager->getAliasForCurrentDirectory()
				&& name != fsManager->getAliasForParentDirectory()
				&&::stat( fullname.c_str(), & buf ) == 0 )
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
	 * errno could be checked here:
	 
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

										
					
const string StandardDirectory::toString( Ceylan::VerbosityLevels level ) const
{

	return "Standard directory referring to path '" + _path + "'" ;
 
}
					
		
			
					
					
// Factory section.										



StandardDirectory & StandardDirectory::Create( const string & newDirectoryName )
{

	return * new StandardDirectory( newDirectoryName, /* create */ true ) ;

}


					
StandardDirectory & StandardDirectory::Open( const string & directoryName ) 
{

	return * new StandardDirectory( directoryName, /* create */ false ) ;

}
			

			
StandardDirectory::StandardDirectory( const string & directoryName,
		bool create ) :
	Directory( directoryName )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
	throw DirectoryCreationFailed( "StandardDirectory constructor: "
		"not supported on the Nintendo DS platform." ) ;
	
#else // CEYLAN_ARCH_NINTENDO_DS


	// Let DirectoryDelegatingException propagate:
	FileSystemManager & fsManager = getCorrespondingFileSystemManager() ;
	
	if ( _path.empty() )
		_path = fsManager.getCurrentWorkingDirectoryPath() ;

				
	// path should never finish with a separator:
	removeLeadingSeparator() ;
		
	
#ifdef CEYLAN_USES_MKDIR

	CEYLAN_LOG( "StandardDirectory constructor: "
		"creating directory reference for " + _path ) ;

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

		// Next split will eat leading '/':
		if ( fsManager.isAbsolutePath( _path ) )
			path = fsManager.getRootDirectoryPrefix() 
				+ fsManager.getSeparatorAsString() ;

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

		// Probably the following could be rewritten with JoinPath:
		
		list<string> nodes = fsManager.splitPath( _path ) ;

		CEYLAN_LOG( "StandardDirectory constructor: creating sub-elements." ) ;

		for ( list<string>::const_iterator it = nodes.begin(); 
			it != nodes.end(); it++ )
		{
		
			path += *it ;
			
#if CEYLAN_ARCH_WINDOWS
			if ( firstPathElement )
			{

				// 'c:' is never a directory, whereas 'C:\' is.
				path += fsManager.getSeparatorAsString() ;
				firstPathElement = false ;
				
			}
#endif // CEYLAN_ARCH_WINDOWS


			CEYLAN_LOG( "StandardDirectory constructor: examining " + path ) ;

			if ( ! fsManager.existsAsDirectory( path ) )
			{

				CEYLAN_LOG( "StandardDirectory constructor: creating " 
					+ path ) ;

				// Mingw's mkdir takes only parameter:
								
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

			path += fsManager.getSeparatorAsString() ;

		} // for it in nodes

	} // if ( create && ! isValid() )


	if ( ! fsManager.isAbsolutePath( _path ) )
		_path = fsManager.joinPath( 
			fsManager.getCurrentWorkingDirectoryPath(),	_path ) ;
			
	CEYLAN_LOG( "Directory reference to " + _path + " done." ) ;

#else // CEYLAN_USES_MKDIR

	throw DirectoryException( "StandardDirectory constructor: operation not "
		"available on this platform." ) ;
		
#endif // CEYLAN_USES_MKDIR

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}

		
			
					
// Protected section.


FileSystemManager & StandardDirectory::getCorrespondingFileSystemManager() const
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

																				
