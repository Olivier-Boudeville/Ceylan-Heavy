/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#include "CeylanLibfatDirectory.h"


#include "CeylanFileSystemManager.h"         // for FileSystemManager
#include "CeylanLibfatFileSystemManager.h"   // for LibfatFileSystemManager

#include "CeylanLogLight.h"          // for CEYLAN_LOG
#include "CeylanLogPlug.h"          // for LogPlug
#include "CeylanOperators.h"         // for toString



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



// Not available in their C++ form:
extern "C"
{

#if CEYLAN_ARCH_NINTENDO_DS

#include "fat.h"        // for Michael "Chishm" Chisholm's libfat

/*
 * For libfat diropen, DIR_ITER, etc., defined in 
 * devkitARM/arm-eabi/include/sys/dir.h:
 *
 */
#include "sys/dir.h"    

#include <fcntl.h> 
#include <unistd.h> 

#endif // CEYLAN_ARCH_NINTENDO_DS

}


#include <cstdlib>

#include <cerrno>    // for EINTR, ENOLCK, etc.
#include <cstdio>    // for unlink


#ifndef PATH_MAX
#define PATH_MAX 256
#endif // PATH_MAX



/*
 * Implementation notes.
 *
 * @note All internal paths kept in directory references (this object)
 * should be absolute paths.
 *
 * @note In a non-static method, no static method should be used, as the former
 * is expected to use the libfat filesystem manager, whereas the latter shall
 * use the default filesystem manager, which may or may not be the libfat
 * one.
 * 
 */
 
using std::string ;
using std::list ;

using namespace Ceylan::Log ;
using namespace Ceylan::System ;




LibfatDirectory::~LibfatDirectory() throw()
{

	// Nothing special here for libfat directories.

}




// LibfatDirectory implementation section.


// Instance methods.
				

			
				
// Directory content subsection.



bool LibfatDirectory::hasDirectory( const string & subdirectoryName ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryLookupFailed( "LibfatDirectory::hasDirectory: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"LibfatDirectory::hasDirectory failed: " + e.toString() ) ;
			 
	}	

	if ( ( subdirectoryName == fsManager->getAliasForCurrentDirectory() )
			|| ( subdirectoryName == fsManager->getAliasForParentDirectory() ) )
		return true ;

	string fullDirPath = fsManager->joinPath( _path, subdirectoryName ) ;
	
	// For the ARM9, using _FAT_stat_r defined in fatdir.c:
	
	struct stat buf ;
	
	return ( ( ::stat( fullDirPath.c_str(), & buf ) == 0 ) 
		&& ( buf.st_mode & S_IFDIR ) ) ;
	

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryLookupFailed( "LibfatDirectory::hasDirectory: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS


}



bool LibfatDirectory::hasFile( const string & fileName ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryLookupFailed( "LibfatDirectory::hasFile: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"LibfatDirectory::hasFile failed: " + e.toString() ) ;
			 
	}	

	string fullDirPath = fsManager->joinPath( _path, fileName ) ;
	
	// For the ARM9, using _FAT_stat_r defined in fatdir.c:
	
	struct stat buf ;
	
	return ( ( ::stat( fullDirPath.c_str(), & buf ) == 0 ) 
		&& ( buf.st_mode & S_IFREG ) ) ;
	

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryLookupFailed( "LibfatDirectory::hasFile: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



bool LibfatDirectory::hasEntry( const string & entryName ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryLookupFailed( "LibfatDirectory::hasEntry: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	FileSystemManager * fsManager ;
	
	try
	{
	
		fsManager = & getCorrespondingFileSystemManager() ;
		
	}
	catch( const DirectoryDelegatingException & e )
	{
	
		throw DirectoryLookupFailed( 
			"LibfatDirectory::hasEntry failed: " + e.toString() ) ;
			 
	}	

	if ( ( entryName == fsManager->getAliasForCurrentDirectory() )
			|| ( entryName == fsManager->getAliasForParentDirectory() ) )
		return true ;

	string fullDirPath = fsManager->joinPath( _path, entryName ) ;
	
	
	// For the ARM9, using _FAT_stat_r defined in fatdir.c:
		
	return ( ::stat( fullDirPath.c_str(), 0 /* no stat wanted */ ) == 0 ) ;
	

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryLookupFailed( "LibfatDirectory::hasEntry: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}


			
					
void LibfatDirectory::getSubdirectories( list<string> & subDirectories ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryLookupFailed( "LibfatDirectory::getSubdirectories: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	
	DIR_ITER* dirIterator = ::diropen( _path.c_str() ) ; 

	// To hold a full filename and string terminator:
	char filenameBuf[PATH_MAX] ; 

	struct stat statBuf ;
	
	if ( dirIterator == 0 ) 
	{
	
		::dirclose( dirIterator ) ;
		throw DirectoryLookupFailed( 
			"LibfatDirectory::getSubdirectories failed: "
			"unable to open the directory " + _path ) ;
	}
			
	while ( ::dirnext( dirIterator, filenameBuf, & statBuf ) == 0 )
		if ( statBuf.st_mode & S_IFDIR )
			subDirectories.push_back( string( filenameBuf ) ) ;
			
	::dirclose( dirIterator ) ;
			   
#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryLookupFailed( "LibfatDirectory::getSubdirectories: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}

					
										
void LibfatDirectory::getFiles( list<string> & files ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryLookupFailed( "LibfatDirectory::getFiles: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	
	DIR_ITER * dirIterator = ::diropen( _path.c_str() ) ; 

	// To hold a full filename and string terminator:
	char filenameBuf[PATH_MAX] ; 

	struct stat statBuf ;
	
	if ( dirIterator == 0 ) 
	{
		::dirclose( dirIterator ) ;
		throw DirectoryLookupFailed( "LibfatDirectory::getFiles failed: "
			"unable to open the directory " + _path ) ;
	}
			
	while ( ::dirnext( dirIterator, filenameBuf, & statBuf ) == 0 )
		if ( statBuf.st_mode & S_IFREG )
			files.push_back( string( filenameBuf ) ) ;
	
	::dirclose( dirIterator ) ;
			   
#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryLookupFailed( "LibfatDirectory::getFiles: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}

	
					
void LibfatDirectory::getEntries( list<string> & entries ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryLookupFailed( "LibfatDirectory::getEntries: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	
	DIR_ITER * dirIterator = ::diropen( _path.c_str() ) ; 

	// To hold a full filename and string terminator:
	char filenameBuf[PATH_MAX] ; 

	if ( dirIterator == 0 ) 
	{
		::dirclose( dirIterator ) ;
		throw DirectoryLookupFailed( "LibfatDirectory::getEntries failed: "
			"unable to open the directory " + _path ) ;
	}
			
	while ( ::dirnext( dirIterator, filenameBuf, /* no stat wanted */ 0 ) == 0 )
		entries.push_back( string( filenameBuf ) ) ;
			   
	::dirclose( dirIterator ) ;
			   
#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryLookupFailed( "LibfatDirectory::getEntries: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS


}


					
void LibfatDirectory::getSortedEntries( list<string> & subDirectories,
	list<string> & files, list<string> & otherEntries ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryLookupFailed( "LibfatDirectory::getSortedEntries: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	
	DIR_ITER * dirIterator = ::diropen( _path.c_str() ) ; 

	// To hold a full filename and string terminator:
	char filenameBuf[PATH_MAX] ; 

	struct stat statBuf ;
	
	if ( dirIterator == 0 )
	{
	
		::dirclose( dirIterator ) ; 
		throw DirectoryLookupFailed( 
			"LibfatDirectory::getSortedEntries failed: "
			"unable to open the directory " + _path ) ;
	}
	
			
	while ( ::dirnext( dirIterator, filenameBuf, & statBuf ) == 0 )
		if ( statBuf.st_mode & S_IFDIR )
			subDirectories.push_back( string( filenameBuf ) ) ;
		else if ( statBuf.st_mode & S_IFREG )
			files.push_back( string( filenameBuf ) ) ;
		else
			otherEntries.push_back( string( filenameBuf ) ) ;
	
	::dirclose( dirIterator ) ;
			   
#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryLookupFailed( "LibfatDirectory::getSortedEntries: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}
	
	
	
					
// Other instance methods.

					
					
const string LibfatDirectory::toString( Ceylan::VerbosityLevels level ) const
{

	return "Libfat directory referring to path '" + _path + "'" ;
 
}
					
		
					
					
// Factory section.										


LibfatDirectory & LibfatDirectory::Create( const string & newDirectoryName )
{

	return * new LibfatDirectory( newDirectoryName, /* create */ true ) ;

}


					
LibfatDirectory & LibfatDirectory::Open( const string & directoryName ) 
{

	return * new LibfatDirectory( directoryName, /* create */ false ) ;

}
			

			
LibfatDirectory::LibfatDirectory( const string & directoryName,
		bool create ) :
	Directory( directoryName )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
		
	throw DirectoryCreationFailed( "LibfatDirectory constructor: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

	
	// Let DirectoryDelegatingException propagate:
	FileSystemManager & fsManager = getCorrespondingFileSystemManager() ;
	
	if ( _path.empty() )
		_path = fsManager.getCurrentWorkingDirectoryPath() ;

				
	// path should never finish with a separator:
	removeLeadingSeparator() ;
		
	
	CEYLAN_LOG( "LibfatDirectory constructor: "
		"creating directory reference for " + _path ) ;

	if ( create && ! isValid() )
	{

		CEYLAN_LOG( "LibfatDirectory constructor: directory '" + _path 
			+ "' does not exist yet and is to be created." ) ;

		/*
		 * Not a valid path, must be created, each intermediate
		 * directory will be created if necessary.
		 *
		 */

		string path ;


		// Next split will eat leading '/':
		if ( fsManager.isAbsolutePath( _path ) )
			path = fsManager.getRootDirectoryPrefix() 
				+ fsManager.getSeparatorAsString() ;


		// Probably the following could be rewritten with JoinPath:
		
		list<string> nodes = fsManager.splitPath( _path ) ;

		CEYLAN_LOG( "LibfatDirectory constructor: creating sub-elements." ) ;

		for ( list<string>::const_iterator it = nodes.begin(); 
			it != nodes.end(); it++ )
		{
		
			path += *it ;
			

			CEYLAN_LOG( "LibfatDirectory constructor: examining " + path ) ;

			if ( ! fsManager.existsAsDirectory( path ) )
			{

				CEYLAN_LOG( "LibfatDirectory constructor: creating " 
					+ path ) ;

				/*
				 * With libfat, mkdir (i.e. _FAT_mkdir_r) takes a second
				 * parameter, 'int mode', but it is ignored.
				 *
				 */
				if ( ::mkdir( path.c_str(), /* mode ignored */ 0 ) == -1 )
					throw DirectoryCreationFailed( 
						"LibfatDirectory constructor failed for path "
						+ path + ": " + explainError() ) ;
			}

			path += fsManager.getSeparatorAsString() ;

		} // for 'it' in nodes

	} // if ( create && ! isValid() )


	if ( ! fsManager.isAbsolutePath( _path ) )
		_path = fsManager.joinPath( 
			fsManager.getCurrentWorkingDirectoryPath(),	_path ) ;
			
	CEYLAN_LOG( "Directory reference to " + _path + " done." ) ;


#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryCreationFailed( "LibfatDirectory constructor "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}

	
			
					
// Protected section.


FileSystemManager & LibfatDirectory::getCorrespondingFileSystemManager() const
{

	try
	{
	
		return LibfatFileSystemManager::GetLibfatFileSystemManager() ;
	
	} 
	catch ( const LibfatFileSystemManagerException & e )
	{
	
		throw DirectoryDelegatingException(
			"LibfatDirectory::getCorrespondingFileSystemManager failed: "
			+ e.toString() ) ;
		
	}	
	
}



void LibfatDirectory::secureCorrespondingFileSystemManager() const
{

	try
	{
	
		LibfatFileSystemManager::SecureLibfatFileSystemManager() ;
	
	} 
	catch ( const LibfatFileSystemManagerException & e )
	{
	
		throw DirectoryDelegatingException(
			"LibfatDirectory::secureCorrespondingFileSystemManager failed: "
			+ e.toString() ) ;
		
	}	

}
																	
