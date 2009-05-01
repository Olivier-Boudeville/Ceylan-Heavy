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


#include "CeylanLibfatFileSystemManager.h"


#include "CeylanLibfatFile.h"      // for LibfatFile
#include "CeylanLibfatDirectory.h" // for LibfatDirectory

#include "CeylanLogPlug.h"         // for LogPlug
#include "CeylanStringUtils.h"     // for StringSize
#include "CeylanOperators.h"       // for toString
#include "CeylanHolder.h"          // for Holder template


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for libdns and al
#endif // CEYLAN_ARCH_NINTENDO_DS




// Not available in their C++ form:
extern "C"
{

#if CEYLAN_ARCH_NINTENDO_DS

#include "fat.h"                   // for Michael "Chishm" Chisholm's libfat

#include <fcntl.h> 
#include <unistd.h> 

#endif // CEYLAN_ARCH_NINTENDO_DS

}



using std::string ;
using std::list ;


using namespace Ceylan::Log ;
using namespace Ceylan::System ;



LibfatFileSystemManager * 
	LibfatFileSystemManager::_LibfatFileSystemManager = 0 ;


const string LibfatFileSystemManager::RootDirectoryPrefix   = ""   ;
const Ceylan::Latin1Char LibfatFileSystemManager::Separator = '/'  ;




/*
 * Implementation of the FileSystemManager mother class.
 *
 * libfat registers itself into devoptab as the default device, hence these
 * sources could be heavily inspired from CeylanStandardFileSystemManager.cc,
 * for files (directories behave differently).
 *
 * @see libfat-sources/source/fatdir.c for the registered implementations.
 * @see http://chishm.drunkencoders.com/libfat/
 *
 */
				

bool LibfatFileSystemManager::existsAsEntry( const string & entryPath ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw EntryLookupFailed( "LibfatFileSystemManager::existsAsEntry: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	// For the ARM9, using _FAT_stat_r defined in fatdir.c:
	
	return ( ::stat( entryPath.c_str(), /* no stat wanted */ 0 ) == 0 ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw EntryLookupFailed( "LibfatFileSystemManager::existsAsEntry: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void LibfatFileSystemManager::createSymbolicLink( 
	const string & linkTarget, const string & linkName )
{

	throw SymlinkFailed( "LibfatFileSystemManager::createSymbolicLink: "
		"never supported with Libfat-based filesystems" ) ;
		
}



time_t LibfatFileSystemManager::getEntryChangeTime( 
	const string & entryPath )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw GetChangeTimeFailed( "LibfatFileSystemManager::getEntryChangeTime: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	// For the ARM9, using _FAT_stat_r defined in fatdir.c:
	struct stat buf ;
	
	if ( ::stat( entryPath.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

	throw GetChangeTimeFailed( 
		"LibfatFileSystemManager::getEntryChangeTime: "
		"unable to get last change time for entry '" + entryPath + "': "
		+ System::explainError() ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw GetChangeTimeFailed( "LibfatFileSystemManager::getEntryChangeTime: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}
					



// Accessors to FilesystemManager constants.


const string & LibfatFileSystemManager::getRootDirectoryPrefix() const
{

	return RootDirectoryPrefix ;
	
}


	    	   					
Ceylan::Latin1Char LibfatFileSystemManager::getSeparator() const
{

	return Separator ;
	
}





// File-related section.



File & LibfatFileSystemManager::createFile( const string & filename, 
	OpeningFlag createFlag,	PermissionFlag permissionFlag ) 
{

	return LibfatFile::Create( filename, createFlag 
		/* no permissionFlag supported */ ) ;
	
}



File & LibfatFileSystemManager::openFile( const string & filename, 
	OpeningFlag openFlag )
{

	return LibfatFile::Open( filename, openFlag ) ;

}



bool LibfatFileSystemManager::existsAsFileOrSymbolicLink( 
	const string & filename ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileLookupFailed(
		"LibfatFileSystemManager::existsAsFileOrSymbolicLink: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	// For the ARM9, using _FAT_stat_r defined in fatdir.c:
	
	struct stat buf ;
	
	// Regular file (no link):
	return ( ::stat( filename.c_str(), & buf ) == 0 
		&& ( buf.st_mode & S_IFREG ) ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileLookupFailed(
		"LibfatFileSystemManager::existsAsFileOrSymbolicLink: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void LibfatFileSystemManager::removeFile( const string & filename ) 
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileRemoveFailed( "LibfatFileSystemManager::removeFile: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	// For the ARM9, using _FAT_unlink_r defined in fatdir.c:

	if ( ::unlink( filename.c_str() ) != 0 )
		throw FileRemoveFailed( 
			"LibfatFileSystemManager::removeFile failed for '"
			+ filename + "': " + System::explainError() ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileRemoveFailed( "LibfatFileSystemManager::removeFile: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void LibfatFileSystemManager::moveFile( const string & sourceFilename,
	const string & targetFilename )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileMoveFailed( "LibfatFileSystemManager::moveFile: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	// For the ARM9, using _FAT_rename_r defined in fatdir.c:

	if ( ::rename( sourceFilename.c_str(), targetFilename.c_str() ) == -1 )
		throw FileMoveFailed( 
			"LibfatFileSystemManager::moveFile failed, from '"
			+ sourceFilename + "' to '" + targetFilename + "': " 
			+ System::explainError() ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileMoveFailed( "LibfatFileSystemManager::moveFile: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void LibfatFileSystemManager::copyFile( const string & sourceFilename, 
	const string & targetFilename )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileCopyFailed( "LibfatFileSystemManager::copyFile: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	try 
	{
	
		Ceylan::Holder<LibfatFile> sourceFile( 
			LibfatFile::Open( sourceFilename ) ) ;
			
		sourceFile->saveAs( targetFilename ) ;
		sourceFile->close() ;
		
		// sourceFile automatically deallocated thanks to Holder.
		
	} 
	catch ( const FileException & e )
	{
		throw FileCopyFailed( 
			"LibfatFileSystemManager::copyFile failed when copying '"
			+ sourceFilename + "' to '" + targetFilename + "': " 
			+ e.toString() ) ;
	}

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileCopyFailed( "LibfatFileSystemManager::copyFile: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



Size LibfatFileSystemManager::getSize( const string & filename ) 
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileSizeRequestFailed( "LibfatFileSystemManager::getSize: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	struct stat buf ;

	if ( ::stat( filename.c_str(), & buf ) == 0 )
		return static_cast<Size>( buf.st_size ) ;

	throw FileSizeRequestFailed( "LibfatFileSystemManager::getSize: "
		"could not stat file '"	+ filename + "': " + System::explainError() ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileSizeRequestFailed( "LibfatFileSystemManager::getSize: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}

	
	
time_t LibfatFileSystemManager::getLastChangeTimeFile( const string & filename )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileLastChangeTimeRequestFailed(
		"LibfatFileSystemManager::getLastChangeTimeFile: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	struct stat buf ;

	if ( ::stat( filename.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

	throw FileLastChangeTimeRequestFailed(
		"LibfatFileSystemManager::getLastChangeTimeFile: "
		"could not stat file '"	+ filename + "': " + System::explainError() ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileLastChangeTimeRequestFailed(
		"LibfatFileSystemManager::getLastChangeTimeFile: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}	



void LibfatFileSystemManager::touch( const string & filename ) 
{

	// no ::utime available.
	
	throw FileTouchFailed( "LibfatFileSystemManager::touch failed: "
		"not supported on libfat-based filesystems" ) ;

}


									
	
// Directory-related section.
			

// Factory-related subsection.

										
Directory & LibfatFileSystemManager::createDirectory( 
	const string & newDirectoryName )
{

	return LibfatDirectory::Create( newDirectoryName ) ;

}

	
					
Directory & LibfatFileSystemManager::openDirectory( 
	const string & directoryName )
{

	return LibfatDirectory::Open( directoryName ) ;

}

	
						
bool LibfatFileSystemManager::existsAsDirectory( 
	const string & directoryPath ) const
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryLookupFailed( "LibfatFileSystemManager::existsAsDirectory: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	// For the ARM9, using _FAT_stat_r defined in fatdir.c:
	
	struct stat buf ;
	
	return ( ::stat( directoryPath.c_str(), & buf ) == 0 
		&& ( buf.st_mode & S_IFDIR ) ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryLookupFailed( "LibfatFileSystemManager::existsAsDirectory: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void LibfatFileSystemManager::removeDirectory( const string & directoryPath, 
	bool recursive )
{	

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryRemoveFailed( "LibfatFileSystemManager::removeDirectory: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	/*
	 * For the ARM9, _FAT_unlink_r defined in fatdir.c is used, as apparently
	 * it can remove (empty) directories as well.
	 *
	throw DirectoryRemoveFailed( "LibfatFileSystemManager::removeDirectory: "
		"not supported on the Nintendo DS" ) ;
	 */

	struct stat buf ;

	if ( directoryPath.empty() )
		throw DirectoryRemoveFailed( 
			"LibfatFileSystemManager::removeDirectory: "
			"void directory specified" ) ;

	// Must be modified (leading separator):
	string thisPath = directoryPath ;

	removeLeadingSeparator( thisPath ) ;


	if ( recursive )
	{

		LibfatDirectory & d = LibfatDirectory::Open( thisPath ) ;

		list<string> nodes ;
		d.getEntries( nodes ) ;

		for ( list<string>::const_iterator it = nodes.begin(); 
			it != nodes.end(); it++ )
		{

			string newPath = joinPath( thisPath, *it ) ;

			if ( ::stat( newPath.c_str(), & buf ) == 0 )
			{
			
				// Unlinks directly files:
				if ( buf.st_mode & S_IFREG  )
				{

					if ( ::unlink( newPath.c_str() ) )
						throw DirectoryRemoveFailed(
							"LibfatFileSystemManager::removeDirectory"
							" failed in unlink for '" + newPath +  "': " 
							+ System::explainError() ) ;
				}
				else

				// Deletes directories:
				if ( buf.st_mode & S_IFDIR )
				{
					// Recursive call:
					removeDirectory( newPath ) ;
				}
				
			}
			else
			{
				// stat failed:
				throw DirectoryRemoveFailed(
					"LibfatFileSystemManager::removeDirectory "
					"failed in stat for '" + newPath +  "': " 
					+ System::explainError() ) ;
			}
		}

	}

	// ::unlink seems to be able to replace ::rmdir here:
	if ( ::unlink( thisPath.c_str() ) )
		throw DirectoryRemoveFailed( 
			"LibfatFileSystemManager::removeDirectory failed for directory '" 
			+ thisPath + "': " + System::explainError() ) ;


#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryRemoveFailed( "LibfatFileSystemManager::removeDirectory: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS


}



void LibfatFileSystemManager::moveDirectory( 
	const string & sourceDirectoryPath, const string & targetDirectoryPath )
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryMoveFailed( "LibfatFileSystemManager::moveDirectory: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	// libfat _FAT_rename_r does not seem to operate on directories.
	
	// Renaming is copying and then removing the source directory:
	try
	{
	
		copyDirectory( sourceDirectoryPath, targetDirectoryPath ) ;
		removeDirectory( sourceDirectoryPath ) ;
		
	}
	catch( const DirectoryException & e )
	{
	
		throw DirectoryMoveFailed( 
			"LibfatFileSystemManager::moveDirectory failed: " 
			+ e.toString() ) ;
		
	}	

#endif // CEYLAN_RUNS_ON_ARM7
		
#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryMoveFailed( "LibfatFileSystemManager::moveDirectory: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void LibfatFileSystemManager::copyDirectory( const string & sourceDirectoryPath,
	const string & targetDirectoryPath )
{

	throw DirectoryCopyFailed( "LibfatFileSystemManager::copyDirectory: "
		"not supported on this platform." ) ;

}



time_t LibfatFileSystemManager::getLastChangeTimeDirectory( 
	const string & directoryPath )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryLastChangeTimeRequestFailed(
		"LibfatFileSystemManager::getLastChangeTimeDirectory: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	struct stat buf ;

	if ( ::stat( directoryPath.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

	throw DirectoryLastChangeTimeRequestFailed(
		"LibfatFileSystemManager::getLastChangeTimeDirectory: "
		"could not stat directory '" + directoryPath + "': " 
		+ System::explainError() ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryLastChangeTimeRequestFailed(
		"LibfatFileSystemManager::getLastChangeTimeDirectory: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}

	

bool LibfatFileSystemManager::isAValidDirectoryPath( 
	const string & directoryString )
{

	// A priori correct:
	return true ;
	
}	
			
		
		
bool LibfatFileSystemManager::isAbsolutePath( const string & path )
{

	return ( path[0] == Separator ) ;
		
}	
			
			
			
std::string LibfatFileSystemManager::getCurrentWorkingDirectoryPath()	
{

	return _currentWorkingDirectory ;
	
}	



void LibfatFileSystemManager::changeWorkingDirectory( 
	const string & newWorkingDirectory )
{

#if CEYLAN_ARCH_NINTENDO_DS

		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw DirectoryChangeFailed( 
		"LibfatFileSystemManager::changeWorkingDirectory: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	if ( ::chdir( newWorkingDirectory.c_str() ) != 0 )
		throw DirectoryChangeFailed(
			"LibfatFileSystemManager::changeWorkingDirectory: "
			"unable to change current working directory to '"
			+ newWorkingDirectory + "': " + explainError() ) ;
			
	_currentWorkingDirectory = newWorkingDirectory ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryChangeFailed(
		"LibfatFileSystemManager::changeWorkingDirectory: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}	




// LibfatFileSystemManager own section.

					
const string LibfatFileSystemManager::toString( Ceylan::VerbosityLevels level ) 
	const
{

	return "Libfat filesystem manager whose current working directory is '"
		+ _currentWorkingDirectory + "'" ;
	
}




// Static section.



LibfatFileSystemManager & LibfatFileSystemManager::GetLibfatFileSystemManager() 
{

	if ( _LibfatFileSystemManager == 0 )
		_LibfatFileSystemManager = new LibfatFileSystemManager() ;
	
	return *_LibfatFileSystemManager ;	
	
}
	

	
void LibfatFileSystemManager::SecureLibfatFileSystemManager() 
{

	if ( _LibfatFileSystemManager == 0 )
		_LibfatFileSystemManager = new LibfatFileSystemManager() ;
		
}
	
	
	
void LibfatFileSystemManager::RemoveLibfatFileSystemManager()
{

	if ( _LibfatFileSystemManager != 0 )
	{

		if ( FileSystemManager::_CurrentDefaultFileSystemManager ==
				_LibfatFileSystemManager )
			_LibfatFileSystemManager = 0 ;
			
		delete _LibfatFileSystemManager ;
		_LibfatFileSystemManager = 0 ;
		
		
	}
}




LibfatFileSystemManager::LibfatFileSystemManager() :
	_currentWorkingDirectory( Ceylan::toString( Separator ) )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw LibfatFileSystemManagerException( 
			"LibfatFileSystemManager constructor: not supported on the ARM7" ) ;

#else // CEYLAN_RUNS_ON_ARM7
	

	// For safety, see http://forum.gbadev.org/viewtopic.php?p=104521#104521:
	System::atomicSleep() ;
	System::atomicSleep() ;
		
	if ( ::fatInit( /* cache size, in sectors */ 8, 
			/* set as default for stdio file device*/ true ) == false )
		throw LibfatFileSystemManagerException( 
			"LibfatFileSystemManager constructor: initialization failed" ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw LibfatFileSystemManagerException( 
		"LibfatFileSystemManager constructor: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS
			
}



LibfatFileSystemManager::~LibfatFileSystemManager() throw()
{

	// Nothing special to switch off this filesystem.
	
}

