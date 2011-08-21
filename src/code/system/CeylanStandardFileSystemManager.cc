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


#include "CeylanStandardFileSystemManager.h"


#include "CeylanStandardFile.h"      // for StandardFile
#include "CeylanStandardDirectory.h" // for StandardDirectory

#include "CeylanStringUtils.h"       // for StringSize
#include "CeylanOperators.h"         // for toString
#include "CeylanHolder.h"            // for Holder template
#include "CeylanRegularExpression.h" // for RegExp


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


#include <fstream>                   // for this class
using std::ifstream ;
using std::ios ;

using std::string ;
using std::list ;



// Not available in their C++ form:
extern "C"
{

#ifdef CEYLAN_USES_DIRECT_H
#include <direct.h>            // for _rmdir
#endif // CEYLAN_USES_DIRECT_H

#ifdef CEYLAN_USES_SYS_STAT_H
#include <sys/stat.h>          // for mode_t
#endif // CEYLAN_USES_SYS_STAT_H

#ifdef CEYLAN_USES_UTIME_H
#include <utime.h>             // for utime
#endif // CEYLAN_USES_UTIME_H

#ifdef CEYLAN_USES_SYS_UTIME_H
#include <sys/utime.h>         // for utime
#endif // CEYLAN_USES_SYS_UTIME_H

}


#ifndef PATH_MAX
#define PATH_MAX 1024
#endif // PATH_MAX


using namespace Ceylan::System ;



StandardFileSystemManager *
	StandardFileSystemManager::_StandardFileSystemManager = 0 ;



#if CEYLAN_ARCH_WINDOWS

// "c:" is a drive, "c:\\" is a directory.
const string StandardFileSystemManager::RootDirectoryPrefix   = "c:" ;
const Ceylan::Latin1Char StandardFileSystemManager::Separator = '\\' ;

#else // CEYLAN_ARCH_WINDOWS

const string StandardFileSystemManager::RootDirectoryPrefix   = ""   ;
const Ceylan::Latin1Char StandardFileSystemManager::Separator = '/'  ;

#endif // CEYLAN_ARCH_WINDOWS





// Implementation of the FileSystemManager mother class.



bool StandardFileSystemManager::existsAsEntry( const string & entryPath ) const
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	return ( ::stat( entryPath.c_str(), & buf ) == 0 ) ;

#else // CEYLAN_USES_STAT

#ifdef CEYLAN_USES__STAT

	struct _stat buf ;

	return ( ::_stat( entryPath.c_str(), & buf ) == 0 ) ;


#else // CEYLAN_USES__STAT

	throw EntryLookupFailed(
		"StandardFileSystemManager::existsAsEntry: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}



void StandardFileSystemManager::createSymbolicLink(
	const string & linkTarget, const string & linkName )
{

#if CEYLAN_USES_SYMBOLIC_LINKS

	if ( ::symlink( linkTarget.c_str(), linkName.c_str() ) == -1 )
		throw SymlinkFailed( "StandardFileSystemManager::createSymbolicLink "
			"failed when creating link '" + linkName
			+ "' which was to point to '" + linkTarget
			+ "': " + System::explainError() ) ;

#else // CEYLAN_USES_SYMBOLIC_LINKS

	throw SymlinkFailed( "StandardFileSystemManager::createSymbolicLink: "
		"symbolic link feature not available" ) ;

#endif // CEYLAN_USES_SYMBOLIC_LINKS

}



time_t StandardFileSystemManager::getEntryChangeTime( const string & entryPath )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	if ( ::stat( entryPath.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

	// "else" clause at the end of the method.

#else // CEYLAN_USES_STAT


#ifdef CEYLAN_USES__STAT

	struct _stat buf ;

	if ( ::_stat( entryPath.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

	// "else" clause at the end of the method.

#else // CEYLAN_USES__STAT

	throw GetChangeTimeFailed( "StandardFileSystemManager::getEntryChangeTime: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__STAT


#endif // CEYLAN_USES_STAT

	throw GetChangeTimeFailed(
		"StandardFileSystemManager::getEntryChangeTime: "
		"unable to get last change time for entry '" + entryPath + "': "
		+ System::explainError() ) ;

}





// Accessors to FilesystemManager constants.



const string & StandardFileSystemManager::getRootDirectoryPrefix() const
{

	return RootDirectoryPrefix ;

}



Ceylan::Latin1Char StandardFileSystemManager::getSeparator() const
{

	return Separator ;

}





// File-related section.



File & StandardFileSystemManager::createFile( const string & filename,
	OpeningFlag createFlag, PermissionFlag permissionFlag )
{

	return StandardFile::Create( filename, createFlag, permissionFlag ) ;

}



File & StandardFileSystemManager::openFile( const string & filename,
	OpeningFlag openFlag )
{

	return StandardFile::Open( filename, openFlag ) ;

}



bool StandardFileSystemManager::existsAsFileOrSymbolicLink(
	const string & filename ) const
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;


#if CEYLAN_USES_SYMBOLIC_LINKS

	// Regular file or link:
	return ( ::stat( filename.c_str(), & buf ) == 0
		&& ( S_ISREG( buf.st_mode ) || S_ISLNK( buf.st_mode ) ) ) ;

#else // CEYLAN_USES_SYMBOLIC_LINKS

	// Regular file:
	return ( ::stat( filename.c_str(), & buf ) == 0
		&& S_ISREG( buf.st_mode ) ) ;

#endif // CEYLAN_USES_SYMBOLIC_LINKS

#else // CEYLAN_USES_STAT

#ifdef CEYLAN_USES__STAT

	struct _stat buf ;

	return ( ::_stat( filename.c_str(), & buf ) == 0
		&& ( buf.st_mode & _S_IFREG ) ) ;

#else // CEYLAN_USES__STAT

	throw FileLookupFailed(
		"StandardFileSystemManager::existsAsFileOrSymbolicLink: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}



void StandardFileSystemManager::removeFile( const string & filename )
{

#ifdef CEYLAN_USES_UNLINK

	if ( ::unlink( filename.c_str() ) != 0 )
		throw FileRemoveFailed(
			"StandardFileSystemManager::removeFile failed for '"
			+ filename + "': " + System::explainError() ) ;

#else // CEYLAN_USES_UNLINK

#ifdef CEYLAN_USES__UNLINK

	if ( ::_unlink( filename.c_str() ) != 0 )
		throw FileRemoveFailed(
			"StandardFileSystemManager::removeFile failed: file '" + filename
			+ "': " + System::explainError() ) ;

#else // CEYLAN_USES__UNLINK

	throw FileRemoveFailed( "StandardFileSystemManager::removeFile: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__UNLINK

#endif // CEYLAN_USES_UNLINK

}



void StandardFileSystemManager::moveFile( const string & sourceFilename,
	const string & targetFilename )
{

#ifdef CEYLAN_USES_RENAME

	// rename can move as well:

	if ( ::rename( sourceFilename.c_str(), targetFilename.c_str() ) == -1 )
		throw FileMoveFailed(
			"StandardFileSystemManager::moveFile failed when moving '"
			+ sourceFilename + "' to '" + targetFilename + "': "
			+ System::explainError() ) ;

#else // CEYLAN_USES_RENAME


	// Moving/Renaming is copying and then removing the source file:
	try
	{

		copyFile( sourceFilename, targetFilename ) ;
		removeFile( sourceFilename ) ;

	}
	catch( const FileException & e )
	{

		throw FileMoveFailed( "StandardFileSystemManager::moveFile failed: "
			+ e.toString() ) ;

	}


#endif // CEYLAN_USES_RENAME

}



void StandardFileSystemManager::copyFile( const string & sourceFilename,
	const string & targetFilename )
{

#if CEYLAN_USES_FILE_DESCRIPTORS


	// saveAs only available with file descriptors for the moment.

	try
	{

		Ceylan::Holder<StandardFile> sourceFile(
			StandardFile::Open( sourceFilename ) ) ;

		sourceFile->saveAs( targetFilename ) ;
		sourceFile->close() ;

		// sourceFile automatically deallocated thanks to Holder.

	}
	catch ( const FileException & e )
	{
		throw FileCopyFailed(
			"StandardFileSystemManager::copyFile failed when copying '"
			+ sourceFilename + "' to '" + targetFilename + "': "
			+ e.toString() ) ;
	}


#else // if CEYLAN_USES_FILE_DESCRIPTORS

	/*
	 * This is only a hack with C++ streams.
	 *
	 * Another hack would be:
	 *
	 * File & sourceFile = File::Open( sourceFilename ) ;
	 * File & targetFile = File::Open( targetFilename ) ;
	 *
	 * targetFile._fstream << sourceFile._fstream.rdbuf() ;
	 *
	 *  - or something adapted from (slower?) -
	 *
	 * while ( true)
	 * {
	 *    char c ;
	 *	  cin.get(c) ;
	 *	  if ( cin.fail() )
	 *		  break;
	 *	  cout << c ;
	 * }
	 *
	 */

	try
	{

		Size fileSize = getSize( sourceFilename ) ;

		StandardFile & sourceFile = StandardFile::Open( sourceFilename ) ;

		// Not knowing the permissions to set with C++ Standard Library:
		StandardFile & targetFile = StandardFile::Create(
			targetFilename, File::CreateToWriteBinary, File::OwnerReadWrite ) ;

		Size written = 0 ;

		Size bufferSize = ( fileSize > File::BigBufferSize ?
			File::BigBufferSize: fileSize ) ;

		char * buf = new char[ bufferSize ] ;

		SignedSize readCount ;

		while ( written < fileSize )
		{

			Size toRead = fileSize - written ;

			if ( toRead > bufferSize )
				toRead = bufferSize ;

			try
			{

				readCount = static_cast<SignedSize>(
					sourceFile.read( buf, toRead ) ) ;

			}
			catch( const InputStream::ReadFailedException & e )
			{

				delete [] buf ;

				throw FileCopyFailed( "StandardFileSystemManager::copyFile "
					"failed when copying '" + sourceFilename + "' to '"
					+ targetFilename + "': " + e.toString() ) ;

			}


			try
			{

				targetFile.write( buf, readCount ) ;

			}
			catch( const OutputStream::WriteFailedException & e )
			{

				delete [] buf ;

				throw FileCopyFailed( "StandardFileSystemManager::copyFile "
					"failed when copying '" + sourceFilename + "' to '"
					+ targetFilename + "': " + e.toString() ) ;

			}

			written += readCount ;
		}

		delete [] buf ;

		targetFile.close() ;
		sourceFile.close() ;

	}
	catch ( const SystemException & e )
	{

		throw FileCopyFailed( "StandardFileSystemManager::copyFile "
			"failed when copying '" + sourceFilename + "' to '"
			+ targetFilename + "': " + e.toString() ) ;

	}

#endif // if CEYLAN_USES_FILE_DESCRIPTORS

}



Size StandardFileSystemManager::getSize( const string & filename )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	if ( ::stat( filename.c_str(), & buf ) == 0 )
		return static_cast<Size>( buf.st_size ) ;

	throw FileSizeRequestFailed( "StandardFileSystemManager::getSize: "
		"could not stat file '" + filename + "': " + System::explainError() ) ;

#else // CEYLAN_USES_STAT


#if CEYLAN_USES_FILE_DESCRIPTORS == 0

	// One more hack with C++ streams:

	ifstream tmpFile ;

	tmpFile.open( filename.c_str(), ios::binary ) ;

	if ( ( ! tmpFile.is_open() ) || ( ! tmpFile.good() ) )
		throw FileSizeRequestFailed(
			"StandardFileSystemManager::getSize: failed for '" + filename
			+ "': error opening file, "
			+ StandardFile::InterpretState( tmpFile ) ) ;

	tmpFile.seekg( 0, ios::end ) ;

	Size size = static_cast<Size>( tmpFile.tellg() ) ;

	tmpFile.close() ;

	return size ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw FileSizeRequestFailed( "StandardFileSystemManager::getSize: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_USES_STAT

}



time_t StandardFileSystemManager::getLastChangeTimeFile(
	const string & filename )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	if ( ::stat( filename.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

#else // CEYLAN_USES_STAT


#ifdef CEYLAN_USES__STAT

	struct _stat buf ;

	if ( ::_stat( filename.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

#else // CEYLAN_USES__STAT

	throw FileLastChangeTimeRequestFailed(
		"StandardFileSystemManager::getLastChangeTimeFile: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__STAT


#endif // CEYLAN_USES_STAT

	throw FileLastChangeTimeRequestFailed(
		"StandardFileSystemManager::getLastChangeTimeFile: "
		"unable to get last change time for file '" + filename + "': "
		+ System::explainError() ) ;

}



void StandardFileSystemManager::touch( const string & filename )
{

#ifdef CEYLAN_USES_UTIME

	if ( ::utime( filename.c_str(), 0 ) )
		throw FileTouchFailed( "StandardFileSystemManager::touch failed for '"
			+ filename + "': " + System::explainError() ) ;

#else // CEYLAN_USES_UTIME

#ifdef CEYLAN_USES__UTIME

	if ( ::_utime( filename.c_str(), 0 ) == -1 )
		throw FileTouchFailed( "StandardFileSystemManager::touch failed for '"
			+ filename + "': " + System::explainError() ) ;

#else // CEYLAN_USES__UTIME

	throw FileTouchFailed( "StandardFileSystemManager::touch "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__UTIME

#endif // CEYLAN_USES_UTIME

}





// Directory-related section.


// Factory-related subsection.


Directory & StandardFileSystemManager::createDirectory(
	const string & newDirectoryName )
{

	return StandardDirectory::Create( newDirectoryName ) ;

}



Directory & StandardFileSystemManager::openDirectory(
	const string & directoryName )
{

	return StandardDirectory::Open( directoryName ) ;

}



bool StandardFileSystemManager::existsAsDirectory(
	const string & directoryPath ) const
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	return ::stat( directoryPath.c_str(), & buf ) == 0
		&& ( S_ISDIR( buf.st_mode ) ) ;

#else // CEYLAN_USES_STAT

#ifdef CEYLAN_USES__STAT

	struct _stat buf ;

	if ( ::_stat( directoryPath.c_str(), & buf ) == 0 )
	{

		if ( buf.st_mode & _S_IFDIR )
		{

			/*
			CEYLAN_LOG( "StandardFileSystemManager::existsAsDirectory: "
				+ directoryPath + " exists and is a directory" ) ;
			*/
			return true ;

		}
		else
		{

			/*
			CEYLAN_LOG( "StandardFileSystemManager::existsAsDirectory: "
				+ directoryPath + " exists but is not a directory" ) ;
			*/
			return false ;

		}

	}
	else
	{

		/*
		CEYLAN_LOG( "StandardFileSystemManager::existsAsDirectory: "
			+ directoryPath + " is not a directory entry" ) ;
		*/

		return false ;

	}

#else // CEYLAN_USES__STAT

	throw DirectoryLookupFailed(
		"StandardFileSystemManager::existsAsDirectory: "
		"operation not supported on this platform." ) ;

#endif // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}



void StandardFileSystemManager::removeDirectory( const string & directoryPath,
	bool recursive )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw DirectoryRemoveFailed( "StandardFileSystemManager::removeDirectory:"
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if defined(CEYLAN_USES_RMDIR) || defined(CEYLAN_USES__RMDIR)


// The case without stat and _stat is not managed:

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

#else // CEYLAN_USES_STAT

	struct _stat buf ;

#endif // CEYLAN_USES_STAT

	if ( directoryPath.empty() )
		throw DirectoryRemoveFailed(
			"StandardFileSystemManager::removeDirectory: "
			"void directory specified" ) ;

	// Must be modified (leading separator):
	string thisPath = directoryPath ;

	removeLeadingSeparator( thisPath ) ;


	if ( recursive )
	{

		StandardDirectory & d = StandardDirectory::Open( thisPath ) ;

		list<string> nodes ;
		d.getEntries( nodes ) ;

		for ( list<string>::const_iterator it = nodes.begin();
			it != nodes.end(); it++ )
		{

			string newPath = joinPath( thisPath, *it ) ;

#ifdef CEYLAN_USES_STAT

			if ( ::stat( newPath.c_str(), & buf ) == 0 )

#else // CEYLAN_USES_STAT

			if ( ::_stat( newPath.c_str(), & buf ) == 0 )

#endif // CEYLAN_USES_STAT

			{

				// Unlinks symlinks and files:
#if CEYLAN_USES_SYMBOLIC_LINKS
				if ( S_ISLNK( buf.st_mode ) || ! S_ISDIR( buf.st_mode ) )
#else // CEYLAN_USES_SYMBOLIC_LINKS

#if CEYLAN_ARCH_WINDOWS
				if ( ! ( buf.st_mode & _S_IFDIR ) )
#else // CEYLAN_ARCH_WINDOWS
				if ( ! S_ISDIR( buf.st_mode ) )
#endif // CEYLAN_ARCH_WINDOWS

#endif // CEYLAN_USES_SYMBOLIC_LINKS
				{


#ifdef CEYLAN_USES_UNLINK
					if ( ::unlink( newPath.c_str() ) )
#else // CEYLAN_USES_UNLINK
					if ( ::_unlink( newPath.c_str() ) )
#endif // CEYLAN_USES_UNLINK
						throw DirectoryRemoveFailed(
							"StandardFileSystemManager::removeDirectory"
							" failed in unlink for '" + newPath + ": "
							+ System::explainError() ) ;
				}
				else

				// Deletes directories:
#if CEYLAN_ARCH_WINDOWS
				if ( buf.st_mode & _S_IFDIR )
#else // CEYLAN_ARCH_WINDOWS
				if ( S_ISDIR( buf.st_mode ) )
#endif // CEYLAN_ARCH_WINDOWS
				{
					// Recursive call:
					removeDirectory( newPath ) ;
				}

			}
			else
			{
				// stat failed:
				throw DirectoryRemoveFailed(
					"StandardFileSystemManager::removeDirectory "
					"failed in stat for " + newPath +  ": "
					+ System::explainError() ) ;
			}
		}

	}

#ifdef CEYLAN_USES_RMDIR

	if ( ::rmdir( thisPath.c_str() ) )
		throw DirectoryRemoveFailed(
			"StandardFileSystemManager::removeDirectory failed in rmdir for "
			+ thisPath + ": " + explainError() ) ;

#elif defined(CEYLAN_USES__RMDIR) // CEYLAN_USES_RMDIR

	if ( ::_rmdir( thisPath.c_str() ) )
		throw DirectoryRemoveFailed(
			"StandardFileSystemManager::removeDirectory failed in _rmdir for "
			+ thisPath + ": " + System::explainError() ) ;

#else // CEYLAN_USES_RMDIR

	throw DirectoryRemoveFailed( "StandardFileSystemManager::removeDirectory "
			"not available on this platform." ) ;

#endif // CEYLAN_USES_RMDIR



#else // CEYLAN_USES_RMDIR

	throw DirectoryRemoveFailed( "StandardFileSystemManager::removeDirectory "
			"not available on this platform." ) ;

#endif // CEYLAN_USES_RMDIR

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void StandardFileSystemManager::moveDirectory(
	const string & sourceDirectoryPath, const string & targetDirectoryPath )
{


#ifdef CEYLAN_USES_RENAME

	// rename can move as well, for directories too:

	if ( ::rename( sourceDirectoryPath.c_str(), targetDirectoryPath.c_str() )
			== -1 )
		throw DirectoryMoveFailed(
			"StandardFileSystemManager::moveDirectory failed when renaming '"
			+ sourceDirectoryPath + "' into '" + targetDirectoryPath + "': "
			+ System::explainError() ) ;

#else // CEYLAN_USES_RENAME


	// Renaming is copying and then removing the source directory:
	try
	{

		copyDirectory( sourceDirectoryPath, targetDirectoryPath ) ;
		removeDirectory( sourceDirectoryPath ) ;

	}
	catch( const DirectoryException & e )
	{

		throw DirectoryMoveFailed(
			"StandardFileSystemManager::moveDirectory failed: "
			+ e.toString() ) ;

	}


#endif // CEYLAN_USES_RENAME

}



void StandardFileSystemManager::copyDirectory(
	const string & sourceDirectoryPath, const string & targetDirectoryPath )
{

	throw DirectoryCopyFailed( "StandardFileSystemManager::copyDirectory: "
		"not supported on this platform." ) ;

}



time_t StandardFileSystemManager::getLastChangeTimeDirectory(
	const string & directoryPath )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	if ( ::stat( directoryPath.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

#else // CEYLAN_USES_STAT


#ifdef CEYLAN_USES__STAT

	struct _stat buf ;

	if ( ::_stat( directoryPath.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

#else // CEYLAN_USES__STAT

	throw DirectoryLastChangeTimeRequestFailed(
		"StandardFileSystemManager::getLastChangeTimeDirectory: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__STAT


#endif // CEYLAN_USES_STAT

	throw DirectoryLastChangeTimeRequestFailed(
		"StandardFileSystemManager::getLastChangeTimeDirectory: "
		"unable to get last change time for directory '" + directoryPath + "': "
		+ explainError() ) ;

}



bool StandardFileSystemManager::isAValidDirectoryPath(
	const string & directoryString )
{

#if CEYLAN_USES_REGEX

	string directoryPattern = "^[" + RootDirectoryPrefix
		+ Separator + "]{1,1}" ;

	Ceylan::RegExp target( directoryString ) ;

	return target.matches( directoryPattern ) ;

#else // CEYLAN_USES_REGEX

	// A priori correct:
	return true ;

#endif // CEYLAN_USES_REGEX

}



bool StandardFileSystemManager::isAbsolutePath( const string & path )
{

	if ( path.empty() )
		return false ;

	/*
	 * Starts with separator, or with prefix (if prefix is used): absolute path.
	 *
	 */

#if CEYLAN_ARCH_WINDOWS

	/* Was:

	if ( ! RootDirectoryPrefix.empty() )
		if ( path.find( RootDirectoryPrefix, 0 ) == 0 )
			return true ;
	 */

	// Prefix is: a drive letter + ':\', ex: 'c:\'
	if ( ( Ceylan::isLetter( path[0] ) ) && ( path[1] == ':' ) )
		return true ;

	return false ;

#else // CEYLAN_ARCH_WINDOWS

	return ( path[0] == Separator ) ;

#endif // CEYLAN_ARCH_WINDOWS

}



std::string StandardFileSystemManager::getCurrentWorkingDirectoryPath()
{

#ifdef CEYLAN_USES_GETCWD

	/*
	 * With following automatic variable, frame size is deemed 'too large for
	 * reliable stack checking':

	char buf[ PATH_MAX + 1 ] ;

	 * Another solution would be to use a static string, but this method would
	 * not be reentrant anymore.
	 *
	 * Hence using dynamic allocation, even if slower:
	 *
	 */

	char * buf = new char[ PATH_MAX + 1 ] ;

	if ( ::getcwd( buf, PATH_MAX ) )
	{

		string res( buf ) ;
		delete [] buf ;

		return res ;

	}
	else
	{

		delete [] buf ;

		throw DirectoryGetCurrentFailed(
			"StandardFileSystemManager::getCurrentWorkingDirectoryPath: "
			"unable to determine current directory: " + explainError() ) ;

	}

#else // CEYLAN_USES_GETCWD

#ifdef CEYLAN_USES__GETCWD


	/*
	 * With following automatic variable, frame size is deemed 'too large for
	 * reliable stack checking':

	char buf[ PATH_MAX + 1 ] ;

	 * Another solution would be to use a static string, but this method would
	 * not be reentrant anymore.
	 *
	 * Hence using dynamic allocation, even if slower:
	 *
	 */

	char * buf = new char[ PATH_MAX + 1 ] ;

	if ( ::_getcwd( buf, PATH_MAX ) )
	{

		string res( buf ) ;
		delete [] buf ;

		return res ;

	}
	else
	{

		delete [] buf ;

		throw DirectoryGetCurrentFailed(
			"StandardFileSystemManager::getCurrentWorkingdirectoryPath: "
			"unable to determine current directory: " + explainError() ) ;

	}

#else // CEYLAN_USES__GETCWD

	throw DirectoryGetCurrentFailed(
		"StandardFileSystemManager::getCurrentWorkingdirectoryPath: "
		"not available on this platform" ) ;

#endif // CEYLAN_USES__GETCWD

#endif // CEYLAN_USES_GETCWD

}



void StandardFileSystemManager::changeWorkingDirectory(
	const string & newWorkingDirectory )
{

#ifdef CEYLAN_USES_CHDIR

	if ( ::chdir( newWorkingDirectory.c_str() ) != 0 )

#else // CEYLAN_USES_CHDIR

#ifdef CEYLAN_USES__CHDIR

	if ( ::_chdir( newWorkingDirectory.c_str() ) != 0 )

#else // CEYLAN_USES__CHDIR

	throw DirectoryChangeFailed(
		"StandardFileSystemManager::changeWorkingDirectory: "
		"not supported on this platform" ) ;

#endif // CEYLAN_USES__CHDIR

#endif // CEYLAN_USES_CHDIR

		// The indendation is correct, we are after a 'if'!
		throw DirectoryChangeFailed(
			"StandardFileSystemManager::changeWorkingDirectory: "
			"unable to change current working directory to "
			+ newWorkingDirectory + ": " + explainError() ) ;

}





// StandardFileSystemManager own section.


const string StandardFileSystemManager::toString(
	Ceylan::VerbosityLevels level ) const
{

  if ( _trackOpenFiles )
	return "Standard filesystem manager " + listOpenFiles() ;
  else
	return "Standard filesystem manager (not tracking open files)" ;

}




// Static section.


StandardFileSystemManager &
	StandardFileSystemManager::GetStandardFileSystemManager()
{

	if ( _StandardFileSystemManager == 0 )
	  _StandardFileSystemManager = new StandardFileSystemManager(
		/* track opened files */ true ) ;

	return *_StandardFileSystemManager ;

}



void StandardFileSystemManager::RemoveStandardFileSystemManager()
{

	if ( _StandardFileSystemManager != 0 )
	{

		if ( FileSystemManager::_CurrentDefaultFileSystemManager ==
				_StandardFileSystemManager )
			FileSystemManager::_CurrentDefaultFileSystemManager = 0 ;

		delete _StandardFileSystemManager ;
		_StandardFileSystemManager = 0 ;

	}

}




StandardFileSystemManager::StandardFileSystemManager( bool trackOpenedFiles ) :
  FileSystemManager( trackOpenedFiles )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw StandardFileSystemManagerException(
		"StandardFileSystemManager constructor: "
		"not supported on the Nintendo DS platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



StandardFileSystemManager::~StandardFileSystemManager() throw()
{

	if ( _StandardFileSystemManager == this )
		_StandardFileSystemManager = 0 ;

	// Nothing else to switch off this filesystem.

}







// Protected section.



FileDescriptor StandardFileSystemManager::Duplicate( FileDescriptor fd )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw DuplicateFailed( "StandardFileSystemManager::Duplicate failed: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	FileDescriptor newFD = ::dup( fd ) ;

	if ( newFD == -1 )
		throw DuplicateFailed(
			"StandardFileSystemManager::Duplicate failed for file descriptor "
			+ Ceylan::toString( fd ) + "." ) ;

	return newFD ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw DuplicateFailed( "StandardFileSystemManager::Duplicate: "
		"file descriptor feature not available." ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}
