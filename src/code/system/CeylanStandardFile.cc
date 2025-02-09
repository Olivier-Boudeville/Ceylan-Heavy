/*
 * Copyright (C) 2003-2013 Olivier Boudeville
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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#include "CeylanStandardFile.h"

#include "CeylanLogPlug.h"                    // for Log primitives
#include "CeylanOperators.h"                  // for toString
#include "CeylanStandardFileSystemManager.h"  // for StandardFileSystemManager
#include "CeylanUtils.h"                      // for checkpoint

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



// Not available in their C++ form:
extern "C"
{

#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for mode_t
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SYS_STAT_H
#include <sys/stat.h>          // for mode_t
#endif // CEYLAN_USES_SYS_STAT_H

#ifdef CEYLAN_USES_FCNTL_H
#include <fcntl.h>             // for mode_t
#endif // CEYLAN_USES_FCNTL_H

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>            // for stat
#endif // CEYLAN_USES_UNISTD_H

}



#include <cerrno>    // for EINTR, ENOLCK, etc.
#include <cstdio>    // for unlink, ftell, etc.
#include <fstream>   // for this class



/*
 * Implementation notes.
 *
 * @note In a non-static method, no static method should be used, as the former
 * is expected to use the standard filesystem manager, whereas the latter shall
 * use the default filesystem manager, which may or may not be the standard one.
 *
 */



using std::fstream ;
using std::ifstream ;

using std::ios ;
using std::ios_base ;

using std::string ;


using namespace Ceylan ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;



/*
 * We will be using here either file-descriptor based file I/O, or the one
 * offered by the C++ Standard Library (fstream and al).
 *
 */


// To test the implementation based on standard C++ streams:
/*
#ifdef CEYLAN_USES_FILE_DESCRIPTORS
	#undef CEYLAN_USES_FILE_DESCRIPTORS
	#define CEYLAN_USES_FILE_DESCRIPTORS 0
#endif // CEYLAN_USES_FILE_DESCRIPTORS
*/


#if CEYLAN_USES_FILE_DESCRIPTORS

// Avoid exposing system-dependent mode_t in the headers:
struct StandardFile::SystemSpecificPermissionFlag
{

	mode_t _mode ;

} ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS





/**
 * Read/write operations can be performed with both the inner low-level rdbuf
 * object of the fstream instances, or directly with the fstream itself.
 *
 * The former is preferred to the latter, since the number of read/written bytes
 * is explicitly known. After tests, the two methods seem to behave the same way
 * (at least with gcc).
 *
 */
#define CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM 1



StandardFileException::StandardFileException( const string & reason ) :
	FileException( reason )
{

}



StandardFileException::~StandardFileException() throw()
{

}





// StandardFile implementation.


StandardFile::~StandardFile() throw()
{

	if ( isOpen() )
	{

		try
		{
			close() ;
		}
		catch( const Stream::CloseException & e )
		{
			LogPlug::error( "StandardFile destructor: close failed: "
				+ e.toString() ) ;
		}

	}

}




// Constructors are in protected section.




// Implementation of instance methods inherited from File.


bool StandardFile::isOpen() const
{

#if CEYLAN_ARCH_NINTENDO_DS

	 return false ;

#else // CEYLAN_ARCH_NINTENDO_DS


#if CEYLAN_USES_FILE_DESCRIPTORS

	return ( _fdes != 0 ) ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	return ( _fstream.is_open() ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



bool StandardFile::close()
{

	//Ceylan::checkpoint( "StandardFile::close called." ) ;

	if ( ! isOpen() )
	{

		LogPlug::warning( "StandardFile::close: file '" +  _name
			+ "' does not seem to have been already opened." ) ;

		return false ;

	}
	else
	{

		// Let's close it.

#if CEYLAN_ARCH_NINTENDO_DS

		throw Stream::CloseException( "StandardFile::close: "
			"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

		/*
		 * Exception and returned boolean are both needed:
		 *
		 *  - all closed files should have been opened previously
		 *  - returned boolean comes from the Stream-inherited signature
		 *
		 */

		// getCorrespondingFileSystemManager() would require a cast to standard:
		StandardFileSystemManager & manager =
		  StandardFileSystemManager::GetStandardFileSystemManager() ;

		manager.declareFileClosing( *this ) ;

#if CEYLAN_USES_FILE_DESCRIPTORS

		// Zeroes _fdes (transmitted by reference):
		return Stream::Close( _fdes ) ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

		_fstream.close() ;
		return true ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

	}

}



void StandardFile::saveAs( const string & newName )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileException( "StandardFile::saveAs: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	// Using the helper factory directly to access to the file descriptor:
	StandardFile & f = StandardFile::Create( newName ) ;
	serialize( f._fdes ) ;
	delete &f ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw FileException(
		"StandardFile::saveAs: not implemented on this platform." ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}




// Locking section.



/**
 * Under Windows-based systems, usual locking primitives do not seem available,
 * at least not with MinGW.
 *
 * So they are currently disabled, even though in
 * mingw-runtime-3.9/include/sys/locking.h constants are defined apparently for
 * '_CRTIMP int __cdecl _locking (int, int, long)', which is defined in
 * mingw-runtime-3.9/include/io.h
 *
 */



void StandardFile::lockForReading() const
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileReadLockingFailed( "StandardFile::lockForReading: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type	= F_RDLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start	= 0 ;
	lk.l_len	= 0 ;

	while ( true )
	{

		if ( ::fcntl( _fdes, F_SETLKW, &lk ) == - 1 )
		{

			ErrorCode error = getError() ;

			if ( error == EINTR  )
				continue ;

			if ( error != ENOLCK )
				throw FileReadLockingFailed(
					"StandardFile::lockForReading: locking failed for '"
						+ _name + "': " + System::explainError() ) ;
			else
				LogPlug::warning( "StandardFile::lockForReading: "
					"lock for reading failed for '" + _name
					+ "': check your NFS daemons (if any)." ) ;

			break ;
		}
		else
		{
			break ;
		}

	}

#else // CEYLAN_USES_FILE_LOCKS

	throw FileReadLockingFailed( "StandardFile::lockForReading: "
		"lock feature not available" ) ;

#endif // CEYLAN_USES_FILE_LOCKS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void StandardFile::unlockForReading() const
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileReadUnlockingFailed( "StandardFile::unlockForReading: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type	= F_UNLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start	= 0 ;
	lk.l_len	= 0 ;

	while ( true )
	{
		if ( ::fcntl( _fdes, F_SETLKW, &lk ) == - 1 )
		{
			if ( errno == EINTR  )
				continue ;

			if ( errno != ENOLCK )
				throw FileReadUnlockingFailed(
					"StandardFile::unlockForReading: unlocking failed for '"
						+ _name + "': " + System::explainError() ) ;
			else
				LogPlug::warning( "StandardFile::unlockForReading: "
					"unlock for reading failed for '" + _name
					+ "': check your NFS daemons (if any)." ) ;
			break ;
		}
		else
		{
			break ;
		}
	}

#else // CEYLAN_USES_FILE_LOCKS

	throw FileReadUnlockingFailed(
		"StandardFile::unlockForReading: lock feature not available" ) ;

#endif // CEYLAN_USES_FILE_LOCKS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void StandardFile::lockForWriting() const
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileWriteLockingFailed( "StandardFile::lockForWriting: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type	= F_WRLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start	= 0 ;
	lk.l_len	= 0 ;

	while ( true )
	{
		if ( ::fcntl( _fdes, F_SETLKW, &lk ) == - 1 )
		{
			if ( errno == EINTR  )
				continue ;

			if ( errno != ENOLCK )
				throw FileWriteLockingFailed(
					"StandardFile::lockForWriting: locking failed for '"
						+ _name + "': " + System::explainError() ) ;
			else
				LogPlug::warning( "StandardFile::lockForWriting: "
					"lock for writing failed for '" + _name
					+ "': check your NFS daemons (if any)." ) ;
			break ;
		}
		else
		{
			break ;
		}
	}

#else // CEYLAN_USES_FILE_LOCKS

	throw FileWriteLockingFailed( "StandardFile::lockForWriting: "
		"lock feature not available" ) ;

#endif // CEYLAN_USES_FILE_LOCKS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void StandardFile::unlockForWriting() const
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileWriteUnlockingFailed( "StandardFile::unlockForWriting: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type	= F_UNLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start	= 0 ;
	lk.l_len	= 0 ;

	while ( true )
	{

		if ( ::fcntl( _fdes, F_SETLKW, &lk ) == - 1 )
		{
			if ( errno == EINTR  )
				continue;

			if ( errno != ENOLCK )
				throw FileWriteUnlockingFailed(
					"StandardFile::unlockForWriting: unlocking failed for '"
						+ _name + "': " + System::explainError() ) ;
			else
				LogPlug::warning( "StandardFile::unlockForWriting: "
					"unlock for writing failed for '" + _name
					+ "': check your NFS daemons (if any)." ) ;
			break ;
		}
		else
		{
			break ;
		}
	}

#else // CEYLAN_USES_FILE_LOCKS

	throw FileWriteUnlockingFailed( "StandardFile::unlockForWriting: "
		"lock feature not available" ) ;

#endif // CEYLAN_USES_FILE_LOCKS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



bool StandardFile::isLocked() const
{

#if CEYLAN_ARCH_NINTENDO_DS

	return false ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type	= F_WRLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start	= 0 ;
	lk.l_len	= 0 ;
	lk.l_pid	= 0 ;

	while ( true )
	{
		if ( ::fcntl( _fdes, F_GETLK, &lk ) == - 1 )
		{
			if ( errno == EINTR  )
				continue ;
			else
				LogPlug::warning( "StandardFile::isLocked: "
					"lock ckecking failed for '" + _name
					+ "': check your NFS daemons (if any)." ) ;
		}
		break ;
	}

	return ( lk.l_pid != 0 ) ;

#else // CEYLAN_USES_FILE_LOCKS

	return false ;

#endif // CEYLAN_USES_FILE_LOCKS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



time_t StandardFile::getLastChangeTime() const
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileLastChangeTimeRequestFailed( "StandardFile::getLastChangeTime:"
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	if ( ::stat( _name.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

	throw FileLastChangeTimeRequestFailed(
		"StandardFile::getLastChangeTime failed for '"
		+ _name + "': " + System::explainError() ) ;

#else // CEYLAN_USES_STAT

	throw FileLastChangeTimeRequestFailed(
		"StandardFile::getLastChangeTime: not available on this platform." ) ;

#endif // CEYLAN_USES_STAT

#endif // CEYLAN_ARCH_NINTENDO_DS

}



Size StandardFile::read( Ceylan::Byte * buffer, Size maxLength )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw InputStream::ReadFailedException( "StandardFile::read:"
		"not supported on the Nintendo DS platform." ) ;


#else // CEYLAN_ARCH_NINTENDO_DS

	setSelected( false ) ;

#if CEYLAN_USES_FILE_DESCRIPTORS

	try
	{

		return System::FDRead( _fdes, buffer, maxLength ) ;

	}
	catch( const Ceylan::Exception & e )
	{
		throw InputStream::ReadFailedException(
			"StandardFile::read failed for file '" + _name + "': "
			+ e.toString() ) ;
	}

#else // CEYLAN_USES_FILE_DESCRIPTORS


#if CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM

	SignedSize n = ( _fstream.rdbuf() )->sgetn( buffer,
		static_cast<std::streamsize>( maxLength ) ) ;

	if ( ! _fstream.good() )
		throw InputStream::ReadFailedException(
			"StandardFile::read failed for file '" + _name
			+ "': " + interpretState() ) ;


	// Actually, n may be negative.
	if ( n < 0 )
		throw InputStream::ReadFailedException(
			"StandardFile::read failed for file '"
			+ _name + "': negative size read" ) ;

	return static_cast<Size>( n ) ;

#else // CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM


	SignedSize n = _fstream.readsome( buffer,
		static_cast<std::streamsize>( maxLength ) ) ;

	if ( ! _fstream.good() )
		throw InputStream::ReadFailedException(
			"StandardFile::read failed for file '" + _name
			+ "': " + interpretState() ) ;

	// Actually, n may be negative.
	if ( n < 0 )
		throw InputStream::ReadFailedException(
			"StandardFile::read failed for file '"
			+ _name + "': negative size read" ) ;

	return static_cast<Size>( n ) ;

#endif // CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



Size StandardFile::write( const string & message )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw OutputStream::WriteFailedException( "StandardFile::write:"
		"not supported on the Nintendo DS platform." ) ;


#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	try
	{

		return System::FDWrite( _fdes, message.c_str(), message.size() ) ;

	}
	catch( const Ceylan::Exception & e )
	{
		throw OutputStream::WriteFailedException(
			"StandardFile::write failed for file '" + _name + "': "
			+ e.toString() ) ;
	}

#else // if CEYLAN_USES_FILE_DESCRIPTORS

	// Will trigger relevant exceptions if needed:
	return write( message.c_str(),
		static_cast<std::streamsize>( message.size() ) ) ;


#endif // if CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



Position StandardFile::tell()
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileException( "StandardFile::tell:"
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	/*
	 * Could not find a ::ftell-like function operating on file descriptors
	 * instead of on FILE pointers, thus using ::lseek for that purpose, not
	 * changing the position but reading it only:
	 */
	off_t pos = ::lseek( _fdes, /* offset */ 0, SEEK_CUR ) ;

	if ( pos < 0 )
		throw FileException( "StandardFile::tell failed for '" + _name
			+ "': " + System::explainError() ) ;

	return pos ;

#else // if CEYLAN_USES_FILE_DESCRIPTORS

	// tellg and tellp should be the same:
	Position pos = _fstream.tellg() ;

	if ( ! _fstream.good() )
		throw FileException( "StandardFile::tell failed for '" + _name
			+ "': " + interpretState() ) ;

   if ( pos < 0 )
		throw FileException( "StandardFile::tell failed for '" + _name
			+ "'" ) ;

	return pos ;

#endif // if CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void StandardFile::seek( Position targetPosition )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileException( "StandardFile::seek:"
		"not supported on the Nintendo DS platform." ) ;


#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	// ::fseek only for FILE*:
	int res = ::lseek( _fdes, targetPosition, SEEK_SET ) ;

	if ( res < 0 )
		throw FileException( "StandardFile::seek failed for '" + _name
			+ "': " + System::explainError() ) ;

#else // if CEYLAN_USES_FILE_DESCRIPTORS

	// seekg and seekp should be the same:
	_fstream.seekg( targetPosition ) ;

	if ( ! _fstream.good() )
		throw FileOpeningFailed( "StandardFile::seek failed for '" + _name
			+ "': " + interpretState() ) ;

#endif // if CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}




Size StandardFile::write( const Ceylan::Byte * buffer, Size maxLength )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw OutputStream::WriteFailedException( "StandardFile::write:"
		"not supported on the Nintendo DS platform." ) ;


#else // CEYLAN_ARCH_NINTENDO_DS


#if CEYLAN_USES_FILE_DESCRIPTORS

	try
	{

		return System::FDWrite( _fdes, buffer, maxLength ) ;

	}
	catch( const Ceylan::Exception & e )
	{
		throw OutputStream::WriteFailedException(
			"StandardFile::write failed for file '" + _name + "': "
			+ e.toString() ) ;
	}

#else // CEYLAN_USES_FILE_DESCRIPTORS



#if CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM


	SignedSize n = ( _fstream.rdbuf() )->sputn( buffer,
		static_cast<std::streamsize>( maxLength ) ) ;

	if ( n < 0 )
		throw OutputStream::WriteFailedException(
			"StandardFile::write failed for file '"
			+ _name + "': negative size written" ) ;

	Size realSize = static_cast<Size>( n ) ;

	if ( realSize < maxLength )
		throw OutputStream::WriteFailedException(
			"StandardFile::write failed for file '" + _name
			+ "', fewer bytes wrote than expected: " + interpretState() ) ;

	/*
	 * Probably useless:

	if ( ! _fstream.good() )
		throw WriteFailed( "File::write failed for file '" + _name
			+ "': " + interpretState() ) ;
	 *
	 */


	if ( Synchronous & _openFlag )
		_fstream.flush() ;

	return realSize ;


#else // CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM


	_fstream.write( buffer, static_cast<std::streamsize>( maxLength ) ) ;

	if ( ! _fstream.good() )
		throw WriteFailed( "StandardFile::write failed for file '" + _name
			+ "': " + interpretState() ) ;

	if ( Synchronous & _openFlag )
		_fstream.flush() ;

	/*
	 * Drawback of the fstream-based method: the written size is not really
	 * known.
	 *
	 */
	return maxLength ;


#endif // CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}




// StandardFile-specific methods.


void StandardFile::serialize( FileDescriptor fd ) const
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw StandardFileException( "StandardFile::serialize: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	// Let StandardFileException propagate:
	FromFDtoFD( _fdes, fd, size() ) ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw StandardFileException( "StandardFile::serialize: "
		"file descriptor feature not available" ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



FileDescriptor StandardFile::getFileDescriptor() const
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	return _fdes ;

#else // if CEYLAN_USES_FILE_DESCRIPTORS

	throw StandardFileException(
		"StandardFile::getFileDescriptor: "
		"file descriptor feature not available" ) ;

#endif // if CEYLAN_USES_FILE_DESCRIPTORS

}




StreamID StandardFile::getStreamID() const
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	return static_cast<StreamID>( getFileDescriptor() ) ;

#else // if CEYLAN_USES_FILE_DESCRIPTORS

	// No appropriate identifier found:
	return -1 ;

#endif // if CEYLAN_USES_FILE_DESCRIPTORS

}



const std::string StandardFile::toString( Ceylan::VerbosityLevels level ) const
{

	string res = "Standard file object for filename '" + _name + "'" ;

#if CEYLAN_USES_FILE_DESCRIPTORS
	res += ", with file descriptor " + Ceylan::toString( getFileDescriptor() ) ;
#endif // CEYLAN_USES_FILE_DESCRIPTORS

	res += ", with opening openFlags = " + Ceylan::toString( _openFlag )
		+ ", with mode openFlags = " + Ceylan::toString( _permissions ) ;

#if CEYLAN_USES_FILE_LOCKS

	if ( _lockedForReading )
		res += ", locked for reading" ;
	else
		res += ", not locked for reading" ;

	if ( _lockedForWriting )
		res += ", locked for writing" ;
	else
		res += ", not locked for writing" ;

#endif // CEYLAN_USES_FILE_LOCKS

	return res ;

}



string StandardFile::InterpretState( const ifstream & inputFile )
{

	if ( inputFile.good() )
		return "Standard file is in clean state (no error)" ;

	string res = "Standard file in error" ;

	if ( inputFile.rdstate() & ifstream::badbit )
		res += ", critical error in stream buffer" ;

	if ( inputFile.rdstate() & ifstream::eofbit )
		res += ", End-Of-File reached while extracting" ;

	if ( inputFile.rdstate() & ifstream::failbit )
		res += ", failure extracting from stream" ;

	return res ;

}



string StandardFile::InterpretState( const fstream & file )
{

	if ( file.good() )
		return "Standard file is in clean state (no error)" ;

	string res = "Standard file in error" ;

	if ( file.rdstate() & fstream::badbit )
		res += ", critical error in stream buffer" ;

	if ( file.rdstate() & fstream::eofbit )
		res += ", End-Of-File reached while extracting" ;

	if ( file.rdstate() & fstream::failbit )
		res += ", failure extracting from stream" ;

	return res ;

}



StandardFile & StandardFile::Create( const std::string & filename,
	OpeningFlag createFlag, PermissionFlag permissionFlag )
{

	// Ensures creation is requested:
	return * new StandardFile( filename, createFlag | File::CreateFile,
		permissionFlag ) ;

}



StandardFile & StandardFile::Open( const std::string & filename,
	OpeningFlag openFlag )
{

	// Ensures creation is not requested:
	return * new StandardFile( filename, openFlag & ~File::CreateFile
		/* use default permission flag */ ) ;

}




// Protected section.


StandardFile::StandardFile( const string & name, OpeningFlag openFlag,
		PermissionFlag permissions ) :
  File( name, openFlag, permissions ),
  _fdes( 0 )
{

	// (File constructor may raise FileException)

#if CEYLAN_ARCH_NINTENDO_DS

	throw StandardFileException( "StandardFile constructor: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

	if ( openFlag != DoNotOpen )
		reopen() ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}





// Implementations of inherited methods.


FileSystemManager & StandardFile::getCorrespondingFileSystemManager() const
{

	try
	{

		return StandardFileSystemManager::GetStandardFileSystemManager() ;

	}
	catch( const StandardFileSystemManagerException & e )
	{

		throw FileDelegatingException(
			"StandardFile::getCorrespondingFileSystemManager failed: "
			+ e.toString() ) ;

	}

}



void StandardFile::reopen()
{

	//Ceylan::checkpoint( "StandardFile::reopen called." ) ;

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileOpeningFailed( "StandardFile::reopen: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	SystemSpecificPermissionFlag myMode ;

	ConvertToFileDescriptorPermissionFlag( _permissions, myMode ) ;

	_fdes = ::open( _name.c_str(), ConvertToFileDescriptorOpenFlag( _openFlag ),
		myMode._mode ) ;

	if ( _fdes < 0 )
		throw FileOpeningFailed( "StandardFile::reopen failed for '" + _name
			+ "': " + System::explainError() ) ;

#else // if CEYLAN_USES_FILE_DESCRIPTORS


	// Second, take care of the permission openFlag:

	/*
	 * With this implementation, _permissions is ignored, no
	 * ConvertToStreamPermissionFlag used.
	 *
	 */

	/*
	 * If Synchronous is requested, turn off the buffering before any I/O
	 * operation:
	 *
	 * @see http://gcc.gnu.org/onlinedocs/libstdc++/27_io/howto.html
	 *
	 * However actual tests, by triggering a division by zero crash, showed that
	 * the 'pubsetbuf' call is not enough, since logs may be lost
	 * nevertheless. Adding the conditional 'flush' in the 'write' method works.
	 *
	 */
	if ( Synchronous & _openFlag )
		_fstream.rdbuf()->pubsetbuf( 0, 0 ) ;

	_fstream.open( _name.c_str(),
		ConvertToStreamOpenFlag( _openFlag, _name ) ) ;

	// if ( ! _fstream ) ... could be used as well.

	if ( ! _fstream.is_open() )
		throw FileOpeningFailed( "StandardFile::reopen failed for '" + _name
			+ "': error opening file, " + interpretState() ) ;

	if ( ! _fstream.good() )
		throw FileOpeningFailed( "StandardFile::reopen failed for '" + _name
			+ "': " + interpretState() ) ;

#endif // if CEYLAN_USES_FILE_DESCRIPTORS

	// getCorrespondingFileSystemManager() would require a cast to standard:
	StandardFileSystemManager & manager =
	  StandardFileSystemManager::GetStandardFileSystemManager() ;

	manager.declareFileOpening( *this ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS


}



string StandardFile::interpretState() const
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	return "StandardFile uses file descriptor " + Ceylan::toString( _fdes ) ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	return InterpretState( _fstream ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

}




// Conversion helper subsection.


int StandardFile::ConvertToFileDescriptorOpenFlag( OpeningFlag openFlag )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw ConversionFailed( "StandardFile ConvertToFileDescriptorOpenFlag: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS



#if CEYLAN_USES_FILE_DESCRIPTORS


#if CEYLAN_DEBUG

	if ( openFlag == DoNotOpen )
		throw ConversionFailed(
			"StandardFile::ConvertToFileDescriptorOpenFlag: "
			"flags specify that the file is not to be opened." ) ;

#endif // CEYLAN_DEBUG


	int actualFlags = 0 ;

	if ( Read & openFlag )
	{
		if ( Write & openFlag )
			actualFlags |= O_RDWR ;
		else
			actualFlags |= O_RDONLY ;
	}
	else
	{
		if ( Write & openFlag )
			actualFlags |= O_WRONLY ;
	}

	if ( CreateFile & openFlag )
		actualFlags |= O_CREAT ;

	if ( TruncateFile & openFlag )
		actualFlags |= O_TRUNC ;

	if ( AppendFile & openFlag )
		actualFlags |= O_APPEND ;

	// Binary: nothing to do here, with file descriptors.


#if CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

	if ( NonBlocking & openFlag )
		actualFlags |= O_NONBLOCK ;

	if ( Synchronous & openFlag )
		actualFlags |= O_SYNC ;

#endif // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

	return actualFlags ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw ConversionFailed( "StandardFile::ConvertToFileDescriptorOpenFlag: "
		"file descriptor feature is not available" ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void StandardFile::ConvertToFileDescriptorPermissionFlag(
	PermissionFlag permissionFlag,
	struct SystemSpecificPermissionFlag & returned )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw ConversionFailed(
		"StandardFile ConvertToFileDescriptorPermissionFlag: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS


#if CEYLAN_USES_FILE_DESCRIPTORS

	mode_t actualPermissions = 0 ;

	if ( permissionFlag & OwnerRead )
		actualPermissions |= S_IRUSR ;

	if ( permissionFlag & OwnerWrite )
		actualPermissions |= S_IWUSR ;

	if ( permissionFlag & OwnerExec )
		actualPermissions |= S_IXUSR ;

#if CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES


	if ( permissionFlag & GroupRead )
		actualPermissions |= S_IRGRP ;

	if ( permissionFlag & GroupWrite )
		actualPermissions |= S_IWGRP ;

	if ( permissionFlag & GroupExec )
		actualPermissions |= S_IXGRP ;


	if ( permissionFlag & OthersRead )
		actualPermissions |= S_IROTH ;

	if ( permissionFlag & OthersWrite )
		actualPermissions |= S_IWOTH ;

	if ( permissionFlag & OthersExec )
		actualPermissions |= S_IXOTH ;


#endif // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

	returned._mode = actualPermissions ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw ConversionFailed(
		"StandardFile::ConvertToFileDescriptorPermissionFlag: "
		"file descriptor feature is not available" ) ;

#endif	 // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



ios_base::openmode StandardFile::ConvertToStreamOpenFlag(
	OpeningFlag openFlag, const std::string & filename )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw ConversionFailed( "StandardFile ConvertToStreamOpenFlag: "
		"not supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS


#if CEYLAN_DEBUG

	if ( openFlag == DoNotOpen )
		throw ConversionFailed( "StandardFile::ConvertToStreamOpenFlag: "
			"flags specify that the file is not to be opened." ) ;

#endif // CEYLAN_DEBUG


	ios_base::openmode actualFlags =
		static_cast<ios_base::openmode>( 0 ) ;

	if ( Read & openFlag )
		actualFlags |= fstream::in ;

	if ( Write & openFlag )
		actualFlags |= fstream::out ;

	// If CreateFile is not set, then the file is expected to exist already:
	if ( ! ( CreateFile & openFlag ) )
	{

		// No creation requested, checking the file already exists:

		bool alreadyExisting = false ;

		try
		{

			alreadyExisting = StandardFileSystemManager::GetStandardFileSystemManager().existsAsFileOrSymbolicLink( filename ) ;

		}
		catch( const FileException & e )
		{

			throw ConversionFailed( "StandardFile::ConvertToStreamOpenFlag: "
				"error while checking the existence of file: "
				+ e.toString() ) ;

		}

		if ( ! alreadyExisting )
			throw ConversionFailed(
				"StandardFile::ConvertToStreamOpenFlag: the file '"
				+ filename + "' does not exist whereas it should "
				"( no 'CreateFile' attribute in opening flag)" ) ;

	}

	if ( TruncateFile & openFlag )
		actualFlags |= fstream::trunc ;

	if ( AppendFile & openFlag )
		actualFlags |= fstream::app ;

	if ( Binary & openFlag )
		actualFlags |= fstream::binary ;

	// fstream::ate not used.

	return actualFlags ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}





// Private section.



void StandardFile::FromFDtoFD( FileDescriptor from, FileDescriptor to,
	Size length )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	Size written = 0 ;

	Size bufferSize = ( length > BigBufferSize ? BigBufferSize: length ) ;

	Ceylan::Byte * buf = new Ceylan::Byte[ bufferSize ] ;

	Size readCount ;

	try
	{

		while ( written < length )
		{

			Size toRead = length - written ;

			if ( toRead > bufferSize )
				toRead = bufferSize ;

			readCount = FDRead( from, buf, toRead ) ;

			FDWrite( to, buf, readCount ) ;

			written += readCount ;

		}

		delete [] buf ;

	} // Most direct common mother exception:
	catch( const Ceylan::Exception & e )
	{

		delete [] buf ;
		throw StandardFileException(
			"StandardFile::FromFDtoFD failed: " + e.toString() ) ;

	}

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw StandardFileException( "StandardFile::FromFDtoFD: "
		"file descriptor feature not available" ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

}
