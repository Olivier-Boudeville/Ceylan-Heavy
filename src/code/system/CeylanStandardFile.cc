#include "CeylanStandardFile.h"

#include "CeylanLogPlug.h"     // for Log primitives
#include "CeylanDirectory.h"   // for Directory
#include "CeylanOperators.h"   // for toString
#include "CeylanStringUtils.h" // for StringSize


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

#ifdef CEYLAN_USES_UTIME_H
#include <utime.h>             // for utime     
#endif // CEYLAN_USES_UTIME_H

#ifdef CEYLAN_USES_SYS_UTIME_H
#include <sys/utime.h>         // for utime     
#endif // CEYLAN_USES_SYS_UTIME_H

}


#include <cerrno>    // for EINTR, ENOLCK, etc.
#include <cstdio>    // for unlink
#include <fstream>   // for this class


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



#if CEYLAN_USES_FILE_DESCRIPTORS

// Avoid exposing system-dependent mode_t in the headers:
struct StandardFile::SystemSpecificPermissionFlag
{
	mode_t _mode ;	
	
} ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS





/**
 * Read/write operations can be performed with both the inner low-level 
 * rdbuf object of the fstream instances, or directly with the fstream itself.
 *
 * The former is preferred to the latter, since the number of read/written
 * bytes is explicitly known. After tests, the two methods seem to behave
 * the same way (at least with gcc).
 *
 */ 
#define CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM 1



StandardFile::StandardFileException::StandardFileException( 
		const string & reason ) throw():
	FileException( reason )
{

}


StandardFile::StandardFileException::~StandardFileException() throw()
{

}



StandardFile::ConversionFailed::ConversionFailed( const string & reason )
		throw():
	StandardFile::StandardFileException( reason )
{

}
		


// Constructors and destructor are in protected section.	
	
	
	
bool StandardFile::close() throw( Stream::CloseException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
	 throw Stream::CloseException( 
	 	"StandardFile::close: no supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS
		
	/*
	 * Exception and returned boolean are both needed:
	 *  - all closed files should have been opened previously
	 *  - returned boolean comes from the Stream-inherited signature
	 *
	 */
	 
#if CEYLAN_USES_FILE_DESCRIPTORS
	
	if ( _fdes > 0 )
	{
		return Stream::Close( _fdes ) ;
	}	
	else
	{

		throw CloseException( "StandardFile::close: file '" +  _name 
			+ "' does not seem to have been already opened." ) ;
			
	}

#else // CEYLAN_USES_FILE_DESCRIPTORS

	if ( _fstream.is_open() )
	{
		_fstream.close() ;
		return true ;
	}	
	else
	{
	
		throw CloseException( "StandardFile::close: file '" +  _name 
			+ "' does not seem to have been already opened." ) ;
			
	}
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS
	
	
}


void StandardFile::remove() throw( FileSystemManager::RemoveFailed )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileSystemManager::RemoveFailed( "StandardFile::remove: "
		"no supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS
		
	try
	{
		close() ;
	}
	catch( const Stream::CloseException & e )
	{
		throw FileSystemManager::RemoveFailed( "StandardFile::remove failed: " 
			+ e.toString() ) ;
	}


	try
	{
		GetStandardFileSystemManager().remove( _name ) ;
	}
	catch( const StandardFileSystemManagerException & e )
	{
		throw RemoveFailed( "StandardFile::remove failed: " + e.toString() ) ;
	}
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



void StandardFile::serialize( FileDescriptor fd ) const 
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw Features::FeatureNotAvailableException( "StandardFile::serialize: "
		"no supported on the Nintendo DS platform." ) ;
	
#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	FromFDtoFD( _fdes, fd, size() ) ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw Features::FeatureNotAvailableException( "StandardFile::serialize: "
		"file descriptor feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void StandardFile::saveAs( const string & newName ) throw( FileException )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw FileException( "StandardFile::saveAs: "
		"no supported on the Nintendo DS platform." ) ;
		
#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	File & f = File::Open( newName ) ;
	serialize( f._fdes ) ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw FileException( 
		"StandardFile::saveAs: not implemented on this platform." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}




// Lock section.


/**
 * Under Windows-based systems, usual locking primitives 
 * do not seem available, at least not with MinGW.
 *
 * So they are currently disabled, even though in 
 * mingw-runtime-3.9/include/sys/locking.h constants are 
 * defined apparently for
 * '_CRTIMP int __cdecl _locking (int, int, long)', which is
 * defined in mingw-runtime-3.9/include/io.h
 *
 */

void StandardFile::lockForReading() const throw( ReadLockingFailed )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw ReadLockingFailed( "StandardFile::lockForReading: "
		"no supported on the Nintendo DS platform." ) ;
		
#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type 	= F_RDLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start 	= 0 ;
	lk.l_len	= 0 ;

	while ( true )
	{
	
		if ( ::fcntl( _fdes, F_SETLKW, &lk ) == - 1 )
		{
		
			ErrorCode error = getError() ;
			
			if ( error == EINTR  )
				continue ;

			if ( error != ENOLCK )
				throw ReadLockingFailed( 
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

	throw ReadLockingFailed( "StandardFile::lockForReading: "
		"lock feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_LOCKS	

#endif // CEYLAN_ARCH_NINTENDO_DS

}


void StandardFile::unlockForReading() const throw( ReadUnlockingFailed )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw ReadUnlockingFailed( "StandardFile::unlockForReading: "
		"no supported on the Nintendo DS platform." ) ;
		
#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type 	= F_UNLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start 	= 0 ;
	lk.l_len	= 0 ;

	while ( true )
	{
		if (::fcntl( _fdes, F_SETLKW, &lk ) == - 1 )
		{
			if ( errno == EINTR  )
				continue ;

			if ( errno != ENOLCK )
				throw ReadUnlockingFailed( 
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

	throw ReadUnlockingFailed(
		"StandardFile::unlockForReading: lock feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_LOCKS

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}


void StandardFile::lockForWriting() const throw( WriteLockingFailed )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw WriteLockingFailed( "StandardFile::lockForWriting: "
		"no supported on the Nintendo DS platform." ) ;
		
#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type 	= F_WRLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start 	= 0 ;
	lk.l_len	= 0 ;

	while ( true )
	{
		if ( ::fcntl( _fdes, F_SETLKW, &lk ) == - 1 )
		{
			if ( errno == EINTR  )
				continue ;

			if ( errno != ENOLCK )
				throw WriteLockingFailed( 
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

	throw WriteLockingFailed( "StandardFile::lockForWriting: "
		"lock feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_LOCKS

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}


void StandardFile::unlockForWriting() const throw( WriteUnlockingFailed )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw WriteUnlockingFailed( "StandardFile::unlockForWriting: "
		"no supported on the Nintendo DS platform." ) ;
		
#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type 	= F_UNLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start 	= 0 ;
	lk.l_len	= 0 ;

	while ( true )
	{
	
		if ( ::fcntl( _fdes, F_SETLKW, &lk ) == - 1 )
		{
			if ( errno == EINTR  )
				continue;

			if ( errno != ENOLCK )
				throw WriteUnlockingFailed( 
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

	throw WriteUnlockingFailed( "StandardFile::unlockForWriting: "
		"lock feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_LOCKS

#endif // CEYLAN_ARCH_NINTENDO_DS

}


bool StandardFile::isLocked() const throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

	return false ;

#else // CEYLAN_ARCH_NINTENDO_DS
	
#if CEYLAN_USES_FILE_LOCKS

	struct flock lk ;
	lk.l_type 	= F_WRLCK ;
	lk.l_whence = SEEK_SET ;
	lk.l_start 	= 0 ;
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




Size StandardFile::read( Ceylan::Byte * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw InputStream::ReadFailedException( "StandardFile::read:"
		"no supported on the Nintendo DS platform." ) ;
		

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
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw OutputStream::WriteFailedException( "StandardFile::write:"
		"no supported on the Nintendo DS platform." ) ;
		

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


Size StandardFile::write( const Ceylan::Byte * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw OutputStream::WriteFailedException( "StandardFile::write:"
		"no supported on the Nintendo DS platform." ) ;
		

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
		throw WriteFailed( "StandardFile::write failed for file '" 
			+ _name + "': negative size written" ) ;

	Size realSize = static_cast<Size>( n ) ;
	
	if ( realSize < maxLength )
		throw WriteFailed( "StandardFile::write failed for file '" 
			+ _name + "', fewer bytes wrote than expected: " 
			+ interpretState() ) ;
			
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
	 * Drawback of the fstream-based method: the written size is not 
	 * really known.
	 *
	 */
	return maxLength ;


#endif // CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS	

#endif // CEYLAN_ARCH_NINTENDO_DS

}


void StandardFile::open( OpeningFlag openFlag, PermissionFlag permissionFlag ) 
	throw( File::AlreadyOpened, File::CouldNotOpen )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw File::CouldNotOpen( "StandardFile::open:"
		"no supported on the Nintendo DS platform." ) ;
		

#else // CEYLAN_ARCH_NINTENDO_DS

	if ( _openFlag != DoNotOpen )
		throw AlreadyOpened( "StandardFile::open: file '" + _name
			+ "' was already opened." ) ;

	_openFlag = openFlag ;
	_permissions = permissionFlag ;

	reopen() ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}


time_t StandardFile::getLastChangeTime() const throw( GetChangeTimeFailed )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw GetChangeTimeFailed( "StandardFile::getLastChangeTime:"
		"no supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	if ( ::stat( _name.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

	throw GetChangeTimeFailed( "StandardFile::getLastChangeTime failed for '"
		+ _name + "': " + System::explainError() ) ;
	
#else // CEYLAN_USES_STAT

	throw GetChangeTimeFailed( "File::getLastChangeTime: "
		"not available on this platform." ) ;
	 
#endif // CEYLAN_USES_STAT

#endif // CEYLAN_ARCH_NINTENDO_DS

}




FileDescriptor StandardFile::getFileDescriptor() const 
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	return _fdes ;
	
#else // if CEYLAN_USES_FILE_DESCRIPTORS

	throw Features::FeatureNotAvailableException( 
		"StandardFile::getFileDescriptor: "
		"file descriptor feature not available" ) ;
		
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	

}




StreamID StandardFile::getStreamID() const throw()
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	return static_cast<StreamID>( getFileDescriptor() ) ;
	
#else // if CEYLAN_USES_FILE_DESCRIPTORS

	// No appropriate identifier found:	
	return -1 ;
	
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	

}



const std::string StandardFile::toString( Ceylan::VerbosityLevels level ) 
	const throw()
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






// Protected section.


StandardFile::StandardFile( const string & name, OpeningFlag openFlag, 
	PermissionFlag permissions )
		throw( File::CouldNotOpen ):
	File( name, openFlag, permissions )
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw File::CouldNotOpen( "StandardFile constructor:"
		"no supported on the Nintendo DS platform." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

	if ( openFlag != DoNotOpen )
		reopen() ;
		
#endif // CEYLAN_ARCH_NINTENDO_DS
		
}


StandardFile::~StandardFile() throw()
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

	

FileSystemManager & StandardFile::getCorrespondingFileSystemManager()
	const throw( FileSystemManagerException )
{

	return StandardFileSystemManager::GetStandardFileSystemManager() ;
	
}
	
