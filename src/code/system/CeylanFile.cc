#include "CeylanFile.h"

#include "CeylanLogPlug.h"     // for Log primitives
#include "CeylanDirectory.h"   // for Directory
#include "CeylanOperators.h"   // for toString
#include "CeylanStringUtils.h" // for StringSize


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


// Not available in their C++ form :
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



#if CEYLAN_USES_FILE_DESCRIPTORS

// Avoid exposing system-dependent mode_t in the headers :
struct File::SystemSpecificPermissionFlag
{
	mode_t _mode ;	
	
} ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS


/*
 * We need our own bitmasks, instead of using #ifdef/#endif pair testing
 * CEYLAN_USES_FILE_DESCRIPTORS, since we allow Read and Write to be 
 * OR'd, whereas for example O_RDONLY | O_WRONLY might be different from
 * O_RDWR.
 *
 */
const OpeningFlag File::Read          = 0x0001 ;
const OpeningFlag File::Write         = 0x0002 ;
const OpeningFlag File::Create        = 0x0004 ;
const OpeningFlag File::Truncate      = 0x0008 ;
const OpeningFlag File::Append        = 0x0010 ;
const OpeningFlag File::Binary        = 0x0020 ;
const OpeningFlag File::NonBlocking   = 0x0040 ;
const OpeningFlag File::Synchronous   = 0x0080 ;


// Special cases :

const OpeningFlag File::CreateToWriteBinary = 
	Read | Write | Create | Truncate | Binary ;
	
const OpeningFlag File::DoNotOpen     = 0xffff ;



// Same case : bitmasks for permission flags.


const PermissionFlag File::OwnerRead          = 0x0001 ;
const PermissionFlag File::OwnerWrite         = 0x0002 ;
const PermissionFlag File::OwnerExec          = 0x0004 ;

const PermissionFlag File::OwnerReadWrite     = 
	File::OwnerRead | File::OwnerWrite ;
	
const PermissionFlag File::OwnerReadWriteExec =
	File::OwnerReadWrite | File::OwnerExec ;


const PermissionFlag File::GroupRead          = 0x0008 ;
const PermissionFlag File::GroupWrite         = 0x0010 ;
const PermissionFlag File::GroupExec          = 0x0020 ;

const PermissionFlag File::GroupReadWrite     = 
	File::GroupRead | File::GroupWrite ;
	
const PermissionFlag File::GroupReadWriteExec = 
	File::GroupReadWrite | File::GroupExec ;


const PermissionFlag File::OthersRead          = 0x0040 ;
const PermissionFlag File::OthersWrite         = 0x0080 ;
const PermissionFlag File::OthersExec          = 0x0100 ;

const PermissionFlag File::OthersReadWrite     = 
	File::OthersRead | File::OthersWrite ;
	
const PermissionFlag File::OthersReadWriteExec = 
	File::OthersReadWrite | File::OthersExec ;
				


const Size File::UsualBufferSize = 10240 ;
const Size File::BigBufferSize   = 1<<19 ;



/**
 * Read/write operations can be performed with both the inner low-level 
 * rdbuf object of the fstream instances, or directly with the fstream itself.
 *
 * The former is preferred to the latter, since the number of read/written
 * bytes is explicitly known. After tests, the two methods seem to behave
 * the same way.
 *
 */ 
#define CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM 1


File::FileException::FileException( const string & reason ) throw() :
	SystemException( reason )
{

}


File::FileException::~FileException() throw()
{

}


// Numerous child classes :	
	
	
File::CouldNotOpen::CouldNotOpen( const string & reason ) throw() :
	File::FileException( reason )
{

}
		

File::CouldNotCreate::CouldNotCreate( const string & reason ) 
		throw() :
	File::FileException( reason )
{

}
		
	
File::CouldNotStatFile::CouldNotStatFile( const string & reason ) 
		throw() :
	File::FileException( reason )
{

}
		
	
File::ReadFailed::ReadFailed( const string & reason ) throw() :
	InputStream::ReadFailedException( reason )
{

}
				
	
File::WriteFailed::WriteFailed( const string & reason ) throw() :
	OutputStream::WriteFailedException( reason )
{

}

		
File::ReadLockingFailed::ReadLockingFailed( const string & reason ) 
		throw() :
	File::FileException( reason )
{

}
			
	
File::ReadUnlockingFailed::ReadUnlockingFailed( const string & reason ) 
		throw() :
	File::FileException( reason )
{

}
				
	
File::WriteLockingFailed::WriteLockingFailed( const string & reason ) 
		throw() :
	File::FileException( reason )
{

}
		
		
File::WriteUnlockingFailed::WriteUnlockingFailed( const string & reason )
		throw() :
	File::FileException( reason )
{

}
			
	
File::TouchFailed::TouchFailed( const string & reason ) throw() :
	File::FileException( reason )
{

}
			
	
File::AlreadyOpened::AlreadyOpened( const string & reason ) throw() :
	File::FileException( reason )
{

}

		
File::RemoveFailed::RemoveFailed( const string & reason ) throw() :
	File::FileException( reason )
{

}

		
File::GetChangeTimeFailed::GetChangeTimeFailed( const string & reason ) 
		throw() :
	File::FileException( reason )
{

}

		
File::CouldNotDuplicate::CouldNotDuplicate( const string & reason ) 
		throw() :
	File::FileException( reason )
{

}
		
	
File::UnlinkFailed::UnlinkFailed( const string & reason ) throw() :
	File::FileException( reason )
{

}
	
		
File::SymlinkFailed::SymlinkFailed( const string & reason ) throw() :
	File::FileException( reason )
{

}
		
	
	
File::CloseFailed::CloseFailed( const string & reason ) throw() :
	File::FileException( reason )
{

}
		
	
File::MoveFailed::MoveFailed( const string & reason ) throw() :
	File::FileException( reason )
{

}
		
	
File::CopyFailed::CopyFailed( const string & reason ) throw() :
	File::FileException( reason )
{

}

		
File::ConversionFailed::ConversionFailed( const string & reason ) throw() :
	File::FileException( reason )
{

}
		
	

File::File( const string & name, OpeningFlag openFlag, 
	PermissionFlag permissions )
		throw( File::CouldNotOpen ):
	InputOutputStream(),	
	_fdes( static_cast<FileDescriptor>( -1 ) ),
	_fstream(),
	_name( name ),
	_openFlag( openFlag ),
	_permissions( permissions ),
	_lockedForReading( false ),
	_lockedForWriting( false )
{

	if ( openFlag != DoNotOpen )
		reopen() ;
		
}



File::File( const string & name, 
			Size length, 
			FileDescriptor fd, 
			PermissionFlag permissionFlag )
		throw( File::CouldNotCreate, File::ReadFailed, File::WriteFailed,
			Features::FeatureNotAvailableException ):
	InputOutputStream(),		
	_fdes( static_cast<FileDescriptor>( -1 ) ),
	_name( name ),
	_openFlag( CreateToWriteBinary ),
	_permissions( permissionFlag ),
	_lockedForReading( false ),
	_lockedForWriting( false )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	File::SystemSpecificPermissionFlag myMode ;
	ConvertToFileDescriptorPermissionFlag( _permissions, myMode ) ;
	_fdes = ::open( 
		name.c_str(), 
		ConvertToFileDescriptorOpenFlag( CreateToWriteBinary ),
		myMode._mode 
	) ;

	if ( _fdes < 0 )
		throw CouldNotCreate( 
			"File constructor : unable to create file '" + name
			+ "' : " + System::explainError() ) ;

	FromFDtoFD( fd, _fdes, length ) ;

#else // if CEYLAN_USES_FILE_DESCRIPTORS	

	throw Features::FeatureNotAvailableException( 
		"File constructor from a file descriptor "
		"called whereas this feature is not available." ) ;
	
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	
	
}



File::~File() throw()
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	if ( _fdes <= 0 )
		return ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	if ( ! _fstream.is_open() )
		return ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

	try
	{
		close() ;
	}
	catch( const Stream::CloseException & e )
	{
		LogPlug::error( "File destructor : close failed : " + e.toString() ) ;
	}
		
}


const std::string & File::getName() const throw()
{ 

	return _name ; 
	
}


bool File::close() throw( Stream::CloseException )
{

	/*
	 * Exception and returned boolean are both needed :
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

		throw CloseException( "File::close : file '" +  _name 
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
	
		throw CloseException( "File::close : file '" +  _name 
			+ "' does not seem to have been already opened." ) ;
			
	}
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS
	
}


void File::remove() throw( RemoveFailed )
{

	try
	{
		close() ;
	}
	catch( const Stream::CloseException & e )
	{
		throw RemoveFailed( "File::remove failed : " + e.toString() ) ;
	}


	try
	{
		Unlink( _name ) ;
	}
	catch( const UnlinkFailed & e )
	{
		throw RemoveFailed( "File::remove failed : " + e.toString() ) ;
	}
	
	
}



void File::serialize( FileDescriptor fd ) const 
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	FromFDtoFD( _fdes, fd, size() ) ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw Features::FeatureNotAvailableException( "File::serialize : "
		"file descriptor feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}



void File::saveAs( const string & name ) throw( FileException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	File f( name ) ;
	serialize( f._fdes ) ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw FileException( 
		"File::saveAs : not implemented on this platform." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS
	
}



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

void File::lockForReading() const throw( ReadLockingFailed,
	Features::FeatureNotAvailableException )
{

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
					"File::lockForReading : locking failed for '" 
						+ _name + "' : " + System::explainError() ) ;
			else
				LogPlug::warning( "Lock for reading failed for '" + _name
					+ "' : check your NFS daemons." ) ;
			break ;
		}
		else
		{
			break ;
		}
	}

#else // CEYLAN_USES_FILE_LOCKS

	throw Features::FeatureNotAvailableException( "File::lockForReading : "
		"lock feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_LOCKS	

}


void File::unlockForReading() const throw( ReadUnlockingFailed,
	Features::FeatureNotAvailableException )
{

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
				continue ;

			if ( errno != ENOLCK )
				throw ReadUnlockingFailed( 
					"File::unlockForReading : unlocking failed for '" 
						+ _name + "' : " + System::explainError() ) ;
			else
				LogPlug::warning( "Unlock for reading failed for '" + _name
					+ "' : check your NFS daemons.") ;
			break ;
		}
		else
		{
			break ;
		}
	}
	
#else // CEYLAN_USES_FILE_LOCKS

	throw Features::FeatureNotAvailableException( "File::unlockForReading : "
		"lock feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_LOCKS
	
}


void File::lockForWriting() const throw( WriteLockingFailed,
	Features::FeatureNotAvailableException )
{

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
					"File::lockForWriting : locking failed for '" 
						+ _name + "' : " + System::explainError() ) ;
			else
				LogPlug::warning( "Lock for writing failed for '" + _name
					+ "' : check your NFS daemons.") ;
			break ;
		}
		else
		{
			break ;
		}
	}
	
#else // CEYLAN_USES_FILE_LOCKS

	throw Features::FeatureNotAvailableException( "File::lockForWriting : "
		"lock feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_LOCKS
	
}


void File::unlockForWriting() const throw( WriteUnlockingFailed,
	Features::FeatureNotAvailableException )
{

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
					"File::unlockForWriting : unlocking failed for '" 
						+ _name + "' : " + System::explainError() ) ;
			else
				LogPlug::warning( "Unlock for writing failed for '" + _name
					+ "' : check your NFS daemons.") ;
			break ;
		}
		else
		{
			break ;
		}
	}
	
#else // CEYLAN_USES_FILE_LOCKS

	throw Features::FeatureNotAvailableException( "File::unlockForWriting : "
		"lock feature not available" ) ;
		
#endif // CEYLAN_USES_FILE_LOCKS

}


bool File::isLocked() const throw()
{

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
				LogPlug::warning( "Lock ckecking failed for '" + _name 
					+ "' : check your NFS daemons." ) ;
		}
		break ;
	}

	return ( lk.l_pid != 0 ) ;
	
#else // CEYLAN_USES_FILE_LOCKS

	return false ;
			
#endif // CEYLAN_USES_FILE_LOCKS
	

}




Size File::size() const throw( File::CouldNotStatFile )
{
	return GetSize( _name ) ;
}


Size File::read( Ceylan::Byte * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

	setSelected( false ) ;

#if CEYLAN_USES_FILE_DESCRIPTORS

	try
	{	

		return System::FDRead( _fdes, buffer, maxLength ) ;	
			
	}
	catch( const Ceylan::Exception & e )
	{
		throw InputStream::ReadFailedException( 
			"File::read failed for file '" + _name + "' : " + e.toString() ) ;
	}
		

#else // CEYLAN_USES_FILE_DESCRIPTORS	


#if CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM

	SignedSize n = ( _fstream.rdbuf() )->sgetn( buffer, 
		static_cast<std::streamsize>( maxLength ) ) ;

	if ( ! _fstream.good() )
		throw InputStream::ReadFailedException( 
			"File::read failed for file '" + _name 
			+ "' : " + interpretState() ) ;


	// Actually, n may be negative.
	if ( n < 0 )
		throw InputStream::ReadFailedException( 
			"File::read failed for file '" 
			+ _name + "' : negative size read" ) ;

	return static_cast<Size>( n ) ;
	
#else // CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM
	
	 	
	SignedSize n = _fstream.readsome( buffer, 
		static_cast<std::streamsize>( maxLength ) ) ;

	if ( ! _fstream.good() )
		throw InputStream::ReadFailedException( 
			"File::read failed for file '" + _name 
			+ "' : " + interpretState() ) ;

	// Actually, n may be negative.
	if ( n < 0 )
		throw InputStream::ReadFailedException( 
			"File::read failed for file '" 
			+ _name + "' : negative size read" ) ;
			
	return static_cast<Size>( n ) ;
	
#endif // CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS	

}

void File::readExactLength( Ceylan::Byte * buffer, Size exactLength ) 
	throw( InputStream::ReadFailedException )
{

	Size remainder = exactLength ;
	Size readCount ;

	Ceylan::Uint32 readFailures = 0 ;
	const Ceylan::Uint32 maxReadFailures = 10000 ;

	try
	{
		
		do
		{

			readCount = read( buffer, remainder ) ;
			
			remainder -= readCount ;

			if ( readCount == 0 )
				readFailures++ ;
			else
				readFailures = 0 ;
				
		}
		while( remainder > 0 && readFailures < maxReadFailures ) ;


	} 
	catch ( const InputStream::ReadFailedException  & e )
	{
		throw InputStream::ReadFailedException( 
			"File::readExactLength : trying to read "
			+ Ceylan::toString( static_cast<Ceylan::Uint32>( exactLength ) ) 
			+ " bytes, actually read "
			+ Ceylan::toString( static_cast<Ceylan::Uint32>( 
					exactLength - remainder ) )
			+ " bytes before following error : " + e.toString() ) ;

	}

	if ( readFailures == maxReadFailures )
		throw InputStream::ReadFailedException( 
			"File::readExactLength : unable to read requested bytes, maybe "
			"trying to read a binary file not opened in Binary mode ?" ) ;

}

bool File::hasAvailableData() const throw()
{
	return true ;
}


Size File::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	try
	{	
			
		return System::FDWrite( _fdes, message.c_str(), message.size() ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		throw OutputStream::WriteFailedException( 
			"File::write failed for file '" + _name + "' : " + e.toString() ) ;
	}

#else // if CEYLAN_USES_FILE_DESCRIPTORS	

	// Will trigger relevant exceptions if needed :
	return write( message.c_str(), 
		static_cast<std::streamsize>( message.size() ) ) ; 
	
			
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	

}


Size File::write( const Ceylan::Byte * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	try
	{	
		return System::FDWrite( _fdes, buffer, maxLength ) ;
	}
	catch( const Ceylan::Exception & e )
	{
		throw OutputStream::WriteFailedException( 
			"File::write failed for file '" + _name + "' : " + e.toString() ) ;
	}

#else // CEYLAN_USES_FILE_DESCRIPTORS	



#if CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM
	 	
		
	SignedSize n = ( _fstream.rdbuf() )->sputn( buffer, 
		static_cast<std::streamsize>( maxLength ) ) ;

	if ( n < 0 )
		throw WriteFailed( "File::write failed for file '" 
			+ _name + "' : negative size written" ) ;

	Size realSize = static_cast<Size>( n ) ;
	
	if ( realSize < maxLength )
		throw WriteFailed( "File::write failed for file '" 
			+ _name + "', fewer bytes wrote than expected : " 
			+ interpretState() ) ;
			
	/*
	 * Probably useless :
	 		
	if ( ! _fstream.good() )
		throw WriteFailed( "File::write failed for file '" + _name 
			+ "' : " + interpretState() ) ;
	 *
	 */
	 
	 
	if ( Synchronous & _openFlag )		
		_fstream.flush() ;		
	
	return realSize ;


#else // CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM
	
	
	_fstream.write( buffer, static_cast<std::streamsize>( maxLength ) ) ; 

	if ( ! _fstream.good() )
		throw WriteFailed( "File::write failed for file '" + _name 
			+ "' : " + interpretState() ) ;

	if ( Synchronous & _openFlag )		
		_fstream.flush() ;		

	/*
	 * Drawback of the fstream-based method : the written size is not 
	 * really known.
	 *
	 */
	return maxLength ;


#endif // CEYLAN_PREFERS_RDBUF_TO_DIRECT_FSTREAM
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS	

}


void File::open( OpeningFlag openFlag, PermissionFlag permissionFlag ) 
	throw( File::AlreadyOpened, File::CouldNotOpen )
{

	if ( _openFlag != DoNotOpen )
		throw AlreadyOpened( "File::open : file '" + _name
			+ "' was already opened." ) ;

	_openFlag = openFlag ;
	_permissions = permissionFlag ;

	reopen() ;

}


time_t File::getLastChangeTime() const throw( GetChangeTimeFailed )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	if ( ::stat( _name.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

	throw GetChangeTimeFailed( "File::getLastChangeTime failed for '"
		+ _name + "' : " + System::explainError() ) ;
	
#else // CEYLAN_USES_STAT

	throw GetChangeTimeFailed( "File::getLastChangeTime : "
		"not available on this platform." ) ;
	 
#endif // CEYLAN_USES_STAT

}




FileDescriptor File::getFileDescriptor() const 
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	return _fdes ;
	
#else // if CEYLAN_USES_FILE_DESCRIPTORS

	throw Features::FeatureNotAvailableException( "File::getFileDescriptor : "
		"file descriptor feature not available" ) ;
		
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	

}




StreamID File::getStreamID() const throw()
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	return static_cast<StreamID>( getFileDescriptor() ) ;
	
#else // if CEYLAN_USES_FILE_DESCRIPTORS

	// No appropriate identifier found :	
	return -1 ;
	
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	

}


StreamID File::getInputStreamID() const throw()
{
	return getStreamID() ;
}


StreamID File::getOutputStreamID() const throw()
{
	return getStreamID() ;
}


const std::string File::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "File object for filename <" + _name + ">" ;

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




FileDescriptor File::Duplicate( FileDescriptor fdes ) 
	throw( CouldNotDuplicate, Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	FileDescriptor newfd = ::dup( fdes ) ;

	if ( newfd == -1 )
		throw CouldNotDuplicate( 
			"File::Duplicate failed for file descriptor "
			+ Ceylan::toString( fdes ) + "." ) ;

	return newfd ;

#else

	throw Features::FeatureNotAvailableException( "File::Duplicate : "
		"file descriptor feature not available." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS	

}




bool File::Exists( const string & name ) throw( CouldNotStatFile )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;
	
	return ( ::stat( name.c_str(), & buf ) == 0 
		&& ( S_ISLNK( buf.st_mode ) || S_ISREG( buf.st_mode ) ) ) ;
	
#else // CEYLAN_USES_STAT

	struct _stat buf ;
	return ( ( ::_stat( name.c_str(), & buf ) == 0 )
		&& ( buf.st_mode & _S_IFREG ) ) ;

#ifdef CEYLAN_USES__STAT

#else // CEYLAN_USES__STAT

	throw CouldNotStatFile( 
		"File::Exists : not available on this platform." ) ;

#endif // CEYLAN_USES__STAT
#endif // CEYLAN_USES_STAT
	
}


bool File::ExistsAsFileOrSymbolicLink( const string & name ) 
	throw( CouldNotStatFile )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

#if CEYLAN_DEBUG_SYSTEM

	/*
	 * Even if all side-effects of this section should be removed, 
	 * it may still a bit hazardous :
	 * behaviour should not change depending on CEYLAN_DEBUG_SYSTEM openFlag.
	 *
	 */

	try
	{
	
	  LogPlug::debug( "File::ExistsAsFileOrSymbolicLink : testing for '" 
	  	+ name + "' from '" 
		+ Directory::GetCurrentWorkingDirectoryName() + "'." ) ;
	}
	
	catch( const Directory::DirectoryException & e )
	{
		LogPlug::error( "File::ExistsAsFileOrSymbolicLink :"
			" could not get current directory : "
			+ e.toString() ) ;
			
		LogPlug::debug( "File::ExistsAsFileOrSymbolicLink : "
			"testing for '" + name + "' probably from a removed directory." ) ;
	}

	int res = ::stat( name.c_str(), & buf ) ;

	LogPlug::debug( "File::ExistsAsFileOrSymbolicLink : stat returned "
		+ Ceylan::toString( res ) ) ;

	if ( res != 0 )
		LogPlug::debug( "Error : " + System::explainError() ) ;

	LogPlug::debug( "File::ExistsAsFileOrSymbolicLink : regular : "
		+ Ceylan::toNumericalString( S_ISREG( buf.st_mode ) ) ) ;

	LogPlug::debug( "File::ExistsAsFileOrSymbolicLink : link : "
		+ Ceylan::toNumericalString( S_ISLNK( buf.st_mode ) ) ) ;

#endif // CEYLAN_DEBUG_SYSTEM


#if CEYLAN_USES_SYMBOLIC_LINKS

	return ( ::stat( name.c_str(), & buf ) == 0 &&
		( S_ISREG( buf.st_mode ) || S_ISLNK( buf.st_mode ) ) ) ;
		
#else // CEYLAN_USES_SYMBOLIC_LINKS 

	return ( ::stat( name.c_str(), & buf ) == 0 &&  S_ISREG( buf.st_mode ) ) ;
	
#endif // CEYLAN_USES_SYMBOLIC_LINKS

#else // CEYLAN_USES_STAT

#ifdef CEYLAN_USES__STAT
	
	struct _stat buf ;

	return ( ::_stat( name.c_str(), & buf ) == 0 
		&& ( buf.st_mode & _S_IFREG ) ) ;

#else CEYLAN_USES__STAT

	throw CouldNotStatFile( "File::ExistsAsFileOrSymbolicLink : "
		"not available on this platform." ) ;

#endif CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT
	 	 
	 
}


void File::MakeSymbolicLink( const string & path, const string & linkname )
	throw( SymlinkFailed )
{

#if CEYLAN_USES_SYMBOLIC_LINKS

	if ( ::symlink( path.c_str(), linkname.c_str() ) == -1 )
		throw SymlinkFailed( 
			"File::MakeSymbolicLink failed when creating link '" 
			+ linkname + "' which was to point to '" + path 
			+ "' : " + System::explainError() ) ;
		
#else // CEYLAN_USES_SYMBOLIC_LINKS

	throw SymlinkFailed( "File::MakeSymbolicLink : "
		"symbolic link feature not available" ) ;
		
#endif // CEYLAN_USES_SYMBOLIC_LINKS


}


void File::Unlink( const string & name ) throw( UnlinkFailed )
{

#ifdef CEYLAN_USES_UNLINK	

	if ( ::unlink( name.c_str() ) != 0 )
		throw UnlinkFailed( "File::Unlink failed for '"
			+ name + "' : " + System::explainError() ) ;
		
#else // CEYLAN_USES_UNLINK

#ifdef CEYLAN_USES__UNLINK	

	if ( ::_unlink( name.c_str() ) != 0 )
		throw RemoveFailed( "File::Unlink failed : file '" + name 
			+ "' : " + System::explainError() ) ;

#else // CEYLAN_USES__UNLINK

	throw RemoveFailed( "File::Unlink : not available on this platform." ) ;

#endif // CEYLAN_USES__UNLINK

#endif // CEYLAN_USES_UNLINK	

		
}


void File::Move( const string & name, const string & newName )
	throw( MoveFailed )
{

#ifdef CEYLAN_USES_RENAME

	if ( ::rename( name.c_str(), newName.c_str() ) == -1 )
		throw MoveFailed( "File::Move failed when renaming '"
			+ name + "' into '" + newName + "' : " 
			+ System::explainError() ) ;
		
#else // CEYLAN_USES_RENAME


	// Renaming is copying and then removing the source file :
	try
	{
		Copy( name, newName ) ;
		Unlink( name ) ;
	}
	catch( const FileException & e )
	{
		throw MoveFailed( "File::Move failed : " + e.toString() ) ;
	}	
	

#endif // CEYLAN_USES_RENAME
		
}


void File::Copy( const string & name, const string & newName )
	throw( CopyFailed )
{


#if CEYLAN_USES_FILE_DESCRIPTORS
	
	try 
	{
	
		File f( name, File::Read ) ;
		f.saveAs( newName ) ;
		f.close() ;
		
	} 
	catch ( const FileException & e )
	{
		throw CopyFailed( "File::Copy failed when copying '"
			+ name + "' to '" + newName + "' : " + e.toString() ) ;
	}
	
	
#else // if CEYLAN_USES_FILE_DESCRIPTORS	

	/*
	 * This is only a hack with C++ streams.
	 *
	 * Another hack would be :
	 *
	 * File sourceFile( name ) ;
	 * File targetFile( newName ) ;
	 * targetFile._fstream << sourceFile._fstream.rdbuf() ;
	 *
	 *  - or something adapted from (slower ?) - 
	 *
	 * while (true)
     * {
	 *    char c;
	 *	  cin.get(c);
	 *	  if (cin.fail())
	 *	      break;
	 *	  cout << c;
	 * }
	 *
	 */
	
	try 
	{

		Size fileSize = File::GetSize( name ) ;
	
		File source( name, Read | Binary ) ;
		
		// Not knowing the permissions to set : 
		File target( newName, CreateToWriteBinary, OwnerReadWrite ) ;
		
		Size written = 0 ;
	
		Size bufferSize = ( fileSize > BigBufferSize ? 
			BigBufferSize : fileSize ) ;

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
					source.read( buf, toRead ) ) ;
			}
			catch( const ReadFailed & e )
			{
				delete [] buf ;
				throw CopyFailed( "File::Copy failed when copying '"
					+ name + "' to '" + newName + "' : " + e.toString() ) ;
			}

			try
			{		
				target.write( buf, readCount ) ;
			}	
			catch( const WriteFailed & e )
			{
				delete [] buf ;
				throw CopyFailed( "File::Copy failed when copying '"
					+ name + "' to '" + newName + "' : " + e.toString() ) ;
			}

			written += readCount ;
		}

		delete [] buf ;
	
		target.close() ;
		source.close() ;
		
	} 
	catch ( const FileException & e )
	{
		throw CopyFailed( "File::Copy failed when copying '"
			+ name + "' to '" + newName + "' : " + e.toString() ) ;
	}
	
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	
 	
}



Size File::GetSize( const string & name ) throw( File::CouldNotStatFile )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	if ( ::stat( name.c_str(), & buf ) == 0 )
		return static_cast<Size>( buf.st_size ) ;

	throw CouldNotStatFile( "File::GetSize : could not stat file '"
		+ name + "' : " + System::explainError() ) ;

#else // CEYLAN_USES_STAT


#if CEYLAN_USES_FILE_DESCRIPTORS == 0 

	// One more hack with C++ streams :
	
	ifstream tmpFile ;
	tmpFile.open( name.c_str(), ios::binary ) ;

	if ( ! tmpFile.is_open() )
		throw CouldNotStatFile( "File::GetSize failed for '" + name
			+ "' : error opening file, " + InterpretState( tmpFile ) ) ;
	
	if ( ! tmpFile.good() )
		throw CouldNotStatFile( "File::GetSize failed for '" + name
			+ "' : " + InterpretState( tmpFile ) ) ;
			
	tmpFile.seekg( 0, ios::end ) ;
	Size size = static_cast<Size>( tmpFile.tellg() ) ;
	tmpFile.close() ;
	
	return size ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw CouldNotStatFile( 
		"File::GetSize : not available on this platform." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS 


#endif // CEYLAN_USES_STAT

}


void File::Touch( const string & name ) throw( File::TouchFailed )
{

#ifdef CEYLAN_USES_UTIME

	if ( ::utime( name.c_str(), 0 ) )
		throw TouchFailed( "File::Touch failed for '" + name 
			+ "' : " + System::explainError() ) ;
		
#else // CEYLAN_USES_UTIME

#ifdef CEYLAN_USES__UTIME

	if ( ::_utime( name.c_str(), 0 ) == -1 )
		throw TouchFailed( "File::Touch failed for '" + name 
			+ "' : " + System::explainError() ) ;

#else // CEYLAN_USES__UTIME

	throw TouchFailed( "File::Touch : not available on this platform." ) ;
	
#endif // CEYLAN_USES__UTIME

#endif // CEYLAN_USES_UTIME

}


bool File::Diff( const std::string & firstFilename,
	const std::string & secondFilename ) throw( FileException )
{

	if ( ! Exists( firstFilename ) )
	{
		LogPlug::warning( "File::Diff : first file '" + firstFilename
			+ "' does not exist." ) ;
		
		return false ;	
	}
	
	
	if ( ! Exists( secondFilename ) )
	{
		LogPlug::warning( "File::Diff : second file '" + secondFilename
			+ "' does not exist." ) ;
		
		return false ;	
	}
	
	Size commonSize = GetSize( firstFilename ) ;
	
	if ( commonSize != GetSize( secondFilename ) )
	{
	
		LogPlug::debug( "File::Diff : "
			"the two files do not have the same size." ) ;
		
		return false ;	
	}
	
	/*
	LogPlug::debug( "Common size is " 
		+ Ceylan::toString( static_cast<Ceylan::Uint32>( commonSize ) )
		+ " bytes." ) ;
	 */

	File first(  firstFilename,  File::Read ) ;
	File second( secondFilename, File::Read ) ;
	
	for ( Size i = 0; i < commonSize; i++ )
	{
		if ( first.readUint8() != second.readUint8() )
			return false ;
	}
	
	return true ;
		
}

	
const string File::TransformIntoValidFilename( const string & name ) throw()
{

	// For MS-DOS/Windows, one may look at gcc 'dosck' as well.
	
	string result ;

	unsigned int characterCount = 0 ;

	// Remove all leading dots '.' :
	while ( name[ characterCount ] == '.' )
		characterCount++ ;

	// (preferred to : for( string::const_iterator it...)

	// Substitute any space " ", slash "/" or back-slash "\" by a dash "-" :

	StringSize nameSize = name.size() ;
	
	for ( ; characterCount < nameSize ; characterCount++ )
	{
		switch( name[ characterCount ] )
		{

			case ' ':
				result += "-" ;
				break ;

			case '/':
				result += "-" ;
				break ;

			case '\\':
				result += "-" ;
				break ;

			/*
			 * This is not strictly needed on most system, but it is 
			 * convenient to avoid ':' in HTML file references 
			 * (<a href="xx::yyy.html">... not recommended)
			 *
			 */
			case ':':
				result += "_" ;
				break ;

			default:
				result += name[ characterCount ] ;
				break ;

		}

	}

	return result ;
}



// Protected section.


void File::reopen() throw( File::CouldNotOpen )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	File::SystemSpecificPermissionFlag myMode ;
	ConvertToFileDescriptorPermissionFlag( _permissions, myMode ) ;
	_fdes = ::open( _name.c_str(), 
		ConvertToFileDescriptorOpenFlag( _openFlag ),
		myMode._mode ) ;

	if ( _fdes < 0 )
		throw CouldNotOpen( "File::reopen failed for '" + _name
			+ "' : " + System::explainError() ) ;
			
			
#else // if CEYLAN_USES_FILE_DESCRIPTORS	


	// Second, take care of the permission openFlag :
	
	/*
	 * With this implementation, _permissions is ignored, no
	 * ConvertToStreamPermissionFlag used.
	 *
	 */
	
	/* 
	 * If Synchronous is requested, turn off the buffering before any
	 * I/O operation :
	 *
	 * @see http://gcc.gnu.org/onlinedocs/libstdc++/27_io/howto.html
	 *
	 * However actual tests, by triggering a division by zero crash, showed
	 * that the 'pubsetbuf' call is not enough since logs may be lost
	 * nevertheless. Adding the conditional 'flush' in the 'write' method
	 * works.
	 *
	 */
	if ( Synchronous & _openFlag )
		_fstream.rdbuf()->pubsetbuf( 0, 0 ) ;
		
	_fstream.open( _name.c_str(), 
		ConvertToStreamOpenFlag( _openFlag, _name ) ) ; 

	// if ( ! _fstream ) ... could be used as well
	
	if ( ! _fstream.is_open() )
		throw CouldNotOpen( "File::reopen failed for '" + _name
			+ "' : error opening file, " + interpretState() ) ;
	
	if ( ! _fstream.good() )
		throw CouldNotOpen( "File::reopen failed for '" + _name
			+ "' : " + interpretState() ) ;
	
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	


}



string File::interpretState() const throw()
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	return "File uses file descriptor " + Ceylan::toString( _fdes ) ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	return InterpretState( _fstream ) ;
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}


string File::InterpretState( const ifstream & inputFile ) throw()
{

	if ( inputFile.good() )
		return "File is in clean state (no error)" ;
		
	string res = "File in error" ;
	
	if ( inputFile.rdstate() & ifstream::badbit )
		res += ", critical error in stream buffer" ;
		
	if ( inputFile.rdstate() & ifstream::eofbit )
		res += ", End-Of-File reached while extracting" ;
		
	if ( inputFile.rdstate() & ifstream::failbit )
		res += ", failure extracting from stream" ;
		
	return res ;
	
}


string File::InterpretState( const fstream & file ) throw()
{

	if ( file.good() )
		return "File is in clean state (no error)" ;
		
	string res = "File in error" ;
	
	if ( file.rdstate() & fstream::badbit )
		res += ", critical error in stream buffer" ;
		
	if ( file.rdstate() & fstream::eofbit )
		res += ", End-Of-File reached while extracting" ;
		
	if ( file.rdstate() & fstream::failbit )
		res += ", failure extracting from stream" ;
		
	return res ;
	
}




int File::ConvertToFileDescriptorOpenFlag( OpeningFlag openFlag ) 
	throw( ConversionFailed, Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS


#if CEYLAN_DEBUG

	if ( openFlag == DoNotOpen )
		throw ConversionFailed( 
			"File::ConvertToFileDescriptorOpenFlag : "
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
	
	if ( Create & openFlag )
		actualFlags |= O_CREAT ;
			
	if ( Truncate & openFlag )
		actualFlags |= O_TRUNC ;
		
	if ( Append & openFlag )
		actualFlags |= O_APPEND ;

	// Binary : nothing to do here, with file descriptors.
	 
		
#if CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

	if ( NonBlocking & openFlag )
		actualFlags |= O_NONBLOCK ;
		
	if ( Synchronous & openFlag )
		actualFlags |= O_SYNC ;
	
#endif // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

	return actualFlags ;

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw Features::FeatureNotAvailableException(
		"ConvertToFileDescriptorOpenFlag : "
		"file descriptor feature is not available" ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS

}


void File::ConvertToFileDescriptorPermissionFlag( 
		PermissionFlag permissionFlag, 
		SystemSpecificPermissionFlag & returned ) 
	throw( ConversionFailed, Features::FeatureNotAvailableException )
{
	
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

	throw Features::FeatureNotAvailableException( 
		"ConvertToFileDescriptorPermissionFlag : "
		"file descriptor feature is not available" ) ;

#endif	 // CEYLAN_USES_FILE_DESCRIPTORS

}



ios_base::openmode File::ConvertToStreamOpenFlag( 
		OpeningFlag openFlag, const string & filename ) 
	throw( ConversionFailed )
{


#if CEYLAN_DEBUG

	if ( openFlag == DoNotOpen )
		throw ConversionFailed( 
			"File::ConvertToFileDescriptorOpenFlag : "
			"flags specify that the file is not to be opened." ) ;
			
#endif // CEYLAN_DEBUG


	ios_base::openmode actualFlags = 
		static_cast<ios_base::openmode>( 0 ) ;

	if ( Read & openFlag )
		actualFlags |= fstream::in ;
	
	if ( Write & openFlag )
		actualFlags |= fstream::out ;
	
	// If Create is not set, then the file is expected to exist already :
	if ( ! ( Create & openFlag ) )
	{
	
		// No creation requested, checking the file already exists :
		
		if ( ! Exists( filename ) )
			throw File::CouldNotOpen( "File::reopen : the file '"
				+ filename + "' does not exist whereas it should "
				"( no 'Create' attribute in opening openFlag)" ) ; 
				
	}
	
	if ( Truncate & openFlag )
		actualFlags |= fstream::trunc ;
	
	if ( Append & openFlag )
		actualFlags |= fstream::app ;
	
	if ( Binary & openFlag )
		actualFlags |= fstream::binary ;
	
	// fstream::ate not used.

	return actualFlags ;

}



/*
 *
 * Not supported by the underlying C++ stream layer :
 
FIXME File::ConvertToStreamPermissionFlag( PermissionFlag permissionFlag ) 
	throw( ConversionFailed )
{

}

 *
 */


void File::FromFDtoFD( FileDescriptor from, FileDescriptor to, Size length )
	throw( IOException, Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	Size written = 0 ;
	
	Size bufferSize = ( length > BigBufferSize ? BigBufferSize : length ) ;

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
		
	}
	catch( const Ceylan::Exception & e)
	{

		delete [] buf ;
		throw IOException( "File::FromFDtoFD failed : " + e.toString() ) ;
		
	}	

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw Features::FeatureNotAvailableException( "File::FromFDtoFD : "
		"file descriptor feature not available" ) ;
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS	
	
}

 
