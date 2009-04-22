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


#include "CeylanFile.h"

#include "CeylanLogPlug.h"           // for Log primitives
#include "CeylanDirectory.h"         // for Directory
#include "CeylanOperators.h"         // for toString
#include "CeylanStringUtils.h"       // for StringSize
#include "CeylanFileSystemManager.h" // for FileSystemManager



using std::string ;


using namespace Ceylan ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;




/*
 * We need our own bitmasks, instead of using #ifdef/#endif pair testing
 * CEYLAN_USES_FILE_DESCRIPTORS, since we allow Read and Write to be 
 * OR'd, whereas for example O_RDONLY | O_WRONLY might be different from
 * O_RDWR.
 *
 */
const OpeningFlag File::Read          = 0x0001 ;
const OpeningFlag File::Write         = 0x0002 ;
const OpeningFlag File::CreateFile    = 0x0004 ;
const OpeningFlag File::TruncateFile  = 0x0008 ;
const OpeningFlag File::AppendFile    = 0x0010 ;
const OpeningFlag File::Binary        = 0x0020 ;
const OpeningFlag File::NonBlocking   = 0x0040 ;
const OpeningFlag File::Synchronous   = 0x0080 ;


// Special cases:

const OpeningFlag File::CreateToWriteBinary = 
	Read | Write | CreateFile | TruncateFile | Binary ;


const OpeningFlag File::OpenToReadBinary = Read | Binary ;


const OpeningFlag File::DoNotOpen = 0xffff ;



// Same case: bitmasks for permission flags.

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




// Numerous FileException child classes:	
	
	
	
FileReadLockingFailed::FileReadLockingFailed( const string & reason ) :
	FileException( reason )
{

}
			
	
	
FileReadUnlockingFailed::FileReadUnlockingFailed( const string & reason ) :
	FileException( reason )
{

}
				
	
	
FileWriteLockingFailed::FileWriteLockingFailed( const string & reason ) :
	FileException( reason )
{

}
		
		
		
FileWriteUnlockingFailed::FileWriteUnlockingFailed( const string & reason ) :
	FileException( reason )
{

}
			


FileDelegatingException::FileDelegatingException( const string & reason ) :
	FileException( reason )
{

}




// File implementation section.


// Static section.


bool File::ExistsAsFileOrSymbolicLink( const string & filename ) 
{

	// Let FileLookupFailed and FileDelegatingException propagate:
	return GetCorrespondingFileSystemManager().existsAsFileOrSymbolicLink(
		filename ) ;
				
}



bool File::Exists( const string & filename ) 	
{

	// Let FileLookupFailed and FileDelegatingException propagate:
	return ExistsAsFileOrSymbolicLink( filename ) ;
				
}



void File::Remove( const string & filename )
{

	// Let FileRemoveFailed and FileDelegatingException propagate:
	GetCorrespondingFileSystemManager().removeFile( filename ) ;	

}



void File::Move( const string & sourceFilename, const string & targetFilename )	
{

	// Let FileMoveFailed and FileDelegatingException propagate:
	GetCorrespondingFileSystemManager().moveFile( sourceFilename,
		targetFilename ) ;

}
	
	
										
void File::Copy( const string & sourceFilename, const string & targetFilename )	
{

	// Let FileCopyFailed and FileDelegatingException propagate:
	GetCorrespondingFileSystemManager().copyFile( sourceFilename, 
		targetFilename ) ;

}



Size File::GetSize( const string & filename )
{

	// Let FileSizeRequestFailed and FileDelegatingException propagate:
	return GetCorrespondingFileSystemManager().getSize( filename ) ;

}



time_t File::GetLastChangeTime( const string & filename )
{

	/*
	 * Let FileLastChangeTimeRequestFailed and FileDelegatingException
	 * propagate:
	 *
	 */
	return GetCorrespondingFileSystemManager().getLastChangeTimeFile( 
		filename ) ;

}

	
						
string File::TransformIntoValidFilename( const string & rawFilename ) 
{

	// Let FileDelegatingException propagates:
	return GetCorrespondingFileSystemManager().transformIntoValidFilename( 
		rawFilename ) ;
			
}



void File::Touch( const string & filename )
{

	// Let FileTouchFailed and FileDelegatingException propagate:
	GetCorrespondingFileSystemManager().touch( filename ) ;
			
}



bool File::Diff( const string & firstFilename, const string & secondFilename ) 
{

	// Let FileDiffFailed and FileDelegatingException propagate:
	return GetCorrespondingFileSystemManager().diff( firstFilename,
		secondFilename ) ;

}




// Constructors are in protected section.	


// Factories section.



File & File::Create( const std::string & filename, OpeningFlag createFlag,
	PermissionFlag permissionFlag )
{

	return GetCorrespondingFileSystemManager().createFile( filename, createFlag,
		permissionFlag ) ;
		
}



File & File::Open( const std::string & filename, OpeningFlag openFlag ) 
{

	return GetCorrespondingFileSystemManager().openFile( filename, openFlag ) ;

}



File::~File() throw()
{

	/*
	 * Could not be factorized here, as reopen is abstract here:
	 * 

	try
	{
		close() ;
	}
	catch( const Stream::CloseException & e )
	{
		LogPlug::error( "File destructor: close failed: " + e.toString() ) ;
	}
		
	 *
	 */
		
}




										
					
const std::string & File::getName() const 
{ 

	return _name ; 
	
}





// Locking section.


void File::lockForReading() const
{
	
	// Meant to be overriden:
	
	throw FileReadLockingFailed( "File::lockForReading: "
		"lock feature not available" ) ;
		
}



void File::unlockForReading() const
{

	// Meant to be overriden:
	
	throw FileReadUnlockingFailed( "File::unlockForReading: "
		"lock feature not available" ) ;
	
}



void File::lockForWriting() const
{

	// Meant to be overriden:
	
	throw FileWriteLockingFailed( "File::lockForWriting: "
		"lock feature not available" ) ;
	
}



void File::unlockForWriting() const
{

	// Meant to be overriden:
	
	throw FileWriteUnlockingFailed( "File::unlockForWriting: "
		"lock feature not available" ) ;

}



bool File::isLocked() const 
{

	// Meant to be overriden:
	
	return false ;	

}



Size File::size() const
{

	// Let FileSizeRequestFailed and FileDelegatingException propagate:
	return getCorrespondingFileSystemManager().getSize( _name ) ;
	
}



void File::readExactLength( Ceylan::Byte * buffer, Size exactLength )
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
			"File::readExactLength: trying to read "
			+ Ceylan::toString( static_cast<Ceylan::Uint32>( exactLength ) ) 
			+ " bytes, actually read "
			+ Ceylan::toString( static_cast<Ceylan::Uint32>( 
					exactLength - remainder ) )
			+ " bytes before following error: " + e.toString() ) ;

	}

	if ( readFailures == maxReadFailures )
		throw InputStream::ReadFailedException( 
			"File::readExactLength: unable to read requested bytes, maybe "
			"trying to read a binary file not opened in Binary mode ?" ) ;

}



bool File::hasAvailableData() const 
{

	return true ;
	
}




void File::open( OpeningFlag openFlag, PermissionFlag permissionFlag ) 
{

	if ( _openFlag != DoNotOpen )
		throw FileAlreadyOpened( "File::open: file '" + _name
			+ "' was already opened." ) ;

	_openFlag    = openFlag ;
	_permissions = permissionFlag ;

	reopen() ;

}



void File::remove()
{

	try
	{
	
		close() ;
		
	}
	catch( const Stream::CloseException & e )
	{
		throw FileRemoveFailed( "File::remove failed: "	+ e.toString() ) ;
	}


	// Let FileDelegatingException and FileRemoveFailed propagate:
	getCorrespondingFileSystemManager().removeFile( _name ) ;
		
}



StreamID File::getInputStreamID() const 
{

	return getStreamID() ;
	
}



StreamID File::getOutputStreamID() const 
{

	return getStreamID() ;
	
}



const std::string File::toString( Ceylan::VerbosityLevels level ) 
	const 
{

	return "File object for filename '" + _name + "'" ;

}






// Protected section.


File::File( const string & name, OpeningFlag openFlag, 
	PermissionFlag permissions ) :
	InputOutputStream(),	
	_name( name ),
	_openFlag( openFlag ),
	_permissions( permissions ),
	_lockedForReading( false ),
	_lockedForWriting( false )
{

	/*
	 * Could not be factorized here as reopen is abstract here:
	 * 
	 
	if ( openFlag != DoNotOpen )
		reopen() ;
		
	 *
	 */
		
}



FileSystemManager & File::GetCorrespondingFileSystemManager()
{

	try
	{
	
		return FileSystemManager::GetAnyDefaultFileSystemManager() ;
	
	}
	catch( const FileSystemManagerException & e )
	{
		throw FileDelegatingException(
			"File::GetCorrespondingFileSystemManager failed: " 
			+ e.toString() ) ;
	}
	
	
}


