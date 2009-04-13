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


#include "CeylanLibfatFile.h"

#include "CeylanLogPlug.h"                    // for Log primitives
#include "CeylanOperators.h"                  // for toString
#include "CeylanLibfatFileSystemManager.h"    // for LibfatFileSystemManager


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


// Not available in their C++ form:
extern "C"
{

#if CEYLAN_ARCH_NINTENDO_DS

#include "fat.h"        // for Chishm's libfat

#include <fcntl.h> 
#include <unistd.h> 

#endif // CEYLAN_ARCH_NINTENDO_DS

}

#include <cstdlib>

#include <cerrno>    // for EINTR, ENOLCK, etc.
#include <cstdio>    // for unlink


/*
 * Implementation notes.
 *
 * @note In a non-static method, no static method should be used, as the former
 * is expected to use the libfat filesystem manager, whereas the latter shall
 * use the default filesystem manager, which may or may not be the libfat
 * one.
 * 
 */


using std::string ;


using namespace Ceylan ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


/*
 * We will be using here file-descriptor based file I/O only.
 *
 */




LibfatFileException::LibfatFileException( const string & reason ) throw():
	FileException( reason )
{

}


LibfatFileException::~LibfatFileException() throw()
{

}

	
		
// LibfatFile implementation.


LibfatFile::~LibfatFile() throw()
{

	// FAT filesystem is using a cache, files have to be closed before shutdown:
	
	if ( isOpen() )
	{
	
		try
		{
			close() ;
		}
		catch( const Stream::CloseException & e )
		{
			LogPlug::error( "LibfatFile destructor: close failed: " 
				+ e.toString() ) ;
		}
		
	}
		
}




// Constructors are in protected section.	

	



// Implementation of instance methods inherited from File.	
	


bool LibfatFile::isOpen() const throw()
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7

	 return false ;
		
#else // CEYLAN_RUNS_ON_ARM7

	return ( _fdes != 0 ) ;

#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	return false ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



bool LibfatFile::close() throw( Stream::CloseException )
{

	if ( ! isOpen() )
	{
	
		LogPlug::warning( "LibfatFile::close: file '" +  _name 
			+ "' does not seem to have been already opened." ) ;
			
		return false ;	
	
	}
	else
	{
	
		// Let's close it.
		
#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
		throw Stream::CloseException( "LibfatFile::close: "
			"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7
		
		/*
		 * Exception and returned boolean are both needed:
		 *  - all closed files should have been opened previously
		 *  - returned boolean comes from the Stream-inherited signature
		 *
		 */
	 	
		return Stream::Close( _fdes ) ;

	 
#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

		throw Stream::CloseException( "LibfatFile::close: "
			"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

	}
		
}



void LibfatFile::saveAs( const string & newName ) throw( FileException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw FileException( "LibfatFile::saveAs: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7
		

	// Using the helper factory directly to access to the file descriptor:
	LibfatFile & f = LibfatFile::Create( newName ) ;
	serialize( f._fdes ) ;
	delete &f ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileException( "LibfatFile::saveAs: "
		"not supported on this platform" ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}




// Locking section.



/**
 * On Nintendo DS, usual locking primitives are not available, hence we do
 * not override the implementations inherited from File.
 *
 */



time_t LibfatFile::getLastChangeTime() const 
	throw( FileLastChangeTimeRequestFailed )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw FileException( "LibfatFile::getLastChangeTime: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	struct stat buf ;

	if ( ::stat( _name.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

	throw FileLastChangeTimeRequestFailed(
		"LibfatFile::getLastChangeTime failed for '"
		+ _name + "': " + System::explainError() ) ;

#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS


	throw FileLastChangeTimeRequestFailed( "LibfatFile::getLastChangeTime:"
		"not supported on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



Size LibfatFile::read( Ceylan::Byte * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw InputStream::ReadFailedException( "LibfatFile::read: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	setSelected( false ) ;

	try
	{	

		return System::FDRead( _fdes, buffer, maxLength ) ;	
			
	}
	catch( const Ceylan::Exception & e )
	{
		throw InputStream::ReadFailedException( 
			"LibfatFile::read failed for file '" + _name + "': " 
			+ e.toString() ) ;
	}
	
#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	throw InputStream::ReadFailedException( "LibfatFile::read:"
		"not supported on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS
		
}



Size LibfatFile::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw OutputStream::WriteFailedException( "LibfatFile::write: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	try
	{	
			
		return System::FDWrite( _fdes, message.c_str(), message.size() ) ;
		
	}
	catch( const Ceylan::Exception & e )
	{
		throw OutputStream::WriteFailedException( 
			"LibfatFile::write failed for file '" + _name + "': " 
			+ e.toString() ) ;
	}

#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	throw OutputStream::WriteFailedException( "LibfatFile::write:"
		"not supported on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



Size LibfatFile::write( const Ceylan::Byte * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw OutputStream::WriteFailedException( "LibfatFile::write: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	try
	{	
	
		return System::FDWrite( _fdes, buffer, maxLength ) ;
		
	}
	catch( const Ceylan::Exception & e )
	{
		throw OutputStream::WriteFailedException( 
			"LibfatFile::write failed for file '" + _name + "': " 
			+ e.toString() ) ;
	}

#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	throw OutputStream::WriteFailedException( "LibfatFile::write:"
		"not supported on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}





Position LibfatFile::tell() throw( FileException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw FileException( "LibfatFile::tell: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	throw FileException( "LibfatFile::tell:"
		"not currently implemented." ) ;

#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileException( "LibfatFile::tell:"
		"not supported on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void LibfatFile::seek( Position targetPosition ) throw( FileException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw FileException( "LibfatFile::seek: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	throw FileException( "LibfatFile::seek:"
		"not currently implemented." ) ;

#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileException( "LibfatFile::seek:"
		"not supported on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}






// LibfatFile-specific methods.


void LibfatFile::serialize( FileDescriptor fd ) const 
	throw( LibfatFileException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw LibfatFileException( "LibfatFile::serialize: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	// Let LibfatFileException propagate:
	FromFDtoFD( _fdes, fd, size() ) ;
	
#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	throw LibfatFileException( "LibfatFile::serialize: "
		"not supported on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}


FileDescriptor LibfatFile::getFileDescriptor() const 
	throw( LibfatFileException )
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw LibfatFileException( "LibfatFile::getFileDescriptor: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	return _fdes ;

#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	throw LibfatFileException( "LibfatFile::getFileDescriptor: "
		"not supported on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}




StreamID LibfatFile::getStreamID() const throw()
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	// No appropriate identifier found:	
	return -1 ;

#else // CEYLAN_RUNS_ON_ARM7

	return static_cast<StreamID>( getFileDescriptor() ) ;

#endif // CEYLAN_RUNS_ON_ARM7

#else // CEYLAN_ARCH_NINTENDO_DS

	// No appropriate identifier found:	
	return -1 ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



const std::string LibfatFile::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "Libfat file object for filename '" + _name + "'" ;

	res += ", with file descriptor " + Ceylan::toString( getFileDescriptor() ) ;
	
	res += ", with opening openFlags = " + Ceylan::toString( _openFlag ) 
		+ ", with mode openFlags = " + Ceylan::toString( _permissions ) ; 

	return res ;
	
}





// LibfatFile factories.


LibfatFile & LibfatFile::Create( const std::string & filename, 
		OpeningFlag createFlag ) throw( FileException )
{

	// Ensures creation is requested:
	return * new LibfatFile( filename, createFlag | File::CreateFile ) ;

}

					
LibfatFile & LibfatFile::Open( const std::string & filename, 
	OpeningFlag openFlag ) throw( FileException )
{

	// Ensures creation is not requested:
	return * new LibfatFile( filename, openFlag & ~File::CreateFile ) ;

}

	
					
										
// Protected section.


LibfatFile::LibfatFile( const string & name, OpeningFlag openFlag ) 
		throw( FileException ):
	File( name, openFlag, /* permissions ignored with libfat */ OwnerReadWrite )
{

	// (File constructor may raise FileException)
	
#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw LibfatFileException( "LibfatFile constructor: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	LibfatFileSystemManager::SecureLibfatFileSystemManager() ;
	
	if ( openFlag != DoNotOpen )
		reopen() ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw LibfatFileException( "LibfatFile constructor: "
		"not supported on this platform." ) ;
		
#endif // CEYLAN_ARCH_NINTENDO_DS
		
}



// Implementations of inherited methods.


FileSystemManager & LibfatFile::getCorrespondingFileSystemManager()
	const throw( FileDelegatingException )
{

	try
	{
	
		return LibfatFileSystemManager::GetLibfatFileSystemManager() ;
	
	}
	catch( const LibfatFileSystemManagerException & e )
	{
	
		throw FileDelegatingException(
			"LibfatFile::getCorrespondingFileSystemManager failed: "
			+ e.toString() ) ;
		
	}
	
}
	
	
	
void LibfatFile::reopen() throw( FileOpeningFailed )
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw FileOpeningFailed( "LibfatFile::reopen: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	_fdes = ::open( _name.c_str(), 
		ConvertToFileDescriptorOpenFlag( _openFlag ) ) ;

	if ( _fdes < 0 )
		throw FileOpeningFailed( "LibfatFile::reopen failed for '" + _name
			+ "': " + System::explainError() ) ;
			
#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw FileOpeningFailed( "LibfatFile::reopen: "
		"not supported on this platform." ) ;
		
#endif // CEYLAN_ARCH_NINTENDO_DS

}



string LibfatFile::interpretState() const throw()
{

	return "LibfatFile uses file descriptor " + Ceylan::toString( _fdes ) ;
	
}




// Conversion helper subsection.


int LibfatFile::ConvertToFileDescriptorOpenFlag( OpeningFlag openFlag ) 
	throw( ConversionFailed )
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw ConversionFailed( "LibfatFile::ConvertToFileDescriptorOpenFlag: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	
#if CEYLAN_DEBUG

	if ( openFlag == DoNotOpen )
		throw ConversionFailed( 
			"LibfatFile::ConvertToFileDescriptorOpenFlag: "
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

	return actualFlags ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw ConversionFailed( "LibfatFile::ConvertToFileDescriptorOpenFlag: "
		"not supported on this platform." ) ;
		
#endif // CEYLAN_ARCH_NINTENDO_DS

}


					
					


// Private section.															

	
	
void LibfatFile::FromFDtoFD( FileDescriptor from, FileDescriptor to,
	Size length ) throw( LibfatFileException )
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw LibfatFileException( "LibfatFile::FromFDtoFD: "
		"not supported on the ARM7 (no libfat available)" ) ;

#else // CEYLAN_RUNS_ON_ARM7

	LibfatFileSystemManager::SecureLibfatFileSystemManager() ;
	
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
		throw LibfatFileException( 
			"LibfatFile::FromFDtoFD failed: " + e.toString() ) ;
		
	}	

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	throw LibfatFileException( "LibfatFile::FromFDtoFD: "
		"not supported on this platform." ) ;
		
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}

