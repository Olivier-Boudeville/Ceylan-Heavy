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


#include "CeylanPipe.h"

#include "CeylanStandardFileSystemManager.h"  // for Duplicate
#include "CeylanLogPlug.h"                    // for Log primitives


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



extern "C" 
{ 

#ifdef CEYLAN_USES_SYS_TIME_H
#include <sys/time.h>          // for ::select
#endif // CEYLAN_USES_SYS_TIME_H

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>            // for ::pipe,::select
#endif // CEYLAN_USES_UNISTD_H


#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for ::select
#endif // CEYLAN_USES_SYS_TYPES_H


#ifdef CEYLAN_USES_STRING_H
#include <string.h>            // for ::select
#endif // CEYLAN_USES_STRING_H


#ifdef CEYLAN_USES_STRINGS_H
#include <strings.h>           // for ::select
#endif // CEYLAN_USES_STRINGS_H


#ifdef CEYLAN_USES_SYS_SELECT_H
#include <sys/select.h>        // for ::select
#endif // CEYLAN_USES_SYS_SELECT_H

}
   
       
using std::string ;

using namespace Ceylan::System ;
using namespace Ceylan::Log ;
using namespace Ceylan ;



Pipe::PipeException::PipeException( const string & reason ) :
	SystemException( reason )
{

}


Pipe::PipeException::~PipeException() throw()
{

}



// Numerous child classes:	
			

Pipe::CouldNotCreate::CouldNotCreate( const string & reason ) :
	Pipe::PipeException( reason )
{

}
		


Pipe::ReadFailed::ReadFailed( const string & reason ) :
	InputStream::ReadFailedException( reason )
{

}
	
				
	
Pipe::WriteFailed::WriteFailed( const string & reason ) :
	OutputStream::WriteFailedException( reason )
{

}




Pipe::Pipe() :
	InputOutputStream()
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw Pipe::CouldNotCreate( "Pipe constructor failed: "
		"not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_USES_FILE_DESCRIPTORS

	if ( ::pipe( _fd ) )
	{
		_fd[ 0 ] = _fd[ 1 ] = -1 ;
		throw CouldNotCreate( "Pipe constructor: " + System::explainError() ) ;
	}
	
#else // CEYLAN_USES_FILE_DESCRIPTORS	

	throw CouldNotCreate( 
		"Pipe constructor called whereas the file desciptor feature "
		"is not available." ) ;
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



Pipe::Pipe( const Pipe & other ) :
	Stream(),
	InputOutputStream()
{

#if CEYLAN_ARCH_NINTENDO_DS

	throw Pipe::CouldNotCreate( "Pipe constructor failed: "
		"not available on the Nintendo DS." ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

	_fd[ 1 ] = -1 ;
	
	try
	{
	
		_fd[ 0 ] = StandardFileSystemManager::Duplicate( other._fd[ 0 ] ) ;
        _fd[ 1 ] = StandardFileSystemManager::Duplicate( other._fd[ 1 ] ) ;
		
	}
	catch( const System::DuplicateFailed & e )
	{
		throw PipeException( "Pipe copy constructor failed: "
			+ e.toString() ) ;
	}
			
#endif // CEYLAN_ARCH_NINTENDO_DS

}



Pipe::~Pipe() throw()
{

	try
	{
	
		close() ;
	
	}
	catch( const Stream::CloseException	& e )
	{
		LogPlug::error( "Pipe destructor failed: " + e.toString() ) ;
	}
	
	
}



Size Pipe::read( char * buffer, Size maxLength ) 
{

	setSelected( false ) ;
	
	SignedSize n = static_cast<SignedSize>( 
		System::FDRead( _fd[ 0 ], buffer, maxLength ) ) ;

	// Actually, n should never be negative:
	if ( n < 0 )
		throw ReadFailedException( "Pipe::read failed: " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;
	
}



Size Pipe::write( const string & message ) 
{

	return write( message.c_str(), message.size() ) ;
	
}



Size Pipe::write( const char * buffer, Size maxLength ) 
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	SignedSize n = System::FDWrite( _fd[ 1 ], buffer, maxLength ) ;

	if ( n < static_cast<SignedSize>( maxLength ) )
		throw WriteFailedException( "Pipe::write failed: " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;

#else // CEYLAN_USES_FILE_DESCRIPTORS	

	throw OutputStream::WriteFailedException( 
		"Pipe::write called whereas the file desciptor feature "
		"is not available." ) ;
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}



bool Pipe::hasAvailableData() const
{

	return System::HasAvailableData( _fd[ 0 ] ) ;
				
}



void Pipe::clearInput()
{

	char c ;
	
	while( hasAvailableData() ) 
		read( &c, 1 ) ;
		
}



bool Pipe::close()
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	bool res = Stream::Close( _fd[ 0 ] ) ;
	
	if ( Stream::Close( _fd[ 1 ] ) )
		return true ;
	else
		return res ;	

#else

	throw Stream::CloseException( "Pipe::close failed: "
		"pipe support not available." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}



StreamID Pipe::getInputStreamID() const
{

	return getReadFileDescriptor() ;
	
}



StreamID Pipe::getOutputStreamID() const
{

	return getOutputStreamID() ;
	
}




// Protected section.


FileDescriptor Pipe::getReadFileDescriptor() const
{

	return _fd[ 0 ] ;
	
}



FileDescriptor Pipe::getWriteFileDescriptor() const
{

	return _fd[ 1 ] ;
	
}

