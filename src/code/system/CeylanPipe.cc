#include "CeylanPipe.h"

#include "CeylanFile.h"        // for Duplicate
#include "CeylanLogPlug.h"     // for Log primitives


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



extern "C" 
{ 

#ifdef CEYLAN_USES_SYS_TIME_H
#include <sys/time.h>          // for ::select
#endif // CEYLAN_USES_SYS_TIME_H

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>            // for ::pipe, ::select
#endif // CEYLAN_USES_UNISTD_H


#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for ::select
#endif // CEYLAN_USES_SYS_TYPES_H


#ifdef CEYLAN_USES_STRING_H
#include <string.h>            // for::select
#endif // CEYLAN_USES_STRING_H


#ifdef CEYLAN_USES_STRINGS_H
#include <strings.h>           // for::select
#endif // CEYLAN_USES_STRINGS_H


#ifdef CEYLAN_USES_SYS_SELECT_H
#include <sys/select.h>        // for::select
#endif // CEYLAN_USES_SYS_SELECT_H

}
       
using std::string ;

using namespace Ceylan::System ;
using namespace Ceylan::Log ;
using namespace Ceylan ;



Pipe::PipeException::PipeException( const string & reason ) throw() :
	SystemException( reason )
{

}


Pipe::PipeException::~PipeException() throw()
{

}



// Numerous child classes :	
			

Pipe::CouldNotCreate::CouldNotCreate( const string & reason ) 
		throw() :
	Pipe::PipeException( reason )
{

}
		

Pipe::ReadFailed::ReadFailed( const string & reason ) throw() :
	InputStream::ReadFailedException( reason )
{

}
				
	
Pipe::WriteFailed::WriteFailed( const string & reason ) throw() :
	OutputStream::WriteFailedException( reason )
{

}



Pipe::Pipe() throw( Pipe::CouldNotCreate, 
		Features::FeatureNotAvailableException ) :
	InputOutputStream()
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	if ( ::pipe( _fd ) )
	{
		_fd[ 0 ] = _fd[ 1 ] = -1 ;
		throw CouldNotCreate( "Pipe constructor : " 
			+ System::explainError() ) ;
	}
	
#else // CEYLAN_USES_FILE_DESCRIPTORS	

	throw Features::FeatureNotAvailableException( 
		"Pipe constructor called whereas the file desciptor feature "
		"is not available." ) ;
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS
	
}


Pipe::Pipe( const Pipe & other ) throw( PipeException,
		Features::FeatureNotAvailableException ) :
	Stream(),
	InputOutputStream()
{

	_fd[ 1 ] = -1 ;
	
	try
	{
	
		_fd[ 0 ] = File::Duplicate( other._fd[ 0 ] ) ;
        _fd[ 1 ] = File::Duplicate( other._fd[ 1 ] ) ;
		
	}
	catch( const File::CouldNotDuplicate & e )
	{
		throw PipeException( "Pipe copy constructor failed : "
			+ e.toString() ) ;
	}		
}



Pipe::~Pipe() throw()
{

	try
	{
	
		close() ;
	
	}
	catch( const Stream::CloseException	& e )
	{
		LogPlug::error( "Pipe destructor failed : " + e.toString() ) ;
	}
	
	
}


Size Pipe::read( char * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

	SignedSize n = System::FDRead( _fd[ 0 ], buffer, maxLength ) ;

	// Actually, n should never be negative :
	if ( n < 0 )
		throw ReadFailedException( "Pipe::read failed : " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;
	
}


Size Pipe::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

	return write( message.c_str(), message.size() ) ;
	
}


Size Pipe::write( const char * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	SignedSize n = System::FDWrite( _fd[ 1 ], buffer, maxLength ) ;

	if ( n < static_cast<SignedSize>( maxLength ) )
		throw WriteFailedException( "Pipe::write failed : " 
			+ System::explainError() ) ;

	return static_cast<Size>( n ) ;

#else // CEYLAN_USES_FILE_DESCRIPTORS	

	throw Features::FeatureNotAvailableException( 
		"Pipe::write called whereas the file desciptor feature "
		"is not available." ) ;
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}


bool Pipe::hasAvailableData() const throw()
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	struct timeval tv ;
	tv.tv_sec  = 0 ;
	tv.tv_usec = 0 ;
	
	// Creates the set of waiting file descriptors :
	fd_set set ;
	FD_ZERO( & set ) ;
	FD_SET( _fd[ 0 ], & set ) ;
	
	Ceylan::Sint32 n = ::select( _fd[ 0 ] + 1, & set, 0, 0, & tv ) ;
	
	if ( n > 0 )
		return FD_ISSET( _fd[ 0 ], & set ) ;
	
	if ( n == -1 )
		LogPlug::error( "Pipe::hasAvailableData failed : " 
			+ System::explainError() ) ;

	return false ;

#else // CEYLAN_USES_FILE_DESCRIPTORS	

	LogPlug::error( "Pipe::hasAvailableData failed : "
		"pipe support not available." ) ; 
		
	return false ;
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS
				
}


void Pipe::clearInput() throw()
{

	char c ;
	
	while( hasAvailableData() ) 
		read( &c, 1 ) ;
		
}


bool Pipe::close() throw( Stream::CloseException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	bool res = Stream::Close( _fd[ 0 ] ) ;
	
	if ( Stream::Close( _fd[ 1 ] ) )
		return true ;
	else
		return res ;	

#else

	throw Stream::CloseException( "Pipe::close failed : "
		"pipe support not available." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}


StreamID Pipe::getInputStreamID() const throw()
{
	return getReadFileDescriptor() ;
}


StreamID Pipe::getOutputStreamID() const throw()
{
	return getOutputStreamID() ;
}


FileDescriptor Pipe::getReadFileDescriptor() const throw()
{
	return _fd[ 0 ] ;
}


FileDescriptor Pipe::getWriteFileDescriptor() const throw()
{
	return _fd[ 1 ] ;
}

