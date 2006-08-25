#include "CeylanMemoryStream.h"

#include "CeylanLogPlug.h"     // for Log primitives
#include "CeylanOperators.h"   // for toString
#include "CeylanStringUtils.h" // for StringSize


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"               // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#if CEYLAN_USES_STRING_H
#include <string.h>           // for memcpy
#endif // CEYLAN_USES_STRING_H


}



using std::string ;


using namespace Ceylan::System ;
using namespace Ceylan::Log ;





MemoryStream::MemoryStreamException::MemoryStreamException( 
		const string & reason ) throw() :
	SystemException( reason )
{

}


MemoryStream::MemoryStreamException::~MemoryStreamException() throw()
{

}



MemoryStream::MemoryStream( Size bufferSize ) throw():
	InputOutputStream(),
	_size( bufferSize ),
	_index( 0 ),
	_buffer()	
{

	_buffer	= new Ceylan::Byte[ bufferSize ] ;
		
}


MemoryStream::~MemoryStream() throw()
{

	delete [] _buffer ;	
	
}


bool MemoryStream::close() throw( Stream::CloseException )
{

	return false ;
	
}


Size MemoryStream::getIndex() const throw()
{ 

	return _index ; 
	
}


Size MemoryStream::getSize() const throw()
{

	return _size ;
	
}


/*
 * write : adds a block in buffer after current index.
 * read : removes a block in buffer before current index.
 *
 */	


Size MemoryStream::read( Ceylan::Byte * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#ifdef CEYLAN_USES_MEMCPY

	LogPlug::debug( "MemoryStream::read begin : " + toString() ) ;
	
	if ( maxLength > _index )
		throw ReadFailedException( "MemoryStream::read : "
			"attempt to read more than available in the buffer" ) ;

	_index -= maxLength ;

	::memcpy( buffer, _buffer + _index, maxLength ) ;

	LogPlug::debug( "MemoryStream::read end : " + toString() ) ;
	
	return maxLength ;
	
#else // CEYLAN_USES_MEMCPY

	throw InputStream::ReadFailedException( "MemoryStream::read : "
		"not supported on this platform" ) ;
		
#endif // CEYLAN_USES_MEMCPY

}


bool MemoryStream::hasAvailableData() const throw()
{
	return _index > 0 ;
}


Size MemoryStream::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

	return write( message.c_str(), message.size() ) ;
	
}


Size MemoryStream::write( const Ceylan::Byte * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{

	LogPlug::debug( "MemoryStream::write begin : " + toString() ) ;

	if ( _index + maxLength > _size )
		throw WriteFailedException( "MemoryStream::write : "
			"buffer overflow, too much data for it" ) ;
	
	::memcpy( _buffer + _index, buffer, maxLength ) ;

	_index += maxLength ;

	LogPlug::debug( "MemoryStream::write end : " + toString() ) ;
	
	return maxLength ;
	
}



StreamID MemoryStream::getStreamID() const throw()
{

	return reinterpret_cast<StreamID>( this ) ;
	
}


StreamID MemoryStream::getInputStreamID() const throw()
{
	return getStreamID() ;
}


StreamID MemoryStream::getOutputStreamID() const throw()
{
	return getStreamID() ;
}


const std::string MemoryStream::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "MemoryStream object of size " + Ceylan::toString( _size )
		+ " bytes, whose index position is " + Ceylan::toString( _index )
		+ " bytes, whose ID is " + Ceylan::toString( getStreamID() ) ;
		
	return res ;
	
}

 
