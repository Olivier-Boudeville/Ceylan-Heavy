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


#include <climits>

// Set to 1 to debug memory stream : 
#define CEYLAN_DEBUG_MEMORY_STREAM 0

#if CEYLAN_DEBUG_MEMORY_STREAM

#define DISPLAY_DEBUG_MEMORY_STREAM(message) LogPlug::debug(message)

#else // CEYLAN_DEBUG_MEMORY_STREAM

#define DISPLAY_DEBUG_MEMORY_STREAM(message) 

#endif // CEYLAN_DEBUG_MEMORY_STREAM 

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
	_len( 0 ),
	_buffer()	
{

	_buffer	= new Ceylan::Byte[ bufferSize ] ;
		
}


MemoryStream::~MemoryStream() throw()
{

	delete [] _buffer ;	
	
}


void MemoryStream::blank() throw()
{

	_index = 0 ;
	_len = 0 ;
	
}


bool MemoryStream::close() throw( Stream::CloseException )
{

	return false ;
	
}


Size MemoryStream::getSize() const throw()
{

	return _size ;
	
}


Size MemoryStream::read( Ceylan::Byte * buffer, Size maxLength ) 
	throw( InputStream::ReadFailedException )
{

#ifdef CEYLAN_USES_MEMCPY

	DISPLAY_DEBUG_MEMORY_STREAM( "MemoryStream::read of " 
		+ Ceylan::toString( maxLength ) + " byte(s), begin : " + toString() ) ;
	
	
	/*
	 * Must not read what has not been written yet :
	 *
	 * Apparently the left part of the modulo (%) must not be negative,
	 * otherwise result is wrong :
	 *
	 */

	// Never read past the write index : 
	if ( maxLength > _len )
		throw ReadFailedException( "MemoryStream::read : "
			"attempt to read more than available in the buffer : "
			+ toString() ) ;

	/*
	 * Either the read index plus the requested size is before the end of 
	 * buffer and there is only one block to read, either the read index 
	 * wil have to loop back to the beginning of buffer, hence with two
	 * blocks.
	 *
	 */
	if ( _index + maxLength > _size )
	{
	
		DISPLAY_DEBUG_MEMORY_STREAM( 
			"MemoryStream::read : two blocks needed" ) ;
	
		/* 
		 * Most complex case : first read from read index to end of buffer,
		 * then from beginning to the rest of requested data (before write 
		 * index in all cases).
		 *
		 */
		Size firstBlockLen = _size - _index ;
		::memcpy( buffer, _buffer + _index, firstBlockLen ) ;
		::memcpy( buffer + firstBlockLen, _buffer, maxLength - firstBlockLen ) ;
			
		
	}
	else
	{
		
		DISPLAY_DEBUG_MEMORY_STREAM( 
			"MemoryStream::read : one block is enough" ) ;
		
		// Simple case : one block.
		::memcpy( buffer, _buffer + _index, maxLength ) ;
	
	}

	_index = ( _index + maxLength ) % _size ;
	_len -= maxLength ;
	
	
	/*
	 * Prefer having index at the beginning of buffer to have a bigger 
	 * usable free block :
	 *
	 */
	if ( _len == 0 )
		_index = 0 ;
			
	DISPLAY_DEBUG_MEMORY_STREAM( "MemoryStream::read end : " + toString() ) ;
	
	return maxLength ;
	
#else // CEYLAN_USES_MEMCPY

	throw InputStream::ReadFailedException( "MemoryStream::read : "
		"not supported on this platform" ) ;
		
#endif // CEYLAN_USES_MEMCPY

}


bool MemoryStream::hasAvailableData() const throw()
{
	return _len > 0 ;
}


Size MemoryStream::write( const string & message ) 
	throw( OutputStream::WriteFailedException )
{

	return write( message.c_str(), message.size() ) ;
	
}


Size MemoryStream::write( const Ceylan::Byte * buffer, Size maxLength ) 
	throw( OutputStream::WriteFailedException )
{
	
	DISPLAY_DEBUG_MEMORY_STREAM( "MemoryStream::write of " 
		+ Ceylan::toString( maxLength ) + " byte(s), begin : " + toString() ) ;

	
	/*
	 * Must not write on what has not been read yet :
	 *
	 * Apparently the left part of the modulo (%) must not be negative,
	 * otherwise result is wrong :
	 *
	 */
	 
	Size maxPossibleWriteSize = _size - _len ;
				
	DISPLAY_DEBUG_MEMORY_STREAM( "Max size for writing : " 
		+ Ceylan::toString( maxPossibleWriteSize ) ) ;
		
	// Never read past the write index : 
	if ( maxLength > maxPossibleWriteSize )
		throw WriteFailedException( "MemoryStream::write : "
			"attempt to write more than free space in buffer (which is "
			+ Ceylan::toString( static_cast<Ceylan::Uint32>( maxPossibleWriteSize ) )
			+ " byte(s)) : " + toString() ) ;

	/*
	 * Either the write index plus the requested size is before the end of 
	 * buffer and there is only one block to read, either the write index 
	 * wil have to loop back to the beginning of buffer, hence with two
	 * blocks.
	 *
	 */
	
	Size endOfBlock = ( _index + _len ) % _size ; 
	if ( endOfBlock + maxLength > _size )
	{
		
		DISPLAY_DEBUG_MEMORY_STREAM( 
			"MemoryStream::write : two blocks needed" ) ;
	
		/* 
		 * Most complex case : first write from write index to end of buffer,
		 * then from beginning to the rest of requested data.
		 *
		 */
		Size firstBlockLen = _size - endOfBlock ;
		::memcpy( _buffer + endOfBlock, buffer, firstBlockLen ) ;
		::memcpy( _buffer, buffer + firstBlockLen, maxLength - firstBlockLen ) ;
		
	}
	else
	{

		DISPLAY_DEBUG_MEMORY_STREAM( 
			"MemoryStream::write : one block is enough" ) ;
		
		// Simple case : one block.
		::memcpy( _buffer + endOfBlock, buffer, maxLength ) ;
		
		
	}
	
	_len += maxLength ;

	DISPLAY_DEBUG_MEMORY_STREAM( "MemoryStream::write end : " + toString() ) ;
	
	return maxLength ;
	
}



// Lower-level access section.


MemoryStream::Index MemoryStream::getIndexOfNextFreeChunk() const throw()
{

	return ( _index + _len ) % _size ;
	
}


Ceylan::Byte * MemoryStream::getAddressOfNextFreeChunk() const throw()
{

	return _buffer + getIndexOfNextFreeChunk() ;
	
}


Size MemoryStream::getSizeOfNextFreeChunk() const throw()
{

	if ( ( _index + _len ) > _size )
		return _index - getIndexOfNextFreeChunk() ;
	else	
		return _size - getIndexOfNextFreeChunk() ;
	
}


MemoryStream::Index MemoryStream::getBlockIndex() const throw()
{ 

	return _index ; 
	
}

Size MemoryStream::getBlockLength() const throw()
{ 

	return _len ; 
	
}


Ceylan::Byte MemoryStream::getElementAt( Index targetIndex ) const throw()
{

	return _buffer[ targetIndex % _size ] ;
	
}


void MemoryStream::increaseFilledBlockOf( Size bytesAdded ) 
	throw( MemoryStreamException )
{

	if ( bytesAdded > getSizeOfNextFreeChunk() )
		throw MemoryStreamException( "MemoryStream::increaseFilledBlockOf : "
			" would not fit in buffer." ) ;
	_len +=	bytesAdded ;	
	
}
	
	
void MemoryStream::moveFilledBlockToBufferStart() throw( MemoryStreamException )
{

	if ( _index == 0 )
		return ;
	
	/*
	 * Here the index is not at the beginning, hence it must be moved.
	 * Some cases are hard to handle :
	 *
	 * indices : 12345    must become 12345
	 * values  : CDEAB                ABCDE
	 * (elements are replacing other elements)
	 * hence a very rudimentary approach is used, with an intermediate buffer : 
	 *
	 */
	Ceylan::Byte * _permutedBuffer = new Ceylan::Byte[ _size ] ;
	
	// Two cases : one whole block to translate, or two chunks to merge :
	if ( _index + _len > _size )
	{
	
		// Two chunks, first goes from _index to end of buffer :
		Size firstChunkSize = _size - _index ;
		
		::memcpy( _permutedBuffer, _buffer + _index, firstChunkSize ) ;
		
		// Second had wrapped-around the end of buffer :
		::memcpy( _permutedBuffer + firstChunkSize, _buffer, 
			_len - firstChunkSize ) ;
		
	}
	else
	{
	
		// Only one block, easy :
		::memcpy( _permutedBuffer, _buffer + _index, _len ) ;
		
	}
	
		
	delete [] _buffer ;
	
	_buffer = _permutedBuffer ;
	
	_index = 0 ;
	// _len unchanged.
	 
}


StreamID MemoryStream::getStreamID() const throw()
{

	/*
	 * Beware to 64-bit machines.
	 *
	 * Basically we are trying to convert a pointer to an int, gcc on
	 * Linux will not accept Ceylan::Uint32 to become Ceylan::Uint64,
	 * whereas Visual C++ on 32 bit will find a pointer truncation
	 * from 'const MemoryStream * const' to Ceylan::Uint32 :
	 *
	 */
#if CEYLAN_ARCH_WINDOWS

	return static_cast<StreamID>( 
		reinterpret_cast<Ceylan::Uint64>( this ) ) ;

#else // CEYLAN_ARCH_WINDOWS

	/*
	 * long and void * have the same size in 32 bit-platforms (4 bytes)
	 * and on 64 bit ones (8 bytes).
	 *
	 */ 
	return static_cast<StreamID>( 
		reinterpret_cast<long>( this ) ) ;

#endif // CEYLAN_ARCH_WINDOWS

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

	string res = "MemoryStream object of size " 
		+ Ceylan::toString( static_cast<Ceylan::Uint32>( _size ) )
		+ " bytes, whose fill index position is " 
		+ Ceylan::toString( static_cast<Ceylan::Uint32>( _index ) ) 
		+ ", whose fill length is "
		+ Ceylan::toString( static_cast<Ceylan::Uint32>( _len ) )
		+ " byte(s), whose ID is " 
		+ Ceylan::toString( getStreamID() ) ;
	
	if ( level != Ceylan::high )
		return res ;
	
	res += ". Displaying buffer content : [ " ;
	
	for ( Index i = 0 ; i < _size-1 ; i++ )
	{
		res += Ceylan::toNumericalString( getElementAt( i ) ) + "-" ;
	}
	
	res += Ceylan::toNumericalString( getElementAt( _size ) ) ;
	
	return res + " ]" ;
	
}

 
