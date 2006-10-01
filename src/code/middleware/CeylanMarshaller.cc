#include "CeylanMarshaller.h"

#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanOperators.h"          // for toString
#include "CeylanInputOutputStream.h"  // for InputOutputStream
#include "CeylanMemoryStream.h"       // for MemoryStream


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;
using namespace Ceylan::Middleware ;


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"             // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H
	
	
			
MarshallException::MarshallException( const string & message ) throw() : 
	MiddlewareException( message )
{

}

MarshallException::~MarshallException() throw()			
{

}



DecodeException::DecodeException( const string & message ) throw() : 
	MarshallException( message )
{

}

DecodeException::~DecodeException() throw()			
{

}



EncodeException::EncodeException( const string & message ) throw() : 
	MarshallException( message )
{

}

EncodeException::~EncodeException() throw()			
{

}




Marshaller::Marshaller( System::InputOutputStream & lowerLevelStream,
		System::Size bufferedSize ) throw() :
	TextDisplayable(),
	_lowerLevelStream( & lowerLevelStream ),
	_bufferStream( 0 )
{

	if ( bufferedSize != 0 )
		_bufferStream = new MemoryStream( bufferedSize /* bytes */ ) ;
		
}


Marshaller::~Marshaller() throw()
{

	// _lowerLevelStream not owned.
	
	if ( isBuffered() )
		delete _bufferStream ;
	
}


System::Size Marshaller::retrieveData( System::Size requestedSize ) 
	throw( DecodeException )
{


#if CEYLAN_DEBUG_MARSHALLERS			
	LogPlug::trace( "Marshaller::retrieveData : " 
		+ Ceylan::toString( requestedSize ) + " byte(s) requested." ) ;
#endif // CEYLAN_DEBUG_MARSHALLERS
	
	if ( ! isBuffered() )
		throw DecodeException( "Marshaller::retrieveData : "
			"no buffer available for this operation." ) ;

	if ( requestedSize > _bufferStream->getSize()  )
		throw DecodeException( "Marshaller::retrieveData : "
			"buffer too small, requesting " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( requestedSize ) ) 
			+ " bytes whereas size of buffer is "
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( _bufferStream->getSize() ) ) ) ;

	/*
	 * Enough total size, but does requested size currently fit in buffer ?
	 *
	 */
	 
	if ( requestedSize > 
			( _bufferStream->getSize() - _bufferStream->getBlockLength() ) )
		throw DecodeException( "Marshaller::retrieveData : "
			"no enough space left in buffer, requesting " 
			+ Ceylan::toString( static_cast<Ceylan::Uint32>( requestedSize ) )
			+ " bytes whereas free space in buffer is "
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( 
					_bufferStream->getSize() - _bufferStream->getBlockLength() ) ) ) ;

	/*
	 * Here we know there is enough free space, we have to make sure that
	 * the requested size is available in one chunk, not split by the end of
	 * buffer :
	 *
	 * (we always request the full free size, even if requestedSize is not zero,
	 * as it cannot lead to asking for fewer bytes than requested)
	 *
	 */
	 
	Size targetFreeSize = _bufferStream->getSizeOfNextFreeChunk() ;
	
	if ( requestedSize > targetFreeSize )
	{
	
		/*
		 * We have here to move first the block of already read data to the 
		 * beginning of buffer :
		 *
		 */
		_bufferStream->moveFilledBlockToBufferStart() ;
		
	}
	
			
	Size readSize ;
			
	try
	{
	
#if CEYLAN_DEBUG_MARSHALLERS			
		LogPlug::debug( "Marshaller::retrieveData : will try to read " 
			+ Ceylan::toString( targetFreeSize ) + " actual byte(s)." ) ;
#endif // CEYLAN_DEBUG_MARSHALLERS
			
		// Avoid useless buffer copy thanks to in-place writing :	
		readSize = _lowerLevelStream->read(
			_bufferStream->getAddressOfNextFreeChunk(), targetFreeSize ) ;

#if CEYLAN_DEBUG_MARSHALLERS			
		LogPlug::debug( "Marshaller::retrieveData : read " 
			+ Ceylan::toString( readSize ) + " actual byte(s)." ) ;
#endif // CEYLAN_DEBUG_MARSHALLERS
			
	}
	catch( const InputStream::ReadFailedException & e )
	{
		throw DecodeException( "Marshaller::retrieveData : " + e.toString() ) ;
	}


	if ( readSize == 0 )
		LogPlug::error( "Marshaller::retrieveData : read zero byte "
			"from lower-level stream (abnormal, as should be selected)." ) ;
			
	try
	{
	
		_bufferStream->increaseFilledBlockOf( readSize ) ;	
		
	}
	catch( const MemoryStream::MemoryStreamException & e )
	{
		throw DecodeException( "Marshaller::retrieveData has a bug : "
			+ e.toString() ) ;
	}

	
#if CEYLAN_DEBUG_MARSHALLERS			
	LogPlug::trace( "Marshaller::retrieveData : " 
		+ Ceylan::toString( readSize ) + " byte(s) read, "
		+ Ceylan::toString(  _bufferStream->getBlockLength() ) 
		+ " byte(s) available now." ) ;
#endif // CEYLAN_DEBUG_MARSHALLERS
		
	return _bufferStream->getBlockLength() ;
			
}


const string Marshaller::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "Marshaller using as underlying lower-level stream "
		+ _lowerLevelStream->toString( level )  ;
	
	if ( _bufferStream != 0 )
		res += ". This marshaller has following internal buffered stream : "
			+ _bufferStream->toString( level ) ;
	else
		res += ". This marshaller has no internal buffered stream" ;
		
	return res ;	
	
}


