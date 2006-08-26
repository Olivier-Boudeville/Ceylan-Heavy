#include "CeylanMarshaller.h"

#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanInputOutputStream.h"  // for InputOutputStream
#include "CeylanMemoryStream.h"       // for MemoryStream


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;
using namespace Ceylan::Middleware ;


				
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
	
	if ( _bufferStream != 0 )
		delete _bufferStream ;
	
}


Size Marshaller::retrieveData() throw( DecodeException )
{

	if ( _bufferStream == 0 )
		throw DecodeException( "Marshaller::retrieveData : "
			"no buffer available for this operation." ) ;
	
	Size targetFreeSize = _bufferStream->getSizeOfNextFreeChunk() ;
	
	if ( targetFreeSize == 0 )
		throw DecodeException( "Marshaller::retrieveData : "
			"buffer full." ) ;
			
	Size readSize ;
			
	try
	{
	
		// Avoid useless buffer copy thanks to in-place writing :	
		readSize =_lowerLevelStream->read(
			_bufferStream->getAddressOfNextFreeChunk(), targetFreeSize ) ;
			
	}
	catch( const InputStream::ReadFailedException & e )
	{
		throw DecodeException( "Marshaller::retrieveData : " + e.toString() ) ;
	}

	try
	{
	
		_bufferStream->increaseFilledBlockOf( readSize ) ;	
		
	}
	catch( const MemoryStream::MemoryStreamException & e )
	{
		throw DecodeException( "Marshaller::retrieveData has a bug : "
			+ e.toString() ) ;
	}
		
	return 	_bufferStream->getBlockLength() ;
			
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


