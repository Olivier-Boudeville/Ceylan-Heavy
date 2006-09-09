#include "CeylanLightWeightMarshaller.h"

#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanInputOutputStream.h"  // for InputOutputStream

#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"             // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;
using namespace Ceylan::Middleware ;


/*
 * The endianness of the target system is to be read from the configure step, 
 * the symbol is CEYLAN_RUNS_ON_LITTLE_ENDIAN.
 *
 */
				


LightWeightMarshaller::LightWeightMarshaller( 
		System::InputOutputStream & lowerLevelStream,
		System::Size bufferedSize ) throw() :
	Marshaller( lowerLevelStream, bufferedSize )
{

}


LightWeightMarshaller::~LightWeightMarshaller() throw()
{

}




/*
 * Decoding (read) basic datatypes section.
 *
 * Decoding needs to be operated in buffered stream if an, otherwise directly
 * on the lower-level I/O stream.
 *
 * @see Ceylan::System::InputStream
 *
 */


// Decode integer types subsection.


Ceylan::Sint8 LightWeightMarshaller::decodeSint8() 
	throw( DecodeException, IOException )
{

	return getEffectiveStream().readSint8() ;
	
	// We let IOException instances propagate.
	
}


Ceylan::Uint8 LightWeightMarshaller::decodeUint8() 
	throw( DecodeException, IOException )
{

	return getEffectiveStream().readUint8() ;
	
	// We let IOException instances propagate.
	
}

					
Ceylan::Sint16 LightWeightMarshaller::decodeSint16() 
	throw( DecodeException, IOException )					
{

	return getEffectiveStream().readSint16() ;
	
	// We let IOException instances propagate.
	
}

										
Ceylan::Uint16 LightWeightMarshaller::decodeUint16() 
	throw( DecodeException, IOException )
{

	return getEffectiveStream().readUint16() ;
	
	// We let IOException instances propagate.
	
}

					
Ceylan::Sint32 LightWeightMarshaller::decodeSint32() 
	throw( DecodeException, IOException )
{

	return getEffectiveStream().readSint32() ;
	
	// We let IOException instances propagate.
	
}

					
					
Ceylan::Uint32 LightWeightMarshaller::decodeUint32() 
	throw( DecodeException, IOException )
{

	return getEffectiveStream().readUint32() ;
	
	// We let IOException instances propagate.
	
}

					
					
Ceylan::Float32 LightWeightMarshaller::decodeFloat32() 
	throw( DecodeException, IOException )
{

	return getEffectiveStream().readFloat32() ;
	
	// We let IOException instances propagate.
	
}

					
															
Ceylan::Float64 LightWeightMarshaller::decodeFloat64() 
	throw( DecodeException, IOException )
{

	return getEffectiveStream().readFloat64() ;
	
	// We let IOException instances propagate.
	
}

					
void LightWeightMarshaller::decodeString( std::string & result ) 
	throw( DecodeException, IOException )
{

	getEffectiveStream().readString( result ) ;
	
	// We let IOException instances propagate.
	
}

					
				
				
				
/*
 * Encoding (write) basic datatypes section.
 *
 * Encoding does not need to be buffered, direct writing is performed.
 *
 * @see Ceylan::System::OutputStream
 *
 */



// Encode integer types subsection.

void LightWeightMarshaller::encodeSint8( Ceylan::Sint8 toEncode )
	throw( EncodeException, System::IOException )
{

	_lowerLevelStream->writeSint8( toEncode ) ;
	
	// We let IOException instances propagate.
	
}


void LightWeightMarshaller::encodeUint8( Ceylan::Uint8 toEncode ) 
	throw( EncodeException, System::IOException )
{

	_lowerLevelStream->writeUint8( toEncode ) ;
	
	// We let IOException instances propagate.
	
}

					
void LightWeightMarshaller::encodeSint16( Ceylan::Sint16 toEncode ) 
	throw( EncodeException, System::IOException )
{

	_lowerLevelStream->writeSint16( toEncode ) ;
	
	// We let IOException instances propagate.
	
}

					
					
void LightWeightMarshaller::encodeUint16( Ceylan::Uint16 toEncode ) 
	throw( EncodeException, System::IOException )					
{

	_lowerLevelStream->writeUint16( toEncode ) ;
	
	// We let IOException instances propagate.
	
}

					
void LightWeightMarshaller::encodeSint32( Ceylan::Sint32 toEncode ) 
	throw( EncodeException, System::IOException )
{

	_lowerLevelStream->writeSint32( toEncode ) ;
	
	// We let IOException instances propagate.
	
}

					
					
void LightWeightMarshaller::encodeUint32( Ceylan::Uint32 toEncode ) 
	throw( EncodeException, System::IOException )
{

	_lowerLevelStream->writeUint32( toEncode ) ;
	
	// We let IOException instances propagate.
	
}

					
					
void LightWeightMarshaller::encodeFloat32( Ceylan::Float32 toEncode ) 
	throw( EncodeException, System::IOException )
{

	_lowerLevelStream->writeFloat32( toEncode ) ;
	
	// We let IOException instances propagate.
	
}

					
										
void LightWeightMarshaller::encodeFloat64( Ceylan::Float64 toEncode ) 
	throw( EncodeException, System::IOException )
{

	_lowerLevelStream->writeFloat64( toEncode ) ;
	
	// We let IOException instances propagate.
	
}

					
															
void LightWeightMarshaller::encodeString( std::string & toEncode ) 
	throw( EncodeException, System::IOException )
{

	_lowerLevelStream->writeString( toEncode ) ;
	
	// We let IOException instances propagate.
	
}

					
					
																				
			
const string LightWeightMarshaller::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "LightWeight" + Marshaller::toString( level ) ;
	
}


