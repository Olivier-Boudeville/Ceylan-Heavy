#include "CeylanOutputStream.h" 

#include "CeylanStringUtils.h" // for StringSize


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H



extern "C"
{

#ifdef CEYLAN_USES_MACHINE_BSWAP_H
#include <machine/bswap.h>     // for bswap_16, etc.
#endif // CEYLAN_USES_MACHINE_BSWAP_H

}


using namespace Ceylan::System ;
using namespace Ceylan::System ;


using std::string ;




OutputStream::OutputStream() throw()
{

}
		
		
OutputStream::~OutputStream() throw()
{

}


Size OutputStream::write( const Ceylan::Byte * buffer, Size length ) 
	throw( WriteFailedException )
{

	throw WriteFailedException( "In OutputStream::write failed : "
		"this method should have been subclassed." ) ;

}

	
	
void OutputStream::writeSint8( Ceylan::Sint8 toWrite ) 
	throw( WriteFailedException )
{

	const Ceylan::Uint8 TypeSize = 1 ;
		
	Size writeCount = write( 
		reinterpret_cast<Ceylan::Byte*>( & toWrite ), TypeSize ) ;
	
	if ( writeCount < TypeSize ) 
		throw WriteFailedException( "In OutputStream::writeSint8" ) ;
	
}	


void OutputStream::writeUint8( Ceylan::Uint8 toWrite ) 
	throw( WriteFailedException )
{

	const Ceylan::Uint8 TypeSize = 1 ;
		
	Size writeCount = write( 
		reinterpret_cast<Ceylan::Byte*>( & toWrite ), TypeSize ) ;
	
	if ( writeCount < TypeSize ) 
		throw WriteFailedException( "In OutputStream::writeUint8" ) ;
	
}	



void OutputStream::writeSint16( Ceylan::Sint16 toWrite ) 
	throw( WriteFailedException )
{

	const Ceylan::Uint8 TypeSize = 2 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = bswap_16( toWrite ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
			
	Size writeCount = write( 
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;
	
	if ( writeCount < TypeSize ) 
		throw WriteFailedException( "In OutputStream::writeSint16" ) ;
	
}	


void OutputStream::writeUint16( Ceylan::Uint16 toWrite ) 
	throw( WriteFailedException )
{

	const Ceylan::Uint8 TypeSize = 2 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = bswap_16( toWrite ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
			
	Size writeCount = write( 
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;
	
	if ( writeCount < TypeSize ) 
		throw WriteFailedException( "In OutputStream::writeUint16" ) ;
	
}	



void OutputStream::writeSint32( Ceylan::Sint32 toWrite ) 
	throw( WriteFailedException )
{

	const Ceylan::Uint8 TypeSize = 4 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = bswap_32( toWrite ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
			
	Size writeCount = write( 
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;
	
	if ( writeCount < TypeSize ) 
		throw WriteFailedException( "In OutputStream::writeSint32" ) ;
	
}	


void OutputStream::writeUint32( Ceylan::Uint32 toWrite ) 
	throw( WriteFailedException )
{

	const Ceylan::Uint8 TypeSize = 4 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = bswap_32( toWrite ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
			
	Size writeCount = write( 
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;
	
	if ( writeCount < TypeSize ) 
		throw WriteFailedException( "In OutputStream::writeUint32" ) ;
	
}	


void OutputStream::writeFloat32( Ceylan::Float32 toWrite ) 
	throw( WriteFailedException )
{

	const Ceylan::Uint8 TypeSize = 4 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = bswap_32( toWrite ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
			
	Size writeCount = write( 
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;
	
	if ( writeCount < TypeSize ) 
		throw WriteFailedException( "In OutputStream::writeFloat32" ) ;
	
}	


void OutputStream::writeFloat64( Ceylan::Float64 toWrite ) 
	throw( WriteFailedException )
{

	const Ceylan::Uint8 TypeSize = 8 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = bswap_64( toWrite ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
			
	Size writeCount = write( 
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;
	
	if ( writeCount < TypeSize ) 
		throw WriteFailedException( "In OutputStream::writeFloat64" ) ;
	
}	


void OutputStream::writeString( string & toWrite )
	throw( WriteFailedException )
{

	StringSize stringSize = toWrite.size() ;
	writeUint16( stringSize ) ;
	
	Size writeCount = write( toWrite.data(), stringSize ) ;
	
	if ( writeCount < stringSize ) 
		throw WriteFailedException( "In OutputStream::writeString" ) ;
	
}


