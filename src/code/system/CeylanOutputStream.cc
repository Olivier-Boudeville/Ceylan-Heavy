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


#include "CeylanOutputStream.h"

#include "CeylanStringUtils.h" // for StringSize
#include "CeylanOperators.h"   // for toString
#include "CeylanLogPlug.h"     // for LogPlug
#include "CeylanEndianness.h"  // for ceylan_bswap_*, etc.


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H





using namespace Ceylan::System ;
using namespace Ceylan::Log ;


using std::string ;




OutputStream::OutputStream( bool blocking ) :
	Stream( blocking )
{

}



OutputStream::~OutputStream() throw()
{

}



Size OutputStream::write( const std::string & message )
{

	throw WriteFailedException(
		"In OutputStream::write (std::string based) failed: "
		"this method should have been subclassed." ) ;

}



Size OutputStream::write( const Ceylan::Byte * buffer, Size length )
{

	throw WriteFailedException(
		"In OutputStream::write (buffer-based) failed: "
		"this method should have been subclassed." ) ;

}



void OutputStream::writeSint8( Ceylan::Sint8 toWrite )
{

	const Ceylan::Uint8 TypeSize = 1 ;

	Size writeCount = write(
		reinterpret_cast<Ceylan::Byte*>( & toWrite ), TypeSize ) ;

	if ( writeCount < TypeSize )
		throw WriteFailedException( "In OutputStream::writeSint8" ) ;

}



void OutputStream::writeUint8( Ceylan::Uint8 toWrite )
{

	const Ceylan::Uint8 TypeSize = 1 ;

	Size writeCount = write(
		reinterpret_cast<Ceylan::Byte*>( & toWrite ), TypeSize ) ;

	if ( writeCount < TypeSize )
		throw WriteFailedException( "In OutputStream::writeUint8" ) ;

}



void OutputStream::writeSint16( Ceylan::Sint16 toWrite )
{

	const Ceylan::Uint8 TypeSize = 2 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.

#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = ceylan_bswap_16( toWrite ) ;

#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	Size writeCount = write(
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;

	if ( writeCount < TypeSize )
		throw WriteFailedException( "In OutputStream::writeSint16" ) ;

}



void OutputStream::writeUint16( Ceylan::Uint16 toWrite )
{

	const Ceylan::Uint8 TypeSize = 2 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.

#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = ceylan_bswap_16( toWrite ) ;

#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	Size writeCount = write(
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;

	if ( writeCount < TypeSize )
		throw WriteFailedException( "In OutputStream::writeUint16" ) ;

}



void OutputStream::writeSint32( Ceylan::Sint32 toWrite )
{

	const Ceylan::Uint8 TypeSize = 4 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.

#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = ceylan_bswap_32( toWrite ) ;

#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	Size writeCount = write(
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;

	if ( writeCount < TypeSize )
		throw WriteFailedException( "In OutputStream::writeSint32" ) ;

}



void OutputStream::writeUint32( Ceylan::Uint32 toWrite )
{

	const Ceylan::Uint8 TypeSize = 4 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.

#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	toWrite = ceylan_bswap_32( toWrite ) ;

#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	Size writeCount = write(
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;

	if ( writeCount < TypeSize )
		throw WriteFailedException( "In OutputStream::writeUint32" ) ;

}



void OutputStream::writeFloat32( Ceylan::Float32 toWrite )
{

	const Ceylan::Uint8 TypeSize = 4 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.

#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	Ceylan::Uint32 * tmp = reinterpret_cast<Ceylan::Uint32 *>( &toWrite ) ;

	// Updates 'toWrite':
	*tmp = ceylan_bswap_32( *tmp ) ;

#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	Size writeCount = write(
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;

	if ( writeCount < TypeSize )
		throw WriteFailedException( "In OutputStream::writeFloat32" ) ;

}



void OutputStream::writeFloat64( Ceylan::Float64 toWrite )
{

	const Ceylan::Uint8 TypeSize = 8 ;

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	// Nothing to do.

#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN


	Ceylan::Uint64 * tmp = reinterpret_cast<Ceylan::Uint64 *>( & toWrite ) ;

	// Updates 'toWrite':

#ifdef CEYLAN_FAKES_64_BIT_TYPE

	Ceylan::byteswap( *tmp ) ;

#else // CEYLAN_FAKES_64_BIT_TYPE

	*tmp = ceylan_bswap_64( *tmp ) ;

#endif // CEYLAN_FAKES_64_BIT_TYPE


#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	Size writeCount = write(
		reinterpret_cast<Ceylan::Byte *>( & toWrite ), TypeSize ) ;

	if ( writeCount < TypeSize )
		throw WriteFailedException( "In OutputStream::writeFloat64" ) ;

}



void OutputStream::writeString( const string & toWrite )
{

	StringSize stringSize = toWrite.size() ;

	if ( stringSize > Ceylan::Uint16Max )
		throw WriteFailedException( "OutputStream::writeString: "
			"string '" + toWrite + "' is too long." ) ;

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
	Log::LogPlug::debug( "OutputStream::writeString: string size is "
		+ Ceylan::toString( static_cast<Ceylan::Uint32>( stringSize ) )
		+ " characters." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS

	writeUint16( static_cast<Ceylan::Uint16>( stringSize ) ) ;

	Size writeCount = write( toWrite.data(), stringSize ) ;

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
	LogPlug::debug( "OutputStream::writeString: wrote "
		+ Ceylan::toString( static_cast<Ceylan::Uint32>( writeCount ) )
		+ " bytes." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS

	if ( writeCount < stringSize )
		throw WriteFailedException( "In OutputStream::writeString" ) ;

}



const std::string OutputStream::toString( Ceylan::VerbosityLevels level ) const
{

	string res = "OutputStream whose ID is "
		+ Ceylan::toString( getOutputStreamID() ) ;

	res += ". This is a " + Stream::toString( level ) ;

	return res ;

}
