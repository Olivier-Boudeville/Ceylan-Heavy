/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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


#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Middleware ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/**
 * Test of Ceylan light-weight marshaller utilities.
 *
 * @see Ceylan::Middleware.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


    try
    {


        LogPlug::info( "Testing Ceylan's light-weight "
			"marshaller implementation." ) ;


		LogPlug::info( "Creating a fake input/output stream in memory." ) ;
		
		MemoryStream * bufferStream = new MemoryStream( 1000 /* bytes */ ) ;
		
		LightWeightMarshaller * myMarshaller = new LightWeightMarshaller(
			*bufferStream ) ;
		
		
		LogPlug::info( "Testing integer marshalling." ) ;
		
		Sint8 encodedSint8 = 122 ;
		myMarshaller->encodeSint8( encodedSint8 ) ;
		
		Sint8 decodedSint8 = myMarshaller->decodeSint8() ;
		
		if ( encodedSint8 != decodedSint8 )
			throw TestException( "Marshalling of Sint8 failed : encoded '"
				+ Ceylan::toNumericalString( encodedSint8 )
				+ "', decoded '" + Ceylan::toNumericalString( decodedSint8 )
				+ "'." ) ;
	
				
		Uint8 encodedUint8 = 202 ;
		myMarshaller->encodeUint8( encodedUint8 ) ;
		
		Uint8 decodedUint8 = myMarshaller->decodeUint8() ;
		
		if ( encodedUint8 != decodedUint8 )
			throw TestException( "Marshalling of Uint8 failed : encoded '"
				+ Ceylan::toNumericalString( encodedUint8 )
				+ "', decoded '" + Ceylan::toNumericalString( decodedUint8 )
				+ "'." ) ;
				

		Sint16 encodedSint16 = 12345 ;
		myMarshaller->encodeSint16( encodedSint16 ) ;
		
		Sint16 decodedSint16 = myMarshaller->decodeSint16() ;
		
		if ( encodedSint16 != decodedSint16 )
			throw TestException( "Marshalling of Sint16 failed : encoded '"
				+ Ceylan::toString( encodedSint16 )
				+ "', decoded '" + Ceylan::toString( decodedSint16 )
				+ "'." ) ;
	
				
		Uint16 encodedUint16 = 55001 ;
		myMarshaller->encodeUint16( encodedUint16 ) ;
		
		Uint16 decodedUint16 = myMarshaller->decodeUint16() ;
		
		if ( encodedUint16 != decodedUint16 )
			throw TestException( "Marshalling of Uint16 failed : encoded '"
				+ Ceylan::toString( encodedUint16 )
				+ "', decoded '" + Ceylan::toString( decodedUint16 )
				+ "'." ) ;
		else
			LogPlug::info( "Marshalling of Uint16 succeeded : encoded '"
				+ Ceylan::toString( encodedUint16 )
				+ "', decoded '" + Ceylan::toString( decodedUint16 )
				+ "'." ) ;
					

		Sint32 encodedSint32 = 1234567 ;
		myMarshaller->encodeSint32( encodedSint32 ) ;
		
		Sint32 decodedSint32 = myMarshaller->decodeSint32() ;
		
		if ( encodedSint32 != decodedSint32 )
			throw TestException( "Marshalling of Sint32 failed : encoded '"
				+ Ceylan::toString( encodedSint32 )
				+ "', decoded '" + Ceylan::toString( decodedSint32 )
				+ "'." ) ;
	
				
		Uint32 encodedUint32 = 3034201 ;
		myMarshaller->encodeUint32( encodedUint32 ) ;
		
		Uint32 decodedUint32 = myMarshaller->decodeUint32() ;
		
		if ( encodedUint32 != decodedUint32 )
			throw TestException( "Marshalling of Uint32 failed : encoded '"
				+ Ceylan::toString( encodedUint32 )
				+ "', decoded '" + Ceylan::toString( decodedUint32 )
				+ "'." ) ;



		LogPlug::info( "Testing floating-point marshalling." ) ;
				 
		 		
		Float32 encodedFloat32 = 1234567.1F ;
		myMarshaller->encodeFloat32( encodedFloat32 ) ;
		
		Float32 decodedFloat32 = myMarshaller->decodeFloat32() ;
		
		if ( ! Ceylan::Maths::AreExactlyEqual(
				encodedFloat32, decodedFloat32 ) )
			throw TestException( "Marshalling of Float32 failed : encoded '"
				+ Ceylan::toString( encodedFloat32 )
				+ "', decoded '" + Ceylan::toString( decodedFloat32 )
				+ "'." ) ;
	
	
		Float64 encodedFloat64 = 1234567.123 ;
		myMarshaller->encodeFloat64( encodedFloat64 ) ;
		
		Float64 decodedFloat64 = myMarshaller->decodeFloat64() ;
		
		if ( ! Ceylan::Maths::AreExactlyEqual(
				encodedFloat64, decodedFloat64 ) )
			throw TestException( "Marshalling of Float64 failed : encoded '"
				+ Ceylan::toString( encodedFloat64 )
				+ "', decoded '" + Ceylan::toString( decodedFloat64 )
				+ "'." ) ;
		else
			LogPlug::info( "Marshalling of Float64 succeeded : encoded '"
				+ Ceylan::toString( encodedFloat64 )
				+ "', decoded '" + Ceylan::toString( decodedFloat64 )
				+ "'." ) ;
		

	
		LogPlug::info( "Testing string marshalling." ) ;
	
		string encodedString = 
			"Here comes the little red riding hood again." ;
		myMarshaller->encodeString( encodedString ) ;
		
		string decodedString ;
		myMarshaller->decodeString( decodedString ) ;
				
		if ( encodedString != decodedString )
			throw TestException( "Marshalling of string failed : encoded '"
				+ encodedString + "', decoded '" + decodedString + "'." ) ;
		else
			LogPlug::info( "Marshalling of string succeeded : encoded '"
				+ encodedString	+ "', decoded '" + decodedString + "'." ) ;
				
				
				
		delete myMarshaller ;
		
		// Marshaller does not own it :
		delete bufferStream ;
		
        LogPlug::info( "End of light-weight marshaller test." ) ;


	}
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught : " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
