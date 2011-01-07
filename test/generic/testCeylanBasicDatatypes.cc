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
using namespace Ceylan::Log ;


#include <exception>
#include <string>

using namespace std ;



/**
 * Test to check the sizes and ranges of most common Ceylan basic datatypes.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder myLog( argc, argv ) ;


    try
    {


		LogPlug::info( "Test to check the sizes and ranges of "
			"most common Ceylan basic datatypes." ) ;



		// Integer section.
		
		
		/*
		 * 8-bit datatypes have to be casted to wider types to avoid
		 * mismatch with char-based toString methods, and because
		 * toNumericalString is for unsigned data only.
		 *
		 */ 
		LogPlug::info( "Size of Ceylan::Sint8 is " 
			+ Ceylan::toString( sizeof( Ceylan::Sint8 ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Sint16>( Ceylan::Sint8Min ) )
			+ ", its maximum value is " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Sint16>( Ceylan::Sint8Max ) ) 
			+ "." ) ;
		
		if ( sizeof( Ceylan::Sint8 ) != 1 )
			throw Ceylan::TestException( 
				"Unexpected size for Ceylan::Sint8 : found "
				+ Ceylan::toString( sizeof( Ceylan::Sint8 ) ) 
				+ " byte(s), instead of 1." ) ;
				 	
		
			
		LogPlug::info( "Size of Ceylan::Uint8 is " 
			+ Ceylan::toString( sizeof( Ceylan::Uint8 ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toNumericalString( Ceylan::Uint8Min )
			+ ", its maximum value is " 
			+ Ceylan::toNumericalString( Ceylan::Uint8Max ) 
			+ "." ) ;
			
		if ( sizeof( Ceylan::Uint8 ) != 1 )
			throw Ceylan::TestException( 
				"Unexpected size for Ceylan::Uint8 : found "
				+ Ceylan::toString( sizeof( Ceylan::Uint8 ) ) 
				+ " byte(s), instead of 1." ) ;
		
			
		// 16-bit section.

		LogPlug::info( "Size of Ceylan::Sint16 is " 
			+ Ceylan::toString( sizeof( Ceylan::Sint16 ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( Ceylan::Sint16Min )
			+ ", its maximum value is " 
			+ Ceylan::toString( Ceylan::Sint16Max ) 
			+ "." ) ;
			
		if ( sizeof( Ceylan::Sint16 ) != 2 )
			throw Ceylan::TestException( 
				"Unexpected size for Ceylan::Sint16 : found "
				+ Ceylan::toString( sizeof( Ceylan::Sint16 ) ) 
				+ " byte(s), instead of 2." ) ;
				 	
			
		LogPlug::info( "Size of Ceylan::Uint16 is " 
			+ Ceylan::toString( sizeof( Ceylan::Uint16 ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( Ceylan::Uint16Min )
			+ ", its maximum value is " 
			+ Ceylan::toString( Ceylan::Uint16Max ) 
			+ "." ) ;
			
		if ( sizeof( Ceylan::Uint16 ) != 2 )
			throw Ceylan::TestException( 
				"Unexpected size for Ceylan::Uint16 : found "
				+ Ceylan::toString( sizeof( Ceylan::Uint16 ) ) 
				+ " byte(s), instead of 2." ) ;
				 	
			
			
		// 32-bit section.
			
		LogPlug::info( "Size of Ceylan::Sint32 is " 
			+ Ceylan::toString( sizeof( Ceylan::Sint32 ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( Ceylan::Sint32Min )
			+ ", its maximum value is " 
			+ Ceylan::toString( Ceylan::Sint32Max ) 
			+ "." ) ;
			
		if ( sizeof( Ceylan::Sint32 ) != 4 )
			throw Ceylan::TestException( 
				"Unexpected size for Ceylan::Sint32 : found "
				+ Ceylan::toString( sizeof( Ceylan::Sint32 ) ) 
				+ " byte(s), instead of 4." ) ;
				 	
			
			
		LogPlug::info( "Size of Ceylan::Uint32 is " 
			+ Ceylan::toString( sizeof( Ceylan::Uint32 ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( Ceylan::Uint32Min )
			+ ", its maximum value is " 
			+ Ceylan::toString( Ceylan::Uint32Max ) 
			+ "." ) ;

		if ( sizeof( Ceylan::Uint32 ) != 4 )
			throw Ceylan::TestException( 
				"Unexpected size for Ceylan::Uint32 : found "
				+ Ceylan::toString( sizeof( Ceylan::Uint32 ) ) 
				+ " byte(s), instead of 4." ) ;
				 	
			
		
		
		// Floating-point section.
		
		
		LogPlug::info( "Size of Ceylan::Float32 is " 
			+ Ceylan::toString( sizeof( Ceylan::Float32 ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( Ceylan::Float32Min )
			+ ", its maximum value is " 
			+ Ceylan::toString( Ceylan::Float32Max ) 
			+ "." ) ;
			
		if ( sizeof( Ceylan::Float32 ) != 4 )
			throw Ceylan::TestException( 
				"Unexpected size for Ceylan::Float32 : found "
				+ Ceylan::toString( sizeof( Ceylan::Float32 ) ) 
				+ " byte(s), instead of 4." ) ;
			
			
			
		LogPlug::info( "Size of Ceylan::Float64 is " 
			+ Ceylan::toString( sizeof( Ceylan::Float64 ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( Ceylan::Float64Min )
			+ ", its maximum value is " 
			+ Ceylan::toString( Ceylan::Float64Max ) 
			+ "." ) ;
			
		if ( sizeof( Ceylan::Float64 ) != 8 )
			throw Ceylan::TestException( 
				"Unexpected size for Ceylan::Float64 : found "
				+ Ceylan::toString( sizeof( Ceylan::Float64 ) ) 
				+ " byte(s), instead of 8." ) ;
			
			
		
		// Non fixed-size datatypes.
		
		LogPlug::info( "Size of Ceylan::Byte is " 
			+ Ceylan::toString( sizeof( Ceylan::Byte ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Sint16>( Ceylan::ByteMin ) )
			+ ", its maximum value is " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Sint16>( Ceylan::ByteMax ) ) 
			+ "." ) ;
		
		
		
		LogPlug::info( "Size of Ceylan::SignedLongInteger is " 
			+ Ceylan::toString( sizeof( Ceylan::SignedLongInteger ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( Ceylan::SignedLongIntegerMin )
			+ ", its maximum value is " 
			+ Ceylan::toString( Ceylan::SignedLongIntegerMax ) 
			+ "." ) ;

		LogPlug::info( "Size of Ceylan::UnsignedLongInteger is " 
			+ Ceylan::toString( sizeof( Ceylan::UnsignedLongInteger ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( Ceylan::UnsignedLongIntegerMin )
			+ ", its maximum value is " 
			+ Ceylan::toString( Ceylan::UnsignedLongIntegerMax ) 
			+ "." ) ;


		LogPlug::info( "Size of Ceylan::LongFloat is " 
			+ Ceylan::toString( sizeof( Ceylan::LongFloat ) )
			+ " byte(s), its minimum value is " 
			+ Ceylan::toString( Ceylan::LongFloatMin ) + " (about " 
			+ Ceylan::toString( static_cast<Ceylan::Float64>(
				Ceylan::LongFloatMin ), /* precision */ 2 )
			+ "), its maximum value is " 
			+ Ceylan::toString( Ceylan::LongFloatMax ) + " (about " 
			+ Ceylan::toString( static_cast<Ceylan::Float64>(
				Ceylan::LongFloatMax ), /* precision*/ 2 )
			+ "). Warning : on some browsers, these two limits "
			"may incorrectly displayed, see the frame source if needed." ) ;


		
		LogPlug::info( "End of basic datatypes test." ) ;
		

    }

    catch ( const Ceylan::Exception & e )
    {
        LogPlug::error( "Ceylan exception caught : "
        	 + e.toString( Ceylan::high ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        LogPlug::error( "Standard exception caught : " 
			 + std::string( e.what() ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        LogPlug::error( "Unknown exception caught" ) ;
       	return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}

