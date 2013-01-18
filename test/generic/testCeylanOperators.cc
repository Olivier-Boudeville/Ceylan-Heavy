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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "Ceylan.h"

#include <iostream>

#include <string>
using std::string ;

#include <exception>


using namespace Ceylan ;
using namespace Ceylan::Log ;



/**
 * Test for conversion operators services.
 *
 * @see Operators.
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{


	  LogPlug::info( "Testing extended string operators, first with "
		"string + Ceylan datatype, then the other way round, "
		"Ceylan datatype + string." ) ;

	  string s ;



	  // Integer datatypes.


	  Ceylan::Sint8 sint8 = -11 ;
	  s = string( "Ceylan::Sint8: -11 ---> " ) + sint8 ;
	  LogPlug::info( s ) ;

	  s = sint8 + string( " ---> Ceylan::Sint8: -11" ) ;
	  LogPlug::info( s ) ;


	  Ceylan::Uint8 uint8 = 2 ;
	  s = string( "Ceylan::Uint8: 2 ---> " ) + uint8 ;
	  LogPlug::info( s ) ;

	  s = uint8 + string( " ---> Ceylan::Uint8: 2" ) ;
	  LogPlug::info( s ) ;



	  Ceylan::Sint16 sint16 = -4 ;
	  s = string( "Ceylan::Sint16: -4  ---> " ) + sint16 ;
	  LogPlug::info( s ) ;

	  s = sint16 + string( " ---> Ceylan::Sint16: -4" ) ;
	  LogPlug::info( s ) ;


	  Ceylan::Uint16 uint16 = 112 ;
	  s = string( "Ceylan::Uint16: 112  ---> " ) + uint16 ;
	  LogPlug::info( s ) ;

	  s = uint16 + string( " ---> Ceylan::Uint16: 112" ) ;
	  LogPlug::info( s ) ;


	  Ceylan::Sint32 sint32 = -34 ;
	  s = string( "Ceylan::Sint32: -34 ---> " ) + sint32 ;
	  LogPlug::info( s ) ;

	  s = sint32 + string( " ---> Ceylan::Sint32: -34" ) ;
	  LogPlug::info( s ) ;


	  Ceylan::Uint32 uint32 = 58 ;
	  s = string( "Ceylan::Uint32: 58 ---> " ) + uint32 ;
	  LogPlug::info( s ) ;

	  s = uint32 + string( " ---> Ceylan::Uint32: 58" ) ;
	  LogPlug::info( s ) ;



	  Ceylan::SignedLongInteger slongint = -308 ;
	  s = string( "Ceylan::SignedLongInteger: -308 ---> " ) + slongint ;
	  LogPlug::info( s ) ;

	  s = slongint + string( " ---> Ceylan::SignedLongInteger: -308" ) ;
	  LogPlug::info( s ) ;


	  Ceylan::UnsignedLongInteger ulongint = 300000 ;
	  s = string( "Ceylan::UnsignedLongInteger: 300000 ---> " ) + ulongint ;
	  LogPlug::info( s ) ;

	  s = ulongint + string( " ---> Ceylan::UnsignedLongInteger: 300000" ) ;
	  LogPlug::info( s ) ;


	  // Sint64 / Uint64 not tested.



	  // Floating-point datatypes.


	  Ceylan::Float32 float32 = -12.342f ;
	  s = string( "Ceylan::Float32: -12.342 ---> " ) + float32 ;
	  LogPlug::info( s ) ;

	  s = float32 + string( " ---> Ceylan::Float32: -12.342" ) ;
	  LogPlug::info( s ) ;


	  Ceylan::Float64 float64 = -1026.23 ;
	  s = string( "Ceylan::Float64: -1026.23 ---> " ) + float64 ;
	  LogPlug::info( s ) ;

	  s = float64 + string( " ---> Ceylan::Float64: -1026.23" ) ;
	  LogPlug::info( s ) ;


	  Ceylan::LongFloat longfloat = -22423242236.15 ;
	  s = string( "Ceylan::Float64: -22423242236.15 ---> " ) + longfloat ;
	  LogPlug::info( s ) ;

	  s = longfloat + string( " ---> Ceylan::Float64: -22423242236.15" ) ;
	  LogPlug::info( s ) ;



	  // Other datatypes.

	  const char * charpointer = "Ceylan is good" ;
	  s = string( "char *: Ceylan is good ---> " ) + charpointer ;
	  LogPlug::info( s ) ;

	  s = charpointer + string( " ---> char *: Ceylan is good" ) ;
	  LogPlug::info( s ) ;


	  const void * pointer = static_cast<const void *>( charpointer ) ;
	  s = string( "const void *: ---> " ) + pointer ;
	  LogPlug::info( s ) ;

	  s = pointer + string( " ---> const void *" ) ;
	  LogPlug::info( s ) ;


	  const char * c = " made of a mix of " ;
	  s = "This string is" + string( c ) + "all strings" + "." ;
	  LogPlug::info( s ) ;



	  Ceylan::Uint8 precision = 3 ;

	  LogPlug::info( "Testing the display of 1/3 "
		"with a precision of only "
		+ Ceylan::toNumericalString( precision ) + " digits: "
		+ Ceylan::toString( 1.0f / 3, precision ) + "." ) ;

	  LogPlug::info( "Testing the display of 4/3 "
		"with a precision of only "
		+ Ceylan::toNumericalString( precision ) + " digits: "
		+ Ceylan::toString( 4.0f / 3, precision ) + "." ) ;

	  LogPlug::info( "Testing the display of float 75312 "
		"with a precision of only "
		+ Ceylan::toNumericalString( precision ) + " digits: "
		+ Ceylan::toString( 75312.0f, precision ) + "." ) ;

	  Ceylan::Uint8 aChar = 0 ;
	  LogPlug::info( "Testing Ceylan::toNumericalString "
		"for eight bit numbers: the number 0 is translated in '"
		+ Ceylan::toNumericalString( aChar )
		+ "' instead of '" + Ceylan::toString( aChar ) + "'." ) ;


	  Sint32 * p = new Sint32[ 3 ] ;

	  LogPlug::info( "Finally, testing output for a pointer: "
		+ Ceylan::toString( p ) ) ;

	  LogPlug::info( "and for a boolean, which may be: "
		+ Ceylan::toString( true )
		+ " (true) or "
		+ Ceylan::toString( false )
		+ " (false)." ) ;

	  LogPlug::info( "Yep, this kind of programming is "
		"a dull task, man." ) ;

	  LogPlug::info( "Now, testing bit fields display." ) ;


	  for ( Ceylan::Uint8 count = 0; count < 21 ; count++ )
	  {

		// Converted to Ceylan::Uint16 not to be taken for a char:
		LogPlug::info( "The number "
		  + Ceylan::toString( static_cast<Ceylan::Uint16>( count ) )
		  + " displays, as a bitfield, as "
		  + Ceylan::toString( count, /* bitfield */ true ) ) ;

	  }


	  for ( Ceylan::Uint8 count = 0; count < 21 ; count++ )
	  {

		// Converted to Ceylan::Uint16 not to be taken for a char:
		LogPlug::info( "The number "
		  + Ceylan::toString( static_cast<Ceylan::Uint16>( count ) )
		  + " displays, in hexadecimal, as "
		  + Ceylan::toHexString( count ) ) ;

	  }

	  // Actually this number would be -16777216 with signed long:
	  Ceylan::UnsignedLongInteger bigNumber = 0xFF000000 ;

	  LogPlug::info( "The number "
		+ Ceylan::toString(
		  static_cast<Ceylan::SignedLongInteger>( bigNumber ) )
		+ " displays, in hexadecimal, as "
		+ Ceylan::toHexString( bigNumber ) ) ;

	  LogPlug::info( "End of extended string operators test." ) ;

	  // Thanks Valgrind for the lacking '[]'!
	  delete [] p ;

	}

	catch ( const Ceylan::Exception & e )
	{
	  std::cerr << "Ceylan exception caught: "
				<< e.toString( Ceylan::high ) << std::endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
	  std::cerr << "Standard exception caught: "
				<< e.what() << std::endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
	  std::cerr << "Unknown exception caught" << std::endl ;
	  return Ceylan::ExitFailure ;

	}

  }

  Ceylan::shutdown() ;

  return Ceylan::ExitSuccess ;

}
