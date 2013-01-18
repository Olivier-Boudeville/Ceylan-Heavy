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
using namespace Ceylan::Maths ;
using namespace Ceylan::Log ;


#include <iostream>      // for cerr, endl
#include <exception>

#include <string>
using std::string ;



class myAddFunctor : public IntToIntFunctor
{

public:


  myAddFunctor( int toAdd ) throw()
	: IntToIntFunctor( toAdd)
  {

  }


  virtual ~myAddFunctor() throw()
  {

  }


  int operator() ( int callParameter ) throw()
  {
	return _creationParameter + callParameter ;
  }


  virtual const std::string toString( Ceylan::VerbosityLevels level
	= Ceylan::high ) const throw()
  {
	return "myAddFunctor" ;
  }


} ;




/**
 * Test of maths basic implementation.
 *
 * @see Maths
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{



	  LogPlug::info( "Testing maths basic implementation." ) ;

	  LogPlug::info( "The constant E has for value "
		+ Ceylan::toString( E ) + "." ) ;

	  LogPlug::info( "The constant Log2E has for value "
		+ Ceylan::toString( Log2E ) + "." ) ;

	  LogPlug::info( "The constant Log10E has for value "
		+ Ceylan::toString( Log10E ) + "." ) ;

	  LogPlug::info( "The constant LogE2 has for value "
		+ Ceylan::toString( LogE2 ) + "." ) ;

	  LogPlug::info( "The constant LogE10 has for value "
		+ Ceylan::toString( LogE10 ) + "." ) ;

	  LogPlug::info( "The constant Pi has for value "
		+ Ceylan::toString( Pi ) + "." ) ;

	  LogPlug::info( "The constant Pi_div_2 has for value "
		+ Ceylan::toString( Pi_div_2 ) + "." ) ;

	  LogPlug::info( "The constant Pi_div_4 has for value "
		+ Ceylan::toString( Pi_div_4 ) + "." ) ;

	  LogPlug::info( "The constant One_div_Pi has for value "
		+ Ceylan::toString( One_div_Pi ) + "." ) ;

	  LogPlug::info( "The constant Two_div_Pi has for value "
		+ Ceylan::toString( Two_div_Pi ) + "." ) ;

	  LogPlug::info( "The constant Two_div_sqrt_Pi has for value "
		+ Ceylan::toString( Two_div_sqrt_Pi ) + "." ) ;

	  LogPlug::info( "The constant Sqrt_2 has for value "
		+ Ceylan::toString( Sqrt_2 ) + "." ) ;

	  LogPlug::info( "The constant One_div_sqrt_2 has for value "
		+ Ceylan::toString( One_div_sqrt_2 ) + "." ) ;

	  LogPlug::info( "Now checking for errors." ) ;

	  if ( ! AreEqual( E, 2.7182818284590452353602874713526625L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant E." ) ;

	  if ( ! AreEqual( Log2E, 1.4426950408889634073599246810018922L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant Log2E." ) ;

	  if ( ! AreEqual( Log10E , 0.4342944819032518276511289189166051L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant Log10E." ) ;

	  if ( ! AreEqual( LogE2, 0.6931471805599453094172321214581766L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant LogE2." ) ;

	  if ( ! AreEqual( LogE10, 2.3025850929940456840179914546843642L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant LogE10." ) ;

	  if ( ! AreEqual( Pi, 3.1415926535897932384626433832795029L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant Pi." ) ;

	  if ( ! AreEqual( Pi_div_2, 1.5707963267948966192313216916397514L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant Pi_div_2." ) ;

	  if ( ! AreEqual( Pi_div_4, 0.7853981633974483096156608458198757L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant Pi_div_4." ) ;

	  if ( ! AreEqual( One_div_Pi, 0.3183098861837906715377675267450287L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant One_div_Pi." ) ;

	  if ( ! AreEqual( Two_div_Pi, 0.6366197723675813430755350534900574L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant Two_div_Pi." ) ;

	  if ( ! AreEqual( Two_div_sqrt_Pi,
		  1.1283791670955125738961589031215452L  ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant Two_div_sqrt_Pi." ) ;

	  if ( ! AreEqual( Sqrt_2, 1.4142135623730950488016887242096981L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant Sqrt_2." ) ;

	  if ( ! AreEqual( One_div_sqrt_2,
		  0.7071067811865475244008443621048490L ) )
		throw Ceylan::TestException(
		  "Bad value for numerical constant One_div_sqrt_2." ) ;


	  LogPlug::info( "Testing templated Max operator: "
		" Max&lt;Ceylan::Uint16&gt;(3,4) = "
		+ Ceylan::toString( Max<Ceylan::Uint16>(3,4) ) ) ;


	  LogPlug::info(
		"Testing rounding facilities (Floor, Ceil and Round):" ) ;


	  // Testing rounding for values in [-2; 2], with 0.1 steps:
	  for ( Ceylan::Sint8 i = -20; i < 21; i++ )
	  {

		Ceylan::Float32 x = static_cast<Ceylan::Float32>( i ) / 10 ;

		LogPlug::info( "For x = <b>" + Ceylan::toString( x ) + "</b>:" ) ;

		LogPlug::info( "Floor(x) = "
		  + Ceylan::toString( Floor( x ) ) + "." ) ;

		LogPlug::info( "Ceil(x) = "
		  + Ceylan::toString( Ceil( x ) ) + "." ) ;

		LogPlug::info( "Round(x) = "
		  + Ceylan::toString( Round( x ) ) + "." ) ;

	  }

	  LogPlug::info( "Testing Log(20) = "
		+ Ceylan::toString( Log( 20.0f ) ) ) ;

	  LogPlug::info( "Testing rounding function with various precisions." ) ;

	  Ceylan::Float32 toRound = 1.676F ;
	  Ceylan::Uint8 precision = 2 ;

	  LogPlug::info( "Rounding " + Ceylan::toString( toRound ) + " to "
		+ Ceylan::toNumericalString( precision )
		+ " figures after the dot: "
		+ Ceylan::toString( Round( toRound, precision ) ) ) ;

	  toRound = -3.234567F ;
	  precision = 3 ;

	  LogPlug::info( "Rounding " + Ceylan::toString( toRound ) + " to "
		+ Ceylan::toNumericalString( precision )
		+ " figures after the dot: "
		+ Ceylan::toString( Round( toRound, precision ) ) ) ;


	  for ( Ceylan::Uint16 count = 511; count < 514; count++ )
		LogPlug::info( "Testing PowerOfTwo: the first power of 2 "
		  "greater or equal to " + Ceylan::toString( count ) + " is "
		  + Ceylan::toString( Ceylan::Maths::NextPowerOfTwo( count ) )
		  + "." ) ;


	  Ceylan::Uint16 multiple = 8 ;

	  for ( Ceylan::Uint16 count = 1; count < 34; count++ )
		LogPlug::info( "Testing NextMultipleOf: the first multiple of "
		  + Ceylan::toString( multiple ) + " greater or equal to "
		  + Ceylan::toString( count ) + " is " + Ceylan::toString(
			Ceylan::Maths::NextMultipleOf( multiple, count ) )
		  + "." ) ;

	  LogPlug::info( "Testing functors: "
		"creating an integer functor adding 5." ) ;

	  myAddFunctor aFunctor( 5 ) ;


	  LogPlug::info( "Calling aFunctor(20), result is: "
		+ Ceylan::toString( aFunctor( 20 ) ) ) ;

	  if ( aFunctor( 20 ) != 25 )
		throw Ceylan::TestException( "Functor returned "
		  + Ceylan::toString( aFunctor( 20 ) )
		  + " instead of 25 !" ) ;



	  // Min section.

	  Ceylan::Uint32 minRes ;

	  minRes = Min( 1, 2, 3 ) ;
	  LogPlug::info( "Testing Min operator: Min( 1, 2, 3 ) = "
		+ Ceylan::toString( minRes ) ) ;
	  if ( minRes != 1 )
		throw Ceylan::TestException( "Three-argument Min failed" ) ;

	  minRes = Min( 2, 2, 3 ) ;
	  LogPlug::info( "Testing Min operator: Min( 2, 2, 3 ) = "
		+ Ceylan::toString( minRes ) ) ;
	  if ( minRes != 2 )
		throw Ceylan::TestException( "Three-argument Min failed" ) ;

	  minRes = Min( 3, 2, 3 ) ;
	  LogPlug::info( "Testing Min operator: Min( 3, 2, 3 ) = "
		+ Ceylan::toString( minRes ) ) ;
	  if ( minRes != 2 )
		throw Ceylan::TestException( "Three-argument Min failed" ) ;

	  minRes = Min( 3, 1, 2 ) ;
	  LogPlug::info( "Testing Min operator: Min( 3, 1, 2 ) = "
		+ Ceylan::toString( minRes ) ) ;
	  if ( minRes != 1 )
		throw Ceylan::TestException( "Three-argument Min failed" ) ;

	  minRes = Min( 2, 3, 1 ) ;
	  LogPlug::info( "Testing Min operator: Min( 2, 3, 1 ) = "
		+ Ceylan::toString( minRes ) ) ;
	  if ( minRes != 1 )
		throw Ceylan::TestException( "Three-argument Min failed" ) ;

	  minRes = Min( 3, 3, 3 ) ;
	  LogPlug::info( "Testing Min operator: Min( 3, 3, 3 ) = "
		+ Ceylan::toString( minRes ) ) ;
	  if ( minRes != 3 )
		throw Ceylan::TestException( "Three-argument Min failed" ) ;


	  LogPlug::info( "End of maths basic test." ) ;


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
