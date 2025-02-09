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


#include "Ceylan.h"
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Linear ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/**
 * Test of linear 3D operations.
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

	  LogPlug::info( "Testing maths linear 3D implementation." ) ;


	  LogPlug::info( "Vector3 section." ) ;

	  Vector3 v1, v2, v3, v4, v5 ;

	  LogPlug::info( "Vector3 creation: " + v1.toString() ) ;
	  if ( v1 != Vector3( 0, 0, 0 ) )
		throw Ceylan::TestException( "Newly created Vector3 not null." ) ;

	  v1.setTo( 1, 2,3 ) ;
	  LogPlug::info( "After modification: " + v1.toString() ) ;
	  if ( v1 != Vector3( 1, 2, 3 ) )
		throw Ceylan::TestException( "Vector3::setTo failed." ) ;


	  v1.nullify() ;
	  LogPlug::info( "Nullifying: " + v1.toString() ) ;
	  if ( v1 != Vector3( 0, 0, 0 ) )
		throw Ceylan::TestException( "Nullified Vector3 not null." ) ;

	  v1.setTo( 7, 5, 9 ) ;
	  v2.setTo( 5, 8, 1 ) ;

	  v3 = v1 + v2 ;
	  LogPlug::info( "Add: v1 " + v1.toString() + " + v2 " + v2.toString()
		+  " = " + v3.toString() ) ;

	  if ( v3 != Vector3( 12, 13, 10 ) )
		throw Ceylan::TestException( "Addition of Vector3 failed." ) ;

	  v3 = v1 - v2 ;
	  LogPlug::info( "Subtract: v1 - v2 = " + v3.toString() ) ;

	  if ( v3 != Vector3( 2, -3, 8 ) )
		throw Ceylan::TestException( "Subtraction of Vector3 failed." ) ;

	  v3 = 3 * v1 ;
	  LogPlug::info( "Scalar multiplication: 3 * v1 = " + v3.toString() ) ;

	  if ( v3 != Vector3( 21, 15, 27 ) )
		throw Ceylan::TestException(
		  "Scalar multiplication of Vector3 failed." ) ;

	  LogPlug::info( "Magnitude of v1: ~v1 = "
		+ Ceylan::toString( v1.magnitude() ) ) ;

	  if ( v1.magnitude() < 12.44 || v1.magnitude() > 12.45 )
		throw Ceylan::TestException(
		  "Magnitude of Vector3 failed, result was: "
		  + v1.toString( Ceylan::low ) ) ;

	  LogPlug::info( "Dot product: ( v1 | v2 ) = "
		+ Ceylan::toString( v1 | v2 ) ) ;

	  if ( ! AreRelativelyEqual( ( v1 | v2 ), static_cast<Real>( 84 ) ) )
		throw Ceylan::TestException( "Dot product of Vector3 failed." ) ;

	  v3 = ( v1 ^ v2 ) ;
	  LogPlug::info( "Cross product: v3 = v1 ^ v2 = " + v3.toString() ) ;

	  if ( v3 != Vector3( -67, 38, 31 ) )
		throw Ceylan::TestException( "Cross product of Vector3 failed." ) ;

	  v3 = ( v1 ^ v2 ) + Vector3( 1, 11, 3 ) ;
	  LogPlug::info( "New v3 = " + v3.toString() ) ;

	  // Beware of operator priorities!

	  v4 = ( v1 ^ (2*v2) ) - (v1|v3)*v2 - (~v3) * v3 ;
	  LogPlug::info( "Complex expression: "
		"( v1 ^ (2*v2) ) - (v1|v3)*v2 - (~v3) * v3 = "
		+ v4.toString() ) ;

	  if ( v4.magnitude() < 7890.8 || v4.magnitude() > 7890.9 )
		throw Ceylan::TestException(
		  "Complex expression evaluation failed." ) ;


	  LogPlug::info( "End of Vector3 section." ) ;



	  LogPlug::info( "Tripoint section." ) ;

	  Tripoint t1, t2 ;

	  LogPlug::info( "Tripoint creation: " + t1.toString() ) ;
	  if ( t1 != Tripoint( 0, 0, 0 ) )
		throw Ceylan::TestException( "Newly created Tripoint not null." ) ;

	  t1.setTo( 1, 2, 3 ) ;
	  LogPlug::info( "After modification: " + t1.toString() ) ;
	  if ( t1 != Tripoint( 1, 2, 3 ) )
		throw Ceylan::TestException( "Tripoint::setTo failed." ) ;

	  t1.nullify() ;
	  LogPlug::info( "Nullifying: " + t1.toString() ) ;
	  if ( t1 != Tripoint( 0, 0, 0 ) )
		throw Ceylan::TestException( "Nullified Tripoint not null." ) ;


	  t1.setTo( 7, 5, 9 ) ;
	  v2.setTo( 5, 8, 1 ) ;

	  t2 = t1 + v2 ;
	  LogPlug::info( "Add: t1 " + t1.toString() + " + v2 " + v2.toString()
		+  " = " + t2.toString() ) ;

	  if ( t2 != Tripoint( 12, 13, 10 ) )
		throw Ceylan::TestException(
		  "Addition of Tripoint and Vector3 failed." ) ;

	  t2 = t1 - v2 ;
	  LogPlug::info( "Subtract: t1 - v2 " + v2.toString()
		+  " = " + t2.toString() ) ;

	  if ( t2 != Tripoint( 2, -3, 8 ) )
		throw Ceylan::TestException(
		  "Subtraction Tripoint minus Vector3 failed." ) ;

	  v1 = vectorize( t1 ) ;
	  if ( v1 != Vector3( 7, 5, 9 ) )
		throw Ceylan::TestException(
		  "vectorize Tripoint to Vector3 failed." ) ;

	  t2.setTo( 5, 4, 1 ) ;
	  v1 = vectorize( t1, t2 ) ;
	  if ( v1 != Vector3( -2, -1, -8 ) )
		throw Ceylan::TestException(
		  "vectorize (Tripoint, Tripoint) to Vector3 failed: "
		  "result was " + v1.toString( Ceylan::low ) ) ;

	  LogPlug::info( "Tripoint vectorizations succeeded." ) ;

	  LogPlug::info( "End of Tripoint section." ) ;




	  LogPlug::info( "Matrix3 section." ) ;

	  Matrix3 m1, m2, m3 ;
	  LogPlug::info( "Creating a new matrix: " + m1.toString() ) ;

	  m1.setTo( 1, 3, 3, 4, 8, 1, 6, 8, 9 ) ;
	  LogPlug::info( "After modification: " + m1.toString() ) ;

	  MatrixIndex index = 1 ;
	  v1.setTo( 5, 0, 0 ) ;
	  m1.setColumn( 1, v1 ) ;
	  LogPlug::info( "Setting column "
		+ Ceylan::toString( static_cast<int>( index ) )
		+ " to " + v1.toString()
		+ ": " + m1.toString() ) ;

	  LogPlug::info( "Trace of this matrix is "
		+ Ceylan::toString( m1.trace() ) + "." ) ;

	  if ( ! AreRelativelyEqual( m1.trace(), static_cast<Real>( 10 ) ) )
		throw Ceylan::TestException( "Incorrect Matrix3 trace returned." ) ;

	  LogPlug::info( "Determinant of this matrix is "
		+ Ceylan::toString( m1.determinant() ) + "." ) ;

	  if ( ! AreRelativelyEqual( m1.determinant(),
		  static_cast<Real>( -150 ) ) )
		throw Ceylan::TestException(
		  "Incorrect Matrix3 determinant returned." ) ;

	  LogPlug::info( "Transposed matrix is " + (~m1).toString() ) ;

	  m3.nullify() ;
	  LogPlug::info( "Null matrix: " + m3.toString() ) ;

	  m3.setToIdentity() ;
	  LogPlug::info( "Identity matrix: " + m3.toString() ) ;


	  m2.setTo( 4, 7, 3, 5, 8, 1, 9, 9, 2 ) ;

	  LogPlug::info( "m1 = " + m1.toString() + ", m2 = " + m2.toString() ) ;

	  m3 = m1 + m2 ;
	  LogPlug::info( "m1 + m2 = " + m3.toString() ) ;
	  if ( m3 != Matrix3( 5, 12, 6, 9, 8, 2, 15, 9, 11 ) )
		throw Ceylan::TestException( "Incorrect Matrix3 addition." ) ;

	  m3 = m1 * m2 ;
	  LogPlug::info( "m1 * m2 = " + m3.toString() ) ;
	  if ( m3 != Matrix3( 56, 74, 14, 25, 37, 14, 105, 123, 36 ) )
		throw Ceylan::TestException( "Incorrect Matrix3 multiplication." ) ;

	  m3 = 2 * m1 ;
	  LogPlug::info( "m3 = 2 * m1 = " + m3.toString() ) ;
	  if ( m3 != Matrix3( 2, 10, 6, 8, 0, 2, 12, 0, 18 ) )
		throw Ceylan::TestException(
		  "Incorrect Matrix3 scalar multiplication." ) ;

	  m3.transpose() ;
	  LogPlug::info( "Transpose m3: ~m3 = " + m3.toString() ) ;
	  if ( m3 != Matrix3( 2, 8, 12, 10, 0, 0, 6, 2, 18 ) )
		throw Ceylan::TestException( "Incorrect Matrix3 transposition: "
		  "result was " + m3.toString( Ceylan::low ) ) ;


	  m3.setTo( 4, 3, 7, 5, 4, 6, 4, 1, 2 ) ;


	  LogPlug::info( "m1 = " + m1.toString() + ", m2 = " + m2.toString()
		+ ", m3 = " + m3.toString() ) ;

	  m3 = ( 2*m1 - 7*m2 ) * ( m3 + ~m2 ) ;
	  LogPlug::info( "Complex expression "
		"m3 = ( 2*m1 - 7*m2 ) * ( m3 + ~m2 ) = "
		+ m3.toString() ) ;

	  if ( m3 != Matrix3( -781, -706, -1061, -923, -898, -1292,
		  -1136, -1156, -1745 ) )
		throw Ceylan::TestException(
		  "Incorrect Matrix3 complex evaluation returned, "
		  "result was " + m3.toString( Ceylan::low ) ) ;

	  LogPlug::info( "Cofactor matrix of m1 is "
		+ Matrix3::Cofactor( m1 ).toString() ) ;

	  m2 = ! m1 ;
	  LogPlug::info( "Reciproqual of m1 is " + m2.toString() ) ;

	  LogPlug::info( "Testing: m1 * m2 = m3 = approximated Identity ?" ) ;
	  m3 = m1 * m2 ;
	  LogPlug::info( "Result is " + m3.toString() ) ;
	  v1.setTo( 100, 100, 100 ) ;


	  /*
	   * Testing only a necessary condition, not using any spcial relaxed
	   * epsilon to overcome computing errors:
	   *
	   */

	  if ( ! AreRelativelyEqual( ~v1, ~( m3*v1) ) )
		throw Ceylan::TestException(
		  "Incorrect Matrix3 inverse returned: ~v1 = "
		  + Ceylan::toString( ~v1 ) + " whereas ~( m3*v1) = "
		  + Ceylan::toString( ~( m3*v1) ) + "." ) ;

	  LogPlug::info( "Inverse matrix is satisfying: ~v1 = "
		+ Ceylan::toString( ~v1 ) + " whereas ~( m3*v1) = "
		+ Ceylan::toString( ~( m3*v1) ) + "." ) ;

	  v1.setTo( 5, 8, 1 ) ;
	  LogPlug::info( "Setting v1 = " + v1.toString() ) ;
	  v2 = m1 * v1 ;
	  LogPlug::info( "Matrix * vector: v2 = m1 * v1 = "
		+ v2.toString() ) ;
	  if ( v2 != Vector3( 48, 21, 39 ) )
		throw Ceylan::TestException(
		  "Incorrect Matrix3-Vector3 multiplication returned." ) ;

	  Vector3 myAxis( 0, 1, 0 ) ;
	  AngleInDegrees myAngle = 0 ;
	  Rotation3DFunctor firstRotation( myAxis, myAngle ) ;
	  LogPlug::info(
		"Testing rotation: creating a new rotation endomorphism "
		"whose description is " + firstRotation.toString() ) ;
	  Matrix3 mrot = Matrix3::CreateFrom( firstRotation ) ;
	  LogPlug::info( "Corresponding matrix is: " + mrot.toString() ) ;

	  myAngle = 180 ;
	  Rotation3DFunctor secondRotation( myAxis, myAngle ) ;
	  LogPlug::info( "Testing rotation: "
		"creating a new rotation endomorphism "
		"whose description is " + secondRotation.toString() ) ;
	  mrot = Matrix3::CreateFrom( secondRotation ) ;
	  LogPlug::info( "Corresponding matrix is: " + mrot.toString() ) ;

	  LogPlug::info( "End of Matrix3 section." ) ;

	  // Here put 4x4 homogeneous matrices.

	  LogPlug::info( "End of maths linear 3D test." ) ;

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
