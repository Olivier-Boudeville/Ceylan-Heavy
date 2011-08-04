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
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Linear ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;



/**
 * Test of linear 2D operations.
 *
 * @see Maths
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


	try
	{

		LogPlug::info( "Testing maths linear 2D implementation." ) ;


		LogPlug::info( "Vector2 section." ) ;

		Vector2 v1, v2, v3, v4, v5 ;

		LogPlug::info( "Vector2 creation: " + v1.toString() ) ;
		if ( v1 != Vector2( 0, 0 ) )
			throw Ceylan::TestException( "Newly created Vector2 not null." ) ;

		v1.setTo( 1, 2 ) ;
		LogPlug::info( "After modification: " + v1.toString() ) ;
		if ( v1 != Vector2( 1, 2 ) )
			throw Ceylan::TestException( "Vector2::setTo failed." ) ;


		v1.nullify() ;
		LogPlug::info( "Nullifying: " + v1.toString() ) ;
		if ( v1 != Vector2( 0, 0 ) )
			throw Ceylan::TestException( "Nullified Vector2 not null." ) ;

		v1.setTo( 7, 5 ) ;
		v2.setTo( 5, 8 ) ;

		v3 = v1 + v2 ;
		LogPlug::info( "Add: v1 " + v1.toString() + " + v2 " + v2.toString()
			+  " = " + v3.toString() ) ;

		if ( v3 != Vector2( 12, 13 ) )
			throw Ceylan::TestException( "Addition of Vector2 failed." ) ;

		v3 = v1 - v2 ;
		LogPlug::info( "Subtract: v1 - v2 = " + v3.toString() ) ;

		if ( v3 != Vector2( 2, -3 ) )
			throw Ceylan::TestException( "Subtraction of Vector2 failed." ) ;

		v3 = 3 * v1 ;
		LogPlug::info( "Scalar multiplication: 3 * v1 = " + v3.toString() ) ;

		if ( v3 != Vector2( 21, 15 ) )
			throw Ceylan::TestException(
				"Scalar multiplication of Vector2 failed." ) ;

		LogPlug::info( "Magnitude of v1: ~v1 = "
			+ Ceylan::toString( v1.magnitude() ) ) ;

		if ( v1.magnitude() < 8.6023 || v1.magnitude() > 8.6024 )
			throw Ceylan::TestException(
				"Magnitude of Vector2 failed, result was: "
				+ v1.toString( Ceylan::low ) ) ;

		LogPlug::info( "Dot product: ( v1 | v2 ) = "
			+ Ceylan::toString( v1 | v2 ) ) ;
		if ( ! AreRelativelyEqual( ( v1 | v2 ), static_cast<Real>( 75 ) ) )
			throw Ceylan::TestException( "Dot product of Vector2 failed." ) ;

		v3.setTo( 11, 1 ) ;
		LogPlug::info( "New v3 is " + v3.toString() ) ;


		// Beware of operator priorities !

		v4 = v1 + 2*v2 - (v1|v3)*v2 - (~v3) * v3 ;
		LogPlug::info(
			"Complex expression: ( v1 ^ (2*v2) ) - (v1|v3)*v2 - (~v3) * v3 = "
			+ v4.toString() ) ;

		// Result should be approximately [ -514,5 ; -646,05 ].
		if ( v4.magnitude() < 825.88 || v4.magnitude() > 825.89 )
			throw Ceylan::TestException(
				"Complex expression evaluation failed, result was: "
				+ v4.toString( Ceylan::low ) ) ;

		LogPlug::info( "End of Vector2 section." ) ;



		LogPlug::info( "Bipoint section." ) ;

		Bipoint t1, t2 ;

		LogPlug::info( "Bipoint creation: " + t1.toString() ) ;
		if ( t1 != Bipoint( 0, 0 ) )
			throw Ceylan::TestException( "Newly created Bipoint not null." ) ;

		t1.setTo( 1, 2 ) ;
		LogPlug::info( "After modification: " + t1.toString() ) ;
		if ( t1 != Bipoint( 1, 2 ) )
			throw Ceylan::TestException( "Bipoint::setTo failed." ) ;

		t1.nullify() ;
		LogPlug::info( "Nullifying: " + t1.toString() ) ;
		if ( t1 != Bipoint( 0, 0 ) )
			throw Ceylan::TestException( "Nullified Bipoint not null." ) ;


		t1.setTo( 7, 5 ) ;
		v2.setTo( 5, 8 ) ;

		t2 = t1 + v2 ;
		LogPlug::info( "Add: t1 " + t1.toString() + " + v2 " + v2.toString()
			+  " = " + t2.toString() ) ;

		if ( t2 != Bipoint( 12, 13 ) )
			throw Ceylan::TestException(
				"Addition of Bipoint and Vector2 failed." ) ;

		t2 = t1 - v2 ;
		LogPlug::info( "Subtract: t1 - v2 " + v2.toString()
			+ " = " + t2.toString() ) ;

		if ( t2 != Bipoint( 2, -3 ) )
			throw Ceylan::TestException(
				"Subtraction Bipoint minus Vector2 failed." ) ;

		v1 = vectorize( t1 ) ;
		if ( v1 != Vector2( 7, 5 ) )
			throw Ceylan::TestException(
				"vectorize Bipoint to Vector2 failed." ) ;

		t2.setTo( 5, 4 ) ;
		v1 = vectorize( t1, t2 ) ;
		if ( v1 != Vector2( -2, -1 ) )
			throw Ceylan::TestException(
				"vectorize (Bipoint, Bipoint) to Vector2 failed: "
				"result was " + v1.toString( Ceylan::low ) ) ;

		LogPlug::info( "Bipoint vectorizations succeeded." ) ;

		LogPlug::info( "End of Bipoint section." ) ;




		LogPlug::info( "Matrix2 section." ) ;

		Matrix2 m1, m2, m3 ;
		LogPlug::info( "Creating a new matrix: " + m1.toString() ) ;

		m1.setTo( 1, 3, 3, 4 ) ;
		LogPlug::info( "After modification: " + m1.toString() ) ;

		MatrixIndex index = 1 ;
		v1.setTo( 5, 0 ) ;
		m1.setColumn( 1, v1 ) ;
		LogPlug::info( "Setting column "
			+ Ceylan::toString( static_cast<int>( index ) )
			+ " to " + v1.toString()
			+ ": "  + m1.toString() ) ;

		LogPlug::info( "Trace of this matrix is "
			+ Ceylan::toString( m1.trace() ) + "." ) ;

		if ( ! AreRelativelyEqual( m1.trace(), static_cast<Real>( 1 ) ) )
			throw Ceylan::TestException( "Incorrect Matrix2 trace returned." ) ;

		LogPlug::info( "Determinant of this matrix is "
			+ Ceylan::toString( m1.determinant() ) + "." ) ;

		if ( ! AreRelativelyEqual( m1.determinant(),
				static_cast<Real>( -15 ) ) )
			throw Ceylan::TestException(
				"Incorrect Matrix2 determinant returned." ) ;

		LogPlug::info( "Transposed matrix is " + (~m1).toString() ) ;

		m3.nullify() ;
		LogPlug::info( "Null matrix: " + m3.toString() ) ;

		m3.setToIdentity() ;
		LogPlug::info( "Identity matrix: " + m3.toString() ) ;


		m2.setTo( 4, 7, 3, 5 ) ;

		LogPlug::info( "m1 = " + m1.toString() + ", m2 = " + m2.toString() ) ;

		m3 = m1 + m2 ;
		LogPlug::info( "m1 + m2 = " + m3.toString() ) ;
		if ( m3 != Matrix2( 5, 12, 6, 5 ) )
			throw Ceylan::TestException( "Incorrect Matrix2 addition." ) ;

		m3 = m1 * m2 ;
		LogPlug::info( "m1 * m2 = " + m3.toString() ) ;
		if ( m3 != Matrix2( 19, 32, 12, 21 ) )
			throw Ceylan::TestException( "Incorrect Matrix2 multiplication." ) ;

		m3 = 2 * m1 ;
		LogPlug::info( "m3 = 2 * m1 = " + m3.toString() ) ;
		if ( m3 != Matrix2( 2, 10, 6, 0 ) )
			throw Ceylan::TestException(
				"Incorrect Matrix2 scalar multiplication." ) ;

		m3.transpose() ;
		LogPlug::info( "Transpose m3: ~m3 = " + m3.toString() ) ;
		if ( m3 != Matrix2( 2, 6, 10, 0 ) )
			throw Ceylan::TestException( "Incorrect Matrix2 transposition: "
				"result was " + m3.toString( Ceylan::low ) ) ;


		m3.setTo( 4, 3, 7, 5 ) ;


		LogPlug::info( "m1 = " + m1.toString() + ", m2 = " + m2.toString()
			+ ", m3 = " + m3.toString() ) ;

		m3 = ( 2*m1 - 7*m2 ) * ( m3 + ~m2 ) ;
		LogPlug::info(
			"Complex expression  m3 = ( 2*m1 - 7*m2 ) * ( m3 + ~m2 ) = "
			+ m3.toString() ) ;
		if ( m3 != Matrix2( -754, -546, -610, -440 ) )
			throw Ceylan::TestException(
				"Incorrect Matrix2 complex evaluation returned, "
				"result was " + m3.toString( Ceylan::low ) ) ;

		LogPlug::info( "Cofactor matrix of m1 is "
			+ Matrix2::Cofactor( m1 ).toString() ) ;

		m2 = ! m1 ;
		LogPlug::info( "Reciproqual of m1 is " + m2.toString() ) ;

		LogPlug::info( "Testing: m1 * m2 = m3 = approximated Identity ?" ) ;
		m3 = m1 * m2 ;
		LogPlug::info( "Result is " + m3.toString() ) ;
		v1.setTo( 100, 100 ) ;


		/*
		 * Testing only a necessary condition, no special relaxed epsilon used
		 * to overcome computing errors:
		 *
		 */

		if ( ! AreRelativelyEqual( ~v1, ~( m3*v1 ) ) )
			throw Ceylan::TestException(
				"Incorrect Matrix2 inverse returned: ~v1 = "
				+ Ceylan::toString( ~v1 ) + " whereas ~( m3*v1) = "
				+ Ceylan::toString( ~( m3*v1) ) + "." ) ;

		LogPlug::info( "Inverse matrix is satisfying: ~v1 = "
				+ Ceylan::toString( ~v1 ) + " whereas ~( m3*v1) = "
				+ Ceylan::toString( ~( m3*v1) ) + "." ) ;

		v1.setTo( 5, 8 ) ;
		LogPlug::info( "Setting v1 = " + v1.toString() ) ;
		v2 = m1 * v1 ;
		LogPlug::info( "Matrix * vector: v2 = m1 * v1 = "
			+ v2.toString() ) ;
		if ( v2 != Vector2( 45, 15 ) )
			throw Ceylan::TestException(
				"Incorrect Matrix2-Vector2 multiplication returned." ) ;

		AngleInDegrees myAngle = 0 ;
		Rotation2DFunctor firstRotation( myAngle ) ;
		LogPlug::info( "Testing rotation: "
			"creating a new rotation endomorphism "
			"whose description is " + firstRotation.toString() ) ;
		Matrix2 mrot = Matrix2::CreateFrom( firstRotation ) ;

		LogPlug::info( "Corresponding matrix is: " + mrot.toString() ) ;

		myAngle = 180 ;
		Rotation2DFunctor secondRotation( myAngle ) ;
		LogPlug::info( "Testing rotation: "
			"creating a new rotation endomorphism whose description is "
			+ secondRotation.toString() ) ;

		mrot = Matrix2::CreateFrom( secondRotation ) ;
		LogPlug::info( "Corresponding matrix is: " + mrot.toString() ) ;

		LogPlug::info( "End of Matrix2 section." ) ;

		LogPlug::info( "Homogeneous Matrix3 section." ) ;
		LogPlug::info( "Homogeneous Matrix3 are to be used in 2D contexts." ) ;

		myAngle = 0 ;
		Vector2 vfirst( 1, 2 ) ;

		HomogeneousMatrix3 first( myAngle, vfirst ) ;
		LogPlug::info(
			"Creating an HomogeneousMatrix3 hm1 with rotation angle "
			+ Ceylan::toString( myAngle ) + " and translation vector "
			+ vfirst.toString() + ": " + first.toString() ) ;

		myAngle = 180 ;
		Vector2 vsecond( 3, 4 ) ;

		HomogeneousMatrix3 second( myAngle, vfirst ) ;
		LogPlug::info(
			"Creating an HomogeneousMatrix3 hm2 with rotation angle "
			+ Ceylan::toString( myAngle ) + " and translation vector "
			+ vsecond.toString() + ": " + second.toString() ) ;

		HomogeneousMatrix3 third = first * second ;
		LogPlug::info( "Result of hm1 * hm2 = " + third.toString() ) ;


		LogPlug::info( "End of Homogeneous Matrix3 section." ) ;

		LogPlug::info( "End of maths linear 2D test." ) ;


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

	Ceylan::shutdown() ;

	return Ceylan::ExitSuccess ;

}
