/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#include "CeylanMatrix2.h"   


#include "CeylanBipoint.h"       // for Bipoint
#include "CeylanVector2.h"       // for Vector2

#include "CeylanEndomorphism.h"  // for Rotation2DFunctor

#include "CeylanLogPlug.h"       // for LogPlug
#include "CeylanOperators.h"     // for toString


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"        // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <iostream>              // for endl, setw
using std::endl ;

#include <sstream>               // for ostringstream
using std::ostringstream ;

#include <iomanip>               // for setw



using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Linear ;


using Ceylan::Maths::Real ;



Matrix2::Matrix2( Real x0, Real x1, Real y0, Real y1 ) 
{

	_mat[0][0] = x0 ;  
	_mat[1][0] = x1 ;  
	_mat[0][1] = y0 ;  
	_mat[1][1] = y1 ;  
	
}



Matrix2::Matrix2( const Matrix2 & source ) :
	Matrix()
{

	for ( MatrixIndex j = 0 ;  j < Dimensions ; j++ )
		for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )
			_mat[i][j] = source._mat[i][j] ;

}



Matrix2::~Matrix2() throw()
{

}



void Matrix2::setTo( Real x0, Real x1, Real y0, Real y1 )
{

	_mat[0][0] = x0 ;  
	_mat[1][0] = x1 ;  
	_mat[0][1] = y0 ;  
	_mat[1][1] = y1 ;  

}



void Matrix2::setColumn( MatrixIndex columnNumber, const Vector2 & newColumn )
{

	for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )
		_mat[columnNumber][i] = newColumn._vec[i] ;

}



void Matrix2::setAllElementsTo( Real commonValue )
{

	for ( MatrixIndex j = 0 ;  j < Dimensions ; j++ )
		for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )
			_mat[i][j] = commonValue ;

}



Real Matrix2::getElementAt( MatrixIndex abscissa, MatrixIndex ordinate ) const
{

#if CEYLAN_DEBUG

	if ( abscissa >= Dimensions || ordinate >= Dimensions )
		throw MathsException( "Matrix2::getElementAt: index out of bounds." ) ;
			
#endif // CEYLAN_DEBUG
		
	return _mat[ abscissa ][ ordinate ] ;
	
}



void Matrix2::setElementAt( MatrixIndex abscissa, 
	MatrixIndex ordinate, Real newValue )
{

#if CEYLAN_DEBUG

	if ( abscissa >= Dimensions || ordinate >= Dimensions )
		throw MathsException( "Matrix2::setElementAt: index out of bounds." ) ;

#endif // CEYLAN_DEBUG
		
	_mat[ abscissa ][ ordinate ] = newValue ;

}



void Matrix2::setToIdentity()
{

	setToDiagonal( 1 ) ;
	 
}


void Matrix2::setToDiagonal( Real diagonalTerm )
{

	for ( MatrixIndex j = 0 ;  j < Dimensions ; j++ )
		for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )
			_mat[i][j] = ( (i==j) ? diagonalTerm: 0 ) ;
	 
}



void Matrix2::transpose()
{

	/*
	
	Real temp = _mat[1][0] ; 

	_mat[1][0] = _mat[0][1] ;
	_mat[0][1] = temp ;
	
	*/
	
	// Computes the transposed matrix.
			
	Real temp ;
		
	for ( MatrixIndex j = 0 ; j < Dimensions ; j++ )
		for ( MatrixIndex i = j+1 ; i < Dimensions ; i++ )
		{
			temp = _mat[j][i] ;	
			_mat[j][i] = _mat[i][j] ; 
			_mat[i][j] = temp ;
		}

}



Real Matrix2::trace() const
{

	Real sum = 0 ;

	for ( MatrixIndex i = 0 ; i < Dimensions ; i++ ) 
		sum += _mat[i][i] ;

	return sum ;

}



Real Matrix2::determinant() const
{

	// x0 * y1 - x1 * y0:
	return _mat[0][0] * _mat[1][1] - _mat[0][1] * _mat[1][0] ;

}



const string Matrix2::toString( VerbosityLevels level ) const
{

	string res ;

	if ( TextDisplayable::GetOutputFormat() == TextDisplayable::html )
	{
	
		res = "<table border=1>" ;
		
  		for ( MatrixIndex j = 0; j < Dimensions; j++ )
		{
			res += "  <tr>\n" ;

    		for ( MatrixIndex i = 0; i < Dimensions; i++ )
				res += "    <td>" + Ceylan::toString( _mat[i][j] ) 
					+ "</td>" ;

			res += "  </tr>\n" ;
		}
			
		res += "</table>" ;
	
	
		return res ;
	}

	// Non-HTML format requested:
	
	ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

   	oss << endl ;

  	for ( MatrixIndex j = 0; j < Dimensions; j++ )
    	for ( MatrixIndex i = 0; i < Dimensions; i++ )
  	  		oss << ( ( i == 0 ) ? "[ ": " " )
         	 	<< std::setw(5) 
				<< _mat[i][j] 
				<< ( ( i== Dimensions-1 ) ? " ]\n": " ") ;
   	oss << endl ;

    res = oss.str() ;


#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
		string message = "Matrix2::toString: conversion error." ;
       	Log::LogPlug::error( message ) ;
		return message ;
	}

#endif // CEYLAN_DEBUG
	
	return res ;	

}



Matrix2 Matrix2::Cofactor( const Matrix2 & m ) 
{
		
	Matrix2 result ;

	result._mat[0][0] =    m._mat[1][1] ;
	result._mat[1][0] =  - m._mat[0][1] ;
	result._mat[0][1] =  - m._mat[1][0] ;
	result._mat[1][1] =    m._mat[0][0] ;

	return result ;

}



Matrix2 Matrix2::Adjoint( const Matrix2 & m ) 
{

	Matrix2 result = Cofactor( m ) ;
	result.transpose() ;
	return result ;
	
	// or: return ~ Cofactor( m ) ;
	
}



Matrix2 Matrix2::CreateFromRotation( AngleInDegrees angle ) 
{

	return CreateFrom( Linear::Rotation2DFunctor( angle ) ) ;
	
}



bool Ceylan::Maths::Linear::operator == ( const Matrix2 & m1, 
	const Matrix2 & m2 )
{

	for ( MatrixIndex j = 0;  j < Matrix2::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix2::Dimensions; i++ )
		{
			if ( ! AreRelativelyEqual<Real>( m1._mat[i][j],
					m2._mat[i][j] ) )
				return false ;
		}

	return true ;
	
}


			
bool Ceylan::Maths::Linear::operator != ( const Matrix2 & m1, 
	const Matrix2 & m2 ) 
{

	return ( ! ( m1 == m2 ) ) ;
	
}



Matrix2 Ceylan::Maths::Linear::operator + ( const Matrix2 & m1, 
	const Matrix2 & m2 )
{

	Matrix2 result ;

	for ( MatrixIndex j = 0; j < Matrix2::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix2::Dimensions; i++ )
			result._mat[i][j] = m1._mat[i][j] + m2._mat[i][j] ;

	return result ;

}



Matrix2 Ceylan::Maths::Linear::operator - ( const Matrix2 & m1, 
	const Matrix2 & m2 )
{

	Matrix2 result ;

	for ( MatrixIndex j = 0; j < Matrix2::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix2::Dimensions; i++ )
			result._mat[i][j] = m1._mat[i][j] - m2._mat[i][j] ;

	return result ;

}



Matrix2 Ceylan::Maths::Linear::operator * ( const Matrix2 & m1, 
	const Matrix2 & m2 )
{

	Matrix2 result ;

	for ( MatrixIndex j = 0; j < Matrix2::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix2::Dimensions; i++ )
			for ( MatrixIndex k = 0; k < Matrix2::Dimensions; k++ )
				result._mat[i][j] += m1._mat[k][j] * m2._mat[i][k] ;

	return result ;

}



Matrix2 Ceylan::Maths::Linear::operator * ( Real lambda, const Matrix2 & m )
{

	Matrix2 result ;

	for ( MatrixIndex j = 0; j < Matrix2::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix2::Dimensions; i++ )
			result._mat[i][j] = lambda * m._mat[i][j] ;

	return result ;

}



Matrix2 Ceylan::Maths::Linear::operator ! ( const Matrix2 & m ) 
{

	// Compute the inverse matrix, mi, so that m.mi = I2.
	
	// Gauss pivot would be real overkill in this case.
	
	/*
	 * Actually, the result would be:
	 * 1/( x0 * y1 - x1 * y0 ) * [ y1, -x1 ; -y0, x0 ]
	 *
	 */
	 
	Real d = m.determinant() ;

	if ( ! IsNull( d ) ) 
		return (1/d) * ~ Matrix2::Cofactor( m ) ;
	else 
 		throw LinearException( 
			"Matrix2: inverse operator ('!'): singular matrix." ) ;

}



Matrix2 Ceylan::Maths::Linear::operator ~ ( const Matrix2 & m )
{

	// Computes the transposed matrix.

	/*
	
	result._mat[1][0] = m._mat[0][1] ;
	result._mat[0][1] = m._mat[1][0] ;

	*/
	
	Matrix2 result( m ) ;

	result.transpose() ;
		
	return result ;

}

