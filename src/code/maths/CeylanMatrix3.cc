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


#include "CeylanMatrix3.h"   


#include "CeylanTripoint.h"   // for Tripoint
#include "CeylanVector3.h"    // for Vector3


#include "CeylanLogPlug.h"    // for LogPlug
#include "CeylanOperators.h"  // for toString


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"     // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <iostream>           // for endl, setw
using std::endl ;

#include <sstream>            // for ostringstream
using std::ostringstream ;

#include <iomanip>            // for setw

using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Linear ;

using Ceylan::Maths::Real ;




Matrix3::Matrix3( Real x0, Real x1, Real x2, 
	Real y0, Real y1, Real y2, Real z0, Real z1, Real z2 )
{

	_mat[0][0] = x0 ;  
	_mat[1][0] = x1 ;  
	_mat[2][0] = x2 ;
	_mat[0][1] = y0 ;  
	_mat[1][1] = y1 ;  
	_mat[2][1] = y2 ;
	_mat[0][2] = z0 ;  
	_mat[1][2] = z1 ; 
	_mat[2][2] = z2 ;
	
}



Matrix3::Matrix3( const Matrix3 & source ) :
	Matrix()
{

	for ( MatrixIndex j = 0;  j < Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Dimensions; i++ )
			_mat[i][j] = source._mat[i][j] ;

}



Matrix3::~Matrix3() throw()
{

}



void Matrix3::setTo( Real x0, Real x1, Real x2, 
	Real y0, Real y1, Real y2, Real z0, Real z1, Real z2 )
{

	_mat[0][0] = x0 ;  
	_mat[1][0] = x1 ;  
	_mat[2][0] = x2 ;
	_mat[0][1] = y0 ;  
	_mat[1][1] = y1 ;  
	_mat[2][1] = y2 ;
	_mat[0][2] = z0 ;  
	_mat[1][2] = z1 ; 
	_mat[2][2] = z2 ;

}



void Matrix3::setColumn( MatrixIndex columnNumber, 
	const Vector3 & newColumn )
{

	for ( MatrixIndex i = 0; i < Dimensions; i++ )
		_mat[columnNumber][i] = newColumn._vec[i] ;
		
}



void Matrix3::setAllElementsTo( Real commonValue )
{

	for ( MatrixIndex j = 0;  j < Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Dimensions; i++ )
			_mat[i][j] = commonValue ;

}



Real Matrix3::getElementAt( MatrixIndex abscissa, 
	MatrixIndex ordinate ) const
{

#if CEYLAN_DEBUG

	if ( abscissa >= Dimensions || ordinate >= Dimensions )
		throw MathsException( "Matrix3::getElementAt: index out of bounds." ) ;
			
#endif // CEYLAN_DEBUG
		
	return _mat[ abscissa ][ ordinate ] ;
	
}



void Matrix3::setElementAt( MatrixIndex abscissa, MatrixIndex ordinate, 
	Real newValue )
{

#if CEYLAN_DEBUG

	if ( abscissa >= Dimensions || ordinate >= Dimensions )
		throw MathsException( "Matrix3::setElementAt: index out of bounds." ) ;
			
#endif // CEYLAN_DEBUG
		
	_mat[ abscissa ][ ordinate ] = newValue ;

}



void Matrix3::setToIdentity()
{

	setToDiagonal( 1 ) ; 
	
}



void Matrix3::setToDiagonal( Real diagonalTerm )
{

	for ( MatrixIndex j = 0; j < Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Dimensions; i++ )
			_mat[i][j] = ( (i==j) ? diagonalTerm : 0 ) ;
	 
}



void Matrix3::transpose()
{

	// Computes the transposed matrix.
			
	Real temp ;
		
	for ( MatrixIndex j = 0; j < Dimensions; j++ )
		for ( MatrixIndex i = j+1; i < Dimensions; i++ )
		{
			temp = _mat[j][i] ;	
			_mat[j][i] = _mat[i][j] ; 
			_mat[i][j] = temp ;
		}
		
}



Real Matrix3::trace() const
{

	Real sum = 0 ;

	for ( MatrixIndex i = 0; i < Dimensions; i++ ) 
		sum += _mat[i][i] ;

	return sum ;

}



Real Matrix3::determinant() const
{

	return _mat[0][0] * ( _mat[1][1] * _mat[2][2] - _mat[1][2] * _mat[2][1] )
		 - _mat[0][1] * ( _mat[1][0] * _mat[2][2] - _mat[1][2] * _mat[2][0] )
		 + _mat[0][2] * ( _mat[1][0] * _mat[2][1] - _mat[1][1] * _mat[2][0] ) ;

}



const string Matrix3::toString( VerbosityLevels level ) const
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
  	  		oss << ( ( i == 0 ) ? "[ " : " " )
         	 	<< std::setw(5) 
				<< _mat[i][j] 
				<< ( ( i== Dimensions-1 ) ? " ]\n" : " ") ;
   	oss << endl ;

    res = oss.str() ;


#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
		string message = "Matrix3::toString: conversion error." ;
       	Log::LogPlug::error( message ) ;
		return message ;
	}

#endif // CEYLAN_DEBUG
	
	return res ;	

}



Matrix3 Matrix3::Cofactor( const Matrix3 & m )
{
	
	Matrix3 result ;

	result._mat[0][0] =  
		m._mat[1][1] * m._mat[2][2] - m._mat[1][2] * m._mat[2][1] ;
		
	result._mat[1][0] =  
		m._mat[0][2] * m._mat[2][1] - m._mat[0][1] * m._mat[2][2] ;
		
	result._mat[2][0] =  
		m._mat[0][1] * m._mat[1][2] - m._mat[0][2] * m._mat[1][1] ;
		
	result._mat[0][1] =  
		m._mat[1][2] * m._mat[2][0] - m._mat[1][0] * m._mat[2][2] ;
		
	result._mat[1][1] =  
		m._mat[0][0] * m._mat[2][2] - m._mat[0][2] * m._mat[2][0] ;
		
	result._mat[2][1] =  
		m._mat[0][2] * m._mat[1][0] - m._mat[0][0] * m._mat[1][2] ;
		
	result._mat[0][2] =  
		m._mat[1][0] * m._mat[2][1] - m._mat[1][1] * m._mat[2][0] ;
		
	result._mat[1][2] =  
		m._mat[0][1] * m._mat[2][0] - m._mat[0][0] * m._mat[2][1] ;
		
	result._mat[2][2] =  
		m._mat[0][0] * m._mat[1][1] - m._mat[0][1] * m._mat[1][0] ;

	return result ;

}



Matrix3 Matrix3::Adjoint( const Matrix3 & m )
{

	Matrix3 result = Cofactor( m ) ;
	result.transpose() ;
	return result ;
	
	// or: return ~ Cofactor( m ) ;
	
}



bool Ceylan::Maths::Linear::operator == ( const Matrix3 & m1, 
	const Matrix3 & m2 )
{

	for ( MatrixIndex j = 0;  j < Matrix3::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix3::Dimensions; i++ )
		{
			if ( ! AreRelativelyEqual<Real>( m1._mat[i][j],
					m2._mat[i][j] ) )
				return false ;
		}

	return true ;
	
}


			
bool Ceylan::Maths::Linear::operator != ( const Matrix3 & m1, 
	const Matrix3 & m2 )
{

	return ( ! ( m1 == m2 ) ) ;
	
}



Matrix3 Ceylan::Maths::Linear::operator + ( const Matrix3 & m1, 
	const Matrix3 & m2 )
{

	Matrix3 result ;

	for ( MatrixIndex j = 0; j < Matrix3::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix3::Dimensions; i++ )
			result._mat[i][j] = m1._mat[i][j] + m2._mat[i][j] ;

	return result ;

}



Matrix3 Ceylan::Maths::Linear::operator - ( const Matrix3 & m1, 
	const Matrix3 & m2 )
{

	Matrix3 result ;

	for ( MatrixIndex j = 0; j < Matrix3::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix3::Dimensions; i++ )
			result._mat[i][j] = m1._mat[i][j] - m2._mat[i][j] ;

	return result ;

}



Matrix3 Ceylan::Maths::Linear::operator * ( const Matrix3 & m1, 
	const Matrix3 & m2 )
{

	Matrix3 result ;

	for ( MatrixIndex j = 0; j < Matrix3::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix3::Dimensions; i++ )
			for ( MatrixIndex k = 0; k < Matrix3::Dimensions; k++ )
				result._mat[i][j] += m1._mat[k][j] * m2._mat[i][k] ;

	return result ;

}



Matrix3 Ceylan::Maths::Linear::operator * ( Real lambda, const Matrix3 & m )
{

	Matrix3 result ;

	for ( MatrixIndex j = 0; j < Matrix3::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix3::Dimensions; i++ )
			result._mat[i][j] = lambda * m._mat[i][j] ;

	return result ;

}



Matrix3 Ceylan::Maths::Linear::operator ! ( const Matrix3 & m ) 
{

	// Compute the inverse matrix, mi, so that m.mi = I3.
	
	// Gauss pivot would be overkill in this case.
	
	Real d = m.determinant() ;

	if ( ! IsNull( d ) ) 
		return (1/d) * ~ Matrix3::Cofactor( m ) ;
	else 
 		throw LinearException( 
			"Matrix3: inverse operator ('!'): singular matrix." ) ;

}



Matrix3 Ceylan::Maths::Linear::operator ~ ( const Matrix3 & m )
{

	// Computes the transposed matrix.
	
	Matrix3 result( m ) ;

	result.transpose() ;
		
	return result ;

}

