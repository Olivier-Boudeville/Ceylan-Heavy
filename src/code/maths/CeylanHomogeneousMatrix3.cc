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


#include "CeylanHomogeneousMatrix3.h"   

#include "CeylanMatrix2.h"              // for Matrix2
#include "CeylanVector2.h"              // for Vector2

#include "CeylanTripoint.h"             // for Tripoint
#include "CeylanVector3.h"              // for Vector3

#include "CeylanLogPlug.h"              // for LogPlug
#include "CeylanOperators.h"            // for toString
#include "CeylanTextDisplayable.h"      // for GetOutputFormat

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      			// for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H

#include <iostream>                     // for endl, setw
using std::endl ;

#include <sstream>                      // for ostringstream
using std::ostringstream ;

#include <iomanip>                      // for setw


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Linear ;




HomogeneousMatrix3::HomogeneousMatrix3( const Matrix2 & r, 
	const Vector2 & v )
{

	// Top-left r matrix:
	setRotationMatrix( r ) ;
	
	// Top-right v vector:
	setTranslationVector( v ) ;
		
	setBottomRow() ;	
	
}



HomogeneousMatrix3::HomogeneousMatrix3( Real r0, Real r1, Real r2, Real r3,
	Real t0, Real t1 )
{

	// Top-left r matrix:
	_mat[0][0] = r0 ;  
	_mat[1][0] = r1 ;    
	_mat[0][1] = r2 ;    
	_mat[1][1] = r3 ;    

	// Top-right v vector:
	_mat[2][0] = t0 ;
	_mat[2][1] = t1 ; 
		
	setBottomRow() ;	

}



HomogeneousMatrix3::HomogeneousMatrix3( AngleInDegrees angle, 
	const Maths::Linear::Vector2 & v )
{

	setRotationMatrix( Matrix2::CreateFromRotation( angle ) ) ;
	setTranslationVector( v ) ;
	setBottomRow() ;
		
}



HomogeneousMatrix3::~HomogeneousMatrix3() throw()
{

	// No dynamic member.
	
}



const string HomogeneousMatrix3::toString( VerbosityLevels level ) const
{


	string res ;

	if ( TextDisplayable::GetOutputFormat() == TextDisplayable::html )
	{
	
		res = "<table border=1>" ;
		
  		for ( MatrixIndex j = 0; j < Dimensions; j++ )
		{
			res += "  <tr>\n" ;

    		for ( MatrixIndex i = 0; i < Dimensions; i++ )
				res += "    <td>" + Ceylan::toString( _mat[i][j] ) + "</td>" ;

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
				<< ( ( i == Dimensions-1 ) ? " ]\n" : " ") ;
   	oss << endl ;

    res = oss.str() ;


#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
		string message = "HomogeneousMatrix3::toString: conversion error." ;
       	Log::LogPlug::error( message ) ;
		return message ;
	}

#endif // CEYLAN_DEBUG
	
	return res ;	

}



void HomogeneousMatrix3::setBottomRow()
{

	// Bottom zeroes:
	_mat[0][2] = 0 ; 
	_mat[1][2] = 0 ;
	
	// Bottom-right one:
	_mat[2][2] = 1 ;

}


void HomogeneousMatrix3::setTranslationVector( const Vector2 & v ) 
{

	_mat[2][0] = v.getElementAt( 0 ) ;
	_mat[2][1] = v.getElementAt( 1 ) ; 
	
}



void HomogeneousMatrix3::setRotationMatrix( const Matrix2 & m )
{

	_mat[0][0] = m.getElementAt( 0, 0 ) ;  
	_mat[1][0] = m.getElementAt( 1, 0 ) ;    
	_mat[0][1] = m.getElementAt( 0, 1 ) ;    
	_mat[1][1] = m.getElementAt( 1, 1 ) ;
	    
}



void HomogeneousMatrix3::setInCanonicalForm()
{

	// Works for homogeneous matrices of all size.
	
	Real bottomRightElement = _mat[Dimensions-1][Dimensions-1] ;
	
	if ( IsNull( bottomRightElement ) )
		throw LinearException( "HomogeneousMatrix3::setInCanonicalForm: "
			"bottom-right element is zero or almost." ) ;
			
	if ( AreRelativelyEqual<Real>( bottomRightElement, 1 ) )
	{
	
		// Nothing to do!
		return ;
		
	}		
	
	// The multiply-by-a-real operator could also be used.

	/*
	 * Optimized thanks to the homogenous form 
	 * (see the 'j < Dimensions - 1' ):
	 *
	 */
	for ( MatrixIndex j = 0;  j < Dimensions - 1; j++ )
		for ( MatrixIndex i = 0; i < Dimensions; i++ )
			_mat[i][j] /= bottomRightElement ;
	
	// Zeroes are expected to be already there.		
	
	_mat[Dimensions-1][Dimensions-1] = 1 ;
			
}



HomogeneousMatrix3 Ceylan::Maths::Linear::operator * ( 
	const HomogeneousMatrix3 & m1, 
	const HomogeneousMatrix3 & m2 )
{

	/*
	 * Matrix block-multiplication teaches us that:
	 * result = [ r, v; 0, 1 ] = 
	 * [ a, b ; 0, 1 ] * [ a', b' ; 0, 1 ] = [ a.a', ab'+b ; 0, 1 ] 
	 *
	 * Therefore, multiplication of HomogeneousMatrix3 returns a
	 * HomogeneousMatrix3, whose computation can be optimized by blocks:
	 *
	 */

	HomogeneousMatrix3 result ;
	
	// r = a.a': 	
	for ( MatrixIndex j = 0; j < Matrix2::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Matrix2::Dimensions; i++ )
			for ( MatrixIndex k = 0; k < Matrix2::Dimensions; k++ )
				result._mat[i][j] += m1._mat[k][j] * m2._mat[i][k] ;
	
	// v = ab'+ b:
	
	for ( MatrixIndex j = 0 ; j < Matrix2::Dimensions ; j++ )
	{
		result._mat[2][j] = /* ab' */ m1._mat[0][j] * m2._mat[2][0] 
			+ m1._mat[1][j] * m2._mat[2][1] 
			+ /* b */ m1._mat[2][j] ;
	}
	
	// Finally, sets the unchanged bottom row.
	result.setBottomRow() ;
	
	return result ;

}

