#include "CeylanVector2.h"   

#include "CeylanMatrix2.h"    			 // for Matrix2
#include "CeylanHomogeneousMatrix3.h"    // for HomogeneousMatrix3
#include "CeylanTripoint.h"   			 // for Tripoint

#include "CeylanLogPlug.h"    			 // for LogPlug
#include "CeylanOperators.h"  			 // for toString
#include "CeylanUtils.h"      			 // for emergencyShutdown

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"                // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <iostream>           			 // for endl, setw
using std::endl ;

#include <sstream>            			 // for ostringstream
using std::ostringstream ;

#include <iomanip>            			 // for setw



using std::string ;


using namespace Ceylan::Maths::Linear ;

using Ceylan::Maths::Real ;



Vector2::Vector2( Real x0, Real x1 ) throw() 
{
	_vec[0] = x0 ;
	_vec[1] = x1 ;
}


Vector2::~Vector2() throw() 
{

}



void Vector2::setTo( Real x0, Real x1 ) throw()
{
	_vec[0] = x0 ;
	_vec[1] = x1 ;
}


void Vector2::nullify() throw()
{
	setAllElementsTo( 0 ) ;
}


void Vector2::setAllElementsTo( Real commonValue ) throw()
{

	for ( MatrixIndex i = 0; i < Dimensions; i++ )
	{	
		_vec[i] = 0 ;
	}
	
}


Real Vector2::getX() const throw()
{
	return _vec[ 0 ] ;
}


Real Vector2::getY() const throw()
{
	return _vec[ 1 ] ;
}


Real Vector2::getElementAt( MatrixIndex index ) const throw()
{

#if CEYLAN_DEBUG

	if ( index >= Dimensions )
		Ceylan::emergencyShutdown( 
			"Vector2::getElementAt : index out of bounds." ) ;
			
#endif // CEYLAN_DEBUG 
		
	return _vec[ index ] ;
	
}


void Vector2::setElementAt( MatrixIndex index, Real newValue ) throw()
{

#if CEYLAN_DEBUG

	if ( index >= Dimensions )
		Ceylan::emergencyShutdown( 
			"Vector2::setElementAt : index out of bounds." ) ;
		
#endif // CEYLAN_DEBUG

	_vec[ index ] = newValue ;
	
}


void Vector2::normalize() throw( LinearException ) 
{

	Real mag = magnitude() ; 
	
	if ( IsNull( mag ) )
		throw LinearException( 
			"Vector2::normalize : null vector cannot be normalized." ) ;
	
	Real factor = 1.0 / mag ;
	
	for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )
	{	
		_vec[i] *= factor ;
	}
		
}


Real Vector2::magnitude()const throw()
{

	// Not using Pow, might be faster this way.
	
	Real sum = 0 ;
	
	for ( MatrixIndex i = 0; i < Dimensions; i++ )
	{	
		sum += _vec[i] * _vec[i] ;
	}

	return Sqrt( sum ) ;
	
}


const string Vector2::toString( VerbosityLevels level ) const throw()
{

	string res ;


	if ( TextDisplayable::GetOutputFormat() == TextDisplayable::html )
	{
	
		res = "<table border=1>" ;
		
  		res += "  <tr>\n" ;

    	for ( MatrixIndex i = 0;  i < Dimensions; i++ )
		{
			res += "  <tr>\n" ;
			res += "    <td>" + Ceylan::toString( _vec[i] ) + "</td>" ;
			res += "  </tr>\n" ;
		}
			
		res += "</table>" ;
	
	
		return res ;
		
	}

	// Non-HTML format requested :
	
	if ( level == high )
	{
    	ostringstream oss ;

		oss.precision( Ceylan::DigitOutputPrecision ) ;

    	oss << endl ;

    	for ( MatrixIndex i = 0; i < Dimensions; i++ )
		{
			oss << "[ " << std::setw(5) << _vec[i] <<" ]" << endl ;
			
 		}
		
		oss << endl ;

    	res = oss.str() ;

#if CEYLAN_DEBUG

    	if ( oss.fail() )
		{
			string message = "Vector2::toString : conversion error." ;
        	Log::LogPlug::error( message ) ;
			return message ;
		}

#endif // CEYLAN_DEBUG
		
		return res ;	
	}
	else
	{
	
		res = "Vector2 : [ " ;
		
    	for ( MatrixIndex i = 0; i < Dimensions; i++ )
		{
			res += Ceylan::toString( _vec[i] ) 
				+ ( ( i == Dimensions-1 ) ? " ]" : " ; " ) ; 
		}		
		
		return res ;
		
	}
	
	// Useless but avoids warnings :
	return res ;
	
}


bool Ceylan::Maths::Linear::operator == ( const Vector2 & v1, 
	const Vector2 & v2 ) throw()
{

   	for ( MatrixIndex i = 0 ; i < Vector2::Dimensions ; i++ )
	{

		/*
		 * Maybe overkill, on absolute comparison may have been 
		 * enough for most purposes.
		 *
		 */
		if ( ! AreRelativelyEqual<Real>( v1._vec[i],
				v2._vec[i] ) )
			return false ;
	}
	
	return true ;		

}

			
bool Ceylan::Maths::Linear::operator != ( const Vector2 & v1, 
	const Vector2 & v2 ) throw() 
{
	return ( ! ( v1 == v2 ) ) ;
}


Vector2 Ceylan::Maths::Linear::operator + ( const Vector2 & v1 , 
	const Vector2 & v2 ) throw()
{

	Vector2 result ;

   	for ( MatrixIndex i = 0 ; i < Vector2::Dimensions ; i++ )
	{
		result._vec[i] = v1._vec[i] + v2._vec[i] ;
	}

	return result ;

}


Vector2 Ceylan::Maths::Linear::operator - ( const Vector2 & v1 , 
	const Vector2 & v2 ) throw()
{

	Vector2 result ;

   	for ( MatrixIndex i = 0 ; i < Vector2::Dimensions ; i++ )
	{
		result._vec[i] = v1._vec[i] - v2._vec[i] ;
	}

	return result ;

}


Vector2 Ceylan::Maths::Linear::operator * ( Real lambda, 
	const Vector2 & v ) throw()
{

	Vector2 result ;

   	for ( MatrixIndex i = 0 ; i < Vector2::Dimensions ; i++ )
	{
		result._vec[i] = lambda * v._vec[i] ;
	}

	return result ;

}


Vector2 Ceylan::Maths::Linear::operator * ( const Matrix2 & m , 
	const Vector2 & v ) throw()
{

	Vector2 result ;

   	for ( MatrixIndex j = 0 ; j < Vector2::Dimensions ; j++ )
		for ( MatrixIndex i = 0 ; i < Vector2::Dimensions ; i++ )
			result._vec[j] += m._mat[i][j] * v._vec[i] ;

	return result ;

}


Vector2 Ceylan::Maths::Linear::operator * ( const HomogeneousMatrix3 & m, 
	const Vector2 & v ) throw()
{

	Vector2 result ;
	
	// Optimized to take part of 0 and 1 of the homogeneous matrix :
	
   	for ( MatrixIndex j = 0 ; j < Vector2::Dimensions ; j++ )
	{
		for ( MatrixIndex i = 0 ; i < Vector2::Dimensions ; i++ )
		{
			result._vec[j] += m._mat[i][j] * v._vec[i] ;
		}
		
		result._vec[j] += m._mat[HomogeneousMatrix3::Dimensions-1][j] ;
	}
	
	return result ;

}



/*
 * No 'Vector2 Ceylan::Maths::Linear::operator ^ ( const Vector2 & v1 ,
 * const Vector2 & v2 ) throw()' can exist.
 *
 */


Real Ceylan::Maths::Linear::operator ~ ( const Vector2 & v ) throw()
{
	return v.magnitude() ;
}


Real Ceylan::Maths::Linear::operator | ( const Vector2 & v1 , 
	const Vector2 & v2 ) throw()
{

	Real result = 0 ;
	
   	for ( MatrixIndex i = 0 ; i < Vector2::Dimensions ; i++ )
	{
		result += v1._vec[i] * v2._vec[i] ;
	}

	return result ;

}


