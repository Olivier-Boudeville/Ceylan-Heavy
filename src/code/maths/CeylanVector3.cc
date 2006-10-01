#include "CeylanVector3.h"   

#include "CeylanMatrix3.h"    // for Matrix3
#include "CeylanTripoint.h"   // for Tripoint

#include "CeylanLogPlug.h"    // for LogPlug
#include "CeylanOperators.h"  // for toString
#include "CeylanUtils.h"      // for emergencyShutdown

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



Vector3::Vector3( Real x0, Real x1, Real x2 ) throw() 
{
	_vec[0] = x0 ;
	_vec[1] = x1 ;
	_vec[2] = x2 ;	
}


Vector3::~Vector3() throw() 
{

}



void Vector3::setTo( Real x0, Real x1, Real x2 ) throw()
{
	_vec[0] = x0 ;
	_vec[1] = x1 ;
	_vec[2] = x2 ;	
}


void Vector3::nullify() throw()
{
	setAllElementsTo( 0 ) ;
}


void Vector3::setAllElementsTo( Real commonValue ) throw()
{

	for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )
	{	
		_vec[i] = 0 ;
	}
	
}


Real Vector3::getElementAt( MatrixIndex index ) const throw()
{

#if CEYLAN_DEBUG

	if ( index >= Dimensions )
		Ceylan::emergencyShutdown( 
			"Vector3::getElementAt : index out of bounds." ) ;
		
#endif // CEYLAN_DEBUG 
		
	return _vec[ index ] ;
}


void Vector3::setElementAt( MatrixIndex index, Real newValue ) throw()
{

#if CEYLAN_DEBUG

	if ( index >= Dimensions )
		Ceylan::emergencyShutdown( 
			"Vector3::setElementAt : index out of bounds." ) ;
		
#endif // CEYLAN_DEBUG 

	_vec[ index ] = newValue ;
	
}


void Vector3::normalize() throw( LinearException ) 
{
	 
	Real mag = magnitude() ; 
	
	if ( IsNull( mag ) )
		throw LinearException( 
			"Vector3::normalize : null vector cannot be normalized." ) ;
	
	Real factor = static_cast<Real>( 1.0 ) / mag ;
	
	for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )
	{	
		_vec[i] *= factor ;
	}
			
}


Real Vector3::magnitude()const throw()
{

	// Not using Pow, might be faster this way.
	
	Real sum = 0 ;
	
	for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )
	{	
		sum += _vec[i] * _vec[i] ;
	}

	return Sqrt( sum ) ;
	
}


const string Vector3::toString( VerbosityLevels level ) const throw()
{

	string res ;

	if ( TextDisplayable::GetOutputFormat() == TextDisplayable::html )
	{
	
		res = "<table border=1>" ;
		
  		res += "  <tr>\n" ;

    	for ( MatrixIndex i = 0; i < Dimensions; i++ )
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
			string message = "Vector3::toString : conversion error." ;
        	Log::LogPlug::error( message ) ;
			return message ;
		}

#endif // CEYLAN_DEBUG
		
		return res ;	
	}
	else
	{
	
		res = "Vector3 : [ " ;
		
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


bool Ceylan::Maths::Linear::operator == ( const Vector3 & v1, 
	const Vector3 & v2 ) throw()
{

   	for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
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

			
bool Ceylan::Maths::Linear::operator != ( const Vector3 & v1, 
	const Vector3 & v2 ) throw() 
{
	return ( ! ( v1 == v2 ) ) ;
}


Vector3 Ceylan::Maths::Linear::operator + ( const Vector3 & v1 , 
	const Vector3 & v2 ) throw()
{

	Vector3 result ;

   	for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
	{
		result._vec[i] = v1._vec[i] + v2._vec[i] ;
	}

	return result ;

}


Vector3 Ceylan::Maths::Linear::operator - ( const Vector3 & v1 , 
	const Vector3 & v2 ) throw()
{

	Vector3 result ;

   	for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
	{
		result._vec[i] = v1._vec[i] - v2._vec[i] ;
	}

	return result ;

}


Vector3 Ceylan::Maths::Linear::operator * ( Real lambda, 
	const Vector3 & v ) throw()
{

	Vector3 result ;

   	for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
	{
		result._vec[i] = lambda * v._vec[i] ;
	}

	return result ;

}


Vector3 Ceylan::Maths::Linear::operator * ( const Matrix3 & m, 
	const Vector3 & v ) throw()
{

	Vector3 result ;

   	for ( MatrixIndex j = 0; j < Vector3::Dimensions; j++ )
		for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
			result._vec[j] += m._mat[i][j] * v._vec[i] ;

	return result ;

}


Vector3 Ceylan::Maths::Linear::operator ^ ( const Vector3 & v1 , 
	const Vector3 & v2 ) throw()
{

	// Meaningless if Dimension is not 3.
	 
	Vector3 result ;

	result._vec[0] = v1._vec[1] * v2._vec[2] - v1._vec[2] * v2._vec[1] ;
	result._vec[1] = v1._vec[2] * v2._vec[0] - v1._vec[0] * v2._vec[2] ;
	result._vec[2] = v1._vec[0] * v2._vec[1] - v1._vec[1] * v2._vec[0] ;

	return result ;

}


Real Ceylan::Maths::Linear::operator ~ ( const Vector3 & v ) throw()
{
	return v.magnitude() ;
}


Real Ceylan::Maths::Linear::operator | ( const Vector3 & v1 , 
	const Vector3 & v2 ) throw()
{

	Real result = 0 ;
	
   	for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
	{
		result += v1._vec[i] * v2._vec[i] ;
	}

	return result ;
	
}


