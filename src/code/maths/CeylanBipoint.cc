#include "CeylanBipoint.h"   


#include "CeylanLogPlug.h"              // for LogPlug
#include "CeylanOperators.h"            // for toString
#include "CeylanUtils.h"                // for emergencyShutdown
#include "CeylanTextDisplayable.h"      // for GetOutputFormat

#include "CeylanVector2.h"              // for Vector2
#include "CeylanMatrix2.h"              // for Matrix2

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"               // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <iostream>                     // for endl, setw
using std::endl ;

#include <sstream>                      // for ostringstream
using std::ostringstream ;

#include <iomanip>                      // for setw


using std::string ;


using namespace Ceylan::Maths::Linear ;

using Ceylan::Maths::Real ;



Bipoint::Bipoint( Real x0, Real x1 ) throw() 
{
	_coordinates[0] = x0 ;
	_coordinates[1] = x1 ;
}


Bipoint::~Bipoint() throw() 
{
}



void Bipoint::setTo( Real x0, Real x1 ) throw()
{
	_coordinates[0] = x0 ;
	_coordinates[1] = x1 ;
}


void Bipoint::setAllElementsTo( Real commonValue ) throw()
{

	for ( MatrixIndex i = 0 ; i < Dimensions ; i++ )
	{	
		_coordinates[i] = 0 ;
	}
	
}



Real Bipoint::getX() const throw()
{
	return _coordinates[ 0 ] ;
}


Real Bipoint::getY() const throw()
{
	return _coordinates[ 1 ] ;
}



Real Bipoint::getElementAt( MatrixIndex index ) const throw()
{

#if CEYLAN_DEBUG

	if ( index >= Dimensions )
		Ceylan::emergencyShutdown( 
			"Bipoint::getElementAt : index out of bounds." ) ;
			
#endif // CEYLAN_DEBUG
		
	return _coordinates[ index ] ;
}


void Bipoint::setElementAt( MatrixIndex index, Real newValue ) throw()
{

#if CEYLAN_DEBUG

	if ( index >= Dimensions )
		Ceylan::emergencyShutdown( 
			"Bipoint::setElementAt : index out of bounds." ) ;
#endif // CEYLAN_DEBUG

	_coordinates[ index ] = newValue ;
	
}



const string Bipoint::toString( VerbosityLevels level ) const throw()
{

	string res ;

	if ( TextDisplayable::GetOutputFormat() == TextDisplayable::html )
	{
	
		res = "<table border=1>" ;
		
  		res += "  <tr>\n" ;

    	for ( MatrixIndex i = 0; i < Dimensions; i++ )
		{
			res += "  <tr>\n" ;
			res += "    <td>" + Ceylan::toString( _coordinates[i] ) + "</td>" ;
			res += "  </tr>\n" ;
		}
			
		res += "</table>" ;
	
	
		return res ;
		
	}

	// Non-HTML format requested, raw text assumed :
	
	if ( level == high )
	{
	
    	ostringstream oss ;

		oss.precision( Ceylan::DigitOutputPrecision ) ;

    	oss << endl ;

    	for ( MatrixIndex i = 0; i < Dimensions; i++ )
		{
			oss << "[ " << std::setw(5) << _coordinates[i] <<" ]" << endl ;
			
 		}
		
		oss << endl ;

    	res = oss.str() ;

#if CEYLAN_DEBUG

    	if ( oss.fail() )
		{
			string message = "Bipoint::toString : conversion error." ;
        	Log::LogPlug::error( message ) ;
			return message ;
		}

#endif // CEYLAN_DEBUG
		
		return res ;	
	}
	else
	{
	
		res = "Bipoint : [ " ;
		
    	for ( MatrixIndex i = 0; i < Dimensions; i++ )
		{
			res += Ceylan::toString( _coordinates[i] ) 
				+ ( ( i == Dimensions-1 ) ? " ]" : " ; " ) ; 
		}		
		
		return res ;
		
	}
				
}


Real Bipoint::Distance( const Bipoint & first, const Bipoint & second ) throw()
{
	return Sqrt( Pow2( second.getX() - first.getX() ) 
		+ Pow2( second.getY() - first.getY() ) ) ;
}


Real Bipoint::DistancePow2( const Bipoint & first, const Bipoint & second )
	throw()
{
	return Pow2( second.getX() - first.getX() ) 
		+ Pow2( second.getY() - first.getY() ) ;
}


bool Ceylan::Maths::Linear::operator == ( 
	const Bipoint & t1, const Bipoint & t2 ) throw()
{

   	for ( MatrixIndex i = 0; i < Bipoint::Dimensions; i++ )
	{
	
		/*
		 * Maybe overkill, on absolute comparison may have been 
		 * enough for most purposes.
		 *
		 */
		if ( ! AreRelativelyEqual<Real>( t1._coordinates[i],
				t2._coordinates[i] ) )
			return false ;
	}
	
	return true ;
	
}

			
bool Ceylan::Maths::Linear::operator != ( const Bipoint & t1, 
	const Bipoint & t2 ) throw() 
{
	return ( ! ( t1 == t2 ) ) ;
}


Bipoint Ceylan::Maths::Linear::operator + ( const Bipoint & t , 
	const Vector2 & v ) throw()
{

	Bipoint result ;

   	for ( MatrixIndex i = 0 ; i < Vector2::Dimensions ; i++ )
	{
		result._coordinates[i] = t._coordinates[i] + v._vec[i] ;
	}

	return result ;

}


Bipoint Ceylan::Maths::Linear::operator - ( const Bipoint & t, 
	const Vector2 & v ) throw()
{

	Bipoint result ;

   	for ( MatrixIndex i = 0 ; i < Vector2::Dimensions ; i++ )
	{
		result._coordinates[i] = t._coordinates[i] - v._vec[i] ;
	}

	return result ;

}


Vector2 Ceylan::Maths::Linear::vectorize( const Bipoint & t ) throw()
{

	Vector2 result ;

   	for ( MatrixIndex i = 0; i < Vector2::Dimensions; i++ )
	{
		result._vec[i] = t._coordinates[i] ;
	}

	return result ;

}


Vector2 Ceylan::Maths::Linear::vectorize( const Bipoint & t1, 
	const Bipoint & t2 ) throw()
{

	Vector2 result ;

   	for ( MatrixIndex i = 0; i < Vector2::Dimensions; i++ )
	{
		result._vec[i] = t2._coordinates[i] - t1._coordinates[i] ;
	}

	return result ;

}
