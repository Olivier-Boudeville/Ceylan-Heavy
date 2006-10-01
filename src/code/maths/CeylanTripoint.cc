#include "CeylanTripoint.h"   

#include "CeylanVector3.h"    // for Vector3
#include "CeylanMatrix3.h"    // for Matrix3


#include "CeylanLogPlug.h"    // for LogPlug
#include "CeylanOperators.h"  // for toString
#include "CeylanUtils.h"      // for emergencyShutdown


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"                    // for CEYLAN_DEBUG
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


Tripoint::Tripoint( Real x0, Real x1, Real x2 ) throw() 
{
	_coordinates[0] = x0 ;
	_coordinates[1] = x1 ;
	_coordinates[2] = x2 ;	
}


Tripoint::~Tripoint() throw() 
{
}



void Tripoint::setTo( Real x0, Real x1, Real x2 ) throw()
{
	_coordinates[0] = x0 ;
	_coordinates[1] = x1 ;
	_coordinates[2] = x2 ;	
}


void Tripoint::setAllElementsTo( Real commonValue ) throw()
{

	for ( MatrixIndex i = 0; i < Dimensions; i++ )
	{	
		_coordinates[i] = 0 ;
	}
	
}


Real Tripoint::getElementAt( MatrixIndex index ) const throw()
{

#if CEYLAN_DEBUG

	if ( index >= Dimensions )
		Ceylan::emergencyShutdown( 
			"Tripoint::getElementAt : index out of bounds." ) ;
			
#endif // CEYLAN_DEBUG
		
	return _coordinates[ index ] ;
	
}


void Tripoint::setElementAt( MatrixIndex index, Real newValue ) throw()
{

#if CEYLAN_DEBUG

	if ( index >= Dimensions )
		Ceylan::emergencyShutdown( 
			"Tripoint::setElementAt : index out of bounds." ) ;
		
#endif // CEYLAN_DEBUG

	_coordinates[ index ] = newValue ;
	
}


const string Tripoint::toString( VerbosityLevels level ) const throw()
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

	// Non-HTML format requested :
	
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
			string message = "Tripoint::toString : conversion error." ;
        	Log::LogPlug::error( message ) ;
			return message ;
		}

#endif // CEYLAN_DEBUG
		
		return res ;	
	}
	else
	{
	
		res = "Tripoint : [ " ;
		
    	for ( MatrixIndex i = 0; i < Dimensions; i++ )
		{
			res += Ceylan::toString( _coordinates[i] ) 
				+ ( ( i == Dimensions-1 ) ? " ]" : " ; " ) ; 
		}		
		
		return res ;
		
	}
	
	// Useless but avoids warnings :
	return res ;
				
}





bool Ceylan::Maths::Linear::operator == ( const Tripoint & t1, 
	const Tripoint & t2 ) throw()
{

   	for ( MatrixIndex i = 0; i < Tripoint::Dimensions; i++ )
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

			
bool Ceylan::Maths::Linear::operator != ( const Tripoint & t1, 
	const Tripoint & t2 ) throw() 
{
	return ( ! ( t1 == t2 ) ) ;
}


Tripoint Ceylan::Maths::Linear::operator + ( const Tripoint & t , 
	const Vector3 & v ) throw()
{

	Tripoint result ;

   	for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
	{
		result._coordinates[i] = t._coordinates[i] + v._vec[i] ;
	}

	return result ;

}


Tripoint Ceylan::Maths::Linear::operator - ( const Tripoint & t , 
	const Vector3 & v ) throw()
{

	Tripoint result ;

   	for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
	{
		result._coordinates[i] = t._coordinates[i] - v._vec[i] ;
	}

	return result ;

}


Vector3 Ceylan::Maths::Linear::vectorize( const Tripoint & t ) throw()
{

	Vector3 result ;

   	for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
	{
		result._vec[i] = t._coordinates[i] ;
	}

	return result ;

}


Vector3 Ceylan::Maths::Linear::vectorize( const Tripoint & t1, 
	const Tripoint & t2 ) throw()
{

	Vector3 result ;

   	for ( MatrixIndex i = 0; i < Vector3::Dimensions; i++ )
	{
		result._vec[i] = t2._coordinates[i] - t1._coordinates[i] ;
	}

	return result ;

}
