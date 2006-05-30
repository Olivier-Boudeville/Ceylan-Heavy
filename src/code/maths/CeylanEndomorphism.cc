#include "CeylanEndomorphism.h"   


#include "CeylanLogPlug.h"    // for LogPlug
#include "CeylanOperators.h"  // for toString

#include "CeylanVector2.h"    // for Vector2
#include "CeylanVector3.h"    // for Vector3


using std::string ;


using namespace Ceylan::Maths::Linear ;



Endomorphism2DFunctor::Endomorphism2DFunctor() throw() 
{	
}


Endomorphism2DFunctor::~Endomorphism2DFunctor() throw() 
{
}


const string Endomorphism2DFunctor::toString( VerbosityLevels level ) const throw()
{
	return "Functor encapsulating a 2D endomorphism" ;
}



Rotation2DFunctor::Rotation2DFunctor( AngleInDegrees angle ) throw() :
	_angle( 0 ) 
{
	_angle = DegreeToRadian( angle ) ;
}


Rotation2DFunctor::~Rotation2DFunctor() throw()
{
}


Vector2 Rotation2DFunctor::operator() ( const Vector2 & v ) throw()
{

	// Hardcoded 2x2 rotation :
	
    return Vector2( 
		Cos( _angle ) * v.getElementAt( 0 ) - Sin( _angle) * v.getElementAt( 1 ),
		Sin( _angle ) * v.getElementAt( 0 ) + Cos( _angle) * v.getElementAt( 1 )
	) ;
		
}


const string Rotation2DFunctor::toString( VerbosityLevels level ) const throw()
{
	return Endomorphism2DFunctor::toString() + " : rotation of angle " 
		+ Ceylan::toString( _angle ) + " radians" ;
}





Endomorphism3DFunctor::Endomorphism3DFunctor() throw() 
{	
}


Endomorphism3DFunctor::~Endomorphism3DFunctor() throw() 
{
}



const string Endomorphism3DFunctor::toString( VerbosityLevels level ) const throw()
{
	return "Functor encapsulating a 3D endomorphism" ;
}



LineProjection3DFunctor::LineProjection3DFunctor( const Vector3 & axis ) throw() :
	_axis( axis ) 
{
	_axis.normalize() ;
}


LineProjection3DFunctor::~LineProjection3DFunctor() throw()
{
}


Vector3 LineProjection3DFunctor::operator() ( const Vector3 & v ) throw()
{
	return ( v | _axis ) *  _axis ;
}


const string LineProjection3DFunctor::toString( VerbosityLevels level ) const throw()
{
	return Endomorphism3DFunctor::toString() + " : line projection on axis " 
		+ _axis.toString() ;
}



Rotation3DFunctor::Rotation3DFunctor( const Vector3 & axis, AngleInDegrees angle ) throw() :
	_axis( axis ),
	_angle( 0 ) 
{
	_axis.normalize() ;
	_angle = DegreeToRadian( angle ) ;
}


Rotation3DFunctor::~Rotation3DFunctor() throw()
{
}


Vector3 Rotation3DFunctor::operator() ( const Vector3 & v ) throw()
{
    return (  Cos( _angle )*v +
	   ( ( v | _axis ) * ( 1 - Cos( _angle ) ) ) * _axis 
	   + Sin( _angle) * ( _axis ^ v )  ) ;
}


const string Rotation3DFunctor::toString( VerbosityLevels level ) const throw()
{
	return Endomorphism3DFunctor::toString() + " : rotation around axis " 
		+ _axis.toString() + " of angle " + Ceylan::toString( _angle ) + " radians" ;
}

