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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "CeylanEndomorphism.h"   


#include "CeylanLogPlug.h"    // for LogPlug
#include "CeylanOperators.h"  // for toString

#include "CeylanVector2.h"    // for Vector2
#include "CeylanVector3.h"    // for Vector3

#include "CeylanMathsBasic.h" // for AngleInDegrees


using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Linear ;




// 2D section.


Endomorphism2DFunctor::Endomorphism2DFunctor() 
{	

}



Endomorphism2DFunctor::~Endomorphism2DFunctor() throw() 
{

}



const string Endomorphism2DFunctor::toString( VerbosityLevels level ) const
{

	return "Functor encapsulating a 2D endomorphism" ;
	
}




Rotation2DFunctor::Rotation2DFunctor( AngleInDegrees angle ) :
	_angle( 0 ) 
{

	_angle = DegreeToRadian( angle ) ;
	
}



Rotation2DFunctor::~Rotation2DFunctor() throw()
{

}


Vector2 Rotation2DFunctor::operator() ( const Vector2 & v )
{

	// Hardcoded 2x2 rotation:
	
    return Vector2( 
		Cos( _angle ) * v.getElementAt( 0 ) 
			- Sin( _angle) * v.getElementAt( 1 ),
		Sin( _angle ) * v.getElementAt( 0 ) 
			+ Cos( _angle) * v.getElementAt( 1 )
	) ;
		
}



const string Rotation2DFunctor::toString( VerbosityLevels level ) const
{

	return Endomorphism2DFunctor::toString() + ": rotation of angle " 
		+ Ceylan::toString( _angle ) + " radians" ;
		
}




// 3D section.


Endomorphism3DFunctor::Endomorphism3DFunctor() 
{	

}


Endomorphism3DFunctor::~Endomorphism3DFunctor() throw()
{

}



const string Endomorphism3DFunctor::toString( VerbosityLevels level ) const
{

	return "Functor encapsulating a 3D endomorphism" ;
	
}



LineProjection3DFunctor::LineProjection3DFunctor( const Vector3 & axis ) :
	_axis( axis ) 
{

	_axis.normalize() ;
	
}



LineProjection3DFunctor::~LineProjection3DFunctor() throw()
{

}



Vector3 LineProjection3DFunctor::operator() ( const Vector3 & v )
{

	return ( v | _axis ) *  _axis ;
	
}



const string LineProjection3DFunctor::toString( VerbosityLevels level ) const
{

	return Endomorphism3DFunctor::toString() + ": line projection on axis " 
		+ _axis.toString() ;
		
}



Rotation3DFunctor::Rotation3DFunctor( const Vector3 & axis, 
		AngleInDegrees angle ) :
	_axis( axis ),
	_angle( 0 ) 
{

	_axis.normalize() ;
	_angle = DegreeToRadian( angle ) ;
	
}



Rotation3DFunctor::~Rotation3DFunctor() throw()
{

}



Vector3 Rotation3DFunctor::operator() ( const Vector3 & v )
{

    return (  Cos( _angle )*v +
	   ( ( v | _axis ) * ( 1 - Cos( _angle ) ) ) * _axis 
	   + Sin( _angle) * ( _axis ^ v )  ) ;
	   
}



const string Rotation3DFunctor::toString( VerbosityLevels level ) const
{

	return Endomorphism3DFunctor::toString() + ": rotation around axis " 
		+ _axis.toString() + " of angle " 
		+ Ceylan::toString( _angle ) + " radians" ;
		
}

