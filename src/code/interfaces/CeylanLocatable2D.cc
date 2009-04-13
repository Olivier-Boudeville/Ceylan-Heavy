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


#include "CeylanLocatable2D.h"


#include "CeylanHomogeneousMatrix3.h"   // for HomogeneousMatrix3
#include "CeylanMathsBasic.h"           // for Real, Abs, EpsilonDouble, etc.

#include "CeylanLogPlug.h"              // for LogPlug
#include "CeylanUtils.h"                // for emergencyShutdown
#include "CeylanOperators.h"            // for toString



using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Maths::Linear ;

using Ceylan::Maths::Real ;



Locatable2D::Locatable2D( Locatable2D & fatherLocatable ) throw() :
	Locatable( fatherLocatable )
{

	blankLocalReferential() ;
	
}


Locatable2D::Locatable2D() throw() :
	Locatable()
{

	blankLocalReferential() ;
	
}


Locatable2D::Locatable2D( Locatable2D & fatherLocatable, 
		Matrix & localReferential ) throw() :
	Locatable( fatherLocatable, localReferential )
{

}


Locatable2D::Locatable2D( Matrix & localReferential ) throw() :
	Locatable( localReferential )
{

}


Locatable2D::~Locatable2D() throw()
{

	// Owned referentials are deleted on parent destructor (~Locatable).
	
}


Matrix & Locatable2D::getLocalReferential() const throw( LocatableException )
{

	if ( _localReferential == 0 )
		throw LocatableException( "Locatable2D::getLocalReferential() : "
			"no local referential available." ) ;

	/*
	 * disabled : 
	 * return * dynamic_cast<Ceylan::Maths::Linear::HomogeneousMatrix3 *>(
	 *	_localReferential ) ;
	 *
	 */
	
	return * _localReferential ;
	
}


void Locatable2D::blankLocalReferential() throw()
{

	// Inefficient but cleaner (could be directly created as the identity).
	if ( _localReferential == 0 )
		_localReferential = new HomogeneousMatrix3() ;
		
	_localReferential->setToIdentity() ;
		
}


Bipoint Locatable2D::getCenter() const throw( LocatableException )
{

	HomogeneousMatrix3 & localMatrix = getLocalMatrix() ;
	
	Real factor = localMatrix.getElementAt( 2, 2 ) ;
	
	// Prefer to avoid '==' comparison with floating point values :
	if ( Maths::IsNull( factor ) )
		throw LocatableException( "Locatable2D::getCenter : "
			"homogeneous factor is too close to zero ("
			+ Ceylan::toString( factor ) + ")." ) ;
		
	return Bipoint( localMatrix.getElementAt( 2, 0 ) / factor, 
		localMatrix.getElementAt( 2, 1 ) / factor ) ;
		
}

 
void Locatable2D::setCenter( const Bipoint & newCenter ) 
	throw( LocatableException )
{

	if ( _localReferential == 0 )
		throw LocatableException( 
			"Locatable2D::setCenter : no local referential available." ) ;
	
	HomogeneousMatrix3 & localMatrix = getLocalMatrix() ;
	
	localMatrix.setElementAt( 2, 0, newCenter.getX() ) ;
	localMatrix.setElementAt( 2, 1, newCenter.getY() ) ;
	
	/*
	 * Will trigger the recomputation of the global referential the
	 * next time it will be needed :
	 *
	 */
	setUpToDateState( false ) ;
	
}


void Locatable2D::setCenter( Real newX, Real newY ) throw( LocatableException )
{

	if ( _localReferential == 0 )
		throw LocatableException( 
			"Locatable2D::setCenter : no local referential available." ) ;
	
	// Avoid typing the dynamic_cast :
	HomogeneousMatrix3 & localMatrix = getLocalMatrix() ;
	
	localMatrix.setElementAt( 2, 0, newX ) ;
	localMatrix.setElementAt( 2, 1, newY ) ;

	/*
	 * Will trigger the recomputation of the global referential the
	 * next time it will be needed :
	 *
	 */
	setUpToDateState( false ) ;
	
}


const string Locatable2D::toString( VerbosityLevels level ) const throw()
{

	string res ;
	
	if ( isAbsolute() )
		res = "Absolute" ;
	else
		res = "Relative" ;
	
	return res + " Locatable2D, " + describe( level ) ;
	
	
}


void Locatable2D::updateFromFather( const Matrix & upToDateFatherReferential )
	throw()
{

	if ( _localReferential == 0 )
		emergencyShutdown( "Locatable2D::updateFromFather : "
			"no local referential available." ) ;

	const HomogeneousMatrix3 * m = 
		dynamic_cast<const HomogeneousMatrix3*>( & upToDateFatherReferential ) ;

	if ( m == 0 )
		emergencyShutdown( 
			"Locatable2D::updateFromFather : father matrix not a Matrix3." ) ;
	
	
	/*
	 * See our users' guide to understand why we take this node's 
	 * local matrix ('Rl') and the father's global matrix ('Rfg'),
	 * from which we compute the new global matrix of this node
	 * thanks to the following multiplication : Rg = Rfg.Rl
	 *
	 */
	 
	_globalReferential = new HomogeneousMatrix3( (*m) 
		* ( * dynamic_cast<const HomogeneousMatrix3*>( 
			_localReferential ) ) ) ;		

}

