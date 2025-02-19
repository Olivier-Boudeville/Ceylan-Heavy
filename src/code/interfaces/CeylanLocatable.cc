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


#include "CeylanLocatable.h"

#include "CeylanMatrix.h"       // for Matrix
#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanUtils.h"        // for emergencyShutdown
#include "CeylanOperators.h"    // for toString
#include "CeylanTypes.h"        // for ListSize


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Maths::Linear ;




LocatableException::LocatableException( const string & message ) :
	Ceylan::Exception( "Locatable exception: " + message )
{

}



LocatableException::~LocatableException() throw()
{

}




ReferentialChangedEvent::ReferentialChangedEvent( EventSource & source ) :
	Event( source )
{

}



ReferentialChangedEvent::~ReferentialChangedEvent() throw()
{

}




// Locatable implementation.



Locatable::Locatable( Locatable & fatherLocatable ) :
	EventSource(),
	EventListener(),
	_father( & fatherLocatable ),
	_localReferential( 0 ),
	_globalReferential( 0 ),
	_changedEvent( 0 ),
	_isUpToDate( false )
{


	/*
	 * Registers this Locatable to its father, so that this one can 
	 * notice him of its future changes.
	 *
	 */
	 
	try
	{
	
		subscribeTo( * _father ) ;
		
	}
	catch( const EventException	& e )
	{
	
		LogPlug::error( "Locatable constructor: " + e.toString() ) ;	
		
	}
	
}



Locatable::Locatable() :
	_father( 0 ),	
	_localReferential( 0 ),
	_globalReferential( 0 ),
	_changedEvent( 0 ),
	_isUpToDate( false )
{

}



Locatable::Locatable( Locatable & fatherLocatable, Matrix & localReferential ) :
	_father( & fatherLocatable ),
	_localReferential( & localReferential ),
	_globalReferential( 0 ),
	_changedEvent( 0 ),
	_isUpToDate( false )
{


	/*
	 * Registers this Locatable to its father, so that this one can 
	 * notice it of its future changes.
	 *
	 */
	try
	{
	
		subscribeTo( * _father ) ;
		
	}
	catch( const EventException	& e )
	{
	
		LogPlug::error( "Locatable constructor failed: " + e.toString() ) ;	
		
	}
	
}



Locatable::Locatable( Matrix & localReferential ) :
	_father( 0 ),	
	_localReferential( & localReferential ),
	_globalReferential( 0 ),
	_changedEvent( 0 ),
	_isUpToDate( false )
{

}



Locatable::~Locatable() throw()
{

	if ( _father != 0 ) 
	{
	
		try 
		{
			detachFromFather() ;	
			
			// Father not owned, therefore not deallocated.
			
		} 
		catch( const LocatableException & e )
		{
			LogPlug::error( "Locatable destructor: " + e.toString() ) ;
		}
		
	}
	
	// Out of carefulness:
	removeAllListeners() ;
	
		
	if ( _localReferential != 0 )
		delete _localReferential ;
			
	if ( _globalReferential != 0 )
		delete _globalReferential ;
			
	if ( _changedEvent != 0 )
		delete _changedEvent ;
			
}



bool Locatable::isAbsolute() const
{

	return ( _father == 0 ) ;
	
}



bool Locatable::hasLocalReferential() const
{

	return ( _localReferential != 0 ) ;
	
}



Matrix & Locatable::getLocalReferential() const
{

	if ( _localReferential == 0 )
		throw LocatableException( "Locatable::getLocalReferential(): "
			"no local referential available." ) ;
	
	return * _localReferential ;

}



void Locatable::setLocalReferential( Matrix & newGlobalReferential ) 
{

	_localReferential = & newGlobalReferential ;
	
}



bool Locatable::hasGlobalReferential() const
{

	return _globalReferential != 0 ; 
	
}



Matrix & Locatable::getGlobalReferential() 
{
	
	if ( _isUpToDate )
	{
	
#if CEYLAN_DEBUG

		if ( _globalReferential == 0 )
			emergencyShutdown( "Locatable::getGlobalReferential(): "
				"Locatable should be up-to-date, "
				"whereas no global referential available." ) ;
				
#endif // CEYLAN_DEBUG
		
		return * _globalReferential ;
		
	}	
	
	if ( isAbsolute() )
	{
	
		if ( _localReferential == 0 )
			throw LocatableException( "Locatable::getGlobalReferential(): "
				"this absolute referential has no "
				"local referential available." ) ;		

		return * _localReferential ;
	}	
	
	
	/*
	 * Here the Locatable is not up-to-date, and it must rely on father 
	 * to return up-to-date parent matrix.
	 *
	 */
	 
	if ( _globalReferential != 0 )
	{
		delete _globalReferential ;	
		_globalReferential = 0 ;
	}
	
	
	/*
	 * Problem: no '*' operator for abstract Matrix class can exist, 
	 * overcomed by calling pure virtual method 'updateFromFather' which
	 * will have to be implemented thanks to dynamic casts in all children,
	 * so that each called the appropriate '*' operator (ex: Matrix3's one). 
	 *
	 */	 
	updateFromFather( _father->getGlobalReferential() ) ;
	
	// False to true up-to-date state not triggering anything:
	setUpToDateState( true ) ;
		
	return * _globalReferential ;

}



bool Locatable::isUpToDate() const
{

	return _isUpToDate ;
	
}



void Locatable::setUpToDateState( bool newState )
{

	/*
	 * Changed state:
	 *   true  -> true : nothing
	 *   false -> false: nothing
	 *   true  -> false: nothing
	 *   false -> true : notify all children
	 *
	 */
	 
	if ( ( newState == false ) && ( _isUpToDate == true ) )
	{
		/*
		 * Propagate the news if and only if it was up-to-date and is 
		 * not any more:
		 *
		 */
		changed() ;
	}
	
	_isUpToDate = newState ;
	
}



void Locatable::beNotifiedOf( const Event & newEvent )
{

	if ( dynamic_cast<const ReferentialChangedEvent *>( & newEvent ) != 0 )
		setUpToDateState( false ) ;
	
	// else: not a ReferentialChangedEvent, event ignored.	
	
}



const string Locatable::toString( VerbosityLevels level ) const
{

	string res ;
	
	if ( isAbsolute() )
		res = "Absolute" ;
	else
		res = "Relative" ;
	
	return res + " Locatable, " + describe( level ) ;
		
}



void Locatable::changed()
{

	// Force recomputation of global referential for next time:
	if ( _globalReferential != 0 )
	{
		delete _globalReferential ;
		_globalReferential = 0 ;
	}	
	
	if ( _changedEvent == 0 )
		_changedEvent = new ReferentialChangedEvent( * this ) ;
		
	notifyAllListeners( * _changedEvent	) ;
	
}



void Locatable::detachFromFather()
{

	if ( _father == 0 ) 
		throw LocatableException( 
			"Locatable::detachFromFather: no father registered." ) ;
	
	try 
	{
		unsubscribeFrom( *_father ) ;
	} 
	catch( const EventException & e )
	{
		// Could be throw LocatableException as well:
		LogPlug::error( "Locatable::detachFromFather failed: " 
			+ e.toString() ) ;
	}
	
}



const string Locatable::describe( VerbosityLevels level ) const
{

	string res ;
	
	if ( _localReferential == 0 )
	{
		res += "no local referential available" ;
	}	
	else
	{
		res += "local referential available" ;
		if ( level == Ceylan::high )
			res += " (" + _localReferential->toString( level ) + " )" ;
	}
	
	if ( _globalReferential == 0 )
	{		
		res += ", no global referential available" ;
	}	
	else
	{
		res += ", global referential available" ;
		if ( level == Ceylan::high )
			res += " (" + _globalReferential->toString( level ) + " )" ;
	}
	
	res += ". " ;
	
	ListSize listenersCount = _listeners.size() ;
	
	if ( listenersCount > 0 )
	{
		if ( listenersCount == 1 )
			res += "There is exactly one referential "
				"that is directly relative to this Locatable" ;		
		else
			res += "There are " 
				+ Ceylan::toString( 
					static_cast<Ceylan::Uint32>( listenersCount ) ) 
				+ " referentials which are directly relative "
					"to this Locatable" ;
	}		
	else
		res += " No Locatable depends on this one" ;
				
	return res ;		

}

