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


#include "CeylanObject.h"

#include "CeylanProcess.h"             // for getHostingPID
#include "CeylanTextIdentifier.h"
#include "CeylanOperators.h"
#include "CeylanLogLight.h"            // for CEYLAN_LOG
#include "CeylanLogPlug.h"             // for LogPlug
#include "CeylanObjectIdentifier.h"    // for constructor
#include "CeylanIdentifier.h"          // for IdentifierException


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <typeinfo>                    // for typeid
#include <cctype>                      // for isdigit


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;



Object::Object( bool trackInstance, bool dropIdentifierOnExit ) :
	IdentifierOwner(),
	Loggable( "Unknown object" ),
	_trackInstance( trackInstance )
{


	CEYLAN_LOG( "Ceylan::Object entering constructor" ) ;

	if ( _trackInstance )
	{

		CEYLAN_LOG( "Object sending its first message "
			"just after allocation." ) ;

		send( "Being allocated now." ) ;

		/*
		 * Used to force rebuilding of its identifier so that it is not mangled:
		 *
		 */
		if ( dropIdentifierOnExit )
			dropIdentifier() ;

	}

}



Object::~Object() throw()
{

	if ( _trackInstance )
	{

		CEYLAN_LOG(
			"Object sending its last message just before deallocation." ) ;

		send( "Being deallocated now." ) ;

	}

}

#include "CeylanUtils.h"
#include "CeylanStringUtils.h"


const std::string Object::getClassName() const
{

	string className = typeid( * this ).name() ;

	return Ceylan::demangleSymbol( className ) ;

}



bool Object::isOfSameType( const Object & other ) const
{

	return ( getClassName() == other.getClassName() ) ;

}



void Object::logState( Ceylan::VerbosityLevels level )
{

	send( toString( level ) ) ;

}



void Object::send( const string & message, LevelOfDetail levelOfDetail )
{

	CEYLAN_LOG( "Object::send: will send message " + message ) ;

	if ( ! hasIdentifier() )
	{

		try
		{
			forgeIdentifier() ;
		}
		catch( const Identifier::IdentifierException & e )
		{
			throw LogException( "Object::send failed: " + e.toString() ) ;

		}

		CEYLAN_LOG( "Object::send: channel name set to "
			+ getIdentifier().toString() ) ;

		setChannelName( getIdentifier().toString() ) ;

	}

	CEYLAN_LOG( "Object::send: effective sending of message " + message ) ;

	Loggable::send( message, levelOfDetail ) ;

}



const string Object::toString( Ceylan::VerbosityLevels level ) const
{

	string result ;

	if ( _trackInstance )
		result = "This Ceylan object instance's life cycle is monitored. " ;
	else
		result = "No monitoring for this Ceylan object "
			"instance's life cycle. " ;

	result += Loggable::toString( level ) + ". "
		+ IdentifierOwner::toString( level ) ;

	return result ;

}



void Object::forgeIdentifier()
{

	CEYLAN_LOG( "Object::forgeIdentifier: new identifier required." ) ;

	if ( hasIdentifier() )
		dropIdentifier() ;

	CEYLAN_LOG( "Object::forgeIdentifier: creating new object identifier." ) ;

	ObjectIdentifier * newID ;

	/*
	 * This was the place where using this instead of *this caused the object
	 * constructor to be mistakenly called (this pointer being converted to
	 * bool, and Object constructor had not the explicit keyword).
	 *
	 */
	try
	{

		newID = new ObjectIdentifier( * this ) ;

	}
	catch( const Identifier::IdentifierException & e )
	{
		throw Log::LogException( "Object::forgeIdentifier: " + e.toString() ) ;
	}


	try
	{

		setIdentifier( *newID ) ;

	}
	catch( const IdentifierNotAvailableException & e )
	{
		throw Log::LogException( "Object::forgeIdentifier: " + e.toString() ) ;
	}


	CEYLAN_LOG( "Object::forgeIdentifier: new ID is "
		+ getIdentifier().toString() ) ;

}



void Object::dropIdentifier()
{

	deleteIdentifier() ;

}
