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


#include "CeylanIdentifierOwner.h"

#include "CeylanIdentifier.h"        // for CeylanIdentifier
#include "CeylanUtils.h"             // for emergencyShutdown
#include "CeylanLogLight.h"          // for CEYLAN_LOG
//#include "CeylanLogPlug.h"             // for LogPlug


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H



using std::string ;

using namespace Ceylan ;



IdentifierNotAvailableException::IdentifierNotAvailableException(
		const string & message ) :
	Ceylan::Exception( message )
{

}



IdentifierNotAvailableException::~IdentifierNotAvailableException() throw()
{

}



IdentifierOwner::IdentifierOwner() :
	_id( 0 )
{

	CEYLAN_LOG( "IdentifierOwner constructor" ) ;

}



IdentifierOwner::~IdentifierOwner() throw()
{

	if ( hasIdentifier() )
		deleteIdentifier() ;

}



Identifier & IdentifierOwner::getIdentifier() const
{

	if ( _id != 0 )
	{
		return * _id ;
	}
	else
	{
		throw IdentifierNotAvailableException(
			"No available identifier to return for "
			"IdentifierOwner::getIdentifier" ) ;
	}

}



void IdentifierOwner::setIdentifier( Identifier & id )
{

	if ( _id != 0 )
	{
		throw IdentifierNotAvailableException(
			"IdentifierOwner::setIdentifier unable to assign "
			"new identifier cause a previous one is still available." ) ;
	}
	else
	{
		_id = & id ;
	}

}



bool IdentifierOwner::hasIdentifier() const
{

	return ( _id != 0 ) ;

}



void IdentifierOwner::deleteIdentifier()
{

#if CEYLAN_DEBUG

	if ( _id != 0 )
	{
		delete _id ;
		_id = 0 ;
	}
	else
	{
		Ceylan::emergencyShutdown(
			"IdentifierOwner::deleteIdentifier: trying to "
			"delete a non-existing identifier." ) ;
	}

#else // CEYLAN_DEBUG

	delete _id ;
	_id = 0 ;

#endif // CEYLAN_DEBUG

}



const string IdentifierOwner::toString( VerbosityLevels level ) const
{

	if ( _id != 0 )
	{
		return "This identifier owner has a registered identifier: "
			+ _id->toString( level ) ;
	}
	else
	{
		return "This identifier owner has no registered identifier" ;
	}

}
