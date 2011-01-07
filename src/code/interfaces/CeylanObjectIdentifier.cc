/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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


#include "CeylanObjectIdentifier.h"

#include "CeylanOperators.h"
#include "CeylanProcess.h"
#include "CeylanNetwork.h"
#include "CeylanRegularExpression.h"
#include "CeylanStringUtils.h"        // for countChars
#include "CeylanObject.h"
#include "CeylanLogLight.h"           // for CEYLAN_LOG (most basic logging)
#include "CeylanLogPlug.h"            // for LogPlug::warning
#include "CeylanNetwork.h"            // for getMostPreciseLocalHostName


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"             // for CEYLAN_DEBUG_EVENTS
#endif // CEYLAN_USES_CONFIG_H


#include <list>

#include <sstream>                    // for << and >> operators
using std::stringstream ;

#include <list>
using std::list ;



using std::string ;

using namespace Ceylan::Log ;
using namespace Ceylan::System ;
using namespace Ceylan::Network ;


const char   ObjectIdentifier::Separator = '/' ;
const string ObjectIdentifier::PIDTag    = "PID-" ;



/**
 * The pattern must match for instance
 * "sonata/PID-1444/N6Ceylan6ObjectE/0x8050fd0".
 *
 * @fixme Pattern needs to be chosen.
 *
 */
const string ObjectIdentifier::Pattern =  "^([0-2]{0,1}[0-9]{0,1}[0-9]{1,1}"
	"[.]{1,1}){3}([0-2]{0,1}[0-9]{0,1}[0-9]{1,1})$" ;



Ceylan::Log::ObjectIdentifier::ObjectIdentifier( const Object & object ) :
	TextIdentifier(),
	_hostname(),
	_pid(),
	_address( 0 ) 
{

	CEYLAN_LOG( "Log::ObjectIdentifier constructor: "
		"creating a new identifier for " 
		+ object.getClassName() + " instance." ) ;
	
	try 
	{
	
		_hostname = Network::getMostPreciseLocalHostName() ;

	} 
	catch( const Network::NetworkException & e )
	{
		throw IdentifierException( "ObjectIdentifier constructor: "
			"unable to get local host name: " + e.toString() ) ;		
	}
	
	try 
	{
	
		_pid = System::Process::GetHostingPID() ;	
			
	} 
	catch( const ProcessException & e )
	{
	
		LogPlug::warning( "ObjectIdentifier constructor: "
			"unable to get hosting PID: " + e.toString()
			+ ", defaulting to null PID" ) ;	
		_pid = 0 ;
					
	}
			
	_className = object.getClassName() ;	
	
	_address = static_cast<const void *>( & object ) ;
	
	CEYLAN_LOG( "Newly created identifier is " + toString() ) ;
	
}



ObjectIdentifier::ObjectIdentifier(	const std::string & hostname, Pid pid,
		const std::string & className, const void * address) :
	_hostname( hostname ),
	_pid( pid ),
	_className( className ),
	_address( address )
{

}



ObjectIdentifier::~ObjectIdentifier() throw()
{

	// Nothing to delete, especially not _address!

}



bool ObjectIdentifier::differentButMatches( const ObjectIdentifier & otherID )
	const
{

	/*
	 * If addresses or other required fields do not match, they are
	 * trivially different:
	 *
	 */
	
	if ( _address != otherID._address )
		return false ;
		
	if ( _pid != otherID._pid )
		return false ;
		
	if ( _hostname != otherID._hostname )
		return false ;


	// Only the class name could differ at this point:
	
	/*
	 * Equality of classnames: identifiers match but are strictly the
	 * same, so return false:
	 *
	 */
	if ( _className == otherID._className )
		return false ;

	// Strictly the same apart the class name, this is the only case where true:
	return true ;
	
}



const string ObjectIdentifier::toString( Ceylan::VerbosityLevels level ) const
{

	return _hostname + Separator + PIDTag + _pid + Separator 		 
		+ _className + Separator + _address ;
	
}



ObjectIdentifier & ObjectIdentifier::generateFromChannelName( 
	const string & channelName )
{

	/*
	 * Let's try to extract the meaningful fields of this compacted
	 * channel name.
	 *
	 */


	/*
	 * Disabled at the moment, pattern matching is to be used on 
	 * subelements only:
	 *
	
	Ceylan::RegExp target( channelName ) ;
	
		 
	if ( ! target.matches( Pattern ) )
		throw IdentifierException( 
			"ObjectIdentifier::generateFromChannelName: unable "
			"to generate an object identifier from channel name <"
			+ channelName + ">: it does match the relevant pattern." ) ;
	*/

	/*
	 * Channel name might be for instance:
	 * "sonata/PID-1444/N6Ceylan6ObjectE/0x8050fd0".
	 *
	 */
	
	// First: there must be exactly three / (Separators):
	
	if ( Ceylan::countChars( channelName, Separator ) != 3 )
		throw IdentifierException( 
			"ObjectIdentifier::generateFromChannelName: there are "
			"not exactly three separators in channel name " 
			+ channelName + "." ) ;
		
	list<string> subelements = Ceylan::split( channelName, Separator ) ;
	
	if ( subelements.size() != 4 )
		throw IdentifierException( 
			"ObjectIdentifier::generateFromChannelName: there are "
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( subelements.size() ) ) 
			+ " elements after having split channel name " 
			+ channelName + " instead of four." ) ;
	
	const string hostnameString = subelements.front() ;
	subelements.pop_front() ;
	
	if ( ! Ceylan::Network::isAValidHostName( hostnameString ) )
		throw IdentifierException( 
			"ObjectIdentifier::generateFromChannelName: '"
			+ hostnameString + "' is not a valid host name." ) ;
			
	string pidString = subelements.front() ;
	subelements.pop_front() ;
	
	/*
	 * Disabled since its core dumps in re_exec ()
	 * (poorly written re library!)
	 *
	 *
	RegExp pidRegExp( pidString ) ;
	
	if ( ! pidRegExp.matches( "^(PID-[0-9]{1,n})$" ) )
		throw IdentifierException( 
			"ObjectIdentifier::generateFromChannelName: "
			+ pidString + " is not a valid PID string." ) ;
	*/
			
	Pid pid ;
	void * address ;
	string classString ;
	
	try 
	{
	
		pid = static_cast<Pid>( 
			Ceylan::stringToUnsignedLong( pidString.substr( 4 ) ) ) ;
	
		classString = subelements.front() ;
		subelements.pop_front() ;

		address = Ceylan::stringToAddress( subelements.front() ) ;
		
	} 
	catch ( const Ceylan::Exception & e )
	{
		throw IdentifierException( 
			"ObjectIdentifier::generateFromChannelName: unable "
			"to deserialize channel name " + channelName 
			+ ": " + e.toString() ) ;
	}
				 
	return * new ObjectIdentifier( hostnameString, pid, 
		classString, address ) ;
	
}

