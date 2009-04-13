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


#include "CeylanVersion.h"

#include "CeylanOperators.h"      // for toNumericalString, etc.
#include "CeylanStringUtils.h"    // for split

#include <list>

// for debug purpose only: #include <iostream>


using std::string ;

using namespace Ceylan ;



VersionException::VersionException( const std::string & message ) throw():
	Ceylan::Exception( message )
{

}


VersionException::~VersionException() throw()
{

}




Version::Version( VersionNumber major, VersionNumber minor, 
		VersionNumber release ) throw():
	_major( major ),
	_minor( minor ),
	_release( release )
{

}


Version::Version( const std::string & versionText ) throw( VersionException )
{

	std::list<string> numbers = Ceylan::split( versionText, '.' ) ;
	
	/*
	 *
	for ( std::list<string>::const_iterator it = numbers.begin(); 
			it != numbers.end(); it++ )
		std::cerr << "Version constructor: " << *it << std::endl ;
	 *
	 */
	 	 
	if ( numbers.size() != 3 )
		throw Ceylan::Exception( "Version constructor: input string '"
			+ versionText 
			+ "' cannot be split into three parts thanks to dots, "
			"we have " + Ceylan::toString( 
				static_cast<Ceylan::Uint32>( numbers.size() ) ) 
			+ " elements: " 
			+ Ceylan::formatStringList( numbers, 
				/* surroundByTicks */ true ) ) ;
	
	try
	{
	
		_major = static_cast<VersionNumber>( 
			Ceylan::stringToUnsignedLong( numbers.front() ) ) ;
		numbers.pop_front() ;
	
		_minor = static_cast<VersionNumber>( 
			Ceylan::stringToUnsignedLong( numbers.front() ) ) ;
		numbers.pop_front() ;
	
		_release = static_cast<VersionNumber>( 
			Ceylan::stringToUnsignedLong( numbers.front() ) ) ;
		numbers.pop_front() ;
			
	}
	catch( const Ceylan::Exception & e )
	{
		throw VersionException( "Version constructor: input string <"
			+ versionText + "> could not be parsed into numbers: " 
			+ e.toString() ) ;
	}
	
	if ( ! numbers.empty() )
		throw VersionException( "Version constructor: input string <"
			+ versionText + "> has more than three elements" ) ;
		
			
}

 
 
Version::~Version() throw()
{

}



Version::VersionNumber Version::getMajorNumber() const throw()
{
	return _major ;
}


void Version::setMajorNumber( VersionNumber newNumber ) throw()
{
	_major = newNumber ;
}



Version::VersionNumber Version::getMinorNumber() const throw()
{
	return _minor ;
}


void Version::setMinorNumber( VersionNumber newNumber ) throw()
{
	_minor = newNumber ;
}



Version::VersionNumber Version::getReleaseNumber() const throw()
{
	return _release ;
}


void Version::setReleaseNumber( VersionNumber newNumber ) throw()
{
	_release = newNumber ;
}


bool Version::isCompatibleWith( const Version & expectedVersion ) const 
	throw( VersionException )
{
 
	return ( *this == expectedVersion ) ;
	 
}


bool Version::canBeComparedWith( const Version & version ) const throw()
{	
	return ( version.isUsualVersionSchemeCompliant() ) ;
}


const string Version::toString( VerbosityLevels level ) const throw()
{

	string rawVersion = Ceylan::toNumericalString( _major ) + "." 
			+ Ceylan::toNumericalString( _minor ) + "." 
			+ Ceylan::toNumericalString( _release ) ;
			
	if ( level == Ceylan::low )
    	return rawVersion ;
		
	if ( level == Ceylan::medium )
    	return "Version " + rawVersion ;
				
	return "Version: major number = " 
		+ Ceylan::toNumericalString( _major ) + ", minor number = "
		+ Ceylan::toNumericalString( _minor ) + ", release number = "
		+ Ceylan::toNumericalString( _release ) ;
		 				
}


bool Version::isUsualVersionSchemeCompliant() const throw()
{
	return true ;
}



bool operator < ( const Ceylan::Version & vFirst, 
	const Ceylan::Version & vSecond ) /* throw( VersionException ) */
{

	if ( ! vFirst.canBeComparedWith( vSecond ) )
		throw VersionException( "Version operator '<': "
			+ vFirst.toString( Ceylan::high ) + " and "
			+ vSecond.toString( Ceylan::high ) + " cannot be compared." ) ;
		
	if ( vFirst.getMajorNumber() < vSecond.getMajorNumber() )
		return true ;
	
	if ( vFirst.getMajorNumber() == vSecond.getMajorNumber() )
	{
		if ( vFirst.getMinorNumber() < vSecond.getMinorNumber() )
			return true ;
			
		if ( vFirst.getMinorNumber() == vSecond.getMinorNumber() )
		{
			if ( vFirst.getReleaseNumber() < vSecond.getReleaseNumber() )
				return true ;		
		}
		
	}
	
	return false ;
		
}


bool operator == ( const Ceylan::Version & vFirst, 
	const Ceylan::Version & vSecond ) /* throw( VersionException ) */
{


	if ( ! vFirst.canBeComparedWith( vSecond ) )
		throw VersionException( "Version operator '==': "
			+ vFirst.toString( Ceylan::high ) + " and "
			+ vSecond.toString( Ceylan::high ) + " cannot be compared." ) ;

	if ( vFirst.getMajorNumber() != vSecond.getMajorNumber() )
		return false ;
	
	if ( vFirst.getMinorNumber() != vSecond.getMinorNumber() )
		return false ;
			
	if ( vFirst.getReleaseNumber() != vSecond.getReleaseNumber() )
		return false ;
		
	return true ;
		
}

