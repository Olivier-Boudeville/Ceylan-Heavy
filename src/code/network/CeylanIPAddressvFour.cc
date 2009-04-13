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


#include "CeylanIPAddressvFour.h"

#include "CeylanOperators.h"
#include "CeylanRegularExpression.h"    // for RegExp


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"               // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


#include <sstream>
using std::stringstream ;


using std::string ;


using namespace Ceylan::Network ;


IPAddressvFour::IPAddressvFour( 
	IPvFourComponent first,	
	IPvFourComponent second, 
	IPvFourComponent third, 
	IPvFourComponent fourth	) throw( IPAddressException ) :
		IPAddress(),
		_first ( first  ),
		_second( second ),
		_third ( third  ),
		_fourth( fourth )
{
	validate() ;
}


IPAddressvFour::IPAddressvFour(	const string & IPString ) 
	throw( IPAddressException )
{

	stringstream ss ;

	ss << IPString ;

	// To avoid reading '112' as three chars instead of one Ceylan::Uint8 :
	Ceylan::Uint16 element ;
	
	// Get rid with 'ch' of the dots between elements :
	char ch ;

	ss >> element ;
	
	/*
	 * Error code if conversion failed (overflow) must be tested :	
	 * (otherwise 'element' might be overflowed and finish into [0,255])
	 *
	 */
	if ( ss.fail() )
		throw IPAddressException( 
			"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString 
			+ "' : conversion error" ) ;
		
	if ( element > 255 )
		throw IPAddressException( 
			"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString 
			+ "' : read too high element : " 
			+ Ceylan::toString( element ) + "." ) ;
	else
		_first = static_cast<IPvFourComponent>( element ) ;		
	
	ss >> ch ;

	ss >> element ;

	if ( ss.fail() )
		throw IPAddressException( 
			"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString 
			+ "' : conversion error" ) ;

	if ( element > 255 )
		throw IPAddressException( 
			"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString 
			+ "' : read too high element : " 
			+ Ceylan::toString( element ) + "." ) ;
	else
		_second = static_cast<IPvFourComponent>( element ) ;		
	ss >> ch ;

	ss >> element ;

	if ( ss.fail() )
		throw IPAddressException( 
			"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString 
			+ "' : conversion error" ) ;

	if ( element > 255 )
		throw IPAddressException( 
			"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString 
			+ "' : read too high element : " 
			+ Ceylan::toString( element ) + "." ) ;
	else
		_third = static_cast<IPvFourComponent>( element ) ;		
	ss >> ch ;

	ss >> element ;

	if ( ss.fail() )
		throw IPAddressException( 
			"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString 
			+ "' : conversion error" ) ;

	if ( element > 255 )
		throw IPAddressException( 
			"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString 
			+ "' : read too high element : " 
			+ Ceylan::toString( element ) + "." ) ;
	else
		_fourth = static_cast<IPvFourComponent>( element ) ;		


	if ( ! ss.eof() )
	{
	
		string remainder ; 
		
		ss >> remainder ;
		
		throw IPAddressException( 
			"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString 
			+ "' (too many elements, still having '"
			+ remainder + "')." ) ;
	}
	
	
	try
	{
	
		validate() ;
		
	}
	catch( const IPAddressException &  )
	{
	
		throw IPAddressException( 
		"IPAddressvFour : unable to construct an IPv4 instance "
			"from invalid IP string '" + IPString + "'." ) ;
			
	}

}


IPAddressvFour::~IPAddressvFour()  throw()
{

}

NetworkAddressType IPAddressvFour::getType() const throw()
{
	return Network::IPv4 ;
}

const string IPAddressvFour::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{
	return Ceylan::toNumericalString( _first  ) + '.'
		 + Ceylan::toNumericalString( _second ) + '.'
		 + Ceylan::toNumericalString( _third  ) + '.'
		 + Ceylan::toNumericalString( _fourth ) ;
}


bool IPAddressvFour::IsValidIPv4( const string & ipString ) throw()
{


#if CEYLAN_USES_REGEX

	const string ipvFourPattern	= "^([0-2]{0,1}[0-9]{0,1}[0-9]{1,1}[.]{1,1})"
		"{3}([0-2]{0,1}[0-9]{0,1}[0-9]{1,1})$" ;

	Ceylan::RegExp target( ipString ) ;

	if ( target.matches( ipvFourPattern ) )
	{

#endif // CEYLAN_USES_REGEX
	
	// Let's validate the string by creating an IPAddressvFour instance from it :
	try
	{
		IPAddressvFour testAddress( ipString ) ;
		return true ;
	}
	catch( const IPAddressException & e )
	{
		return false ;
	}


#if CEYLAN_USES_REGEX

	}

	return false ;
	
#endif // CEYLAN_USES_REGEX

}


void IPAddressvFour::validate() const throw( IPAddressException )
{

	/*
	 * Always within range thanks to the IPvFourComponent data-type :
	  
	if ( ! ( _first <= 255 && _second <= 255 
			&& _third <= 255 && _fourth <= 255 ) )
		throw IPAddressException( "IPAddressvFour::validate : "
			"invalid IPv4 address : " + toString() ) ;
	
	 * 
	 * Hence nothing special to check.
	 *
	 */
	 
}
