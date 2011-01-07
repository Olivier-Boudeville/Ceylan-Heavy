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


#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




const string newHostname      = "ceylan" ;
const string newDomainname    = "osdl" ;

const string validHostname    = "ceylan.sourceforge.net" ;
const string invalidHostname1 = "http://ceylan.sourceforge.net" ;
const string invalidHostname2 = "192.110.0.4" ;
const string invalidHostname3 = "ceylan/sourceforge/net" ;



/**
 * Test of Ceylan Network utilities.
 *
 * @see Ceylan::Network.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


	try
	{
		
		bool onlineMode = false ;

		std::string executableName ;
		std::list<std::string> options ;
		
		Ceylan::parseCommandLineOptions( executableName, options, argc, argv ) ;
				
		std::string token ;
		bool tokenEaten ;
		
		while ( ! options.empty() )
		{
		
			token = options.front() ;
			options.pop_front() ;

			tokenEaten = false ;
		
			if ( token == "--online" )
			{
				LogPlug::info( "Online mode selected, "
					"in-depth network testing will be performed" ) ;
				onlineMode = true ;
				tokenEaten = true ;
			} else
			if ( LogHolder::IsAKnownPlugOption( token ) )
			{
				// Ignores log-related (argument-less) options.
				tokenEaten = true ;
			}
			
			if ( ! tokenEaten )
			{
				LogPlug::error( 
					"Unexpected command line argument : " + token ) ;
			}
		
		}

        LogPlug::info( "Testing Ceylan's network implementation." ) ;


		if ( ! onlineMode )
		{
			LogPlug::warning( "Not in online mode, test stops here." ) ;
			return Ceylan::ExitSuccess ;
		}

		/*
		 * We have to create HostDNSEntry instances in a try/catch pair
		 * since this test can be run on a computer with no availabe DNS
		 * (for example simply if it is not connected to the Internet).
		 *
		 */
		 
		 
		string ceylanFirstHost  = "ceylan.esperide.com" ;
		
		try
		{
		
			HostDNSEntry ceylanFirst( ceylanFirstHost ) ;

			LogPlug::info( "DNS entry for Ceylan first host '" 
				+ ceylanFirstHost + "' is : " + ceylanFirst.toString() ) ;
				
		} 
		catch ( const NetworkException & e )
		{
			LogPlug::warning( "Resolving the DNS for '" 
				+ ceylanFirstHost + "' failed : " + e.toString() ) ;
		}
			
			
			
		string ceylanSecondHost = "ceylan.sourceforge.net" ;
		
		try
		{
		
			HostDNSEntry ceylanSecond( ceylanSecondHost ) ;

			LogPlug::info( "DNS entry for Ceylan second host '" 
				+ ceylanSecondHost + "' is : " + ceylanSecond.toString() ) ;
		
		} 
		catch ( const NetworkException & e )
		{
			LogPlug::warning( "Resolving the DNS for '" 
				+ ceylanSecondHost + "' failed : " + e.toString() ) ;
		}


		string googleHost = "google.fr" ;
		
		try
		{
				
			HostDNSEntry google( googleHost ) ;

			LogPlug::info( "DNS entry for google host '" 
				+ googleHost + "' is : " + google.toString() ) ;
		
		} 
		catch ( const NetworkException & e )
		{
			LogPlug::warning( "Resolving the DNS for '" 
				+ googleHost + "' failed : " + e.toString() ) ;
		}

		
		
		string wikipediaHost = "wikipedia.com" ;
				
		try
		{
		
			HostDNSEntry wikipedia( wikipediaHost ) ;

			LogPlug::info( "DNS entry for wikipedia host '" 
				+ wikipediaHost + "' is : " + wikipedia.toString() ) ;
		} 
		catch ( const NetworkException & e )
		{
			LogPlug::warning( "Resolving the DNS for '" 
				+ wikipediaHost + "' failed : " + e.toString() ) ;
		}

		LogPlug::info( "Resolving now names from (numerical) IP addresses." ) ;

		string numericalAddress = "82.225.152.215" ;
		
		// Reverse look-up may fail :

		try
		{

			IPAddressvFour tempv4( numericalAddress ) ;
			LogPlug::info( "FQDN for IPAddressvFour instance " 
				+ tempv4.toString() + " is : '"
				+ getFQDNFromIP( tempv4 ) + "'." ) ;

		}
		catch( const NetworkException )
		{
			LogPlug::error( "Unable to perform a reverse look-up for "
				+ numericalAddress ) ;
		}


		try
		{
			LogPlug::info( "FQDN for numerical address " + numericalAddress 
				+ " is : '"	+ getFQDNFromIPv4( numericalAddress ) + "'." ) ;
		}
		catch( const NetworkException )
		{
			LogPlug::error( "Unable to perform a reverse look-up for "
				+ numericalAddress ) ;
		}

		LogPlug::info( "FQDN for " + wikipediaHost + " is : '"
			+ getFQDNFromHostname( wikipediaHost ) + "'." ) ;
			
		/**
		 * Depending on the regular expression feature being available or not,
		 * warnings may be issued if checkings are relaxed.
		 *
		 */
		if ( isAValidHostName( validHostname ) )
			LogPlug::info( "'" + validHostname 
				+ "' is a valid host name indeed." ) ;
		else
			LogPlug::warning( "'" + validHostname 
				+ "' is actually a valid host name, "
				"but not recognized as such." ) ;
				

		if ( ! isAValidHostName( invalidHostname1 ) )
			LogPlug::info( "'" + invalidHostname1 
				+ "' is an invalid host name indeed." ) ;
		else
			LogPlug::warning( "'" + invalidHostname1
				+ "' is actually an invalid host name, "
				"but not recognized as such." ) ;
				
				
		if ( ! isAValidHostName( invalidHostname2 ) )
			LogPlug::info( "'" + invalidHostname2 
				+ "' is an invalid host name indeed." ) ;
		else
			LogPlug::warning( "'" + invalidHostname2
				+ "' is actually an invalid host name, "
				"but not recognized as such." ) ;
				
				
		if ( ! isAValidHostName( invalidHostname3 ) )
			LogPlug::info( "'" + invalidHostname3 
				+ "' is an invalid host name indeed." ) ;
		else
			LogPlug::warning( "'" + invalidHostname3
				+ "' is actually an invalid host name, "
				"but not recognized as such." ) ;
			


		string savedLocalHostName = getLocalHostName() ;
		
		LogPlug::info( "The local host name is : '" 
			+ savedLocalHostName + "'." ) ;
		
		string savedLocalHostDomainName ;
		
		// Not available on all platforms :
		try
		{
		
			savedLocalHostDomainName = getLocalHostDomainName() ;
			LogPlug::info( "The local domain name is : '" 
				+ savedLocalHostDomainName + "'." ) ;
		}
		catch( const NetworkException & e )
		{
			LogPlug::error( "Unable to retrieve the domaine name : "
				+ e.toString() ) ;
		}

		LogPlug::info( "The most precise host name (aiming FQDN) is : '" 
			+ getMostPreciseLocalHostName() + "'." ) ;	

		LogPlug::info( "Trying to set local host name to '" + newHostname 
			+ "' (this fails when run by a non-priviledged user)." ) ;
		
		try 
		{	
		
			setLocalHostName( newHostname ) ;
		
			LogPlug::info( "The local host name is now : '" 
				+ getLocalHostName() + "'." ) ;
				
			setLocalHostName( savedLocalHostName ) ;	

			LogPlug::info( "The local host name once restored is : '" 
				+ getLocalHostName() + "'." ) ;
			
			
		} 
		catch( const NetworkException & e ) 
		{
			LogPlug::info( "Network exception raised : " + e.toString() ) ;
		}	
		
		
		LogPlug::info( "Trying to set local domain name to '" + newDomainname 
			+ "' (this fails when run by a non-priviledged user)." ) ;
			
		try 
		{	
		
			setLocalHostDomainName( newDomainname ) ;

			LogPlug::info( "The local host domain name is now : '" 
				+ getLocalHostDomainName() + "'." ) ;
				
			setLocalHostName( savedLocalHostDomainName ) ;	

			LogPlug::info( "The local host domain name once restored is : '" 
				+ getLocalHostDomainName() + "'." ) ;
			
		} 
		catch( const NetworkException & e ) 
		{
			LogPlug::info( "Network exception raised : " + e.toString() ) ;
		}	
		
		
		LogPlug::info( "Getting protocol name of " + invalidHostname1 + " : '" 
			+ Ceylan::URI::getProtocolName( invalidHostname1 ) + "'." ) ;
			
		LogPlug::info( "Getting embedded URI of " + invalidHostname1 + " : '" 
			+ URI::getEmbeddedURI( invalidHostname1 ) + "'." ) ;
		
			
		LogPlug::info( "Getting protocol name of " + validHostname + " : '" 
			+ URI::getProtocolName( validHostname ) + "'." ) ;
			
		LogPlug::info( "Getting embedded URI of " + validHostname + " : '" 
			+ URI::getEmbeddedURI( validHostname ) + "'." ) ;
		
		
        LogPlug::info( "End of network test." ) ;


	}
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught : " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
