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


// FIXME : add ping to avoid failure if network not here

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


        LogPlug::info( "Testing Ceylan's network implementation." ) ;


		// FIXME avoids waiting if not network :
		return Ceylan::ExitSuccess ;
		
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

		
		string numericalAddress = "82.225.152.215" ;
				
		IPAddressvFour tempv4( numericalAddress ) ;
		LogPlug::info( "FQDN for " + tempv4.toString() + " is : '"
			+ getFQDNFromIP( tempv4 ) + "'." ) ;

		LogPlug::info( "FQDN for " + numericalAddress + " is : '"
			+ getFQDNFromIPv4( numericalAddress ) + "'." ) ;

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
			


		LogPlug::info( "The local host name is : '" + getLocalHostName()
			+ "'." ) ;
		
		LogPlug::info( "The local domain name is : '" 
			+ getLocalHostDomainName() + "'." ) ;
		
		LogPlug::info( "The most precise host name (aiming FQDN) is : '" 
			+ getMostPreciseLocalHostName() + "'." ) ;	

		LogPlug::info( "Trying to set local host name to '" + newHostname 
			+ "' (this fails when run by a non-priviledged user)." ) ;
		
		try 
		{	
		
			setLocalHostName( newHostname ) ;
			
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
