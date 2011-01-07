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
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


#include <string>
using std::string ;

#include <exception>




/**
 * Test of Ceylan IPv4 addresses handling.
 *
 * @see IPAddress, IPAddressvFour.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder myLog( argc, argv ) ;


    try
    {

        LogPlug::info( "Testing IPv4 implementation." ) ;

		LogPlug::info( "Creating ip1, a new IPv4 address object, "
			"out of numerical values." ) ;
		
		IPAddressvFour ip1( 192,168,0,4 ) ; 
		
		LogPlug::info( "ip1 is : " + ip1.toString() ) ;
		
		const string validIPString    = "192.168.0.4" ;
		const string invalidIPString1 = "192.168.0.4.5" ;
		const string invalidIPString2 = "19216804" ;
		const string invalidIPString3 = "256.168.0.4" ;
		
		LogPlug::info( "Creating ip2, a new IPv4 address object, "
			"out of string '" + validIPString + "'." ) ;
		
		IPAddressvFour ip2( validIPString ) ; 
		
		LogPlug::info( "ip2 is : " + ip2.toString() ) ;
		
		
		bool exceptionRaised = false ;
		LogPlug::info( "Trying to create ip3, "
			"an invalid IPv4 object from string '" + invalidIPString1
			+ "'." ) ;
			
		try 
		{
			
			IPAddressvFour ip3( invalidIPString1 ) ;
			
		} 
		catch( const Ceylan::Exception & e ) 
		{
		
			LogPlug::info( "Ceylan exception raised : " + e.toString() ) ;
			exceptionRaised = true ;
			
		}	
			
		if ( ! exceptionRaised )
			throw Ceylan::TestException( 
				"IPAddressvFour constructor from string should have "
				"raised an exception when used with IP string " 
				+ invalidIPString1 ) ;
				
		exceptionRaised = false ;
		

		LogPlug::info( "Trying to create ip4, "
			"an invalid IPv4 object from string '" + invalidIPString2
			+ "'." ) ;
			
		try 
		{
			
			IPAddressvFour ip4( invalidIPString2 ) ;
			
		} 
		catch( const Ceylan::Exception & e ) 
		{
		
			LogPlug::info( "Ceylan exception raised : " + e.toString() ) ;
			exceptionRaised = true ;
			
		}	
			
		if ( ! exceptionRaised )
			throw Ceylan::TestException( 
				"IPAddressvFour constructor from string should have "
				"raised an exception when used with IP string " 
				+ invalidIPString2 ) ;
				
		exceptionRaised = false ;		
		
				
		LogPlug::info( "Trying to create ip5, "
			"an invalid IPv4 object from string " + invalidIPString1 ) ;
			
		try 
		{
			
			IPAddressvFour ip5( invalidIPString3 ) ;
			
		} 
		catch( const Ceylan::Exception & e ) 
		{
		
			LogPlug::info( "Ceylan exception raised : " + e.toString() ) ;
			exceptionRaised = true ;
			
		}
				
		if ( ! exceptionRaised )
			throw Ceylan::TestException( 
				"IPAddressvFour constructor from string should have "
				"raised an exception when called with IP string " 
				+ invalidIPString3 ) ;
						
				
        LogPlug::info( "End of IPv4 test." ) ;


    }

    catch ( const Ceylan::Exception & e )
    {
        LogPlug::error( "Ceylan exception caught : "
        	 + e.toString( Ceylan::high ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        LogPlug::error( "Standard exception caught : " 
			 + std::string( e.what() ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        LogPlug::error( "Unknown exception caught" ) ;
       	return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}

