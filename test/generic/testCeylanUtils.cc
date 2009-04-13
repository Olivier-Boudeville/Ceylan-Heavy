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


#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <string>
using std::string ;

#include <list>
using std::list ;

#include <exception>



/**
 * Test of Ceylan utilities.
 *
 * @see CeylanUtils.h.
 *
 *
 */
int main( int argc, char * argv[] )
{
	
	

	/*
	 * Verifies that the versions of the Ceylan header files used to 
	 * compile this test and the one of the currently linked Ceylan
	 * library are compatible :
	 *
	 */
	CHECK_CEYLAN_VERSIONS() ;


	LogHolder logger( argc, argv ) ;


    try
    {


        LogPlug::info( "Testing Ceylan utilities." ) ;

        LogPlug::info( "The version of the Ceylan library "
			"currently linked is " 
        	+ Ceylan::GetVersion().toString() + "." ) ; 

        LogPlug::info( "This test has been compiled against the "
        	+ Ceylan::LibtoolVersion( 
				Ceylan::actualCeylanHeaderLibtoolVersion ).toString() 
        	+ " version of the Ceylan header files." ) ;
		
		LogPlug::info( "Let's now check for particular compatibilities "
			"by specifying various versions and testing whether they are "
			"deemed compatible with the version currently linked with."
		) ; 
		
		
		/*
		 * As the version of the linked version will change, only messages 
		 * are output, no TestException is raised.
		 *
		 */ 
		
		Ceylan::LibtoolVersion firstTargetVersion( "0.4.0" ) ;
		
		if ( /* library version */                                           \
                Ceylan::GetVersion().isCompatibleWith( firstTargetVersion ) )
        	LogPlug::info( "The version of the Ceylan library "
				"this program is currently linked with ("
				+ Ceylan::GetVersion().toString() 
				+ ") is compatible with the following specified version : "
				+ firstTargetVersion.toString() ) ;
		else
        	LogPlug::info( "The version of the Ceylan library "
				"this program is currently linked with ("
				+ Ceylan::GetVersion().toString() 
				+ ") is not compatible with the following specified version : "
				+ firstTargetVersion.toString() ) ;



		Ceylan::LibtoolVersion secondTargetVersion( "3.4.2" ) ;
		
		if ( /* library version */                                           \
                Ceylan::GetVersion().isCompatibleWith( secondTargetVersion ) )
        	LogPlug::info( "The version of the Ceylan library "
				"this program is currently linked with ("
				+ Ceylan::GetVersion().toString() 
				+ ") is compatible with the following specified version : "
				+ secondTargetVersion.toString() ) ;
		else
        	LogPlug::info( "The version of the Ceylan library "
				"this program is currently linked with ("
				+ Ceylan::GetVersion().toString() 
				+ ") is not compatible with the following specified version : "
				+ secondTargetVersion.toString() ) ;

        LogPlug::info( "End of Ceylan utilities test." ) ;

 
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
