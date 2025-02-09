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


#include "Ceylan.h"
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;






/**
 * Test of Ceylan environment variables utilities.
 *
 * @see Ceylan::System.
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{


	  LogPlug::info( "Testing Ceylan's environment variable service." ) ;


	  string notToBeFound = "CEYLAN_SECRET_VARIABLE" ;

	  LogPlug::info( "Getting value for presumably not set "
		+ notToBeFound + " environment variable: ["
		+ getEnvironmentVariable( notToBeFound ) + "]." ) ;

	  if ( isEnvironmentVariableSet( notToBeFound ) )
		throw Ceylan::TestException( notToBeFound
		  + " environment variable "
		  + "was expected not to be set, instead has a value: ["
		  + getEnvironmentVariable( notToBeFound )
		  + "]." ) ;

	  string toBeFound = "PATH" ;

	  LogPlug::info( "Getting value for presumably set "
		+ toBeFound + " environment variable: ["
		+ getEnvironmentVariable( toBeFound ) + "]." ) ;

	  string newValue = "Ceylan rocks, my friend." ;

	  LogPlug::info( "Setting " + notToBeFound + " to [" + newValue
		+ "]." ) ;

	  setEnvironmentVariable( notToBeFound, newValue ) ;

	  LogPlug::info( "Getting value for "
		+ notToBeFound + " environment variable: ["
		+ getEnvironmentVariable( notToBeFound ) + "]." ) ;

	  if ( getEnvironmentVariable( notToBeFound ) != newValue )
		throw Ceylan::TestException(
		  "Unable to read in environment variable "
		  "what was written in it: read ["
		  + getEnvironmentVariable( notToBeFound )
		  + "], should have read [" + newValue + "]." ) ;


	  LogPlug::info( "Now unsetting "
		+ notToBeFound + " environment variable" ) ;

	  // Allows as well to run the test twice without side effect:
	  unsetEnvironmentVariable( notToBeFound ) ;

	  LogPlug::info( "Getting value for "
		+ notToBeFound + " environment variable: ["
		+ getEnvironmentVariable( notToBeFound ) + "]." ) ;

	  if ( getEnvironmentVariable( notToBeFound ) != "" )
		throw Ceylan::TestException(
		  "Unable to unset environment variable "
		  + notToBeFound + ": after unsetting, was still ["
		  + getEnvironmentVariable( notToBeFound )
		  + "]." ) ;

	  LogPlug::info( "End of environment variable test." ) ;


	}

	catch ( const Ceylan::Exception & e )
	{
	  std::cerr << "Ceylan exception caught: "
				<< e.toString( Ceylan::high ) << std::endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
	  std::cerr << "Standard exception caught: "
				<< e.what() << std::endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
	  std::cerr << "Unknown exception caught" << std::endl ;
	  return Ceylan::ExitFailure ;

	}

  }

  Ceylan::shutdown() ;

  return Ceylan::ExitSuccess ;

}
