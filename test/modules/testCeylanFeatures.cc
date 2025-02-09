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
using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Features ;


#include <iostream>      // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/**
 * Test of optional features management.
 *
 * @see CeylanFeatures.h
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{


	  LogPlug::info( "Testing optional features management." ) ;

	  // Testing an example of feature:

	  if ( ! Features::areRegularExpressionsSupported() )
	  {

		bool caught = false ;

		try
		{
		  checkForSupportedFeatures( Features::RegularExpressions ) ;
		}
		catch( const FeatureNotAvailableException & e )
		{
		  LogPlug::info( "Correctly caught expected exception: "
			+ e.toString() ) ;
		  caught = true ;
		}

		if ( ! caught )
		  throw Ceylan::TestException(
			"Expected a feature exception, nothing thrown." ) ;

	  }
	  else
	  {
		checkForSupportedFeatures( Features::RegularExpressions ) ;
	  }

	  LogPlug::info( Features::describeAvailableFeatures() ) ;

	  LogPlug::info( "End of feature test." ) ;

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
