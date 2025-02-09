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
using namespace Ceylan::Maths::Random ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/**
 * Test of white noise generator.
 *
 * @see Maths
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{


	  LogPlug::info( "Testing white noise generator's implementation." ) ;

	  Seed seed = 145 ;
	  RandomValue lowerLimit = 0 ;
	  RandomValue upperLimit = 3 ;

	  LogPlug::info( "Creating a white noise generator whose seed is "
		+ Ceylan::toString( seed )
		+ ", whose output range is "
		+ Ceylan::toString( lowerLimit ) + " (included) to "
		+ Ceylan::toString( upperLimit ) + " (excluded)." ) ;

	  WhiteNoiseGenerator myGen( lowerLimit, upperLimit, seed ) ;

	  LogPlug::info( "Displaying generator's state: " + myGen.toString() ) ;

	  for ( Ceylan::Uint8 i = 0 ; i < 50 ; i ++ )
		LogPlug::info( "New value is "
		  + Ceylan::toString( myGen.getNewValue() ) ) ;


	  LogPlug::info( "End of white noise generator's test." ) ;


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
