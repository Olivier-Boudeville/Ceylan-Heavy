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

#include <exception>
#include <string>


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;



/**
 * Test of timestamp facility.
 *
 * @see Timestamp
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{


	  LogPlug::info( "Testing Timestamp's implementation." ) ;


	  LogPlug::info( "First time-stamp: " ) ;
	  Timestamp firstStamp ;
	  LogPlug::info( firstStamp.toString() ) ;

	  if ( Features::areFileDescriptorsSupported() )
	  {
		// 1/10 second waiting:
		Ceylan::System::basicSleep( 0 /* second */,
		  /* nanoseconds */ 100000000 ) ;
	  }
	  else
	  {

		LogPlug::warning( "Ceylan::System::basicSleep not available, "
		  "defaulting to Ceylan::System::sleepForSeconds" ) ;

		// Otherwise sleep for one second:
		Ceylan::System::sleepForSeconds( 1 ) ;
	  }

	  LogPlug::info( "Second time-stamp: " ) ;
	  Timestamp secondStamp ;
	  LogPlug::info( secondStamp.toString() ) ;

	  // Sleep for a full second:
	  Ceylan::System::sleepForSeconds( 1 ) ;


	  LogPlug::info( "Third time-stamp: " ) ;
	  Timestamp thirdStamp ;
	  LogPlug::info( thirdStamp.toString() ) ;

	  LogPlug::info( "Testing comparison operator "
		"applied to Timestamps: " ) ;

	  if ( firstStamp < secondStamp )
		LogPlug::info( "First timestamp is inferior to second." ) ;
	  else
	  {
		if ( secondStamp < firstStamp )
		  throw Ceylan::TestException( "First timestamp ("
			+ firstStamp.toString()
			+ ") should be at most equal to second ("
			+ secondStamp.toString() + "), if not inferior." ) ;
		else
		  LogPlug::info( "First timestamp is equal to second." ) ;
	  }


	  if ( thirdStamp < firstStamp  )
		throw Ceylan::TestException(
		  "Third timestamp (" + thirdStamp.toString()
		  + ") is inferior to first (" + firstStamp.toString()
		  + "), which is abnormal." ) ;
	  else
		LogPlug::info( "Third timestamp is superior "
		  "or equal to first." ) ;

	  LogPlug::info( "Testing now duration descriptions." ) ;

	  Second duration = 0 ;

	  LogPlug::info( "A duration of " + Ceylan::toString( duration )
		+ " second is described as: "
		+ Timestamp::DescribeDuration( duration ) + "." ) ;

	  duration = 1 ;

	  LogPlug::info( "A duration of " + Ceylan::toString( duration )
		+ " second is described as: "
		+ Timestamp::DescribeDuration( duration ) + "." ) ;

	  duration = 60 ;

	  LogPlug::info( "A duration of " + Ceylan::toString( duration )
		+ " seconds is described as: "
		+ Timestamp::DescribeDuration( duration ) + "." ) ;

	  duration = 188 ;

	  LogPlug::info( "A duration of " + Ceylan::toString( duration )
		+ " seconds is described as: "
		+ Timestamp::DescribeDuration( duration ) + "." ) ;

	  duration = 8000 ;

	  LogPlug::info( "A duration of " + Ceylan::toString( duration )
		+ " seconds is described as: "
		+ Timestamp::DescribeDuration( duration ) + "." ) ;

	  duration = 1036827 ;

	  LogPlug::info( "A duration of " + Ceylan::toString( duration )
		+ " seconds is described as: "
		+ Timestamp::DescribeDuration( duration ) + "." ) ;

	  duration = 157681234 ;

	  LogPlug::info( "A duration of " + Ceylan::toString( duration )
		+ " seconds is described as: "
		+ Timestamp::DescribeDuration( duration ) + "." ) ;

	  duration = 1892160000 ;

	  LogPlug::info( "A duration of " + Ceylan::toString( duration )
		+ " seconds is described as: "
		+ Timestamp::DescribeDuration( duration ) + "." ) ;


	  LogPlug::info( "End of Timestamp test." ) ;


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
