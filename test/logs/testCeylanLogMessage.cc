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
using namespace Ceylan::Log ;


#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;

#include <string>
using std::string ;

#include <exception>



/**
 * Test of LogMessage implementation for the Log system.
 *
 * @see Channel
 *
 */
int main( int argc, char * argv[] )
{

  {

	try
	{

	  cout << endl << "Testing LogMessage implementation."
		   << endl << endl ;

	  LogMessage myMessage( "Hello channels!", "AChannel" ) ;

	  cout << "Displaying LogMessage : " << endl ;
	  cout << myMessage.toString() << endl ;


	  cout << endl << "End of test for LogMessage implementation ." ;


	  /*
	   * No LogMessage to delete, Aggregators take care of it (ownership taken).
	   *
	   */

	}
	catch ( const Ceylan::Exception & e )
	{
	  cerr << "Ceylan exception caught: "
		   << e.toString( Ceylan::high ) << endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
	  cerr << "Standard exception caught: " << e.what() << endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
	  cerr << "Unknown exception caught" << endl ;
	  return Ceylan::ExitFailure ;

	}

  }

  Ceylan::shutdown() ;

  return Ceylan::ExitSuccess ;

}
