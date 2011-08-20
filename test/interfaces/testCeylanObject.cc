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


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




class ExampleOne : public Object
{

public:

  virtual void aVirtualMethod()
  {

  }

} ;



class ExampleTwo : public ExampleOne
{

public:

  ExampleTwo()
  {

  }

  virtual void aVirtualMethod()
  {

  }

} ;




/**
 * Test for Object implementation.
 *
 * @see Object
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{

	  LogPlug::info( "Starting the test of the Ceylan Object class" ) ;

	  Object * p1 = new ExampleOne() ;
	  Object * p2 = new ExampleOne() ;
	  Object * p3 = new ExampleTwo() ;

	  LogPlug::info( "p1->getClassName(): " + p1->getClassName() ) ;
	  LogPlug::info( "p2->getClassName(): " + p2->getClassName() ) ;
	  LogPlug::info( "p3->getClassName(): " + p3->getClassName() ) ;

	  LogPlug::info( "p1->isOfSameType( * p2 ): "
		+ toString( p1->isOfSameType( * p2 ) ) ) ;

	  LogPlug::info( "p1->isOfSameType( * p3 ): "
		+ toString( p1->isOfSameType( * p3 ) ) ) ;

	  p1->send( "This is p1 and I am sending a message." ) ;

	  p2->send( "This is p2 and I will not let p1 send a message "
		"without doing so." ) ;

	  p3->send( "Hey you p1 and p2, did not you forget something, uh? "
		"I am the famous p3." ) ;

	  p1->send( "I am the most verbose of all!" ) ;

	  p1->send( "I can prove it!" ) ;

	  delete p1 ;
	  delete p2 ;
	  delete p3 ;


	  LogPlug::info( "End of Object class test." ) ;

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
