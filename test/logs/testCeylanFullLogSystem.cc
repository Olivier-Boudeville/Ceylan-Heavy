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

#include <exception>



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


namespace One
{

  class ExampleThree : public ExampleTwo
  {


  } ;


  namespace Two
  {

	class ExampleFour : public ExampleThree
	{

	} ;

  }


}




/**
 * Test of the full log system.
 *
 */
int main( int argc, char * argv[] )
{


  {

	LogHolder logger( argc, argv ) ;


	try
	{

	  cout << endl << "Testing full log system." ;

	  LogPlug::info(    "This is a info message"        ) ;
	  LogPlug::trace(   "This is a trace message"       ) ;
	  LogPlug::debug(   "This is a debug message"       ) ;
	  LogPlug::warning( "This is a warning message"     ) ;
	  LogPlug::error(   "This is an error message"      ) ;
	  LogPlug::fatal(   "This is a fatal error message" ) ;

	  LogPlug::info( "This test is meant to prove "
		"that the whole log system is working." ) ;


	  LogPlug::info( "i.e.: LogHolder, LogSource, Ceylan::Object, "
		"LogTransport, Log Listener, and LogAggregators" ) ;

	  LogPlug::info( "To test all the plugs and aggregators, "
		"add in the command-line --consolePlug "
		"(logs are output directly in the console), --classicalPlug "
		"(they are stored in a .log text file) or --HTMLPlug "
		"(they are aggegated in a set of web pages" ) ;


	  Object * p1 = new ExampleOne() ;
	  p1->send( "If I do not send any message, my name be will mangled "
		"for the rest of my life, what a pity." ) ;

	  Object * p2 = new ExampleOne() ;
	  Object * p3 = new ExampleTwo() ;

	  // Will sadly stay mangled since will stay mute:
	  Object * p4 = new ExampleTwo() ;

	  p2->send( "Ceylan rocks!" ) ;

	  p3->send( "OSDL rocks as well!" ) ;
	  p3->send( "Look at <a href=\"http://ceylan.sourceforge.net\" "
		"target=\"_blank\">here</a> for more details about Ceylan." ) ;
	  p3->send( "Look at <a href=\"http://osdl.sourceforge.net\" "
		"target=\"_blank\">here</a> for more details about OSDL." ) ;
	  p3->send( "Sorry for the infamous plug, pal, I had to." ) ;

	  {

		/*
		 * This example is made to understand how automatic variables can be
		 * mixed up.
		 *
		 * Class names will be the ones of the last instanciated objects.
		 *
		 */

		// a1 will be mixed up with a3.
		ExampleTwo a1 ;
		a1.send( "We, automatic object, are able to speak too "
		  "(a1 speaking)." ) ;

		// a2 will be mixed up with a4.
		ExampleOne a2 ;
		a2.send( "What a hangover this mornin', burp (a2 speaking)." ) ;
		a2.send( "What am I doing here?" ) ;
		a2.send( "Where is the exit door?" ) ;

		a1.send( "As long as we are not deallocated, "
		  "though (a1 speaking)." ) ;
		a1.send( "Bye bye! " ) ;
	  }

	  One::ExampleThree a3 ;
	  a3.send( "Hi!! Ha!! (I have the same address as a1 but I am a3)" ) ;

	  One::ExampleThree a3bis ;
	  a3bis.send( "If I send a message, "
		"my name will be mangled differently." ) ;

	  One::Two::ExampleFour a4 ;
	  a4.send( "Alteeee youp oh!" ) ;

	  One::Two::ExampleFour a4bis ;
	  a4bis.send( "If I send a message too, "
		"my name will be mangled differently." ) ;

	  LogPlug::info( "The main point of this test is "
		"to experiment a full-blown use of the Log system." ) ;

	  LogPlug::info( "We indeed make use of the full log system "
		"(<b>wooooh</b>!) with Ceylan::Object instances "
		"whose behaviour is tracked down." ) ;

	  LogPlug::info( "You can check that mangled names "
		"have been automatically corrected in the corresponding "
		"log channels." ) ;

	  LogPlug::info( "Everything seems to be fully functionnal, guys!" ) ;

	  cout << endl << "End of full log system test." << endl ;

	  delete p1 ;
	  delete p2 ;
	  delete p3 ;
	  delete p4 ;


	  /*
	   * Choose the way the system halts.
	   *
	   * This is a test to show that Log system is well managed, i.e.  that it
	   * at least tries to aggregate and save the log in case something went
	   * wrong.
	   *
	   * One can test various stop conditions by selecting the appropriate one,
	   * building and running the test and finally by checking whether the logs
	   * are really available.
	   *
	   * The LogHolder is used in all cases.
	   *
	   * The 'StopOnException' case is meant to deal with correctly managed
	   * exceptions (not unexpected ones, i.e. whenever the raised exception is
	   * not listed in an explicit exception specification).
	   *

	   */
	  enum StopCondition { NormalStop, StopOnException, StopOnExit,
						   StopOnDivisionByZero } ;


	  /*
	   * The result depends on the log plug being used and the stop condition.
	   *
	   * The raw (classical) plug is the default one (used in case none is
	   * specified).
	   *
	   * On normal stop, all plugs behave correctly and cannot loose any log.
	   *
	   *  - with the console plug: nothing can be lost in any stop condition,
	   * even if redirected (ex: 'myProgram 1> logs.txt 2>&1')
	   *
	   *  - with the raw (classical) plug:
	   *		+ StopOnException: nothing is lost
	   *		+ StopOnExit: currently buffered logs are lost, past ones are
	   * kept
	   *		+ StopOnDivisionByZero
	   *
	   * Nevertheless the plugs can be safe (not loosing any log) by setting
	   * their write mode to
	   *
	   *  - with the HTML plug:
	   *		+ StopOnException: all logs are lost
	   *
	   * Any stop condition other than 'NormalStop' will lead to label this test
	   * as failed.
	   *
	   */

	  StopCondition currentChosenStop = NormalStop ;
	  //StopCondition currentChosenStop = StopOnException ;
	  //StopCondition currentChosenStop = StopOnExit ;
	  //StopCondition currentChosenStop = StopOnDivisionByZero ;

	  Ceylan::Uint8 a = 1 ;
	  Ceylan::Uint8 b = 0 ;
	  Ceylan::Uint8 c ;

	  switch( currentChosenStop )
	  {

	  case NormalStop:
		break ;

	  case StopOnException:
		throw Ceylan::TestException( "Testing whether log system "
		  "resists to an exception being raised." ) ;
		break ;

	  case StopOnExit:
		LogPlug::warning( "Stopping with a violent ::exit" ) ;
		::exit( 0 ) ;
		break ;

	  case StopOnDivisionByZero:
		LogPlug::warning( "Division by zero now!" ) ;
		c = a / b ;
		break ;

	  default:
		throw Ceylan::TestException(
		  "Unknown stop condition selected." ) ;
		break ;

	  }


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
