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


#include <exception>

#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;

#include <string>
using std::string ;



class ExampleOfObject : public Ceylan::Object
{

public:


  ExampleOfObject() throw( LogException )
  {

  }


  virtual void aVirtualMethod()
  {

  }

} ;




/**
 * Test of LogPlugHTML implementation for the Log system.
 *
 * Messages will be stored and, if a smart aggregator is requested, they will be
 * checked for mangled names. The relevant log informations will be output in an
 * HTML set of pages, so that their reading is user-friendly.
 *
 * @see LogPlugHTML
 *
 */
int main( int argc, char * argv[] )
{


  {

	try
	{

	  // To avoid writing logs alongside the test executable:
	  std::string speakerName ;

	  Ceylan::System::Directory::StripFilename( argv[0],
		/* base path */ 0, & speakerName ) ;

	  LogPlugHTML::StartService( speakerName, /* smart */ true ) ;

	  LogPlug::info( "Starting testing LogPlugHTML implementation "
		"in non-immediate mode." ) ;

	  // No LogHolder to set the output format, doing it now instead:
	  TextDisplayable::SetOutputFormat( Ceylan::TextDisplayable::html ) ;

	  LogPlug::info( "Dumping some informations about the log system: "
		+ LogPlugHTML::ToString() ) ;

	  LogPlug::info( "Creating a Ceylan::Object "
		"whose class name will start mangled, "
		"in order to check whether the smart demangling works.") ;

	  ExampleOfObject * myExample = new ExampleOfObject() ;

	  myExample->send( "Ceylan rocks!" ) ;
	  myExample->send( "OSDL rocks!" ) ;

	  delete myExample ;

	  LogPlug::info( "End of test for LogPlugHTML implementation." ) ;


	  /*
	   * No LogMessage to delete, Aggregators take care of it (ownership taken).
	   *
	   */


	}

	catch ( const Ceylan::Exception & e )
	{
	  cerr << "Ceylan exception caught: "
		   << e.toString( Ceylan::high ) << endl ;
	  LogPlugHTML::StopService() ;
	  return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
	  cerr << "Standard exception caught: " << e.what() << endl ;
	  LogPlugHTML::StopService() ;
	  return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
	  cerr << "Unknown exception caught" << endl ;
	  LogPlugHTML::StopService() ;
	  return Ceylan::ExitFailure ;

	}

  }

  LogPlugHTML::StopService() ;

  Ceylan::shutdown() ;

  return Ceylan::ExitSuccess ;

}
