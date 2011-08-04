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


#include <exception>

#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;





class FirstExampleOfObject : public Object
{

	public:

		virtual void aVirtualMethod()
		{

		}

} ;



class SecondExampleOfObject : public FirstExampleOfObject
{

	public:

		SecondExampleOfObject()
		{

		}

		virtual void aVirtualMethod()
		{

		}

} ;



/**
 * Test of HTML implementation of the Log system plug.
 *
 * @see LogPlugHTMl, LogPlug.
 *
 */
int main( int argc, char * argv[] )
{

	try
	{

		cout << endl << "Testing LogPlugHTML implementation "
			"of the Log system." << endl << endl ;

		// To avoid writing logs alongside the test executable:
		std::string speakerName ;
		Ceylan::System::Directory::StripFilename( argv[0],
			/* base path */ 0, & speakerName ) ;

		LogPlugHTML::StartService( speakerName ) ;

		LogPlug::info(    "This is a info message"        ) ;
		LogPlug::trace(   "This is a trace message"       ) ;
		LogPlug::debug(   "This is a debug message"       ) ;
		LogPlug::warning( "This is a warning message"     ) ;
		LogPlug::error(   "This is an error message"      ) ;
		LogPlug::fatal(   "This is a fatal error message" ) ;

		LogPlug::info( "This is another info message" ) ;

		Object * p1 = new FirstExampleOfObject() ;
		p1->send( "If I do not send any message, my name be will mangled "
			"for the rest of my life, what a pity." ) ;

		Object * p2 = new FirstExampleOfObject() ;
		Object * p3 = new SecondExampleOfObject() ;

		p2->send( "Ceylan rocks!" ) ;
		p2->send( "Look <a href=\"http://ceylan.sourceforge.net\" "
			"target=\"_blank\">here</a> for more details." ) ;

		p3->send( "OSDL rocks!" ) ;
		p3->send( "Look <a href=\"http://osdl.sourceforge.net\" "
			"target=\"_blank\">here</a> for more details." ) ;
		p3->send( "Sorry for the infamous plug, pal, I had to." ) ;

		{
			SecondExampleOfObject a1 ;
			a1.send( "We, automatic object, are able to speak too." ) ;

			FirstExampleOfObject a2 ;
			a2.send( "What a hangover this mornin', burp." ) ;
			a2.send( "What am I doing here?" ) ;
			a2.send( "Where is the exit door?" ) ;

			a1.send( "As long as we are not deallocated, though." ) ;
			a1.send( "Bye bye!" ) ;
		}

		LogPlug::info( "The main point of this test is "
			"to experiment a full-blown use of the Log system." ) ;

		LogPlug::info( "We indeed make use of a "
			"smart HTML aggregator (<b>wooooh</b>!) "
			"with Ceylan::Object instances whose behaviour is tracked down." ) ;

		LogPlug::info( "You can check that mangled names "
			"have been automatically corrected." ) ;

		LogPlug::info( "Everything seems to be fully functionnal, guys!" ) ;

		cout << "Shutting down LogPlugHTML Service." << endl ;

		delete p1 ;
		delete p2 ;
		delete p3 ;

		LogPlugHTML::StopService() ;

		cout << endl << "End of LogPlugHTML test." << endl ;


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
		cerr << "Standard exception caught: "
			 << e.what() << endl ;
		LogPlugHTML::StopService() ;
		return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
		cerr << "Unknown exception caught" << endl ;
		LogPlugHTML::StopService() ;
		return Ceylan::ExitFailure ;

	}

	Ceylan::shutdown() ;

	return Ceylan::ExitSuccess ;

}
