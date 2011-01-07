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

#include <exception>



/**
 * Test of LogHolder.
 *
 * @note One can specify the various log options to test the available
 * plugs (ex: --consolePlug, --classicalPlug, --HTMLPlug, --nullPlug).
 *
 * For example: './testCeylanLogHolder --HTMLPlug'.
 *
 * @see LogHolder, LogPlug.
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;
		
    try
    {

        cout << endl << "Testing LogHolder implementation."
        	<< endl << endl ;

        LogPlug::info(    "This is a info message"        ) ;
        LogPlug::trace(   "This is a trace message"       ) ;
        LogPlug::debug(   "This is a debug message"       ) ;
        LogPlug::warning( "This is a warning message"     ) ;
        LogPlug::error(   "This is an error message"      ) ;
        LogPlug::fatal(   "This is a fatal error message" ) ;

        LogPlug::info( "This is another info message" ) ;

        cout << endl << "End of LogHolder test." << endl ;


		/*
		 * One can test by uncommenting the next line that even if an 
		 * exception is raised,
		 * Log system is correctly shut down.
		 *

		throw Ceylan::Exception( "This is a test to show that "
			"Log system is well managed" ) ;
		 
		 * @see testCeylanFullLogSystem.cc
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
        cerr << "Standard exception caught: " 
			 << e.what() << endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        cerr << "Unknown exception caught" << endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
