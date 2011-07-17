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
using namespace Ceylan::Log ;


#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;

#include <string>
using std::string ;

#include <exception>



/**
 * Test of Channel implementation for the Log system.
 *
 * @see Channel
 *
 */
int main( int argc, char * argv[] )
{

	try
	{

		// We are debugging the log system, so it cannot be used here.

		cout << endl << "Testing Channel implementation."
			 << endl << endl ;

		const string firstName = "FirstChannel" ;

		cout << "Creating a channel named "
			<< firstName << endl ;

		LogChannel channelOne( firstName ) ;

		cout << "Displaying this Channel information:" << endl ;
		cout << channelOne.toString() << endl ;


		cout << "Adding first message" << endl ;
		channelOne.addMessage(
			* new LogMessage(
				"Hello world!",
				firstName,
				MaximumLevelOfDetailForMessage,
				* new Ceylan::Timestamp()
			)
		) ;

		cout << "Displaying this Channel information:" << endl ;
		cout << channelOne.toString() << endl ;

		cout << "Adding second message" << endl ;
		channelOne.addMessage(
			* new LogMessage(
				"For my second message, I would like to emphasize "
				"the fact that Ceylan rocks!",
				firstName,
				MaximumLevelOfDetailForMessage,
				* new Ceylan::Timestamp()
			)
		) ;

		cout << "Displaying this Channel information:" << endl ;
		cout << channelOne.toString() << endl ;

		cout << endl << "End of test for Channel implementation." ;


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

	return Ceylan::ExitSuccess ;

}
