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
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <exception>

#include <iostream>
using std::cerr ;
using std::endl ;

#include <string>
using std::string ;

#include <list>
using std::list ;



/**
 * Test of File locator services.
 *
 * @see FileLocator, File, Directory.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder myLog( argc, argv ) ;


	try
	{


		LogPlug::info( "Testing File locator implementation." ) ;

		FileLocator myLocator ;

		LogPlug::info( "Creating a new file locator: "
			+ myLocator.toString() ) ;

		LogPlug::info( "Current working directory is "
			+ Directory::GetCurrentWorkingDirectoryPath() ) ;

		// When run from build tree:
		string firstPath  = "../src/code" ;

		myLocator.addPath( firstPath ) ;
		LogPlug::info( "Adding a first UNIX path: " + myLocator.toString() ) ;

		// When run against an install:
		string secondPath = "../../include/Ceylan" ;

		myLocator.addPath( secondPath ) ;
		LogPlug::info( "Adding a second UNIX path: " + myLocator.toString() ) ;

		myLocator.addPath( secondPath ) ;
		LogPlug::info( "Adding this second path twice: "
			+ myLocator.toString() ) ;

		string thirdPath = "../code" ;
		myLocator.addPath( thirdPath ) ;
		LogPlug::info( "Adding a path for Windows tests: "
			+ myLocator.toString() ) ;

		string toFind = "Ceylan.h" ;
		string found  ;


		// If run from one of the trees, should succeed.
		try
		{
			found = myLocator.find( toFind ) ;
		}
		catch ( const FileLocatorException & e )
		{
			throw Ceylan::TestException( "File locator test failed: "
				+ e.toString() ) ;
		}

		LogPlug::info( "Testing if '" + toFind + "' is found: yes, in '"
				+ found + "'." ) ;

		string notToFind = "Ceylan-does-not-exist.txt" ;
		bool notFound = false ;

		try
		{
			found = myLocator.find( notToFind ) ;
		}
		catch ( const FileLocatorException & e )
		{
			notFound = true ;
			LogPlug::info( "Testing if '" + notToFind
				+ "' is found: no, correct: " + e.toString() ) ;
		}

		if ( ! notFound )
			throw Ceylan::TestException(
				"File locator test failed: file '" + notToFind
				+ "' abnormally found through " + myLocator.toString() ) ;


		myLocator.removePath( "Hello" ) ;
		LogPlug::info( "Removing a non-registered path: "
			+  myLocator.toString() ) ;

		myLocator.removePath( firstPath ) ;
		LogPlug::info( "Removing first path: "
			+  myLocator.toString() ) ;


		// Just a test, does not indicates anything:
		try
		{
			found = myLocator.find( toFind ) ;
		}
		catch( const FileLocatorException )
		{
			// Not found.
		}

		if ( found.empty() )
			LogPlug::info( "Testing if '" + toFind + "' is found: no, for "
				+ myLocator.toString()   ) ;
		else
			LogPlug::info( "Testing if '" + toFind + "' is found: yes, for "
				+ myLocator.toString()   ) ;


		LogPlug::info( "End of File locator test." ) ;


	}

	catch ( const Ceylan::Exception & e )
	{
		LogPlug::error( "Ceylan exception caught: "
			 + e.toString( Ceylan::high ) ) ;
		return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
		LogPlug::error( "Standard exception caught: "
			 + std::string( e.what() ) ) ;
		return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
		LogPlug::error( "Unknown exception caught" ) ;
		return Ceylan::ExitFailure ;

	}

	Ceylan::shutdown() ;

	return Ceylan::ExitSuccess ;

}
