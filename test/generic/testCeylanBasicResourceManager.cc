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

#include <exception>
#include <string>


using namespace Ceylan ;
using namespace Ceylan::Log ;


/**
 * Test class for Basic Resource manager.
 *
 */
class TestResource : public Ceylan::Resource
{

	public:


		TestResource( const std::string & name ) throw() :
			_name( name )
		{

		}

		virtual ~TestResource()  throw()
		{
			LogPlug::info( "TestResource '" + _name + "' deallocated." ) ;
		}


		virtual const std::string toString( VerbosityLevels level
			= Ceylan::high ) const throw()
		{
			return "TestResource '" + _name + "'." ;
		}


	private:

		std::string _name ;


} ;



/**
 * Test of basic Resource manager.
 *
 * @see BasicResourceManager
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;

	try
	{

		LogPlug::info( "Testing Basic Resource manager implementation" ) ;


		// Indented block to force premature manager deallocation.
		{

			LogPlug::info( "Creating a Basic Resource manager." ) ;
			BasicResourceManager<Ceylan::Latin1Char> manager ;
			LogPlug::info( "New manager state is: "
				+ manager.toString( Ceylan::high ) ) ;

			LogPlug::info( "Creating a set of resources, "
				"each named after a letter "
				"and whose key will be that letter." ) ;

			TestResource * a = new TestResource( "a" ) ;
			TestResource * b = new TestResource( "b" ) ;
			TestResource * c = new TestResource( "c" ) ;


			LogPlug::info( "Causing cache misses "
				"(not put in cache, hence cannot be found)" ) ;
			manager.get( 'a' ) ;
			manager.get( 'a' ) ;

			LogPlug::info( "New manager state: "
				+ manager.toString( Ceylan::high ) ) ;

			LogPlug::info( "Causing cache hits" ) ;
			manager.takeOwnershipOf( 'a', *a ) ;
			manager.takeOwnershipOf( 'b', *b ) ;
			manager.takeOwnershipOf( 'c', *c ) ;


			if ( manager.get( 'a' ) != a )
				throw TestException( "Basic Resource Manager failed "
					"to return correct pointer for cached resource." ) ;
			else
				LogPlug::info( "Correct resource returned, "
					"new manager state is: "
					+ manager.toString( Ceylan::high ) ) ;


		}

		LogPlug::info( "Here all the cached resources "
			"should have been deallocated." ) ;

		LogPlug::info( "End of Basic Resource manager test." ) ;

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

	Ceylan::shutdown() ;

	return Ceylan::ExitSuccess ;

}
