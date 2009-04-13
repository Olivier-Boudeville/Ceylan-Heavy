/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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
#include <list>


using namespace Ceylan ;
using namespace Ceylan::Log ;



/**
 * Test of Ceylan command line option management.
 *
 * @example : "testCeylanCommandLineOptions --consolePlug --countedOption 3
 * --unknownOption --htmlPlug"
 *
 * @see Ceylan::parseCommandLineOptions.
 *
 */
int main( int argc,  char * argv[] )
{


	LogHolder myLog( argc, argv ) ;

    try
    {

		LogPlug::info( "Testing command line option management." ) ;

		std::string executableName ;
		std::list<std::string> options ;
		
		Ceylan::parseCommandLineOptions( executableName, options, argc, argv ) ;
		
		LogPlug::info( "Executable name is : '" + executableName 
			+ "', option count is " 
			+ Ceylan::toString( static_cast<Ceylan::Uint32>( options.size() ) )
			+ ", option list is " 
			+ formatStringList( options, /* surroundByTicks */ true ) ) ;
		
		
		LogPlug::info( "Example of use of Ceylan::parseCommandLineOptions "
			"to parse easily options, with detection of unsupported options "
			"and use of options taking arguments." ) ;
		
		
		
		// No switch allowed, using if/else clauses :
		
		
		std::string token ;
		bool tokenEaten ;
		
		
		while ( ! options.empty() )
		{
		
			token = options.front() ;
			options.pop_front() ;

			tokenEaten = false ;
						
			if ( token == "--batch" )
			{
				LogPlug::info( "Batch mode selected (but ignored)" ) ;
				tokenEaten = true ;
			} else
			if ( token == "--interactive" )
			{
				LogPlug::info( "Interactive mode selected (but ignored)" ) ;
				tokenEaten = true ;
			} else		
			if ( token == "--online" )
			{
				LogPlug::info( "Online mode selected (but ignored)" ) ;
				tokenEaten = true ;
			} else		
			if ( token == "--countedOption" )
			{
				if ( options.empty() )
					throw CommandLineParseException( "Option " + token 
						+ " expected one argument, none found." ) ;
				
				std::string count = options.front() ;
				options.pop_front() ;
				LogPlug::info( "Option with argument selected, argument is : "
					+ count ) ;
				tokenEaten = true ;
			} else
			if ( LogHolder::IsAKnownPlugOption( token ) )
			{
				// Ignores log-related (argument-less) options.
				tokenEaten = true ;
			}
			
			if ( ! tokenEaten )
			{
				throw CommandLineParseException( 
					"Unexpected command line argument : " + token ) ;
			}
		
		}
			
        LogPlug::info( "End of command line option management test." ) ;


    }

    catch ( const Ceylan::Exception & e )
    {
        LogPlug::error( "Ceylan exception caught : "
        	 + e.toString( Ceylan::high ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        LogPlug::error( "Standard exception caught : " 
			 + std::string( e.what() ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        LogPlug::error( "Unknown exception caught" ) ;
       	return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}

