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

#include <exception>

#include <string>
using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Log ;


const string firstExpression = "192.168.0.7" ;
const string firstPattern    = "^([0-2]{0,1}[0-9]{0,1}[0-9]{1,1}[.]"
  "{1,1}){3}([0-2]{0,1}[0-9]{0,1}[0-9]{1,1})$" ;



/**
 * Test of regular expression support.
 *
 * @see CeylanRegularExpression, TCL regular expressions.
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder myLog( argc, argv ) ;

	try
	{

	  LogPlug::info( "Testing Regular Expressions support." ) ;

	  if ( ! Features::areRegularExpressionsSupported() )
	  {
		LogPlug::warning( "Regular expression support not available, "
		  "nothing tested." ) ;
		return Ceylan::ExitSuccess ;
	  }


	  LogPlug::info( "Testing whether first expression <"
		+ firstExpression + "> matches first pattern <"
		+ firstPattern + ">: " ) ;

	  RegExp firstRegExp( firstExpression ) ;

	  LogPlug::info( Ceylan::toString(
		  firstRegExp.matches( firstPattern ) ) ) ;


	  LogPlug::info( "End of Regular Expressions test." ) ;

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

  }

  Ceylan::shutdown() ;

  return Ceylan::ExitSuccess ;

}
