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


using namespace Ceylan ;
using namespace Ceylan::Log ;


/**
 * Test of Singleton class.
 *
 * @see Singleton.
 *
 */
int main( int argc,  char * argv[] )
{


	LogHolder myLog( argc, argv ) ;

    try
    {

		LogPlug::info( "Testing Singleton's implementation." ) ;

        LogPlug::info( "I want a Singleton (first time)" ) ;
        Singleton & myFirstSingleton = Singleton::GetSingleton() ;
        LogPlug::info( "I got " + Ceylan::toString( & myFirstSingleton ) ) ;

        LogPlug::info( "Again (second time)" ) ;
        Singleton & mySecondSingleton = Singleton::GetSingleton() ;
        LogPlug::info( "I got " + Ceylan::toString( & mySecondSingleton ) ) ;

        LogPlug::info( "Forcing Singleton deallocation" ) ;
        Singleton::DeleteSingleton() ;
		
        LogPlug::info( "Forcing uselesss Singleton deallocation" ) ;		
        Singleton::DeleteSingleton() ;

        LogPlug::info( "End of Singleton test." ) ;


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

