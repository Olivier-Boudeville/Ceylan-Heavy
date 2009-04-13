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
using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;

#include <string>
using std::string ;

#include <exception>



/**
 * Test of LogTransportListenerRaw implementation for the Log system.
 *
 * @see LogTransportListenerRaw
 *
 */
int main( int argc, char * argv[] )
{


    try
    {

        cout << endl << "Testing LogTransportListenerRaw implementation."
        	 << endl << endl ;        
			 
		// First pre-requesite for this test : a valid aggragator.	 
		LogAggregatorRaw rawAggregator( "testLogTransportListenerRaw.log" ) ;


		// Second pre-requesite for this test :	a valid message. 
		LogMessage * aMessage = new LogMessage( "Hello virtual log bus !",
			"A Channel" ) ;
			
			
		// Real testing : create the tested object.
		LogTransportListenerRaw rawLogBus( rawAggregator ) ;
		
		// And let the ball rollin' :
		rawLogBus.propagate( * aMessage ) ;
		
		cout << endl << "End of test for LogTransportListenerRaw "
			 << "implementation." << endl ;
		
		/*
		 * No LogMessage to delete, Aggregators take care of it 
		 *(ownership taken).
		 *
		 * Aggregation will be triggered now, since rawAggregator is going
		 * out of scope.
		 *
		 */
		
		
	}
	catch ( const Ceylan::Exception & e )
    {
        cerr << "Ceylan exception caught : "
        	 << e.toString( Ceylan::high ) << endl ;
        return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        cerr << "Standard exception caught : " << e.what() << endl ;
        return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        cerr << "Unknown exception caught" << endl ;
        return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
