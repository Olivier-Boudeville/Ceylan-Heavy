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
