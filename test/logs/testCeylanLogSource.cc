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
 * Test of LogSource implementation for the Log system.
 *
 * @see LogSource
 *
 */
int main( int argc, char * argv[] )
{


    try
    {

        cout << endl << "Testing LogSource implementation."
        	 << endl << endl ;        
			 
		// First pre-requesite for this test : a valid aggragator.	 
		LogAggregatorRaw rawAggregator( "testLogSource.log" ) ;
			
		// Second pre-requesite for this test : a valid log bus. 
		LogTransportListenerRaw rawLogBus( rawAggregator ) ;
			
		// Real testing : create the tested object.
		LogSource source( "A Log Source", rawLogBus ) ;
		
		// And let the ball rollin' :
		source.send( "Hello sunny world !!!!" ) ;
				
		
		cout << endl << "End of test for LogSource implementation."
			 << endl ;
		
		/*
		 * No LogMessage to delete, Aggregators take care of it 
		 * (ownership taken).
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
