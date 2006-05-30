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
 * Test of LogAggregatorRaw implementation for the Log system.
 *
 * @see LogAggregatorRaw
 *
 */
int main( int argc, char * argv[] )
{


    try
    {

        cout << endl << "Testing LogAggregatorRaw implementation."
        	 << endl << endl ;        
			 
		LogAggregatorRaw rawAggregator( "testLogAggregatorRaw.log" ) ;
		
		rawAggregator.createBasicChannel( "FirstChannel" ) ;
		
		bool exceptionRaised = false ;
		try 
		{
		
			rawAggregator.createBasicChannel( "FirstChannel" ) ;
			
		} 
		catch( const LogException & e )
		{
			cout << "OK, creating an already created channel "
				"raises an exception." << endl ;
			exceptionRaised = true ;	 
		}
		
		if ( ! exceptionRaised )
			cout << "Warning : creating twice the same channel "
				"does not raise a log exception "
				"(the exception should be raised only if "
				"Ceylan is compiled with CEYLAN_DEBUG)" ;
		
		rawAggregator.createBasicChannel( "SecondChannel" ) ;
				
		LogMessage * toSecondChannel = new LogMessage( 
			"Hello second channel !", "SecondChannel" ) ;
		
		cout << "Displaying LogMessage : " << toSecondChannel->toString() 
			<< endl ;
			
		rawAggregator.store( * toSecondChannel ) ;
				
		cout << endl << "End of test for LogAggregatorRaw implementation."
			 << endl ;
		
		/*
		 * No LogMessage to delete, Aggregators take care of it 
		 * (ownership taken).
		 *
		 * Aggregation triggered by rawAggregator deletion since going
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
