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
 * Test of LogMessage implementation for the Log system.
 *
 * @see Channel
 *
 */
int main( int argc, char * argv[] )
{


    try
    {

        cout << endl << "Testing LogMessage implementation."
        	 << endl << endl ;        
					
		LogMessage myMessage( "Hello channels !", "AChannel" ) ;
		
		cout << "Displaying LogMessage : " << endl ;
		cout << myMessage.toString() << endl ;
			

		cout << endl << "End of test for LogMessage implementation ." ;
		
		
		/*
		 * No LogMessage to delete, Aggregators take care of it 
		 * (ownership taken).
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
