#include "Ceylan.h"
using namespace Ceylan::Log ;


#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;

#include <string>
using std::string ;

#include <exception>



/**
 * Test of Channel implementation for the Log system.
 *
 * @see Channel
 *
 */
int main( int argc, char * argv[] )
{

    try
    {

		// We are debugging the log system, so it cannot be used here.
		
        cout << endl << "Testing Channel implementation ."
        	 << endl << endl ;
			 
		const string firstName = "FirstChannel" ;
			 
		cout << "Creating a channel named " 
			<< firstName << endl ;

		LogChannel channelOne( firstName ) ;
		
		cout << "Displaying this Channel informations :" << endl ;
		cout << channelOne.toString() << endl ;
				
		
		cout << "Adding first message" << endl ;
		channelOne.addMessage( 
			* new LogMessage( 
				"Hello world !",
				firstName,
				MaximumLevelOfDetailForMessage,
				* new Ceylan::Timestamp()
			)	
		) ;

		cout << "Displaying this Channel informations :" << endl ;
		cout << channelOne.toString() << endl ;

		cout << "Adding second message" << endl ;
		channelOne.addMessage( 
			* new LogMessage( 
				"For my second message, I would like to emphasize "
				"the fact that Ceylan rocks !",
				firstName,
				MaximumLevelOfDetailForMessage,
				* new Ceylan::Timestamp()
			)	
		) ;
		
		cout << "Displaying this Channel informations :" << endl ;
		cout << channelOne.toString() << endl ;
		
        cout << endl << "End of test for Channel implementation ." ;
		
		
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
