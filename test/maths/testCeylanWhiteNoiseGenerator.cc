#include "Ceylan.h"
using namespace Ceylan::Maths::Random ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/**
 * Test of white noise generator.
 *
 * @see Maths
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;
	

    try
    {


        LogPlug::info( "Testing white noise generator's implementation." ) ;

		Seed seed = 145 ;
		RandomValue lowerLimit = 0 ;
		RandomValue upperLimit = 3 ;
		
		LogPlug::info( "Creating a white noise generator whose seed is " 
			+ Ceylan::toString( seed )
			+ ", whose output range is "
			+ Ceylan::toString( lowerLimit ) + " (included) to " 
			+ Ceylan::toString( upperLimit ) + " (excluded)." ) ;
		
		WhiteNoiseGenerator myGen( lowerLimit, upperLimit, seed ) ;
		
		LogPlug::info( "Displaying generator's state : " + myGen.toString() ) ;
		
		for ( Ceylan::Uint8 i = 0 ; i < 50 ; i ++ )
			LogPlug::info( "New value is " 
				+ Ceylan::toString( myGen.getNewValue() ) ) ;


		LogPlug::info( "End of white noise generator's test." ) ;
		
		
	}
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught : " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
