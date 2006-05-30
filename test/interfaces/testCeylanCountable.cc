#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <string>
using std::string ;

#include <iostream>    // for cerr, endl
#include <exception>



class CountableExample : public Countable
{

    public:


		CountableExample() throw() 
		{
		
		}
	
	
		virtual ~CountableExample() throw() 
		{
		
		}		
			
			
} ;



/**
 * Test for Countable implementation.
 *
 * @see Countable
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;
	

    try
    {
	
		LogPlug::info( "Starting testing Countable class" ) ;

		LogPlug::info( "Getting initial counts : " + Countable::ToString() ) ;
		CountableExample myExample1 ;
					
		LogPlug::info( "Getting counts after first automatic allocation : " 
			+ Countable::ToString() ) ;
		
		CountableExample * myExample2  = new CountableExample() ;
		LogPlug::info( "Getting counts after first allocation with new : " 
			+ Countable::ToString() ) ;
		
		
		{
			CountableExample myExample3 ;

			LogPlug::info( "Getting counts after second automatic allocation : " 
				+ Countable::ToString() ) ;

		}
		
		LogPlug::info( "Getting counts after first deallocation "
			"of automatic variable : " + Countable::ToString() ) ;
		

		delete myExample2 ;
		
		LogPlug::info( "Getting counts after first deallocation by delete : " 
				+ Countable::ToString() ) ;
		
		if ( Countable::GetInstanceCount() != 1 )
			throw Ceylan::TestException( 
				"Countable current count should be exactly 1, is "
				+ Ceylan::toString( Countable::GetInstanceCount() ) 
				+ " instead." ) ;
				
		if ( Countable::GetMaximumInstanceCount() != 3 )
			throw Ceylan::TestException( 
				"Countable maximum count should be exactly 3, is "
				+ Ceylan::toString( Countable::GetMaximumInstanceCount() ) 
				+ " instead." ) ;
				
			
		LogPlug::info( "End of Countable class test." ) ;
					

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
