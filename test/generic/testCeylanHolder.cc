#include "Ceylan.h"


#include <exception>
#include <string>


using namespace Ceylan ;
using namespace Ceylan::Log ;



/**
 * Test class for holder template.
 *
 * Displays many informations about its life cycle in its own log channel.
 *
 */
class TestHolder: public Ceylan::Object
{

	public:
	
	
		TestHolder() throw()
		{
			send( "Created a TestHolder." ) ;
			Count++ ;
		}
	
	
		int sayHello( Ceylan::Uint8 aNumber )
		{
		
			send( "Hello !" ) ;
			return aNumber ;
			
		}
		
		
		~TestHolder() throw()
		{
			send( "Destroying a TestHolder." ) ;
			Count-- ;
		}
		
		
		static GetCount() throw()
		{
			return Count ;
		}	
		
		
	private:
	
	
		static Ceylan::Uint8 Count = 0 ;
		
		
		TestHolder( const TestHolder & source ) throw() ;
			
		TestHolder & operator = ( const TestHolder & source ) throw() ;
		
} ;


#include <iostream>



/**
 * Test of holder facility.
 *
 * @see CeylanHolder.h (Holder)
 *	
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;


    try
    {


		LogPlug::info( "Testing Holder template." ) ;

		LogPlug::info( "Before Holder creation, holder count is "
			+ Ceylan::toNumericalString( TestHolder::GetCount() ) ) ;
		
		if ( TestHolder::GetCount() != 0 )
			throw Ceylan::TestException( "Wrong initial test holder count." ) ;
			
		Holder<TestHolder> myHolder( & new TestHolder() ) ;

		LogPlug::info( "After first Holder creation, holder count is "
			+ Ceylan::toNumericalString( TestHolder::GetCount() ) ) ;

		if ( TestHolder::GetCount() != 1 )
			throw Ceylan::TestException( 
				"Wrong test holder count after first Holder creation." ) ;
		
		if ( myHolder->sayHello( 17 ) != 17 )
			throw Ceylan::TestException( 
				"A call to held object returned a wrong result." ) ;
		
		
		// This block will force myOtherHolder deallocation.
		{
		
			Holder<TestHolder> myOtherHolder( & new TestHolder() ) ;

			if ( TestHolder::GetCount() != 2 )
				throw Ceylan::TestException( 
					"Wrong test holder count after second Holder creation." ) ;
		
			LogPlug::info( "Forcing deallocation of second Holder" ) ;
													
		}

		if ( TestHolder::GetCount() != 1 )
			throw Ceylan::TestException( 
				"Wrong test holder count after first Holder deletion." ) ;

		
        LogPlug::info( "End of Holder pointer test." ) ;


	}
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught: "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught: " 
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
