#include "Ceylan.h"


#include <exception>
#include <string>


using namespace Ceylan ;
using namespace Ceylan::Log ;


/**
 * Test class for counter pointer.
 *
 * Displays many informations about its life cycle in its own log channel.
 *
 */
class TestCountedPointer : public Ceylan::Object
{

	public:
	
	
		TestCountedPointer() throw()
		{
			send( "Created a TestCountedPointer." ) ;
		}
	
	
		~TestCountedPointer() throw()
		{
			send( "Destroying a TestCountedPointer." ) ;
		}
		
		
		TestCountedPointer & operator = ( const TestCountedPointer & source )
			throw()
		{
			send( "Assigning a TestCountedPointer." ) ;
			return * this ; 			
		}	
		
		
	private:
	
		TestCountedPointer( const TestCountedPointer & source ) throw() ;
			
		
} ;


/// Pass by value.
Ceylan::ReferenceCount aFunction( counted_ptr<TestCountedPointer> counted )
{

	LogPlug::info( "aFunction : argument reference count is : " 
		+ counted.toString() + "." ) ;
	
	return counted.getReferenceCount() ;
	
}


#include <iostream>



/**
 * Test of counted pointer facility.
 *
 * @see CeylanCountedPointer.h (counted_ptr)
 *	
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;


    try
    {


		LogPlug::info( "Testing counted pointer implementation." ) ;

		LogPlug::info( "Normally, exactly one TestCountedPointer "
			"instance should be created." ) ;

		counted_ptr<TestCountedPointer> myCounted = new TestCountedPointer() ;
		LogPlug::info( "Displaying first counted pointer : " 
			+ myCounted.toString() ) ;

		
		if ( myCounted.getReferenceCount() != 1 )
			throw Ceylan::TestException( 
				"#1 : expected reference count equal to one." ) ;
		
		// This block will force myOtherCounted deallocation.
		{
		
			counted_ptr<TestCountedPointer> myOtherCounted = myCounted ;
		
			LogPlug::info( 
				"Displaying first counted pointer after assignment : " 
				+ myCounted.toString() ) ;
				
			if ( myCounted.getReferenceCount() != 2 )
				throw Ceylan::TestException( 
					"#2 : expected reference count equal to two, "
					"got instead " 
					+ Ceylan::toString( myCounted.getReferenceCount() ) 
					+ "." ) ;

			LogPlug::info( "Displaying second counted pointer : " 
				+ myOtherCounted.toString() ) ;
				
			if ( myOtherCounted.getReferenceCount() != 2 )
				throw Ceylan::TestException( 
					"#3 : expected reference count equal to two, " 	
					"got instead " 
					+ Ceylan::toString( myOtherCounted.getReferenceCount() ) 
					+ "." ) ;

			Ceylan::ReferenceCount result = aFunction( myOtherCounted ) ;
			if ( result != 3 )
				throw Ceylan::TestException( 
					"#4 : expected reference count equal to three, "
					"got instead " + Ceylan::toString( result ) + "." ) ;
									
		}

		Ceylan::ReferenceCount result = aFunction( myCounted ) ;
		
		if ( result != 2 )
			throw Ceylan::TestException( 
				"#5 : expected reference count equal to two, "
				"got instead " + Ceylan::toString( result ) + " )." ) ;

		if ( myCounted.getReferenceCount() != 1 )
				throw Ceylan::TestException( 
					"#6 : expected reference count equal to one, "	
					"got instead " 
					+ Ceylan::toString( myCounted.getReferenceCount() ) 
					+ "." ) ;

		
		
		LogPlug::info( "Displaying final counted pointer : " 
			+ myCounted.toString() ) ;
		
        LogPlug::info( "End of counted pointer test." ) ;


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
