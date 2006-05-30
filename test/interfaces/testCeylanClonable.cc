#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/// Test for Clonable interface, with use of multiple inheritance.
class ClonableExample : public Ceylan::Clonable, 
	public Ceylan::TextDisplayable
{

    public:


		explicit ClonableExample( Ceylan::Uint8 state = 0 ) throw() :
			_state( state )
		{
		
		}
	
	
		virtual ~ClonableExample() throw() 
		{
		
		}		
			
			
		/**
		 * @note Returns 'Clonable' instead of 'ClonableExample' since 
		 * adjusting pointers for covariant returns is not implemented yet
		 * with gcc (3.3.5).
		 *
		 */
		virtual Clonable & clone() const throw( ClonableException ) 
		{
			return * new ClonableExample( _state ) ;
		
		}
			
		
		virtual const std::string toString( 
			Ceylan::VerbosityLevels level = Ceylan::high ) const throw() 
		{
			return "ClonableExample instance, with state equal to " 
				+ Ceylan::toNumericalString( _state ) + "." ;
		}
					
		
	private:
	
		Ceylan::Uint8 _state ;
		
		
} ;



/**
 * Test for Clonable implementation.
 *
 * @see Clonable
 *
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;
	

    try
    {
	
		LogPlug::info( "Starting testing Clonable class" ) ;

		ClonableExample myExample( 5 ) ;
		LogPlug::info( "Getting initial state for Clonable : " 
			+ myExample.toString() ) ;
				
		ClonableExample * clonePointer = 
			dynamic_cast<ClonableExample *>( & myExample.clone() ) ;
		
		if ( clonePointer == 0 )
			throw Ceylan::TestException( "Clone test failed, "
				"clone was not actually a ClonableExample" ) ;			
			
		ClonableExample	& myClone = * clonePointer ;
		
		LogPlug::info( "Getting state of Clone : " + myClone.toString() ) ;
		
		
		if ( myExample.toString() != myClone.toString() )
			throw Ceylan::TestException( "Clone test failed, "
				"clone has not the same state as the original" ) ;			
				
		LogPlug::info( "Clone has the right state." ) ; 	
		
		if ( & myExample == & myClone )
			throw Ceylan::TestException( "Clone test failed, "
				"clone is the same object as the original" ) ;			

		LogPlug::info( "Clone is not the same object as the original." ) ; 	
				
		// Ownership was transferred to the caller (the test itself) :
		delete & myClone ;
			
		LogPlug::info( "End of Clonable class test." ) ;
					

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
