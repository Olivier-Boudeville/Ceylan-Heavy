#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;
using namespace Ceylan::System::Signal ;


#include <iostream>

#include <string>
using std::string ;


bool first_triggered = false ;
bool second_triggered = false ;


void my_first_signal_handler( int signalNumber )
{

	string message = "I am my_first_signal_handler and I received signal #"
		+ Ceylan::toString( signalNumber ) + " : '"
		+ Ceylan::System::Signal::toString( signalNumber ) + "'." ;
		
	LogPlug::info( message ) ;
	
	std::cout << message << std::endl ;
	
	first_triggered = true ;
		
}


void my_second_signal_handler( int signalNumber )
{

	string message = "I am my_second_signal_handler and I received signal #"
		+ Ceylan::toString( signalNumber ) + " : '"
		+ Ceylan::System::Signal::toString( signalNumber ) + "'." ;

	LogPlug::info( message ) ;
	
	std::cout << message << std::endl ;
	
	second_triggered = true ;
				
}


/**
 * Test of Ceylan signal management.
 *
 * @see Ceylan::System::Signal.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


    try
    {


        LogPlug::info( "Testing Ceylan's signal support." ) ;

		if ( ! Features::areSignalsSupported() )
		{
			LogPlug::warning( "Signal support not available, "
				"nothing tested." ) ;
			return Ceylan::ExitSuccess ;
		
		}
		
		
		// No more SEGV !
		setHandler( InvalidMemoryReference, my_first_signal_handler ) ;
        LogPlug::info( "First handler set." ) ;
		
		raise( InvalidMemoryReference ) ;
        LogPlug::info( "InvalidMemoryReference raised." ) ;

		if ( ! first_triggered )
			throw Ceylan::TestException( "Expected InvalidMemoryReference "
				"not raised" ) ;
		
		// No more FPE !
		setHandler( FloatingPointException, my_second_signal_handler ) ;
        LogPlug::info( "Second handler set." ) ;
		
		Ceylan::Float32 a = 1.0f ;
		Ceylan::Float32 b = 0.0f ;
		
		/*
		 * Happy divide by zero (curiously missed on at least some GNU/Linux) :
		 *
		 * The display of c is made so that the compiler does not get rid of
		 * these instructions deemed useless.
		 *
		 */
		Ceylan::Float32 c = a / b ;
        LogPlug::info( "FloatingPointException triggered with c='"
			+ Ceylan::toString( c ) + "'." ) ;
				
		raise( FloatingPointException ) ;
        LogPlug::info( "FloatingPointException raised (second attempt)." ) ;

		if ( ! second_triggered )
			throw Ceylan::TestException( "Expected InvalidMemoryReference "
				"not raised" ) ;

        LogPlug::info( "End of signal test." ) ;


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
