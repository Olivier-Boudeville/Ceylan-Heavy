#include "Ceylan.h"

#include <exception>


using namespace Ceylan ;
using namespace Ceylan::Log ;


/**
 * Test of Singleton class.
 *
 * @see Singleton.
 *
 */
int main( int argc,  char * argv[] )
{


	LogHolder myLog( argc, argv ) ;

    try
    {

		LogPlug::info( "Testing Singleton's implementation." ) ;

        LogPlug::info( "I want a Singleton (first time)" ) ;
        Singleton & myFirstSingleton = Singleton::GetSingleton() ;
        LogPlug::info( "I got " + Ceylan::toString( & myFirstSingleton ) ) ;

        LogPlug::info( "Again (second time)" ) ;
        Singleton & mySecondSingleton = Singleton::GetSingleton() ;
        LogPlug::info( "I got " + Ceylan::toString( & mySecondSingleton ) ) ;

        LogPlug::info( "Forcing Singleton deallocation" ) ;
        Singleton::DeleteSingleton() ;
		
        LogPlug::info( "Forcing uselesss Singleton deallocation" ) ;		
        Singleton::DeleteSingleton() ;

        LogPlug::info( "End of Singleton test." ) ;


    }

    catch ( const Ceylan::Exception & e )
    {
        LogPlug::error( "Ceylan exception caught : "
        	 + e.toString( Ceylan::high ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        LogPlug::error( "Standard exception caught : " 
			 + std::string( e.what() ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        LogPlug::error( "Unknown exception caught" ) ;
       	return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}

