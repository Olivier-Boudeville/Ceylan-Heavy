#include "Ceylan.h"

#include <exception>


using namespace Ceylan ;
using namespace Ceylan::Log ;


using namespace std ;


/**
 * Test of Locale support.
 *
 * @see Singleton.
 *
 */
int main( int argc,  char * argv[] )
{


	LogHolder myLog( argc, argv ) ;

    try
    {

		LogPlug::info( "Testing Locale implementation." ) ;

        LogPlug::info( "Creating a locale." ) ;
		
		LocalizationSettings myLocaleSettings ;
		
        LogPlug::info( "Displaying it: " + myLocaleSettings.toString() ) ;
		
     	LogPlug::info( "Declaring support for french." ) ;
		myLocaleSettings.addSupportedLocale( "french" ) ;
		
        LogPlug::info( "Displaying it: " + myLocaleSettings.toString() ) ;
		
		// Not even known:
		string locale = "vulcan" ;
		
		if ( myLocaleSettings.isSupported( locale ) )
			throw Ceylan::TestException( "Locale '" + locale 
				+ "' should not be declared as supported." ) ;
				
		// Not supported here:
		locale = "german" ;
			
		if ( myLocaleSettings.isSupported( locale ) )
			throw Ceylan::TestException( "Locale '" + locale 
				+ "' should not be declared as supported." ) ;
				
		locale = "french" ;

		if ( ! myLocaleSettings.isSupported( locale ) )
			throw Ceylan::TestException( "Locale '" + locale 
				+ "' should be declared as supported." ) ;
				
        LogPlug::info( "End of Locale test." ) ;


    }

    catch ( const Ceylan::Exception & e )
    {
        LogPlug::error( "Ceylan exception caught: "
        	 + e.toString( Ceylan::high ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        LogPlug::error( "Standard exception caught: " 
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

