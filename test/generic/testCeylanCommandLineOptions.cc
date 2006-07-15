#include "Ceylan.h"

#include <exception>
#include <string>
#include <list>


using namespace Ceylan ;
using namespace Ceylan::Log ;



/**
 * Test of Ceylan command line option management.
 *
 * @see Ceylan::parseCommandLineOptions.
 *
 */
int main( int argc,  char * argv[] )
{


	LogHolder myLog( argc, argv ) ;

    try
    {

		LogPlug::info( "Testing command line option management." ) ;

		std::string executableName ;
		std::list<std::string> options ;
		
		Ceylan::parseCommandLineOptions( executableName, options, argc, argv ) ;
		
		LogPlug::info( "Executable name is : '" + executableName 
			+ "', option count is " + Ceylan::toString( options.size() ) 
			+ ", option list is " 
			+ formatStringList( options, /* surroundByTicks */ true ) ) ;
		
		LogPlug::info( "Example of use to parse easily options." ) ;
		
		std::string token ;
		bool tokenEaten ;
		
		
		while ( ! options.empty() )
		{
		
			tokenEaten = false ;
			
			token = options.front() ;
			options.pop_front() ;
			
			if ( token == "--consolePlug" )
			{
				LogPlug::info( "Console plug selected" ) ;
				tokenEaten = true ;
			} else
			if ( token == "--htmlPlug" )
			{
				LogPlug::info( "HTML plug selected" ) ;
				tokenEaten = true ;
			} else
			if ( token == "--classicalPlug" )
			{
				LogPlug::info( "Classical plug selected" ) ;
				tokenEaten = true ;
			} else		
			if ( token == "--countOption" )
			{
				std::string count = options.front() ;
				options.pop_front() ;
				tokenEaten = true ;
				LogPlug::info( "Option with argument selected, argument is : "
					+ count ) ;
			}
			
			if ( ! tokenEaten )
			{
				LogPlug::error( "Unexpected command line argument : "
					+ token ) ;
			}
		
		}
			
        LogPlug::info( "End of command line option management test." ) ;


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

