#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Features ;


#include <iostream>	   // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/**
 * Test of optional features management.
 *
 * @see CeylanFeatures.h
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;	


	try
	{


		LogPlug::info( "Testing optional features management." ) ;

		// Testing an example of feature :
		
		if ( ! Features::areRegularExpressionsSupported() )
		{
			
			bool caught = false ;
			
			try
			{
				checkForSupportedFeatures( Features::RegularExpressions ) ;
			}
			catch( const FeatureNotAvailableException & e )
			{
				LogPlug::info( "Correctly caught expected exception : " 
					+ e.toString() ) ;
				caught = true ;
			}
			
			if ( ! caught )
				throw Ceylan::TestException( 
					"Expected a feature exception, nothing thrown." ) ;
		
		}
		else
		{
			checkForSupportedFeatures( Features::RegularExpressions ) ;
		}	
		
		LogPlug::info( Features::describeAvailableFeatures() ) ;
			
		LogPlug::info( "End of feature test." ) ;
		
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
