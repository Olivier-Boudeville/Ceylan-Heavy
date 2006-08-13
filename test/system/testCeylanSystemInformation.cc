#include "Ceylan.h"


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;



using namespace Ceylan::System ;  // for all tested primitives
using namespace Ceylan::Log ;



/**
 * Test of Ceylan primitives for querying system informations.
 *
 * @see Ceylan::System.
 *
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;


    try
    {


        LogPlug::info( "Testing Ceylan's primitives for "
			"querying system informations." ) ;

		
		/*
		 * These features depends on sysinfo, which is not available on all
		 * platforms (ex : it is lacking on vanilla FreeBSD 5.4-RELEASE).
		 *
		 */
		
		try
		{
		
		
			LogPlug::info( "Number of seconds since the system booted : " 
				+ Ceylan::toString( getSecondsSinceSystemBoot() ) + "." ) ;
		
			LogPlug::info( "Current number of running processes : " 
				+ Ceylan::toString( getTotalProcessCount() ) + "." ) ;
		
			LogPlug::info( "Total size of usable main memory, in bytes : " 
				+ Ceylan::toString( getTotalSystemMemorySize() ) + "." ) ;
		
			LogPlug::info( 
				"Size of currently available main memory, in bytes : " 
				+ Ceylan::toString( getFreeSystemMemorySize() ) + "." ) ;
		
			LogPlug::info( "Total size of swap memory, in bytes : " 
				+ Ceylan::toString( getTotalSwapMemorySize() ) + "." ) ;
		
			LogPlug::info( 
				"Size of currently available swap memory, in bytes : " 
				+ Ceylan::toString( getFreeSwapMemorySize() ) + "." ) ;
		
			LogPlug::info( "Total size of usable high memory, in bytes : " 
				+ Ceylan::toString( getTotalHighMemorySize() ) + "." ) ;
		
			LogPlug::info( 
				"Size of currently available high memory, in bytes : " 
				+ Ceylan::toString( getFreeHighMemorySize() ) + "." ) ;
		
			LogPlug::info( "Size of memory currently being shared, in bytes : " 
				+ Ceylan::toString( getSharedMemorySize() ) + "." ) ;
		
			LogPlug::info( 
				"Size of memory currently used by buffers, in bytes : " 
				+ Ceylan::toString( getBuffersMemorySize() ) + "." ) ;
			
		}
		catch ( const SystemException & e )
		{
			LogPlug::error( "Test failed on a non-fatal error : "
				+ e.toString() ) ;
		}
		
		
        LogPlug::info( "End of querying system information test." ) ;


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
