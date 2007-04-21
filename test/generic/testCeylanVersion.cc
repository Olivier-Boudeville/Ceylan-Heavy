#include "Ceylan.h"


#include <string>
using std::string ;

#include <list>
using std::list ;

#include <exception>



using namespace Ceylan ;
using namespace Ceylan::Log ;


/**
 * Test of Ceylan management of version information.
 *
 * @see CeylanUtils.h for CHECK_CEYLAN_VERSIONS definition
 *
 * @see testCeylanUtils.cc for the test of actual header/library versions.
 *
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;

    try
    {

		LogPlug::info( "Testing Ceylan versions." ) ;
		
		LogPlug::info( "Testing basic versions." ) ;

		Ceylan::Version v1( 0, 0, 0 ) ;
		Ceylan::Version v2( 1, 2, 3 ) ;
		Ceylan::Version v3( 1, 2, 3 ) ;
		
		string versionText = "12.34.243" ;
		Ceylan::Version v4( versionText ) ;
		
		if ( v1 < v2 )
		{
			LogPlug::info( "Version number " + v1.toString() 
				+ " is strictly smaller than "
				+ v2.toString() + ", that's correct." ) ;
		}
		else
			throw Ceylan::TestException( v1.toString() 
				+ " is incorrectly declared as strictly smaller than " 
				+ v2.toString() + "." ) ;
		
		
		if ( v2 < v1 )
			throw Ceylan::TestException( v2.toString() 
				+ " is incorrectly declared as strictly smaller than " 
				+ v1.toString() + "." ) ;
		else
		{
			LogPlug::info( "Version number " + v2.toString() 
				+ " is not strictly smaller than "
				+ v1.toString() + ", that's correct." ) ;
		}

		
		if ( v3 < v2 )
			throw Ceylan::TestException( v3.toString() 
				+ " is incorrectly declared as strictly smaller than " 
				+ v2.toString() + "." ) ;
		else		
		{
			LogPlug::info( "Version number " + v3.toString() 
				+ " is not strictly smaller than "
				+ v2.toString() + ", that's correct." ) ;
		}

		if ( v3 == v2 )
		{
			LogPlug::info( "Version number " + v3.toString() + " is equal to "
				+ v2.toString() + ", that's correct." ) ;
		}
		else
			throw Ceylan::TestException( v3.toString() + " and " + v2.toString()
				+ " are incorrectly declared as different." ) ;

		if ( v1 == v2 )
		{
			throw Ceylan::TestException( v1.toString() + " and " + v2.toString()
				+ " are incorrectly declared as equal." ) ;
		}
		else
			LogPlug::info( "Version number " + v1.toString() 
				+ " is not equal to "
				+ v3.toString() + ", that's correct." ) ;


		LogPlug::info( "Version corresponding to " + versionText + " is '"
			+ v4.toString() + "'." ) ;




		LogPlug::info( "Testing Libtool versions." ) ;


		LogPlug::info( "Testing Basic and Libtool version mixing." ) ;



		Ceylan::LibtoolVersion lv1( 0, 0, 0 ) ;
		Ceylan::LibtoolVersion lv2( 3, 2, 2 ) ;
		
		bool raised = false ;
		
		try
		{
			Ceylan::LibtoolVersion lv3( 1, 2, 3 ) ;
		}
		catch( const Ceylan::VersionException )
		{
			raised = true ;
		}
		
		if ( ! raised )
			throw Ceylan::Exception( 
				"No Libtool version 1.2.3 should exist." ) ;
		else
			LogPlug::info( "Libtool age in constructor correctly enforced." ) ;


		raised = false ;
		try
		{
			v1.isCompatibleWith( lv1 ) ;
		}
		catch( const Ceylan::VersionException )
		{
			raised = true ;
		}
		
		if ( ! raised )
			throw Ceylan::Exception( 
				"Basic versions and Libtool versions should not be allowed "
				"to be mixed." ) ;
		else
			LogPlug::info( "Non-mixing of basic versions and Libtool versions "
				"correctly enforced." ) ;
				
				
		raised = false ;
		try
		{
			lv1.isCompatibleWith( v1 ) ;
		}
		catch( const Ceylan::VersionException )
		{
			raised = true ;
		}
		
		if ( ! raised )
			throw Ceylan::Exception( 
				"Libtool versions and Basic versions should not be allowed "
				"to be mixed." ) ;
		else
			LogPlug::info( "Non-mixing of Libtool versions and basic versions "
				"correctly enforced." ) ;



		LogPlug::info( "Testing Libtool versions age compatibility." ) ;

		if ( lv2.isCompatibleWith( lv1 ) )
			throw Ceylan::Exception( lv2.toString() 
				+ " should not be deemed compatible with " + lv1.toString() ) ;
		else
			LogPlug::info( lv2.toString() + " is indeed not compatible with " 
				+ lv1.toString() ) ;
				
		Ceylan::LibtoolVersion lv4( 6, 2, 3 ) ;
		if ( ! lv4.isCompatibleWith( lv2 ) )
			throw Ceylan::Exception( lv4.toString() 
				+ " should be deemed compatible with " + lv2.toString() ) ;
		else
			LogPlug::info( lv4.toString() + " is indeed compatible with " 
				+ lv2.toString() ) ;
		
							
        LogPlug::info( "End of Ceylan version test." ) ;

 
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
