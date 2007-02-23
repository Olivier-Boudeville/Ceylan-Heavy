#include "Ceylan.h"
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <exception>

#include <iostream>
using std::cerr ;
using std::endl ;

#include <string>
using std::string ;

#include <list>
using std::list ;



/**
 * Test of File locator services.
 *
 * @see FileLocator, File, Directory.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder myLog( argc, argv ) ;


    try
    {
	

        LogPlug::info( "Testing File locator implementation." ) ;
		
		FileLocator myLocator ;
		
        LogPlug::info( "Creating a new file locator : " 
			+ myLocator.toString() ) ;
		
		LogPlug::info( "Current working directory is " 
			+ Directory::GetCurrentWorkingDirectoryName() ) ;

		// When run from build tree :
		string firstPath  = "../src/code" ;
		
		myLocator.addPath( firstPath ) ;
        LogPlug::info( "Adding a first UNIX path : " + myLocator.toString() ) ;
		
		// When run against an install :		
		string secondPath = "/usr/local/include/Ceylan" ;

		myLocator.addPath( secondPath ) ;
        LogPlug::info( "Adding a second UNIX path : " + myLocator.toString() ) ;
		
		myLocator.addPath( secondPath ) ;
        LogPlug::info( "Adding this second path twice : " 
			+ myLocator.toString() ) ;

		string thirdPath = "../code" ;
		myLocator.addPath( thirdPath ) ;
        LogPlug::info( "Adding a path for Windows tests : " 
			+ myLocator.toString() ) ;

		string toFind = "Ceylan.h" ;
		string found  ;
		
		
		// If run from one of the trees, should succeed.
		try
		{
			found = myLocator.find( toFind ) ;
		}
		catch ( const FileLocatorException & e )
		{
			throw Ceylan::TestException( "File locator test failed : " 
				+ e.toString() ) ;
		}
		
		LogPlug::info( "Testing if '" + toFind + "' is found : yes, in '"
				+ found + "'." ) ;
				 
		string notToFind = "Ceylan-does-not-exist.txt" ;
		bool notFound = false ;
		
		try 
		{
 			found = myLocator.find( notToFind ) ;
		}	
		catch ( const FileLocatorException & e )
		{
			notFound = true ;
			LogPlug::info( "Testing if '" + notToFind 
				+ "' is found : no, correct : " 	+ e.toString() ) ;
		}
		
		if ( ! notFound )
			throw Ceylan::TestException( 
				"File locator test failed : file '" + notToFind 
				+ "' abnormally found through " + myLocator.toString() ) ;
		
		
		myLocator.removePath( "Hello" ) ;
		LogPlug::info( "Removing a non-registered path : " 
			+  myLocator.toString() ) ;
		
		myLocator.removePath( firstPath ) ;
		LogPlug::info( "Removing first path : " 
			+  myLocator.toString() ) ;
	
	
		// Just a test, does not indicates whether 
		try
		{
			found = myLocator.find( toFind ) ;
		}
		catch( const FileLocatorException & e )
		{
		
		}
			
		if ( found.empty() )
			LogPlug::info( "Testing if '" + toFind + "' is found : no, for " 
				+ myLocator.toString()   ) ;
		else
			LogPlug::info( "Testing if '" + toFind + "' is found : yes, for " 
				+ myLocator.toString()   ) ;
			
		
        LogPlug::info( "End of File locator test." ) ;


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

