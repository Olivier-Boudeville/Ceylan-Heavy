#include "Ceylan.h"

#include <iostream>

#include <string>
using std::string ;

#include <list>
using std::list ;

#include <exception>


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;




/**
 * Test for the libfat support offered by the Ceylan library on Nintendo DS.
 *
 * Test coverage is far less complete than for usual computer platforms though.
 *
 */
int main( int argc, char * argv[] )
{

	 

	LogHolder myLog( argc, argv ) ;

	
    try
    {
	
		
		LogPlug::info( "Test of Ceylan support for libfat filesystems" ) ;
		
		LogPlug::info( "Opening current directory." ) ;
		
		Directory & d = Directory::Open() ; 

		LogPlug::info( "Current directory opened." ) ;

		list<string> subDirectories, files, otherEntries ;
		
		LogPlug::info( "Listing its content." ) ;
		d.getSortedEntries( subDirectories, files, otherEntries ) ;
		
		
		if ( subDirectories.empty() )
		{
		
			LogPlug::info( "  + no subdirectory found" ) ;
			
		}	
		else
		{
		
			LogPlug::info( "  + subdirectories:" ) ;
			
			for ( list<string>::const_iterator it = subDirectories.begin();
					it != subDirectories.end(); it++ )
				LogPlug::info( "     * " + (*it) ) ;
		}	
			  
		
		if ( files.empty() )
		{
		
			LogPlug::info( "  + no file found" ) ;
			
		}	
		else
		{
		
			LogPlug::info( "  + files:" ) ;
			
			for ( list<string>::const_iterator it = files.begin();
					it != files.end(); it++ )
				LogPlug::info( "     * " + (*it) ) ;
		}	
			
			  
		if ( otherEntries.empty() )
		{
		
			LogPlug::info( "  + no other entry found" ) ;
			
		}	
		else
		{
		
			LogPlug::info( "  + other entries:" ) ;
			
			for ( list<string>::const_iterator it = otherEntries.begin();
					it != otherEntries.end(); it++ )
				LogPlug::info( "     * " + (*it) ) ;
		}	
		
		// LogHolder out of scope: log browser triggered.
			
    }
   
    catch ( const Ceylan::Exception & e )
    {
	
        LogPlug::error( "Ceylan exception caught: " 
			+ e.toString( Ceylan::high ) ) ;
			
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
	
        LogPlug::error( string( "Standard exception caught: " ) + e.what() ) ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
	
        LogPlug::error( "Unknown exception caught" ) ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
