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
 * Basic tests of the Ceylan library on Nintendo DS.
 *
 * Test coverage is far less complete than for usual computer platforms though.
 *
 */
int main( int argc, char * argv[] )
{
	 

	LogHolder myLog( argc, argv ) ;

	
    try
    {
				
		
		LogPlug::info( "Testing some basic facilities of the DS" ) ;		

		InitializeIPC() ;
		
		LogPlug::info( "Hi, user '" + Ceylan::System::GetUserName() + "'" ) ;

		switch ( Ceylan::System::GetBatteryStatus() )	
		{
		
			case WellCharged:
				LogPlug::info( "DS batteries are well charged." ) ;
				break ;
		
			case AlmostEmpty:
				LogPlug::info( "DS batteries are almost empty." ) ;
				break ;
		
			default:
				throw TestException( "Unexpected battery status returned." ) ;
				break ;
			
		}
		
		
		switch ( Ceylan::System::GetDSType() )	
		{
		
			case DSFat:
				LogPlug::info( "This is a DS Fat (initial model)." ) ;
				break ;
		
			case DSLite:
				LogPlug::info( "This is a DS Lite." ) ;
				break ;
		
			default:
				throw TestException( "Unexpected DS type returned." ) ;
				break ;
			
		}

		LogPlug::info( "There are " 
			+ Ceylan::toString( System::getFreeSystemMemorySize() ) 
			+ " free bytes in RAM (main memory), whereas there are "
			+ Ceylan::toString( System::getUsedSystemMemorySize() )
			+ " used bytes in the same RAM. Thus its total size (the sum) is " 
			+ Ceylan::toString( System::getFreeSystemMemorySize() 
				+ System::getUsedSystemMemorySize() ) 
			+ " bytes, to be compared with the total system memory size, "
			+ Ceylan::toString( System::getTotalSystemMemorySize() ) 
			+ " bytes. Difference, "
			+ Ceylan::toString( System::getTotalSystemMemorySize()
				- ( System::getFreeSystemMemorySize() 
					+ System::getUsedSystemMemorySize() ) )
			+ " bytes, should be mainly the ROM itself." ) ;
							
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

	LogPlug::info( "Exit on success (no error)" ) ;
	
    return Ceylan::ExitSuccess ;

}
