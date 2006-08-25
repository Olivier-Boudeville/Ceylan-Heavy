#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Maths ;
using namespace Ceylan::System ;


#include <string>
using std::string ;




/**
 * Test of Ceylan memory stream management.
 *
 * @see Ceylan::System::MemoryStream.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


    try
    {


        LogPlug::info( "Testing Ceylan's memory stream support." ) ;

		Size streamSize = 20 ;
		
		MemoryStream myMemoryStream( streamSize /* bytes */ ) ;
		
		Ceylan::Uint8 toWrite = 0 ;
					
		// First, fill up the buffer and check everything fit in it :
		for ( Size s = 0; s < streamSize; s++ )
		{
			LogPlug::info( "Fill test : writing '" 
					+ Ceylan::toNumericalString( toWrite ) + "'." ) ;
			myMemoryStream.writeUint8( toWrite ) ;
			toWrite++ ;
		}
		
		Ceylan::Uint8 toRead  = 0 ;
		Ceylan::Uint8 readValue ;
			
		// Empties the buffer :	
		for ( Size s = 0; s < streamSize; s++ )
		{
			readValue = myMemoryStream.readUint8() ;
			if ( readValue != toRead )
				throw TestException( "Fill test : expected to read '" 
						+ Ceylan::toNumericalString( toRead ) + "', read '"
						+ Ceylan::toNumericalString( readValue ) + "'." ) ;
			else
				LogPlug::info( "Fill test : read '" 
					+ Ceylan::toNumericalString( toRead ) + "'." ) ;
								
			toRead++ ;			
		}
			
			
		LogPlug::info( "Fill test succeeded, now trying disordered test." ) ;
		
		
		// Second, disordered read/write operations :
		Ceylan::Uint32 io_count = 0 ;
		
		Ceylan::Uint32 inBuffer = 0 ;
		
		bool shouldRead ;
		

		// Can draw 0 or 1 :
		Random::WhiteNoiseGenerator myGenerator( 0, 2 ) ;
		
		// Read/write in disorder and wrap-around the full buffer :
		
		while ( io_count < 10000 )
		{
		
			if ( ( io_count % 50 ) == 0 )
				LogPlug::debug( "State of memory stream : " 
					+ myMemoryStream.toString() ) ;
				
				
			if ( inBuffer == 0 )
			{
				// Order to write (only possibility) :
				LogPlug::debug( "Forced writing." ) ;
				shouldRead = false ;		
			}
			else if ( inBuffer == streamSize )
			{
			
				// Can only read :
				LogPlug::debug( "Forced reading." ) ;
				shouldRead = true ;
			}
			else
			{ 
			
				// Can write or read :
			
				if ( myGenerator.getNewValue() == 0 ) 
				{
					// 0 : write
					shouldRead = false ;
				}
				else
				{
					// 1 : read
					shouldRead = true ;
				
				}
			}
			
			if ( shouldRead )
			{
			
				readValue = myMemoryStream.readUint8() ;
				if ( readValue != toRead )
					throw TestException( "Disordered test : expected to read '" 
						+ Ceylan::toNumericalString( toRead ) + "', read '"
						+ Ceylan::toNumericalString( readValue ) + "'." ) ;
				toRead++ ;
				inBuffer-- ;		
			
			}
			else 
			{
				myMemoryStream.writeUint8( toWrite ) ;
				toWrite++ ;
				inBuffer++ ;
			}
		
			io_count++ ;
		
		}
		
        LogPlug::info( "Disordered test succeeded." ) ;
			
        LogPlug::info( "End of memory stream test." ) ;


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
