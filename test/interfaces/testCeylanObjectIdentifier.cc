#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <iostream>
using std::cout ;
using std::cerr ;
using std::endl ;


#include <exception>

#include <string>
using std::string ;




/**
 * Test for ObjectIdentifier implementation.
 *
 * @see Object
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder myLog( argc, argv ) ;


    try
    {

		LogPlug::info( "Testing ObjectIdentifier implementation" ) ;

		ObjectIdentifier id1( /* hostname */ "ceylan.sourceforge.net", 
			/* PID */ 155, /* class name */ "Beholder", 
			/* address */ reinterpret_cast<void *> ( 10000 ) ) ;
					
		LogPlug::info( "First ObjectIdentifier is " + id1.toString() ) ;
		
				 		
		ObjectIdentifier id2( "ceylan.sourceforge.net", 155,
			 "NMangledBeholder14", reinterpret_cast<void *> ( 10000 ) ) ;		
		LogPlug::info( "Second ObjectIdentifier is " + id2.toString() ) ;
		
		ObjectIdentifier id3( "ceylan.sourceforge.net", 155, "Beholder", 
			 reinterpret_cast<void *> ( 22222 ) ) ;		
		LogPlug::info( "Third ObjectIdentifier is " + id3.toString() ) ;
		
		LogPlug::info( "Does first is different but match itself ? " 
			 + Ceylan::toString( id1.differentButMatches( id1 ) ) ) ;
			 			
		LogPlug::info( "Does first is different but match second ? " 
			 + Ceylan::toString( id1.differentButMatches( id2 ) ) ) ;
		LogPlug::info( "Does first is different but match third ? "  
			 + Ceylan::toString( id1.differentButMatches( id3 ) ) ) ;
		
		string validChannelName    =
			 "ceylan.sourceforge.net/PID-3100/Griffon/5000" ;
			 
		string invalidChannelName1 =
			"loggable://ceylan.sourceforge.net/PID-3100/Griffon/5000" ;
			
		string invalidChannelName2 =
			"ceylan.sourceforge.net/PID-3100/Griffon/5000/14" ;
			
		string invalidChannelName3 =
			"ceylan.sourceforge.net/PID-hello/Griffon/5000" ;
		
		
		LogPlug::info( "Generating an ObjectIdentifier "
			"for valid channel name " + validChannelName ) ;
			 
		ObjectIdentifier::generateFromChannelName( validChannelName ) ;
		
		LogPlug::info( "Trying to generate an ObjectIdentifier "
			"from invalid channel name " + invalidChannelName1 ) ;
		
		bool exceptionRaised = false ;
		
		try
		{
		
			ObjectIdentifier::generateFromChannelName( invalidChannelName1 ) ;
			
		} 
		catch( const Identifier::IdentifierException & e )
		{
			LogPlug::info( "Correct, exception raised : " + e.toString() ) ;
			exceptionRaised = true ;
		}
		
		if ( ! exceptionRaised )
		{
			throw TestException( "Invalid channel name '" + invalidChannelName1 
				+ "' did not triggered an exception." ) ;
		}
		else
		{
			exceptionRaised = false ;
		}	
		
				
		try
		{
		
			ObjectIdentifier::generateFromChannelName( invalidChannelName2 ) ;
			
		} 
		catch( const Identifier::IdentifierException & e )
		{
			LogPlug::info( "Correct, exception raised : " + e.toString() ) ;
			exceptionRaised = true ;		
		}
		
		if ( ! exceptionRaised )
		{
			throw TestException( "Invalid channel name '"
				+ invalidChannelName1 + "' did not triggered an exception." ) ;
		}
		else
		{
			exceptionRaised = false ;
		}	
		
				
		try
		{
		
			ObjectIdentifier::generateFromChannelName( invalidChannelName3 ) ;
			
		} 
		catch( const Identifier::IdentifierException & e )
		{

			LogPlug::info( "Correct, exception raised : " + e.toString() ) ;
			exceptionRaised = true ;		
		}
		
		if ( ! exceptionRaised )
		{
			throw TestException( "Invalid channel name '"
				+ invalidChannelName1 + "' did not triggered an exception." ) ;
		}
		else
		{
			exceptionRaised = false ;
		}	
							 	
        LogPlug::info( "End of ObjectIdentifier implementation test." ) ;


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

