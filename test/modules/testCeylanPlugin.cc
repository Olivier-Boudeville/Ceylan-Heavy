#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <exception>

#include <string>
using std::string ;

#include <iostream>



extern "C" 
{

	Ceylan::Uint16 functionFromExecutable( Ceylan::Uint8 arg, 
		std::string & outString ) throw() ;
}


/**
 * This is an example function that the plugin will try to call directly.
 * Note the in/out parameter 'aString', passed by reference. 
 *
 *
 */
Ceylan::Uint16 functionFromExecutable( Ceylan::Uint8 anInt, 
	std::string & aString ) throw()	
{

	LogPlug::info( "functionFromExecutable called with anInt = '" 
		+ Ceylan::toString( anInt ) + "', aString = '" + aString + "'." ) ;
		
	return 112 ;

}
	

typedef Ceylan::Uint32 TestFunction( const std::string & message ) 
	/* throw() */ ;


/**
 * Test for Module handling services.
 *
 * @see Module
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;	


	try
	{


		LogPlug::info( "Testing Plugin implementation." ) ;


		if ( ! Features::arePluginsSupported() )
		{
			LogPlug::warning( "Plugin support not available, "
				"nothing tested." ) ;
			return Ceylan::ExitSuccess ;
		}
		
		
		Plugin myPlugin( "ceylan-test-plugin" ) ;
		
		Ceylan::Sint16 expectedConstant = 123 ;
		
		Ceylan::Sint16 readConstant = 
			* reinterpret_cast<Ceylan::Sint16 *>(
				myPlugin.getSymbol( "my_test_constant" ) ) ;

		if ( readConstant != expectedConstant )
			throw Ceylan::TestException( 
				"Reading a constant from plugin failed : expecting '"
				+ Ceylan::toString( expectedConstant ) + "', read '"
				+ Ceylan::toString( readConstant ) + "'." ) ;
		else
			LogPlug::info( "Successfully read a constant from plugin." ) ;
	
	
		//TestFunction * readFunction = 0 ;
		/*
		 * How to transtype void * to a function pointer ?
		 * error: ISO C++ forbids casting between pointer-to-function and
		 * pointer-to-object

			
		TestFunction * readFunction = 
			reinterpret_cast<TestFunction *>(
				myPlugin.getSymbol( "my_test_function" ) ) ;

		 */
		
		TestFunction * readFunction = (TestFunction *)
				myPlugin.getSymbol( "my_test_function" ) ;
		
		string fromMain = "I am a string set from main()" ;		

		Ceylan::Uint32 expectedReturnValue = 17 ;
		Ceylan::Uint32 readReturnValue = readFunction( fromMain ) ;
		
		if ( readReturnValue != expectedReturnValue )
			throw Ceylan::TestException( 
				"Executing a function from plugin failed : "
				"expecting return value '"
				+ Ceylan::toString( expectedReturnValue ) + "', read '"
				+ Ceylan::toString( readReturnValue ) + "'." ) ;
		else
			LogPlug::info( "Successfully executed a function from plugin." ) ;
	
		const string otherPluginName = "ceylan-test-unknown-plugin" ;
		
		LogPlug::info( "Now testing with a plugin which was unknown at "
			"build time for this test" ) ;
			
		Ceylan::Sint16 expectedOtherConstant = 100 ;
		
		Plugin myOtherPlugin( "ceylan-test-unknown-plugin" ) ;
			
		readConstant = 
			* reinterpret_cast<Ceylan::Sint16 *>(
				myOtherPlugin.getSymbol( "my_test_other_constant" ) ) ;

		if ( readConstant != expectedOtherConstant )
			throw Ceylan::TestException( 
				"Reading a constant from an unknown plugin failed : expecting '"
				+ Ceylan::toString( expectedOtherConstant ) + "', read '"
				+ Ceylan::toString( readConstant ) + "'." ) ;
		else
			LogPlug::info( "Successfully read a constant "
				"from an unknown plugin." ) ;
				
		readFunction = (TestFunction *)
				myOtherPlugin.getSymbol( "my_test_other_function" ) ;

		Ceylan::Uint32 expectedOtherReturnValue = 20 ;
		readReturnValue = readFunction( fromMain ) ;
		
		if ( readReturnValue != expectedOtherReturnValue )
			throw Ceylan::TestException( 
				"Executing a function from an unknown plugin failed : "
				"expecting return value '"
				+ Ceylan::toString( expectedOtherReturnValue ) + "', read '"
				+ Ceylan::toString( readReturnValue ) + "'." ) ;
		else
			LogPlug::info( 
				"Successfully executed a function from an unknown plugin." ) ;
				
		LogPlug::info( "End of Plugin test." ) ;
		
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
