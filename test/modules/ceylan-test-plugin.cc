#include "Ceylan.h"  // for definitions : Ceylan::Uint32, etc.

#include <iostream>  // for cout

#include <string>
using std::string ;

//#define my_test_constant ceylan_test_plugin_LTX_my_test_constant
//#define my_test_function ceylan_test_plugin_LTX_my_test_function


extern "C" 
{

	Ceylan::Sint16 my_test_constant = 123 ;
	
	
	/// This is a simple C function with C-linkage.
	Ceylan::Uint32 my_test_function( const std::string & message ) /* throw() */
	{

		/*message = string( "I am my_test_plugin and and I read : '" ) + message
			+ string( "'." ) ; 
		*/
		
		std::cout << "I am my_test_function and and I read : '"
			<< message << "'.\n";
			
		Ceylan::Log::LogPlug::info( 
			"Ah ah i can use Ceylan methods as well !" ) ;
			
		return 17 ;
			
	}

}
