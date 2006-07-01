#include "Ceylan.h"  // for definitions : Ceylan::Uint32, etc.

#include <iostream>  // for cout


extern "C" 
{


	Ceylan::Sint16 my_test_constant = 123 ;
	
	
	/// This is a simple C function with C-linkage.
	Ceylan::Uint32 my_test_function( const std::string & message ) throw() 
	{

		message = "I am my_test_plugin and and I read : '" + message
			+ "'." ; q
			
		return 17 ;
			
	}

}
