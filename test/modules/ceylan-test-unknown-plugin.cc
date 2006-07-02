#include "Ceylan.h"  // for definitions : Ceylan::Uint32, etc.

#include <iostream>  // for cout

#include <string>
using std::string ;


/**
 * This is almost the same plugin as ceylan-test-plugin, except it is not 
 * known from testCeylanPlugin when this executable is built.
 *
 * @note Plugin names must be canonized : 'ceylan-test-unknown-plugin.cc'
 * becomes 'ceylan_test_unknown_plugin'.
 *
 *
 
CEYLAN_PLUGIN_DECLARE_NAME(ceylan_test_unknown_plugin)

CEYLAN_PLUGIN_EXPORTED_SYMBOL(my_test_constant)
CEYLAN_PLUGIN_EXPORTED_SYMBOL(my_test_function)

CEYLAN_PLUGIN_INTERNAL_SYMBOL(my_internal_variable)
CEYLAN_PLUGIN_INTERNAL_SYMBOL(my_internal_function)

 */
	
#define my_test_constant ceylan_test_unknown_plugin##_LTX_##my_test_constant

#define my_test_function ceylan_test_unknown_plugin##_LTX_##my_test_function

#define my_internal_variable _##ceylan_test_unknown_plugin##_##my_internal_variable

#define my_internal_function _##ceylan_test_unknown_plugin##_##my_internal_function


// Basic exported informations to be specified by all plugins : 
#define Description ceylan_test_unknown_plugin##_LTX_##Description
#define Url ceylan_test_unknown_plugin##_LTX_##Url
#define Author ceylan_test_unknown_plugin##_LTX_##Author
#define AuthorMail ceylan_test_unknown_plugin##_LTX_##AuthorMail
#define Version ceylan_test_unknown_plugin##_LTX_##Version
#define Licence ceylan_test_unknown_plugin##_LTX_##Licence


/*
 * Common definitions.
 *
 */
	
extern const string Description = 
	"A run-time discovered test plugin for the Ceylan library" ;
	
extern const string Url        = "http://ceylan.sourceforge.net" ;
extern const string Author     = "Olivier Boudeville" ;
extern const string AuthorMail = "olivier.boudeville@esperide.com" ;
extern const string Version    = "0.0.2" ;
extern const string Licence    = "LGPL" ;


extern "C" 
{


	Ceylan::Sint16 my_test_constant = 100 ;
	
	
	Ceylan::Uint32 my_internal_variable = 2 ;
	
	
	void my_internal_function()
	{
		Ceylan::Log::LogPlug::info( 
			"Ah ah I, ceylan-test-unknown-plugin.cc, can use "
				"Ceylan methods as well !" ) ;
	}
	
	
	/// This is a simple C function with C-linkage.
	Ceylan::Uint32 my_test_function( 
		const std::string & message ) /* throw() */
	{
		
		std::cout << "I am my_test_function from ceylan-test-unknown-plugin.cc"
			<< " and I read : '" << message << "'.\n";
			
		my_internal_function() ;
			
		return 20 ;
			
	}
	

}
