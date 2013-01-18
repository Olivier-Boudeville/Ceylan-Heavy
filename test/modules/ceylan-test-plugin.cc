/* 
 * Copyright (C) 2003-2013 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "Ceylan.h"  // for definitions : Ceylan::Uint32, etc.

#include <iostream>  // for cout

#include <string>
using std::string ;


/**
 * These defines allows to implement the same API in all plugins, while 
 * preventing name clashes when more than one plugin is loaded at once.
 *
 * From the executable, each symbol can be retrieved in turn by appending
 * the common symbol name to the plugin name.
 *
 * The process does not seem to be easily automatized.
 *
 */
//CEYLAN_PLUGIN_DECLARE_NAME(ceylan_test_plugin)
#define plugin_name ceylan_test_plugin

//CEYLAN_PLUGIN_EXPORTED_SYMBOL(my_test_constant)
/*
 * Does not work : 
 * #define my_test_constant plugin_name##_LTX_##my_test_constant
 *
 */
#define my_test_constant ceylan_test_plugin##_LTX_##my_test_constant

//CEYLAN_PLUGIN_EXPORTED_SYMBOL(my_test_function)
#define my_test_function ceylan_test_plugin##_LTX_##my_test_function


//CEYLAN_PLUGIN_INTERNAL_SYMBOL(my_internal_variable)
#define my_internal_variable _##ceylan_test_plugin##_##my_internal_variable

//CEYLAN_PLUGIN_INTERNAL_SYMBOL(my_internal_function)
#define my_internal_function _##ceylan_test_plugin##_##my_internal_function


// Basic exported informations to be specified by all plugins : 
#define Description ceylan_test_plugin##_LTX_##Description
#define Url ceylan_test_plugin##_LTX_##Url
#define Author ceylan_test_plugin##_LTX_##Author
#define AuthorMail ceylan_test_plugin##_LTX_##AuthorMail
#define Version ceylan_test_plugin##_LTX_##Version
#define Licence ceylan_test_plugin##_LTX_##Licence


/*
 * Note : if the strings are not declared with 'extern', they will not be
 * exported symbols and the loader will not find them.
 * However, if they were int or char *, they would be exported !
 *
 * With 'nm --demangle ceylan-test-plugin.so' one can see :
 * 000026a4 b ceylan_test_plugin_LTX_Description
 * 00002690 D ceylan_test_plugin_LTX_my_test_constant
 * 00000cbc T ceylan_test_plugin_LTX_my_test_function
 *
 * b : lower-case hence not exported.
 *
 */



/*
 * Common definitions.
 *
 */
 	
extern const string Description = 
	"A built-time declared test plugin for the Ceylan library" ;	

extern const string Url 	   = "http://ceylan.sourceforge.net" ;
extern const string Author     = "Olivier Boudeville" ;   
extern const string AuthorMail = "olivier.boudeville@esperide.com" ;
extern const string Version    = "0.0.1" ;
extern const string Licence    = "LGPL" ;


extern "C" 
{


	Ceylan::Sint16 my_test_constant = 123 ;

	Ceylan::Uint32 my_internal_variable = 3 ;

	void my_internal_function()
	{
		Ceylan::Log::LogPlug::info( 
			"Oh-oh-oh I, ceylan-test-plugin.cc, can use "
				"Ceylan methods as well !" ) ;
	}
	
	
	/// This is a simple C function with C-linkage.
	Ceylan::Uint32 my_test_function( const std::string & message ) /* throw() */
	{

		
		std::cout << "I am my_test_function from ceylan-test-plugin.cc"
			<< " and I read : '" << message << "'.\n";
			
		my_internal_function() ;
			
		return 17 ;
			
	}
	
	
	
	
	

}
