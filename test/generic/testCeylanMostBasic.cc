#include "Ceylan.h"

#include <iostream>
#include <ctime>


/*
 * Most basic test to know whether compile chain is sane.
 *
 * It is linked to the Ceylan library even though it does not use it.
 *
 */

int main( int argc, char * argv[] )
{

	std::cout << "Hello, Ceylan world !" << std::endl ;
	
	std::cerr << "The size of a time_t variable is " << sizeof( time_t )
		<< " bytes, and the size of Ceylan::Sint32 is " 
		<< sizeof( Ceylan::Sint32 ) << " bytes." << std::endl ;

	/*
	 * Not used anymore :
	  
	std::cerr << "The size of a Float80 is " << sizeof( Ceylan::Float80  )
		<< " bytes." << std::endl ;
	 */
	
	std::cerr << "The size of a StringSize variable is " 
		<< sizeof( Ceylan::StringSize )
		<< " bytes, and the size of a ListSize variable is " 
		<< sizeof( Ceylan::ListSize ) << " bytes." << std::endl ;
	
	return Ceylan::ExitSuccess ;
	
}

