#ifndef CEYLAN_HEADER_VERSION_H_
#define CEYLAN_HEADER_VERSION_H_


#include <string>


namespace Ceylan
{


	/**
	 * This is the libtool version of the Ceylan headers, as defined in the
	 * configure step.
	 *
	 * Allows to detect run-time mismatches between the Ceylan headers a 
	 * program or a library was compiled with, and the actual Ceylan library
	 * it is then linked to.
	 *
	 */
	CEYLAN_DLL const std::string actualCeylanHeaderLibtoolVersion 
		= "0.6.0" ;

	
	/*
	 * NEVER check-in this file if there is no more CEYLAN_LIBTOOL_VERSION 
	 * above !
	 *
	 */
	 		
}


#endif // CEYLAN_HEADER_VERSION_H_

