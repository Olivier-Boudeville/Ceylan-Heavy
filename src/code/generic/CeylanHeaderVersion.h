#ifndef CEYLAN_HEADER_VERSION_H_
#define CEYLAN_HEADER_VERSION_H_

#include <string>



/*
 * This file is dedicated to the Windows version, as on UNIX it is overwritten
 * by the configure-time generated one.
 *
 */
 
 

// Allowed because on Windows (hence not installed) :
#include "CeylanConfig.h"  // for  CEYLAN_LIBTOOL_VERSION

 
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
		= CEYLAN_LIBTOOL_VERSION ;
		
}


#endif // CEYLAN_HEADER_VERSION_H_
