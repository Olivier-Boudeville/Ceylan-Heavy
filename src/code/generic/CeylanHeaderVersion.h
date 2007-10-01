#ifndef CEYLAN_HEADER_VERSION_H_
#define CEYLAN_HEADER_VERSION_H_

#include <string>



/*
 * This file is dedicated to the Windows version, as on UNIX it is overwritten
 * by the configure-time generated one.
 *
 * @note This Windows version needs CeylanHeaderVersion.cc, whereas the UNIX
 * one does not.
 *
 */
 
 
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
	 * @note Cannot declare here :
	 * 'extern CEYLAN_DLL const std::string actualCeylanHeaderLibtoolVersion 
	 * 	= CEYLAN_LIBTOOL_VERSION ;' because with Visual C++ it leads to
	 * multiple definitions for actualCeylanHeaderLibtoolVersion.
	 *
	 */
	extern CEYLAN_DLL const std::string actualCeylanHeaderLibtoolVersion ;
		
}


#endif // CEYLAN_HEADER_VERSION_H_
