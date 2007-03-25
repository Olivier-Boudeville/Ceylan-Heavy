#include "CeylanHeaderVersion.h"

// Allowed because on Windows (hence not installed) :
#include "CeylanConfig.h"  // for  CEYLAN_LIBTOOL_VERSION

/*
 * This file exists only for the Windows build, as on
 * UNIX the CeylanHeaderVersion.h header file declares
 * and defines actualCeylanHeaderLibtoolVersion with no
 * multiple definitions when linking.
 *
 */
const std::string Ceylan::actualCeylanHeaderLibtoolVersion 
	= CEYLAN_LIBTOOL_VERSION ;

