#include "CeylanHeaderVersion.h"


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfigForWindows.h"        // for CEYLAN_LIBTOOL_VERSION
#endif // CEYLAN_USES_CONFIG_H


// The DS has CEYLAN_LIBTOOL_VERSION defined on the command line.


/*
 * This file exists only for the non-UNIX builds, as on
 * UNIX the (generated) CeylanHeaderVersion.h header file declares
 * and defines actualCeylanHeaderLibtoolVersion with no
 * multiple definitions when linking.
 *
 */
const std::string Ceylan::actualCeylanHeaderLibtoolVersion 
	= CEYLAN_LIBTOOL_VERSION ;

