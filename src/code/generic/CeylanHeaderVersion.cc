#include "CeylanHeaderVersion.h"


#ifdef CEYLAN_RUNS_ON_WINDOWS

#include "CeylanConfigForWindows.h"    // for CEYLAN_LIBTOOL_VERSION

/*
 * This is the CeylanHeaderVersion.cc version for Windows.
 *
 * On UNIX platforms, this file will be overwritten by the configure-generated
 * CeylanHeaderVersion.cc
 *
 */

const std::string Ceylan::actualCeylanHeaderLibtoolVersion
    = CEYLAN_LIBTOOL_VERSION ;

#else // CEYLAN_RUNS_ON_WINDOWS

#error "This version of CeylanHeaderVersion.cc is for Windows platforms only."

#endif // CEYLAN_RUNS_ON_WINDOWS
