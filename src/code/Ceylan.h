/*
 * This is the Ceylan library, a free portable high-level open-source
 * general-purpose C++ library.
 *
 * Copyright (C) 2003-2013 Olivier Boudeville
 *
 * The Ceylan library is released under a disjunctive bi-license LGPL/GPL
 * (choose the one that you prefer).
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License or the
 * GNU Lesser General Public License, as published by the Free Software
 * Foundation; either version 3 of the Licenses, or (at your option)
 * any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * You can contact the author at olivier (dot) boudeville (at) esperide (dot) com
 *
 * More information available in COPYING.LIB and at Ceylan's official website:
 * http://ceylan.sourceforge.net
 *
 */


#ifndef CEYLAN_H_
#define CEYLAN_H_


/**
 * Main header file for Ceylan Project.
 *
 * @see http://ceylan.sourceforge.net.
 *
 * Add below all include files corresponding to Ceylan modules which are to be
 * explicitly exported in the Ceylan library.
 *
 * Each top-level module may include others modules as well, and so on.
 *
 */


/**
 * A Ceylan-using application or library may be compiled against Ceylan header
 * files of a particular version, and dynamically linked with a Ceylan library
 * of a different version.
 *
 * Even if retro-compatibility is searched after, necessary API changes often
 * break backward compatibility, which can be determined thanks to interface
 * versions and ages, according to Libtool conventions.
 *
 * To know to which Ceylan version a set of installed headers corresponds, the
 * CeylanHeaderVersion.h file (generated at configure time) should be read, for
 * the actualCeylanHeaderLibtoolVersion variable. Including only Ceylan.h is
 * enough to have everything available.
 *
 * The CeylanConfig.h file (generated at configure time) cannot be read for
 * version informations, as it is on purpose not installed, to avoid clashes.
 *
 * To know to which Ceylan version an installed library corresponds, its
 * Ceylan::GetVersion function should be called. It is defined in
 * trunk/src/code/generic/CeylanUtils.h, and retrieves the version information
 * which was defined in its CEYLAN_LIBTOOL_VERSION when it was compiled.
 *
 * On Windows, the CeylanConfig.h file in the sources is used, it includes in
 * turn CeylanConfigForWindows.h which have hardcoded version values.
 *
 * On UNIX, CeylanConfig.h is generated at configure time and overwrites the
 * default CeylanHeaderVersion.h file in the sources.
 *
 * A Ceylan-using program can make use of the CHECK_CEYLAN_VERSIONS (defined in
 * trunk/src/code/generic/CeylanUtils.h) to check automatically whether versions
 * of library and headers match.
 *
 * It the test fails (header and library versions cannot work together), at
 * runtime the issue will be detected thanks to the macro, and a message will
 * explain the situation, ex:
 *
 * Fatal error: Ceylan is performing an emergency shutdown since an abnormal
 * situation occured. Ceylan library version currently linked (Libtool version:
 * current interface number = 0, current revision number = 5, interface age = 0)
 * is not compatible with the one read from the Ceylan header files used to
 * compile this application (Libtool version: current interface number = 1,
 * current revision number = 6, interface age = 0), aborting.
 *
 * @see CeylanConfig.h, CeylanConfigForWindows.h, and generic/CeylanUtils.h in
 * the 'generic' module for more informations, including the macro definition.
 *
 * @see test/generic/testCeylanUtils.cc for the macro use.
 *
 */

// Only defined and set to 1 when cross-compiling for the Nintendo DS:
#if defined(CEYLAN_ARCH_NINTENDO_DS) && CEYLAN_ARCH_NINTENDO_DS == 1

// So that DS-specific defines are readily available to the user:
#include "CeylanConfigForNintendoDS.h"

#endif // defined(CEYLAN_ARCH_NINTENDO_DS) && CEYLAN_ARCH_NINTENDO_DS == 1



// Include below all header files to be exported for Ceylan users.

#include "CeylanGenericIncludes.h"
#include "CeylanInterfacesIncludes.h"
#include "CeylanLogIncludes.h"
#include "CeylanMathsIncludes.h"
#include "CeylanMiddlewareIncludes.h"
#include "CeylanModulesIncludes.h"
#include "CeylanNetworkIncludes.h"
#include "CeylanSystemIncludes.h"



#endif // CEYLAN_H_
