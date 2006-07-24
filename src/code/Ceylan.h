#ifndef CEYLAN_H_
#define CEYLAN_H_


/*
 * This is the Ceylan library, a free portable high-level open-source 
 * general-purpose C++ library.
 *
 * Copyright (C) 2006 Olivier Boudeville.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 * 
 * You can contact the author at olivier.boudeville@esperide.com
 *
 * More informations available in COPYING.LIB and at Ceylan's official website
 * http://ceylan.sourceforge.net
 *
 */


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
 * The following header, coming from the configuration step, is included so 
 * that applications or libraries compiled with this set of header files can
 * remember their built-time Ceylan version.
 *
 * They can then can compare it with respect to the Ceylan version they are
 * executed with, which can be retrieved thanks to a call to Ceylan::GetVersion
 * declared in CeylanUtils.h.
 *
 * More precisely, a Ceylan-using application or library may be compiled 
 * against Ceylan header files of a particular version, and dynamically linked
 * with a Ceylan library of a different version. Even if retro-compatibility is
 * sought after, necessary API changes often break this backward compatibility,
 * which can be determined thanks to interface versions and ages.
 *
 * @see CeylanVersion.h and CeylanUtils.h in the 'generic' module for more
 * informations. 
 *
 */


// Please include below all header files to be exported for Ceylan users.

#include "CeylanGenericIncludes.h"
#include "CeylanInterfacesIncludes.h"
#include "CeylanLogIncludes.h"
#include "CeylanMathsIncludes.h"
#include "CeylanMiddlewareIncludes.h"
#include "CeylanModulesIncludes.h"
#include "CeylanNetworkIncludes.h"
#include "CeylanSystemIncludes.h"


#endif // CEYLAN_H_


