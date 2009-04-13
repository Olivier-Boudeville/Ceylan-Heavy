/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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

