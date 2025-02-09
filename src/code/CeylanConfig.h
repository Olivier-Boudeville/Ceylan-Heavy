/*
 * Copyright (C) 2003-2011 Olivier Boudeville
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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef _SRC_CODE_CEYLANCONFIG_H
#define _SRC_CODE_CEYLANCONFIG_H 1



/*
 * This is the CeylanConfig.h version for Windows.
 *
 * On UNIX platforms, this file will be overwritten by the configure-generated
 * CeylanConfig.h
 *
 * @note Do *not* check-in this file if replaced by its UNIX configure-generated
 * counterpart.
 *
 */

#ifdef CEYLAN_RUNS_ON_WINDOWS
#include "CeylanConfigForWindows.h"
#endif // CEYLAN_RUNS_ON_WINDOWS

/* once: _SRC_CODE_CEYLANCONFIG_H */
#endif // _SRC_CODE_CEYLANCONFIG_H
