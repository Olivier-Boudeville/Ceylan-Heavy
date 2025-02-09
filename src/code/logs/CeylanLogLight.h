/*
 * Copyright (C) 2003-2013 Olivier Boudeville
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


#ifndef CEYLAN_LOG_LIGHT_H_
#define CEYLAN_LOG_LIGHT_H_


/*
 * This header is private and not meant to be installed. Hence it can depend on
 * CeylanConfig.h.
 *
 */

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for CEYLAN_DEBUG_LOG
#endif // CEYLAN_USES_CONFIG_H



/**
 * Very basic log facility made to debug ... the full Log system! (bootstrap)
 *
 */

#if CEYLAN_DEBUG_LOG


	#if CEYLAN_ARCH_NINTENDO_DS


		#ifdef CEYLAN_RUNS_ON_ARM7

			// No log available on the ARM7 yet:
			#define CEYLAN_LOG(messageString) ;

		#else // CEYLAN_RUNS_ON_ARM7

			// For iprintf and al:
			#include <stdio.h>

			#define CEYLAN_LOG(messageString) ::iprintf( "[Debug] %s\n", ((messageString).c_str()) )


		#endif // CEYLAN_RUNS_ON_ARM7


	#else // CEYLAN_ARCH_NINTENDO_DS


	#include <iostream>

	#define CEYLAN_LOG(message) std::cerr << "[LogLight] " << message << std::endl << std::flush

	#endif // CEYLAN_ARCH_NINTENDO_DS


#else // CEYLAN_DEBUG_LOG

	#define CEYLAN_LOG(message)

#endif // CEYLAN_DEBUG_LOG



#endif // CEYLAN_LOG_LIGHT_H_
