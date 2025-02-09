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


#ifndef CEYLAN_ENVIRONMENT_VARIABLES_H_
#define CEYLAN_ENVIRONMENT_VARIABLES_H_


#include "CeylanUtils.h"    // for UtilsException

#include <string>



namespace Ceylan
{



	namespace System
	{



		// Some environment variables facilities.


		/// Returns whether the specified environment variable is set.
		CEYLAN_DLL bool isEnvironmentVariableSet(
			const std::string & variableName ) ;



		/**
		 * Returns the value of specified environment variable.
		 * If the variable was not set, returns an empty string.
		 *
		 * @throw UtilsException if this operation is not supported or failed.
		 *
		 */
		CEYLAN_DLL const std::string getEnvironmentVariable(
			const std::string & variableName ) ;



		/**
		 * Sets specified environment variable to the specified value.
		 *
		 * @throw UtilsException if an error occured, which is very rare, and
		 * should be due to insufficient memory space to allocate the new
		 * environment.
		 *
		 */
		CEYLAN_DLL void setEnvironmentVariable(
			const std::string & variableName,
			const std::string & variableValue ) ;



		/**
		 * Unsets specified environment variable.
		 *
		 * @throw UtilsException if an error occured, which is very rare.
		 *
		 */
		CEYLAN_DLL void unsetEnvironmentVariable(
			const std::string & variableName ) ;


	}


}



#endif // CEYLAN_ENVIRONMENT_VARIABLES_H_
