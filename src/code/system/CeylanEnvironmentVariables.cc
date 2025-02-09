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


#include "CeylanEnvironmentVariables.h"

#include "CeylanSystem.h"      // for getError, explainError

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H



#include <cstdlib>             // for getenv, putenv, _dupenv_s


using std::string ;

using namespace Ceylan::System ;



bool Ceylan::System::isEnvironmentVariableSet( const string & variableName )
{

	return ( ! getEnvironmentVariable( variableName ).empty() ) ;

}



const string Ceylan::System::getEnvironmentVariable(
	const string & variableName )
{

#ifdef CEYLAN_USES_GETENV

	// Returns a pointer in the environment, should not be deallocated.

	const char * value = ::getenv( variableName.c_str() ) ;

	if ( value != 0 )
		return string( value ) ;

	return "" ;

#else // CEYLAN_USES_GETENV

#ifdef CEYLAN_USES__DUPENV_S

	char * returnedString ;
	size_t returnedLength ;

	if ( ::_dupenv_s( &returnedString, &returnedLength,
			variableName.c_str() ) != 0 )
		throw UtilsException(
			"Ceylan::System::getEnvironmentVariable failed: "
			+ System::explainError() ) ;
	if ( returnedString == 0 )
	{

		// Not found:
		return "" ;

	}
	else
	{

		string res( returnedString ) ;
		::free( returnedString ) ;
		return res ;

	}

#else // CEYLAN_USES__DUPENV_S

	throw UtilsException( "Ceylan::System::getEnvironmentVariable: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__DUPENV_S

#endif // CEYLAN_USES_GETENV

}



void Ceylan::System::setEnvironmentVariable( const string & variableName,
	const string & variableValue )
{

	// setenv could be used as well.

#ifdef CEYLAN_USES_PUTENV

	const string envString = variableName + "=" + variableValue ;
	char * newEnv = new char[ envString.size() + 1 ] ;

	/*
	 * Maybe to be preferred for C-style compliance:
	 * char * newEnv = static_cast<char *>( ::malloc( envString.size() + 1 ) ) ;
	 *
	 */

	envString.copy( newEnv, envString.size() );
	newEnv[ envString.size() ] = 0 ;
	// or: ::strcpy( newEnv, envString.c_str() ) ;

	// ::setenv is less widely supported than ::putenv
	if ( ::putenv( newEnv ) != 0 )
	{
		// Maybe newEnv should be freed.
		throw Ceylan::UtilsException( "Ceylan::SetEnvironmentVariable: "
			"unable to set the value of environment variable ["
			+ variableName + "] to [" + variableValue + "]: "
			+ System::explainError() ) ;
	}

	/*
	 * 'newEnv' not deleted here.  In most cases, depending on the libc, this
	 * non-freed memory allocation is not really a memory leak, see 'putenv'
	 * manual ('putenv' may take ownership of the given char *).
	 *
	 * So we had to add a Valgrind suppression for that.
	 *
	 */
	//delete [] newEnv ;

#else // CEYLAN_USES_PUTENV

#ifdef CEYLAN_USES__PUTENV_S


	if ( ::_putenv_s( variableName.c_str(), variableValue.c_str() ) != 0 )
	{
		throw Ceylan::UtilsException( "Ceylan::SetEnvironmentVariable: "
			"unable to set the value of environment variable ["
			+ variableName + "] to [" + variableValue + "]: "
			+ System::explainError() ) ;
	}

#else // CEYLAN_USES__PUTENV_S


	throw UtilsException( "Ceylan::System::setEnvironmentVariable: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__PUTENV_S

#endif // CEYLAN_USES_PUTENV

}



void Ceylan::System::unsetEnvironmentVariable( const string & variableName )
{

#ifdef CEYLAN_USES_UNSETENV

	::unsetenv( variableName.c_str() ) ;

#else // CEYLAN_USES_UNSETENV

	setEnvironmentVariable( variableName, "" ) ;

#endif // CEYLAN_USES_UNSETENV

}
