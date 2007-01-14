#include "CeylanEnvironmentVariables.h"

#include "CeylanSystem.h"      // for getError, explainError

#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H



#include <cstdlib>             // for getenv, putenv, _dupenv_s


using std::string ;

using namespace Ceylan::System ;



bool Ceylan::System::isEnvironmentVariableSet( const string & variableName )
	throw()
{
	return ( ! getEnvironmentVariable( variableName ).empty() ) ;
}


const string Ceylan::System::getEnvironmentVariable( 
	const string & variableName ) throw( UtilsException )
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
			"Ceylan::System::getEnvironmentVariable failed : "
			+ System::explainError() ) ;
	if ( returnedString == 0 )
	{
		// Not found :
		return "" ;
	}
	else
	{

		string res( returnedString ) ;
		::free( returnedString ) ;
		return res ;

	}

#else // CEYLAN_USES__DUPENV_S

	throw UtilsException( "Ceylan::System::getEnvironmentVariable : "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__DUPENV_S

#endif // CEYLAN_USES_GETENV
	
}


void Ceylan::System::setEnvironmentVariable( const string & variableName, 
	const string & variableValue ) throw( UtilsException )
{
	
	// setenv could be used as well.
	
#ifdef CEYLAN_USES_PUTENV

	const string envString = variableName + "=" + variableValue ;
	char * newEnv = new char[ envString.size() + 1 ] ;
	
	/*
	 * Maybe to be preferred for C-style compliance : 
	 * char * newEnv = static_cast<char *>( ::malloc( envString.size() + 1 ) ) ;
	 *
	 */
	
	envString.copy( newEnv, envString.size() );
	newEnv[ envString.size() ] = 0 ;
	// or : ::strcpy( newEnv, envString.c_str() ) ;
	
	if ( ::putenv( newEnv ) != 0 )
	{
		// Maybe newEnv should be freed.
		throw Ceylan::UtilsException( "Ceylan::SetEnvironmentVariable : "
			"unable to set the value of environment variable [" 
			+ variableName + "] to [" + variableValue + "] : "
			+ System::explainError() ) ;
	}
	
	/* 
	 * 'newEnv' not deleted here.
	 * In most cases, depending on the libc, this non-freed memory allocation
	 * is not really a memory leak, see 'putenv' manual ('putenv' may take
	 * ownership of the given char *).
	 *
	 */

#else // CEYLAN_USES_PUTENV

#ifdef CEYLAN_USES__PUTENV_S


	if ( ::_putenv_s( variableName.c_str(), variableValue.c_str() ) != 0 )
	{
		throw Ceylan::UtilsException( "Ceylan::SetEnvironmentVariable : "
			"unable to set the value of environment variable [" 
			+ variableName + "] to [" + variableValue + "] : "
			+ System::explainError() ) ;
	}
	
#else // CEYLAN_USES__PUTENV_S


	throw UtilsException( "Ceylan::System::setEnvironmentVariable : "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__PUTENV_S
		
#endif // CEYLAN_USES_PUTENV
			
}	


void Ceylan::System::unsetEnvironmentVariable( const string & variableName )
	throw( UtilsException )
{

#ifdef CEYLAN_USES_UNSETENV

	::unsetenv( variableName.c_str() ) ;
	
#else // CEYLAN_USES_UNSETENV

	setEnvironmentVariable( variableName, "" ) ;
		
#endif // CEYLAN_USES_UNSETENV		
			
}	

