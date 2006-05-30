#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H

#include "CeylanEnvironmentVariables.h"


#include "CeylanSystem.h"       // for getError, explainError



#include <cstdlib>              // for getenv, putenv



using std::string ;

using namespace Ceylan::System ;



bool Ceylan::System::isEnvironmentVariableSet( const string & variableName )
	throw()
{
	return ( ! getEnvironmentVariable( variableName ).empty() ) ;
}


const string Ceylan::System::getEnvironmentVariable( 
	const string & variableName ) throw()
{


#if CEYLAN_USES_GETENV
	
	// Returns a pointer in the environment, should not be deallocated.
	
	const char * value = ::getenv( variableName.c_str() ) ;
	
	if ( value != 0 )
		return string( value ) ;
		
	return "" ;
	
#else // CEYLAN_USES_GETENV

	throw UtilsException( "Ceylan::System::getEnvironmentVariable : "
		"not available on this platform." ) ;
		
#endif // CEYLAN_USES_GETENV
	
}


void Ceylan::System::setEnvironmentVariable( const string & variableName, 
	const string & variableValue ) throw( UtilsException )
{
	

#if CEYLAN_USES_PUTENV

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
			+ System::explainError( System::getError() ) ) ;
	}
	
	/* 
	 * 'newEnv' not deleted here.
	 * In most cases, depending on the libc, this non-freed memory allocation
	 * is not really a memory leak, see 'putenv' manual ('putenv' may take
	 * ownership of the given char *).
	 *
	 */

#else // CEYLAN_USES_PUTENV

	throw UtilsException( "Ceylan::System::setEnvironmentVariable : "
		"not available on this platform." ) ;
		
#endif // CEYLAN_USES_PUTENV
			
}	


void Ceylan::System::unsetEnvironmentVariable( const string & variableName )
	throw( UtilsException )
{


#if CEYLAN_USES_PUTENV

	// Could reuse too the 'setEnvironmentVariable' method.
	
	char * newEnv = new char[ variableName.size() + 1 ] ;
	
	/*
	 * Maybe to be preferred for C-style compliance : 
	 * char * newEnv = static_cast<char *>( ::malloc( variable.size() + 1 ) ) ;
	 *
	 */
	
	variableName.copy( newEnv, variableName.size() );
	newEnv[ variableName.size() ] = 0 ;
	// or : ::strcpy( newEnv, variableName.c_str() ) ;
	
	if ( ::putenv( newEnv ) != 0 )
	{
		// Maybe newEnv should be freed.
		throw UtilsException( "Ceylan::UnsetEnvironmentVariable : "
			"unable to unset environment variable [" + variableName + "] : " 
			+ System::explainError( System::getError() ) ) ;
	}
	
	/* 
	 * 'newEnv' not deleted here.
	 * In most cases, depending on the libc, this non-freed memory allocation
	 * is not really a memory leak, see 'putenv' manual ('putenv' may take
	 * ownership of the given char *).
	 *
	 */

#else // CEYLAN_USES_PUTENV

	throw UtilsException( "Ceylan::System::unsetEnvironmentVariable : "
		"not available on this platform." ) ;
		
#endif // CEYLAN_USES_PUTENV
			
			
}	

