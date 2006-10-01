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
		bool isEnvironmentVariableSet( const std::string & variableName )
			throw() ;


		/**
		 * Returns the value of specified environment variable.
		 * If the variable was not set, returns an empty string.
		 *
		 * @throw UtilsException if this operation is not supported or
		 * failed.
		 *
		 */
		const std::string getEnvironmentVariable( 
			const std::string & variableName ) throw( UtilsException ) ;
	
	
		/**
		 * Sets specified environment variable to the specified value.
		 *
		 * @throw UtilsException if an error occured, which is very rare,
		 * and should be due to insufficient memory space to allocate 
		 * the new environment.
		 *
		 */
		void setEnvironmentVariable( const std::string & variableName, 
			const std::string & variableValue ) throw( UtilsException ) ;
	
		
		/**
		 * Unsets specified environment variable.
		 *
		 * @throw UtilsException if an error occured, which is very rare.
		 *
		 */
		void unsetEnvironmentVariable( const std::string & variableName ) 
			throw( UtilsException ) ;
	


	}
	
}

#endif // CEYLAN_ENVIRONMENT_VARIABLES_H_
