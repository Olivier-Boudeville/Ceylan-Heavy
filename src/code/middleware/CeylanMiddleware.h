#ifndef CEYLAN_MIDDLEWARE_H_
#define CEYLAN_MIDDLEWARE_H_


#include "CeylanException.h"        // for inheritance of Exception


#include <string>



namespace Ceylan
{	
	
	
	namespace Middleware
	{


		
		/**
		 * Exception to be raised whenever a middleware issue arises.
		 *
		 */
		class MiddlewareException : public Ceylan::Exception 
		{
		
			public:
			
			
				MiddlewareException( const std::string & message ) throw() ;
				virtual ~MiddlewareException() throw() ;
				
		
		} ;
		
	}
	
}		



#endif // CEYLAN_MIDDLEWARE_H_
