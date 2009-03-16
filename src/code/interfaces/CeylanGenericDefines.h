#ifndef CEYLAN_GENERIC_DEFINES_H_
#define CEYLAN_GENERIC_DEFINES_H_


#include "CeylanException.h"  // for inheritance


#include <string>



// Allows to avoid complex header dependencies.


namespace Ceylan
{


	class GenericMVCException : public Ceylan::Exception
	{
	
		public:
		
			
			GenericMVCException( const std::string & message ) throw() :
				Ceylan::Exception( message )
			{
			
			}
			
			
			virtual ~GenericMVCException() throw()
			{
			
			}
			
	} ;
	

}	


#endif // CEYLAN_GENERIC_DEFINES_H_

