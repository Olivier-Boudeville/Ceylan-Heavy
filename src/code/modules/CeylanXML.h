#ifndef CEYLAN_XML_H_
#define CEYLAN_XML_H_


#include "CeylanException.h"        // for Ceylan::Exception

#include <string>



namespace Ceylan
{


	/**
	 * Gathers all XML-related operations, including parsing. 
	 *
	 *
	 */
	namespace XML
	{
	
	
	
		/**
		 * Exception to be raised when an XML operation failed.
		 *
		 */
		class CEYLAN_DLL XMLException : public Ceylan::Exception
		{
	
			public:
		
				explicit XMLException( const std::string & reason ) throw() ;
				virtual ~XMLException() throw() ;

		} ;
		
		
	
	}


}	


#endif // CEYLAN_XML_H_

