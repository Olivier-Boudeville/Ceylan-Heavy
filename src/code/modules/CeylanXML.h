#ifndef CEYLAN_XML_H_
#define CEYLAN_XML_H_


#include "CeylanException.h"        // for Ceylan::Exception

#include <string>
#include <map>



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
		
		
		/// Name of an XML markup.
		typedef std::string MarkupName ;


		/// Name of an XML attribute.
		typedef std::string AttributeName ;
		
		/// Value of an XML attribute.
		typedef std::string AttributeValue ;


		/// Dictionary of name/value pairs.
		typedef std::map<AttributeName,AttributeValue> AttributeMap ;
				
		
		
		/// ISO-8859-15 is the 'Latin-1 with euro' encoding.
		const std::string Latin1WithEuroEncoding = "ISO-8859-15" ;
				
				
				
		/// The '<' character.
		const char LowerThan       = '<' ;
				
		/// The '>' character.
		const char HigherThan      = '>' ;
			
		/// The '"' character.
		const char DoubleQuote     = '"' ;
			
		/// The '=' character.
		const char Equal           = '=' ;
			
		/// The '?' character.
		const char QuestionMark    = '?' ;
			
		/// The '!' character.
		const char ExclamationMark = '!' ;
			
		/// The '/' character.
		const char Slash           = '/' ;
			
			
				
		/**
		 * In XML, ends of line are always coded with only the 
		 * LF (0x0A) character, even on platforms such as Windows
		 * where it is usually CR-LF.
		 *
		 */
		const char EndOfLine = 0x0A ;
		
	
	}


}	


#endif // CEYLAN_XML_H_

