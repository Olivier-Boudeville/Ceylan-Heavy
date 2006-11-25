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
		CEYLAN_DLL const std::string Latin1WithEuroEncoding = "ISO-8859-15" ;
				
				
				
		/// The '<' character.
		CEYLAN_DLL const char LowerThan       = '<' ;
				
		/// The '>' character.
		CEYLAN_DLL const char HigherThan      = '>' ;
			
		/// The '"' character.
		CEYLAN_DLL const char DoubleQuote     = '"' ;
			
		/// The '=' character.
		CEYLAN_DLL const char Equal           = '=' ;
			
		/// The '?' character.
		CEYLAN_DLL const char QuestionMark    = '?' ;
			
		/// The '!' character.
		CEYLAN_DLL const char ExclamationMark = '!' ;
			
		/// The '/' character.
		CEYLAN_DLL const char Slash           = '/' ;
			
			
				
		/**
		 * In XML, ends of line are always coded with only the 
		 * LF (0x0A) character, even on platforms such as Windows
		 * where it is usually CR-LF.
		 *
		 */
		CEYLAN_DLL const char EndOfLine = 0x0A ;
		
	
	}


}	


#endif // CEYLAN_XML_H_

