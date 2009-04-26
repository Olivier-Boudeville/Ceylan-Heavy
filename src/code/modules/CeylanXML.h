/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


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
		
				explicit XMLException( const std::string & reason ) ;
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

