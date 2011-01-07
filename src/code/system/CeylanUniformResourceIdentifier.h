/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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


#ifndef CEYLAN_UNIFORM_RESOURCE_IDENTIFIER_H_
#define CEYLAN_UNIFORM_RESOURCE_IDENTIFIER_H_



#include <string>



namespace Ceylan
{



	/**
	 * Allows to handle Uniform Resource Identifiers.
	 *
	 * Not in the Ceylan::Network namespace since this one is one is a feature
	 * that can be disabled, whereas URI are generic base facilities, used for
	 * example with Loggabe instances.
	 * 
	 */
	namespace URI
	{
	
	
	
		/**
		 * The protocol separator, between the protocol name and the
		 * embedded URI.
		 *
		 * Example: the protocol separator in
		 * 'http://osdl.sourceforge.net' is '://'. It is the usual one.
		 *
		 */
		extern const std::string ProtocolSeparator  ;
				 
		 
		 
		/**
		 * Returns the protocol name of specified URI, if any.
		 *
		 * @note returns an empty string if no protocol name is found.
		 *
		 * @example: 'http://osdl.sourceforge.net' returns 'http'
		 *
		 * @see getEmbeddedURI, ProtocolSeparator
		 *
		 */
		CEYLAN_DLL const std::string getProtocolName( 
			const std::string & fullURI ) ;  


		 
		/**
		 * Returns the protocol name of specified URI, if any.
		 *
		 * @note returns an empty string if no protocol name is found.
		 *
		 * @example: 'http://osdl.sourceforge.net' returns 'http'
		 *
		 * @see getEmbeddedURI, ProtocolSeparator
		 *
		 */
		CEYLAN_DLL const std::string getProtocolName( 
			const std::string & fullURI, 
		 	const std::string & protocolSeparator ) ;  



		/**
		 * Returns the URI after having removed the protocol informations, 
		 * i.e. the protocol name and the protocol separator.
		 *
		 * @example: 'http://ceylan.sourceforge.net' returns 
		 *'ceylan.sourceforge.net'
		 *
		 * @see getProtocolName, ProtocolSeparator
		 *
		 */
		CEYLAN_DLL const std::string getEmbeddedURI( 
			const std::string & fullURI ) ;  
	
	
	
		/**
		 * Returns the URI after having removed the protocol informations, 
		 * i.e. the protocol name and the protocol separator.
		 *
		 * @example: 'http://ceylan.sourceforge.net' returns 
		 *'ceylan.sourceforge.net'
		 *
		 * @see getProtocolName, ProtocolSeparator
		 *
		 */
		CEYLAN_DLL const std::string getEmbeddedURI( 
			const std::string & fullURI,
		 	const std::string & protocolSeparator ) ;  
			
	
	}
	
	
}	



#endif // CEYLAN_UNIFORM_RESOURCE_IDENTIFIER_H_

