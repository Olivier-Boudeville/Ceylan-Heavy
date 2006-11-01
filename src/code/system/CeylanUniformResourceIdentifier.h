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
		 * Example : the protocol separator in
		 * 'http://osdl.sourceforge.net' is '://'. It is the usual one.
		 *
		 */
		extern const std::string ProtocolSeparator  ;
				 
		 
		 
		/**
		 * Returns the protocol name of specified URI, if any.
		 *
		 * @note returns an empty string if no protocol name is found.
		 *
		 * @example : 'http://osdl.sourceforge.net' returns 'http'
		 *
		 * @see getEmbeddedURI, ProtocolSeparator
		 *
		 */
		CEYLAN_DLL const std::string getProtocolName( 
			const std::string & fullURI ) throw() ;  

		 
		/**
		 * Returns the protocol name of specified URI, if any.
		 *
		 * @note returns an empty string if no protocol name is found.
		 *
		 * @example : 'http://osdl.sourceforge.net' returns 'http'
		 *
		 * @see getEmbeddedURI, ProtocolSeparator
		 *
		 */
		CEYLAN_DLL const std::string getProtocolName( 
				const std::string & fullURI, 
		 		const std::string & protocolSeparator ) 
			throw() ;  



		/**
		 * Returns the URI after having removed the protocol informations, 
		 * i.e. the protocol name and the protocol separator.
		 *
		 * @example : 'http://ceylan.sourceforge.net' returns 
		 *'ceylan.sourceforge.net'
		 *
		 * @see getProtocolName, ProtocolSeparator
		 *
		 */
		CEYLAN_DLL const std::string getEmbeddedURI( 
			const std::string & fullURI ) throw() ;  
	
	
		/**
		 * Returns the URI after having removed the protocol informations, 
		 * i.e. the protocol name and the protocol separator.
		 *
		 * @example : 'http://ceylan.sourceforge.net' returns 
		 *'ceylan.sourceforge.net'
		 *
		 * @see getProtocolName, ProtocolSeparator
		 *
		 */
		CEYLAN_DLL const std::string getEmbeddedURI( 
				const std::string & fullURI,
		 		const std::string & protocolSeparator ) 
			throw() ;  
	
	}
	
}	


#endif // CEYLAN_UNIFORM_RESOURCE_IDENTIFIER_H_
