#ifndef CEYLAN_NETWORK_H_
#define CEYLAN_NETWORK_H_


#include "CeylanException.h"          // for inheritance
#include "CeylanTextDisplayable.h"    // for inheritance
#include "CeylanTypes.h"              // for Ceylan::Uint16


#include <string>
#include <list>


namespace Ceylan
{

	
	/**
	 * Basic network management, which is an optional Ceylan feature.
	 *
	 * @see Ceylan::URI for Uniform Resource Identifiers facilities.
	 *
	 * @note IPv6 is currently not supported.
	 *
	 */
	namespace Network
	{
	
	
		/// Exception class for network services.
		class CEYLAN_DLL NetworkException : public Ceylan::Exception
		{
			public:
				explicit NetworkException( const std::string & message ) 
					throw() ;
				virtual ~NetworkException() throw() ;
		} ;
		
		
		
		/**
		 * Records the known types of network addresses.
		 *
		 */
		enum NetworkAddressType
		{
		
			// Known also as AF_INET :
			IPv4,
			
			// Known also as AF_INET6 :			
			IPv6

		
		} ;
		
		
		// HostDNSEntry records IP addresses :
		class IPAddress ;
		
		
		/**
		 * Describes the DNS entry corresponding to a specified host.
		 *
		 * @note Do not expect this class to be reentrant at all.
		 *
		 */
		class CEYLAN_DLL HostDNSEntry : public Ceylan::TextDisplayable
		{
		
		
			/**
			 * Opaque handle for forward-declared but undefined struct
			 * pointer to system host entry, used to avoid including 
			 * system-specific headers such as netdb.h.
			 *
			 * Otherwise the API exposed by Ceylan would depend on these
			 * headers, then on a config.h that should then be installed 
			 * but may clash with others, and so on.
			 *
			 */
			struct SystemSpecificHostEntry ;
			
			
			public:
			
				
				/**
				 * Creates a DNS description for specified host name.
				 *
				 * @param hostName the name of host to create a DNS 
				 * description for. 
				 *
				 * @throw NetworkException if the operation is not 
				 * supported on the platform, or is supported but failed.
				 *
				 * @see RFC 1884 for the description of IPv6 addresses.
				 *
				 */
				explicit HostDNSEntry( const std::string & hostName ) 
					throw( NetworkException ) ;
		
		
				/**
				 * Creates a DNS description for specified host name.
				 *
				 * @param ip an IPv4 (or, in the future, IPv6) instance.
				 *
				 * @throw NetworkException if the operation is not 
				 * supported on the platform, or is supported but failed.
				 *
				 * @see RFC 1884 for the description of IPv6 addresses.
				 *
				 * @see IPAddressvFour
				 *
				 */
				explicit HostDNSEntry( const IPAddress & ip ) 
					throw( NetworkException ) ;
		
		
				/// Basic virtual destructor.
				virtual ~HostDNSEntry() throw() ;
		
		
				/**
				 * Returns the official name of this host.
				 *
				 * @throw NetworkException if the operation failed.
				 *
				 */
				std::string getOfficialHostName() const 
					throw( NetworkException ) ;
				
				
				/**
				 * Returns the list of alias for this host.
				 *
				 * @note Ownership of the list is transferred to the caller,
				 * who will have to deallocate it when finished with it.
				 *
				 */
				std::list<std::string> & getAliasList() const throw() ;
				
				
				/**
				 * Returns the type of the network address of this entry. 
				 *
				 * @note It is actually the type of the official address,
				 * since there can be several addresses of several types
				 * in the address (alias) list.
				 *
				 */
				NetworkAddressType getAddressType() const throw() ;
				
				
				/**
				 * Returns the list of known network address of this entry. 
				 *
				 * @note Ownership of the list is transferred to the caller,
				 * who will have to deallocate it when finished with it.
				 *
				 */
				std::list<IPAddress *> & getAddresses() const throw() ;
				
				
	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
	             */
	            virtual const std::string toString( 
						Ceylan::VerbosityLevels level = Ceylan::high )
					const throw() ;


				/// The maximum number of characters of an hostname.
				static const Ceylan::Uint16 HostNameMaxLength ;



			protected:
		
			
				/**
				 * Helper method shared by constructors.
				 *
				 */
				virtual void manageHostEntry() throw( NetworkException ) ;
				
				
				/**
				 * The system-specific DNS host entry being used internally.
				 *
				 */				 
				SystemSpecificHostEntry * _internalEntry ;



			private:
				
			
		
				/**
				 * Copy constructor made private to ensure that it will 
				 * never be called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */			 
				HostDNSEntry( const HostDNSEntry & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				HostDNSEntry & operator = ( const HostDNSEntry & source )
					throw() ;
				
							
		} ;
		
		
		
		
		// Local host section.
		
		
		
		/// Returns local host's name, not including the domain name.
		CEYLAN_DLL const std::string getLocalHostName() 
			throw( NetworkException ) ;
		
		/// Sets local host's name.
		CEYLAN_DLL void setLocalHostName( const std::string & newHostName )
			throw( NetworkException ) ;
			
		
		/**
		 * Returns local host's domain name.
		 *
		 * @return a string containing the domain name, or an empty string 
		 * if no appropriate name is found. For example, on Linux systems, 
		 * if NIS is not in use, this method reads the content of
		 * /proc/sys/kernel/domainname, which might be "(none)" or an empty
		 * string : in both of these last cases, an empty string is returned.
		 *
		 * @throw NetworkException if the operation failed.
		 *
		 */
		CEYLAN_DLL const std::string getLocalHostDomainName() 
			throw( NetworkException ) ;
		
		
		/// Sets local host's domain name.
		CEYLAN_DLL void setLocalHostDomainName( 
			const std::string & newHostName ) throw( NetworkException ) ;
					

		/**
		 * Returns most precise available local host name.
		 *
		 * Tries to gather most useful information about host name, 
		 * including FQDN.
		 *
		 * @note Reverse name look-up thanks to IP address could be
		 * performed too.
		 *
		 */
		CEYLAN_DLL const std::string getMostPreciseLocalHostName() 
			throw( NetworkException ) ;




		// Section for all hosts.
		
		
		
		/**
		 * Returns the fully qualified domain name (FQDN) of specified host.
		 *
		 * @param ip an IPAddress instance, be it IPv4 or IPv6.
		 *
		 * @throw NetworkException if the search or the conversion failed.
		 *
		 */
		CEYLAN_DLL const std::string getFQDNFromIP( const IPAddress & ip )
			throw( NetworkException ) ;


		/**
		 * Returns the fully qualified domain name (FQDN) of specified host.
		 *
		 * @param ipString a string describing the IPv4 address, such as
		 * "82.225.152.215".
		 *
		 */
		CEYLAN_DLL const std::string getFQDNFromIPv4( 
			const std::string & ipString ) throw( NetworkException ) ;


		/**
		 * Returns the fully qualified domain name (FQDN) of specified host.
		 *
		 * @param hostName the name of host to create a DNS description for. 
		 * It is a host name (ex : "esperide.com"), not a stringified IP
		 * address.
		 *
		 */
		CEYLAN_DLL const std::string getFQDNFromHostname( 
			const std::string & hostname ) throw( NetworkException ) ;


		/**
		 * Returns the fully qualified domain name (FQDN) from specified DNS
		 * entry instance.
		 *
		 * @param entry a previously created HostDNSEntry.
		 *
		 */
		CEYLAN_DLL const std::string getFQDNFromDNSEntry( 
			const HostDNSEntry & entry ) throw( NetworkException ) ;


			
		/**
		 * Returns whether specified string is a valid host name.
		 *
		 * @note Always returns true if no regular expression feature is 
		 * available.
		 *
		 * @see Ceylan::Features::RegularExpressions
		 *
		 */
		CEYLAN_DLL bool isAValidHostName( const std::string & hostnameString ) 
			throw() ;
		

		/**
		 * The numerical error code that may be returned by specific
		 * network primitives, namely Windows socket ones.
		 *
		 */
		typedef int SocketError ;

		
		/**
		 * Returns a string describing the error associated with the
		 * specified error code.
		 *
		 * @param errorCode the socket error that should be translated.
		 *
		 */
		CEYLAN_DLL std::string interpretSocketError( SocketError errorCode ) 
			throw() ;

		/// Returns the latest socket error.
		CEYLAN_DLL SocketError getSocketError() throw() ;

		/// Returns the diagnosis string corresponding to latest socket error.
		CEYLAN_DLL std::string explainSocketError() throw() ;



		// Insert here real network-specific code.

		
		/*
		 * Using here the only configuration-specific preprocessor symbol that
		 * may exist in Ceylan public headers :
		 *
		 */
#ifdef CEYLAN_RUNS_ON_WINDOWS


		/**
		 * The network manager takes care of the initialization and shutdown
		 * of the network subsystem.
		 *
		 * This is done automatically, thanks to a static instance.
		 *
		 */
		class CEYLAN_DLL NetworkManager
		{

			public:

				/// Initializes the network subsystem.
				NetworkManager() throw( NetworkException ) ;

				/// Closes the network subsystem.
				~NetworkManager() throw() ;


			private:


				/// The overall singleton-like manager instance.
				static NetworkManager _Manager ;

				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				NetworkManager( const NetworkManager & source ) 
					throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				NetworkManager & operator = ( 
					const NetworkManager & source )	throw() ;


		} ;

		
#endif // CEYLAN_RUNS_ON_WINDOWS



	}
	
}	



#endif // CEYLAN_NETWORK_H_

