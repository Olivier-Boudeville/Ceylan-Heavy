/*
 * Copyright (C) 2003-2013 Olivier Boudeville
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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#include "CeylanNetwork.h"

#include "CeylanOperators.h"
#include "CeylanLogPlug.h"              // for Log plug
#include "CeylanSystem.h"               // for explainError
#include "CeylanStringUtils.h"          // for formatStringList
#include "CeylanRegularExpression.h"    // for RegExp
#include "CeylanIPAddressvFour.h"       // for IPAddressvFour
#include "CeylanUtils.h"                // for checkpoint



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"               // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H




extern "C"
{

// for sethostname, gethostname, setdomainname, getdomainname:
#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_SYS_UTSNAME_H
#include <sys/utsname.h>      // for uname
#endif // CEYLAN_USES_SYS_UTSNAME_H

#ifdef CEYLAN_USES_STRING_H
#include <string.h>           // for memcpy
#endif // CEYLAN_USES_STRING_H

#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>       // for inet_ntoa
#endif // CEYLAN_USES_SYS_SOCKET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>       // for inet_ntoa
#endif // CEYLAN_USES_NETINET_IN_H

#ifdef CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>        // for inet_ntoa
#endif // CEYLAN_USES_ARPA_INET_H

#ifdef CEYLAN_USES_WINSOCK2_H
#include <winsock2.h>         // for network errors
#endif // CEYLAN_USES_WINSOCK2_H

#ifdef CEYLAN_USES_SYS_SYSTEMINFO_H
#include <sys/systeminfo.h>   // for Solaris sysinfo
#endif // CEYLAN_USES_SYS_SYSTEMINFO_H

}



// Templates cannot be declared to have 'C' linkage:
#ifdef CEYLAN_USES_WS2TCPIP_H
#include <ws2tcpip.h>  // for getaddrinfo
#endif // CEYLAN_USES_WS2TCPIP_H



/*
 * Useful documentation resources:
 *
 *  - http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winsock/winsock/porting_socket_applications_to_winsock.asp
 *
 *  - http://tangentsoft.net/wskfaq/articles/bsd-compatibility.html
 *
 *
 * Maybe a ping facility could be provided (see IPPROTO_ICMP).
 *
 */



using namespace Ceylan::Network ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;

using namespace std ;




NetworkException::NetworkException( const string & message ) :
	Ceylan::Exception( message )
{

}



NetworkException::~NetworkException() throw()
{

}




const Ceylan::Uint16 Ceylan::Network::HostDNSEntry::HostNameMaxLength = 256 ;



#ifdef CEYLAN_USES_WS2TCPIP_H


/**
 * Avoid exposing system-dependent ws2tcpip.h in the headers:
 *
 */
struct HostDNSEntry::SystemSpecificHostEntry
{

	/*
	 * This will contain a linked list of addrinfo structures containing address
	 * informations about the host:
	 *
	 */
	struct addrinfo* _entryList ;

	SystemSpecificHostEntry() :
		_entryList( 0 )
	{

	}

} ;


#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

#include <netdb.h>     // for hostent


/**
 * Avoid exposing system-dependent netdb.h in the headers:
 *
 * @note The hostent structure is described in 'man gethostbyname':
 *
 */
struct HostDNSEntry::SystemSpecificHostEntry
{

	hostent * _entry ;

	SystemSpecificHostEntry() :
		_entry( 0 )
	{

	}

} ;


#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H




/*
 * Some of HostDNSEntry methods will return bogus values (ex: empty structures)
 * whenever called whereas system-specific headers (ex: netdb.h or ws2tcpip.h)
 * are not available.
 *
 * This should not hurt, since the instances these methods should apply to
 * cannot exist, since their constructors raise an exception in this context.
 *
 */




HostDNSEntry::HostDNSEntry( const std::string & hostName ) :
	_internalEntry( 0 )
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	_internalEntry = new SystemSpecificHostEntry ;

	// Prepare hints about the type of socket the caller supports:
	struct addrinfo callerHints ;
	::memset( &callerHints, 0, sizeof(callerHints) ) ;
	callerHints.ai_family   =  PF_UNSPEC /* IPv4 only: PF_INET */ ;
	callerHints.ai_socktype = /* any socket type */ 0 ;
	callerHints.ai_protocol = /* any protocol */    0 ;
	callerHints.ai_flags    = AI_CANONNAME ;

#if CEYLAN_DEBUG_NETWORK
	LogPlug::debug( "HostDNSEntry constructor: "
			"running getaddrinfo for '" + hostName + "'." ) ;
#endif // CEYLAN_DEBUG_NETWORK

	if ( ::getaddrinfo( hostName.c_str(), /* port */ 0,
			&callerHints, &_internalEntry->_entryList ) != 0 )
	{
		LogPlug::error( "HostDNSEntry constructor failed: "
			+ Network::explainSocketError() ) ;

		throw NetworkException( "HostDNSEntry constructor failed: "
			+ Network::explainSocketError() ) ;

	}

	// Here _entryList should be correctly set.

	manageHostEntry() ;

	// Here the pointer in _internalEntry is not null for sure.

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	_internalEntry = new SystemSpecificHostEntry ;

	_internalEntry->_entry =::gethostbyname( hostName.c_str() ) ;

	// Hard to factor code without creating a string most often useless:
	if ( _internalEntry->_entry == 0 )
		LogPlug::error(
			"HostDNSEntry constructor failed for argument '"
			+ hostName + "'." ) ;

	manageHostEntry() ;

	// Here the pointer in _internalEntry is not null for sure.


#else // CEYLAN_USES_NETDB_H

	throw NetworkException( "HostDNSEntry constructor from hostname failed: "
		"not supported on this platform." ) ;

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}



HostDNSEntry::HostDNSEntry( const IPAddress & ip )
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	_internalEntry = new SystemSpecificHostEntry ;

	// Prepare hints about the type of socket the caller supports:
	struct addrinfo callerHints ;
	::memset( &callerHints, 0, sizeof(callerHints) ) ;
	callerHints.ai_family   = AF_INET ;
	callerHints.ai_socktype = /* any socket type */ 0 ;
	callerHints.ai_protocol = /* any protocol    */ 0 ;
	callerHints.ai_flags    = AI_NUMERICHOST ;

	if ( ::getaddrinfo( ip.toString().c_str(), /* port */ 0,
			&callerHints, &_internalEntry->_entryList ) != 0 )
		throw NetworkException( "HostDNSEntry constructor failed: "
			+ Network::explainSocketError() ) ;

	// Here _entryList should be correctly set.

	manageHostEntry() ;

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	_internalEntry = new SystemSpecificHostEntry ;

	// Let's convert "82.225.152.215" in a binary form, for gethostbyaddr:
	struct in_addr binaryIp ;

	if ( ::inet_aton( ip.toString().c_str(), &binaryIp ) == 0 )
		throw NetworkException( "HostDNSEntry constructor from IP failed: "
			"the conversion of " + ip.toString() + " to binary IP failed." ) ;

	if ( ip.getType() == Network::IPv4 )
		_internalEntry->_entry =::gethostbyaddr(
			reinterpret_cast<const char *>( &binaryIp ),
			sizeof(in_addr), AF_INET ) ;
	else
		throw NetworkException( "HostDNSEntry constructor from IP failed: "
			"address type not supported on this platform." ) ;

	// Hard to factor code without creating a string most often useless:
	if ( _internalEntry->_entry == 0 )
		LogPlug::error(
			"HostDNSEntry constructor failed for argument '"
			+ ip.toString() + "'." ) ;

	manageHostEntry() ;


#else // CEYLAN_USES_NETDB_H

	throw NetworkException( "HostDNSEntry constructor from IP failed: "
		"not supported on this platform." ) ;

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}



HostDNSEntry::~HostDNSEntry() throw()
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	if ( _internalEntry != 0 )
	{
		::freeaddrinfo( _internalEntry->_entryList ) ;
		delete _internalEntry ;
	}

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	// _internalEntry->_entry may point at static data, so not deallocated.
	if ( _internalEntry != 0 )
		delete _internalEntry ;

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}



string HostDNSEntry::getOfficialHostName() const
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	if ( _internalEntry->_entryList->ai_canonname == 0 )
		throw NetworkException( "HostDNSEntry::getOfficialHostName: "
			"no available canonical name." ) ;

	return string( _internalEntry->_entryList->ai_canonname ) ;

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	return string( _internalEntry->_entry->h_name ) ;

#else // CEYLAN_USES_NETDB_H

	throw NetworkException( "HostDNSEntry::getOfficialHostName: "
			"not implemented yet." ) ;

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}



list<string> & HostDNSEntry::getAliasList() const
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	list<string> & res = * new list<string> ;

	addrinfo * currentAlias = _internalEntry->_entryList->ai_next ;

	while ( currentAlias != 0 )
	{
		if ( currentAlias->ai_canonname == 0 )
		{

			// Do not know what to do when this happen:
			LogPlug::warning( "HostDNSEntry::getAliasList: "
				"an alias had not canonical name, "
				"it has been skipped." ) ;
		}
		else
			res.push_back( string( currentAlias->ai_canonname ) ) ;

		currentAlias = currentAlias->ai_next ;
	}

	return res ;

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	list<string> & res = * new list<string> ;

	char** alias = _internalEntry->_entry->h_aliases ;

	while ( (*alias) != 0 )
	{
		res.push_back( string( *alias ) ) ;
		alias++ ;
	}

	return res ;

#else // CEYLAN_USES_NETDB_H

	return * new list<string> ;

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}



NetworkAddressType HostDNSEntry::getAddressType() const
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	switch( _internalEntry->_entryList->ai_family )
	{

		case AF_INET:
			return IPv4 ;
			break ;

		case AF_INET6:
			return IPv6 ;
			break ;

		default:
			LogPlug::error( "HostDNSEntry::getAddressType: "
				"unknown address type, returning IPv4 instead." ) ;
			return IPv4 ;
			break ;

	}

	return IPv4 ;

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	switch( _internalEntry->_entry->h_addrtype )
	{

		case AF_INET:
			return IPv4 ;
			break ;

		case AF_INET6:
			return IPv6 ;
			break ;

		default:
			LogPlug::error( "HostDNSEntry::getAddressType: "
				"unknown address type, returning IPv4 instead." ) ;
			return IPv4 ;
			break ;

	}

#else // CEYLAN_USES_NETDB_H

	return IPv4 ;

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}



list<IPAddress *> & HostDNSEntry::getAddresses() const
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	list<IPAddress *> & res = * new list<IPAddress *> ;

	// This structure is evil.

	addrinfo * currentAddressInfo = _internalEntry->_entryList ;

	struct sockaddr_in * currentAddressStruct ;
	struct in_addr * currentEffectiveAddress ;

	do
	{

		/*
		 * currentAddressInfo->ai_addr is a pointer to 'struct sockaddr' whereas
		 * we need a pointer to 'struct sockaddr_in':
		 *
		 * @see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winsock/winsock/sockaddr_2.asp
		 *
		 */
		currentAddressStruct = reinterpret_cast<struct sockaddr_in*>(
			currentAddressInfo->ai_addr ) ;

		char * decodedAddress ;

		switch( currentAddressInfo->ai_family )
		{

			case PF_INET:
				currentEffectiveAddress = & currentAddressStruct->sin_addr ;
				decodedAddress =::inet_ntoa( *currentEffectiveAddress ) ;
				if ( decodedAddress != 0 )
					res.push_back(
						new IPAddressvFour( string( decodedAddress ) ) ) ;
				else
					LogPlug::error( "HostDNSEntry::getAddresses: "
						"unable to decode address, skipping it." ) ;
				break ;

			case PF_INET6:
				LogPlug::error( "HostDNSEntry::getAddresses: "
					"IPv6 not supported yet, skipping address entry." ) ;
			break ;

			default:
				LogPlug::error( "HostDNSEntry::getAddresses: "
					"unsupported address type ("
					+ Ceylan::toString( currentAddressInfo->ai_family )
					+ "), skipping address entry." ) ;
			break ;

		}

		currentAddressInfo = currentAddressInfo->ai_next ;

	}
	while ( currentAddressInfo != 0 ) ;

	return res ;


#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	list<IPAddress *> & res = * new list<IPAddress *> ;

	// The h_addr_list structure is evil.

	Ceylan::Uint16 addrLen = _internalEntry->_entry->h_length ;

	struct in_addr currentAddressBuffer ;

	char ** currentAddress = _internalEntry->_entry->h_addr_list ;

	switch( getAddressType() )
	{

		case IPv4:

			while ( *currentAddress != 0 )
			{

				::memcpy( & currentAddressBuffer, *currentAddress, addrLen ) ;

				string decodedAddress(
					::inet_ntoa( currentAddressBuffer ) ) ;

				res.push_back( new IPAddressvFour( decodedAddress ) ) ;
				currentAddress++ ;

			}
			break ;


		case IPv6:
			LogPlug::error( "HostDNSEntry::getAddresses: "
				"IPv6 not supported yet, returning empty list." ) ;
			break ;

		default:
			LogPlug::error( "HostDNSEntry::getAddresses: "
				"unexpected address type, returning empty list." ) ;
			break ;

	}

	return res ;


#else // CEYLAN_USES_NETDB_H

	return * new list<IPAddress *> ;

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}



void HostDNSEntry::DeleteAdresses( std::list<IPAddress *> & addresses )
{

  // Avoids memory leaks:
  for ( std::list<IPAddress *>::const_iterator it = addresses.begin();
		it != addresses.end(); it++ )
	  delete *it ;

  delete & addresses ;

}



const string HostDNSEntry::toString( Ceylan::VerbosityLevels level ) const
{

	string res ;

	try
	{
		res = "The host '" + getOfficialHostName() + "' has " ;
	}
	catch( const NetworkException & e )
	{
		res = "HostDNSEntry::toString: "
			"no official name could be found for this host (abnormal)" ;
		LogPlug::error( res ) ;
		return res ;
	}

	list<string> * alias = & getAliasList() ;

	if ( alias->empty() )
	{
		res += "no alias. " ;
	}
	else
	{
		res += "following name alias: " + formatStringList( *alias ) ;

	}
	delete alias ;

	res += "Its address type is " ;

	switch( getAddressType() )
	{

		case IPv4:
			res += "IPv4" ;
			break ;

		case IPv6:
			res += "IPv6" ;
			break ;

		default:
			res += "unknown (abnormal)" ;
			break ;

	}

	res += ". Its known network addresses are: " ;

	list<IPAddress *> * addresses = & getAddresses() ;

	list<string> addressDescriptions ;

	for ( list<IPAddress *>::const_iterator it = addresses->begin();
			it != addresses->end(); it++ )
		addressDescriptions.push_back( (*it)->toString( level ) ) ;

	DeleteAdresses( *addresses ) ;

	return res + formatStringList( addressDescriptions ) ;

}



void HostDNSEntry::manageHostEntry()
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	if ( _internalEntry->_entryList == 0 )
		throw NetworkException( "HostDNSEntry constructor failed "
					"(in HostDNSEntry::manageHostEntry), no entry found" ) ;

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	if ( _internalEntry->_entry == 0 )
	{

		switch( h_errno )
		{

			case HOST_NOT_FOUND:
				throw NetworkException( "HostDNSEntry constructor failed: "
					"the specified host is unknown." ) ;
				break ;

			case NO_ADDRESS /* equal to NO_DATA */:
				throw NetworkException( "HostDNSEntry constructor failed: "
					"the requested name is valid but "
					"does not have an IP address." ) ;
				break ;

			case NO_RECOVERY:
				throw NetworkException( "HostDNSEntry constructor failed: "
					"a non-recoverable name server error occurred." ) ;
				break ;

			case TRY_AGAIN:
				throw NetworkException( "HostDNSEntry constructor failed: "
					"a temporary error occurred on "
					"an authoritative name server. Try again later" ) ;
				break ;

			default:
				throw NetworkException( "HostDNSEntry constructor failed: "
					"unexpected error code" ) ;
				break ;

		}
	}

#else // CEYLAN_USES_NETDB_H

	// Nothing special to do.

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}





// Separate functions.



const string Ceylan::Network::getLocalHostName()
{

#ifdef CEYLAN_USES_WINSOCK2_H

	// HOST_NAME_MAX does not seem to be widely defined:
	char hostBuffer[ HostDNSEntry::HostNameMaxLength  + 1 ] ;

	if ( ::gethostname( hostBuffer, HostDNSEntry::HostNameMaxLength ) != 0 )
		throw NetworkException( "Ceylan::Network::getLocalHostName: "
			"unable to determine local host name (error code: "
			+ Ceylan::toString( WSAGetLastError() ) + ")." ) ;

	return string( hostBuffer ) ;

#else // CEYLAN_USES_WINSOCK2_H

#ifdef CEYLAN_USES_GETHOSTNAME

	// int uname(struct utsname *buf) in sys/utsname.h coud be used as well.

	// HOST_NAME_MAX does not seem to be widely defined:
	char hostBuffer[ HostDNSEntry::HostNameMaxLength  + 1 ] ;

	if ( ::gethostname( hostBuffer, HostDNSEntry::HostNameMaxLength ) != 0 )
		throw NetworkException( "Ceylan::Network::getLocalHostName: "
			"unable to determine local host name: " + explainError() ) ;

	return string( hostBuffer ) ;

#else // CEYLAN_USES_GETHOSTNAME

	throw NetworkException( "Ceylan::Network::getLocalHostName: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES_GETHOSTNAME

#endif // CEYLAN_USES_WINSOCK2_H

}



void Ceylan::Network::setLocalHostName( const string & newHostName )
{

#ifdef CEYLAN_USES_SETHOSTNAME

#if CEYLAN_ARCH_SOLARIS

	/*
	 * Some versions of Solaris do not declare sethostname in the right header,
	 * or do not seem to have it, using sysinfo instead:
	 *
	 */
	const char * buf = newHostName.c_str() ;
	if ( ::sysinfo( SI_SET_HOSTNAME, const_cast<char *>( buf ),
			newHostName.size() /* + 1? */ ) == -1 )
		throw NetworkException( "Ceylan::Network::setLocalHostName: "
			"unable to set local host name on Solaris to "
			+ newHostName + ": " + explainError() ) ;

#else // CEYLAN_ARCH_SOLARIS

	if ( ::sethostname( newHostName.c_str(), newHostName.size() ) )
		throw NetworkException( "Ceylan::Network::setLocalHostName: "
			"unable to set local host name to "
			+ newHostName + ": " + explainError() ) ;

#endif // CEYLAN_ARCH_SOLARIS


#else // CEYLAN_USES_SETHOSTNAME

	throw NetworkException( "Ceylan::Network::setLocalHostName: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES_SETHOSTNAME

}



const string Ceylan::Network::getLocalHostDomainName()
{

#ifdef CEYLAN_USES_GETDOMAINNAME

	char domainBuffer[ 256 ] ;

	if ( ::getdomainname( domainBuffer, 255 ) != 0 )
		throw NetworkException( "Unable to determine local host domain name: "
			+ explainError() ) ;

	string res( domainBuffer ) ;

	if ( res == "(none)" )
		return "" ;

	return res ;

#else // CEYLAN_USES_GETDOMAINNAME

	throw NetworkException( "Ceylan::Network::getLocalHostDomainName: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES_GETDOMAINNAME

}



void Ceylan::Network::setLocalHostDomainName( const string & newDomainName )
{

#ifdef CEYLAN_USES_SETDOMAINNAME

	if ( ::setdomainname( newDomainName.c_str(), newDomainName.size() ) )
		throw NetworkException( "Unable to set local host domain name to "
			+ newDomainName + ": "
			+ explainError() ) ;

#else // CEYLAN_USES_SETDOMAINNAME

	throw NetworkException( "Ceylan::Network::setLocalHostDomainName: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES_SETDOMAINNAME

}



/// Tells whether the specified name should be considered as blank.
bool isABlankHostname( const string & name )
{

  if ( name == "localhost.localdomain" )
	return true ;

  if ( name == "localhost6.localdomain6" )
	return true ;

  return false ;

}


const string Ceylan::Network::getMostPreciseLocalHostName()
{

#if CEYLAN_ARCH_NINTENDO_DS

	// dswifi not used here, returning a constant:
	return "nds" ;

#else // CEYLAN_ARCH_NINTENDO_DS


	string guessedFullHostname ;

#ifdef CEYLAN_USES_UNAME

	struct utsname buf ;

	if ( ::uname( & buf ) != 0 )
	{
		LogPlug::error( "Ceylan::Network::getMostPreciseLocalHostName: "
			"unable to determine name of local host: " + explainError() ) ;
	}
	else
	{

		guessedFullHostname = buf.nodename ;

		//Ceylan::checkpoint( "uname told: " + guessedFullHostname ) ;

		// Needing a FQDN, checking for dots in the host name:
		if ( Ceylan::countChars( guessedFullHostname, '.' ) != 0
		  && ( ! isABlankHostname( guessedFullHostname ) ) )
			return guessedFullHostname ;

	}

#endif // CEYLAN_USES_UNAME


	/*
	 * Here we have no real FQDN, let's try with the DNS:
	 *
	 */

	/*
	 * Do not catch any exception, since we cannot succeed without the basic
	 * hostname:
	 *
	 */
	string thisHostname = getLocalHostName() ;

	//Ceylan::checkpoint( "getLocalHostName told: " + thisHostname ) ;

	bool found = false ;

	try
	{

		guessedFullHostname = getFQDNFromHostname( thisHostname ) ;
		found = true ;

	}
	catch( const NetworkException & e )
	{
		LogPlug::debug( "Ceylan::Network::getMostPreciseLocalHostName: "
			"FQDN not available: " + e.toString() ) ;
	}

	if ( found )
	{

	  //Ceylan::checkpoint( "getFQDNFromHostname told: " + thisHostname ) ;

	  if ( ! isABlankHostname( guessedFullHostname )  )
		return guessedFullHostname ;

	}


	/*
	 * Damned, not found.
	 * Last try: hostname.domainname, let's go for the domain name:
	 *
	 */

	string domain ;

	try
	{

		// This is the *domain* here:
		domain = getLocalHostDomainName() ;

	}
	catch( const NetworkException & e )
	{
		LogPlug::debug( "Ceylan::Network::getMostPreciseLocalHostName: "
			"domain name not available: " + e.toString() ) ;
		domain = "" ;

	}

	if ( ! domain.empty() )
	{
		// We seem to have a good domain name, let's keep it.
		return thisHostname + "." + domain ;
	}

	// Failed to have a FQDN, returning the mere hostname instead:
	return thisHostname ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}





// Section for all hosts.



const string Ceylan::Network::getFQDNFromIP( const IPAddress & ip )
	{

	HostDNSEntry searched( ip ) ;

	return getFQDNFromDNSEntry( searched ) ;

}



const string Ceylan::Network::getFQDNFromIPv4( const std::string & ipString )
{

	IPAddressvFour ip( ipString ) ;

	return getFQDNFromIP( ip ) ;

}



const string Ceylan::Network::getFQDNFromHostname(
	const std::string & hostname )
{

	HostDNSEntry searched( hostname ) ;

	return getFQDNFromDNSEntry( searched ) ;

}



const string Ceylan::Network::getFQDNFromDNSEntry( const HostDNSEntry & entry )
{

	string current ;

	try
	{
		current = entry.getOfficialHostName() ;
	}
	catch( const NetworkException & e )
	{
		LogPlug::debug( "Ceylan::Network::getFQDNFromDNSEntry: "
			"no official name found." ) ;
	}

	// Try the official name as FQDN:
	if ( Ceylan::countChars( current, '.' ) != 0 )
		return current ;

	// No, try alias:
	list<string> * alias = & entry.getAliasList() ;

	for ( list<string>::const_iterator it = alias->begin() ;
			it != alias->end(); it++ )
	{
		if ( Ceylan::countChars( (*it), '.' ) != 0 )
		{
			current = *it ;
			break ;
		}
	}

	delete alias ;

	if ( current.empty() )
		throw NetworkException( "Ceylan::Network::getFQDNFromDNSEntry "
			"failed to find any FQDN from following entry: "
			+ entry.toString() ) ;

	return current ;

}



bool Ceylan::Network::isAValidHostName( const string & hostnameString )
{

#if CEYLAN_USES_REGEX

	Ceylan::RegExp target( hostnameString ) ;

	return target.matches(
		"^(([a-z]|[A-Z]{1,1})([a-z]|[A-Z]|[.]|[0-9]|[-]){0,})$" ) ;

#else // CEYLAN_USES_REGEX

	// Check disabled:
	return true ;

#endif // CEYLAN_USES_REGEX

}



/*
 * On Windows, error codes should be interpreted according to the following
 * table:
 *
 * http://msdn.microsoft.com/library/en-us/winsock/winsock/windows_sockets_error_codes_2.asp
 *
 * Sadly the conversion from an error code to a textual message seems to be up
 * to the programmer.
 *
 */
string Ceylan::Network::interpretSocketError( SocketError errorCode )
{

#if CEYLAN_ARCH_WINDOWS

	switch( errorCode )
	{

	case WSAEINTR:
		return "interrupted function call (WSAEINTR): "
			"a blocking operation was interrupted "
			"by a call to WSACancelBlockingCall" ;
		break ;

	case WSAEACCES:
		return "permission denied (WSAEACCES): "
			"an attempt was made to access a socket in a way forbidden "
			"by its access permissions" ;
		break ;

	case WSAEFAULT:
		return "bad address (WSAEFAULT): "
			"the system detected an invalid pointer address in attempting "
			"to use a pointer argument of a call" ;
		break ;

	case WSAEINVAL:
		return "invalid argument (WSAEINVAL): "
			"some invalid argument was supplied" ;
		break ;

	case WSAEMFILE:
		return "too many open sockets (WSAEMFILE)" ;
		break ;

	case WSAEWOULDBLOCK:
		return "resource temporarily unavailable (WSAEWOULDBLOCK): "
			"this error is returned from operations on nonblocking sockets "
			"that cannot be completed immediately" ;
		break ;

	case WSAEINPROGRESS:
		return "operation now in progress (WSAEINPROGRESS): "
			"a blocking operation is currently executing" ;
		break ;

	case WSAEALREADY:
		return "operation already in progress (WSAEALREADY): "
			"an operation was attempted on a nonblocking socket "
			"with an operation already in progress" ;
		break ;

	case WSAENOTSOCK:
		return "socket operation on nonsocket (WSAENOTSOCK): "
			"an operation was attempted on something that is not a socket" ;
		break ;

	case WSAEDESTADDRREQ:
		return "destination address required (WSAEDESTADDRREQ): "
			"a required address was omitted from an operation on a socket" ;
		break ;

	case WSAEMSGSIZE:
		return "message too long (WSAEMSGSIZE): "
			"a message sent on a datagram socket was larger than supported" ;
		break ;

	case WSAEPROTOTYPE:
		return "wrong protocol type for socket (WSAEPROTOTYPE)" ;
		break ;

	case WSAENOPROTOOPT:
		return "bad protocol option (WSAENOPROTOOPT): "
			"unknown, invalid or unsupported option or level" ;
		break ;

	case WSAEPROTONOSUPPORT:
		return "requested protocol not supported on this system "
			"(WSAEPROTONOSUPPORT)" ;
		break ;

	case WSAESOCKTNOSUPPORT:
		return "socket type not supported (WSAESOCKTNOSUPPORT): "
			"the support for the specified socket type does not exist "
			"in this address family" ;
		break ;

	case WSAEOPNOTSUPP:
		return "operation not supported (WSAEOPNOTSUPP): "
			"the attempted operation is not supported "
			"for the type of object referenced" ;
		break ;

	case WSAEPFNOSUPPORT:
		return "protocol family not supported on this system "
			"(WSAEPFNOSUPPORT)" ;
		break ;

	case WSAEAFNOSUPPORT:
		return "address family not supported by protocol family "
			"(WSAEAFNOSUPPORT)" ;
		break ;

	case WSAEADDRINUSE:
		return "address already in use (WSAEADDRINUSE): "
			"typically, only one usage of each socket address "
			"(protocol/IP address/port) is permitted" ;
		break ;

	case WSAEADDRNOTAVAIL:
		return "cannot assign requested address (WSAEADDRNOTAVAIL): "
			"the requested address is not valid in its context" ;
		break ;

	case WSAENETDOWN:
		return "network is down (WSAENETDOWN): "
			"a socket operation encountered a dead network" ;
		break ;

	case WSAENETUNREACH:
		return "network is unreachable (WSAENETUNREACH): "
			"a socket operation was attempted to an unreachable network" ;
		break ;

	case WSAENETRESET:
		return "network dropped connection on reset (WSAENETRESET)" ;
		break ;

	case WSAECONNABORTED:
		return "software caused connection abort (WSAECONNABORTED): "
			"an established connection was aborted by the software "
			"in your host computer" ;
		break ;

	case WSAECONNRESET:
		return "connection reset by peer (WSAECONNRESET): "
			"an existing connection was forcibly closed by the remote host" ;
		break ;

	case WSAENOBUFS:
		return "no buffer space available (WSAENOBUFS)" ;
		break ;

	case WSAEISCONN:
		return "socket is already connected (WSAEISCONN): "
			"a connect request was made on an already-connected socket" ;
		break ;

	case WSAENOTCONN:
		return "socket is not connected (WSAENOTCONN): "
			"a request to send or receive data was disallowed because "
			"the socket is not connected and/or no address was supplied" ;
		break ;

	case WSAESHUTDOWN:
		return "cannot send after socket shutdown (WSAESHUTDOWN): "
			"a request to send or receive data was disallowed because "
			"the socket had already been shut down in that direction" ;
		break ;

	case WSAETIMEDOUT:
		return "a connection attempt failed because of a time-out "
			"(WSAETIMEDOUT)" ;
		break ;

	case WSAECONNREFUSED:
		return "connection refused (WSAECONNREFUSED): "
			"no connection could be made because "
			"the target computer actively refused it" ;
		break ;

	case WSAEHOSTDOWN:
		return "host is down (WSAEHOSTDOWN): "
			"a socket operation failed because the destination host is down" ;
		break ;

	case WSAEHOSTUNREACH:
		return "no route to host (WSAEHOSTUNREACH): "
			"a socket operation was attempted to an unreachable host" ;
		break ;

	case WSAEPROCLIM:
		return "too many processes (WSAEPROCLIM): "
			"a Windows Sockets implementation may have a limit on "
			"the number of applications that can use it simultaneously" ;
		break ;

	case WSASYSNOTREADY:
		return "network subsystem is unavailable (WSASYSNOTREADY)" ;
		break ;

	case WSAVERNOTSUPPORTED:
		return "Winsock.dll version out of range (WSAVERNOTSUPPORTED): "
			"the current Windows Sockets implementation "
			"does not support the Windows Sockets specification version "
			"requested by the application" ;
		break ;

	case WSANOTINITIALISED:
		return "successful WSAStartup not yet performed (WSANOTINITIALISED)" ;
		break ;

	case WSAEDISCON:
		return "graceful shutdown in progress (WSAEDISCON)" ;
		break ;

	case WSATYPE_NOT_FOUND:
		return "class type not found (WSATYPE_NOT_FOUND)" ;
		break ;

	case WSAHOST_NOT_FOUND:
		return "host not found (WSAHOST_NOT_FOUND): "
			"no such host is known" ;
		break ;

	case WSATRY_AGAIN:
		return "nonauthoritative host not found (WSATRY_AGAIN): "
			"this is usually a temporary error during host name resolution" ;
		break ;

	case WSANO_RECOVERY:
		return "nonrecoverable error (WSANO_RECOVERY) occurred "
			"during a database lookup" ;
		break ;

	case WSANO_DATA:
		return "valid name, no data record of requested type (WSANO_DATA)" ;
		break ;

	case WSA_INVALID_HANDLE:
		return "specified event object handle is invalid "
			"(WSA_INVALID_HANDLE)" ;
		break ;

	case WSA_INVALID_PARAMETER:
		return "one or more parameters are invalid (WSA_INVALID_PARAMETER)" ;
		break ;

	case WSA_IO_INCOMPLETE:
		return "overlapped I/O event object not in signaled state "
			"(WSA_IO_INCOMPLETE): the application has tried to determine the"
			" status of an overlapped operation which is not yet completed" ;
		break ;

	case WSA_IO_PENDING:
		return "overlapped operations will complete later (WSA_IO_PENDING): "
			"the application has initiated an overlapped operation that "
			"cannot be completed immediately" ;
		break ;

	case WSA_NOT_ENOUGH_MEMORY:
		return "insufficient memory available (WSA_NOT_ENOUGH_MEMORY)" ;
		break ;

	case WSA_OPERATION_ABORTED:
		return "overlapped operation aborted (WSA_OPERATION_ABORTED)" ;
		break ;

/*
 * Not supported, at least not on Windows XP:

	case WSAINVALIDPROCTABLE:
		return "invalid procedure table from service provider "
			"(WSAINVALIDPROCTABLE)" ;
		break ;

	case WSAINVALIDPROVIDER:
		return "invalid service provider version number "
			"(WSAINVALIDPROVIDER)" ;
		break ;

	case WSAPROVIDERFAILEDINIT:
		return "unable to initialize a service provider "
			"(WSAPROVIDERFAILEDINIT)" ;
		break ;
*/

	case WSASYSCALLFAILURE:
		return "system call failure (WSASYSCALLFAILURE): "
			"generic error code" ;
		break ;

	default:
		return "unknown socket error code #"
			+ Ceylan::toString( errorCode ) + " (abnormal)" ;

	}

#else // CEYLAN_ARCH_WINDOWS

	return "nothing appropriate for the current platform "
		"(abnormal)" ;

#endif // CEYLAN_ARCH_WINDOWS

}



SocketError Ceylan::Network::getSocketError()
{

#if CEYLAN_ARCH_WINDOWS

	return WSAGetLastError() ;

#else // CEYLAN_ARCH_WINDOWS

	LogPlug::error( "Ceylan::Network::getSocketError called "
		"whereas not on Windows." ) ;

	return 0 ;

#endif // CEYLAN_ARCH_WINDOWS

}



std::string Ceylan::Network::explainSocketError()
{

#if CEYLAN_ARCH_WINDOWS

	return interpretSocketError( getSocketError() ) ;

#else // CEYLAN_ARCH_WINDOWS

	return "Ceylan::Network::explainSocketError should not be "
		"used on this platform" ;

#endif // CEYLAN_ARCH_WINDOWS

}





// NetworkManager section.


#if CEYLAN_ARCH_WINDOWS

NetworkManager NetworkManager::_Manager ;


NetworkManager::NetworkManager()
{

#if CEYLAN_DEBUG_SYSTEM

	Ceylan::display( "NetworkManager constructor: "
		"initializing network subsystem." ) ;

#endif // CEYLAN_DEBUG_SYSTEM

	WORD requestedVersion = MAKEWORD( 2, 2 ) ;
	WSADATA wsaData ;

	int result = WSAStartup( requestedVersion, & wsaData ) ;

	if ( result != 0 )
		throw NetworkException( "NetworkManager constructor failed: "
			"no usable WinSock DLL found (error code: "
			+ Ceylan::toString( result ) + ")." ) ;

	/*
	 * Confirms that the WinSock DLL supports 2.2.
	 *
	 * @note If the DLL supports versions greater than 2.2 in addition to 2.2,
	 * it will still return 2.2 in wVersion since that is the requested version.
	 *
	 */
	if ( LOBYTE( wsaData.wVersion ) != 2 || HIBYTE( wsaData.wVersion ) != 2 )
	{
		WSACleanup() ;
		throw NetworkException( "NetworkManager constructor failed: "
			"this version of Winsock DLL is not supported." ) ;
	}

}



NetworkManager::~NetworkManager() throw()
{

#if CEYLAN_DEBUG_SYSTEM

	Ceylan::display( "NetworkManager destructor: "
		"shutting down network subsystem." ) ;

#endif // CEYLAN_DEBUG_SYSTEM

	WSACleanup() ;

}


#endif // CEYLAN_ARCH_WINDOWS
