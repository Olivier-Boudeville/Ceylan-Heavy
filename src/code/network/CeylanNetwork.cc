#include "CeylanNetwork.h"

#include "CeylanOperators.h"
#include "CeylanLogPlug.h"              // for Log plug
#include "CeylanSystem.h"               // for explainError
#include "CeylanStringUtils.h"          // for formatStringList
#include "CeylanRegularExpression.h"    // for RegExp
#include "CeylanIPAddressvFour.h"       // for IPAddressvFour


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"               // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

// for sethostname, gethostname, setdomainname, getdomainname :
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
#include <winsock2.h>  // for network errors
#endif // CEYLAN_USES_WINSOCK2_H

}

// Templates cannot be declared to have 'C' linkage :
#ifdef CEYLAN_USES_WS2TCPIP_H
//#include <ws2tcpip.h>  // for getaddrinfo
#endif // CEYLAN_USES_WS2TCPIP_H


/*
 * Useful documentation resources :
 *   - http://msdn.microsoft.com/library/default.asp?url=/library/en-us/winsock/winsock/porting_socket_applications_to_winsock.asp
 *   - http://tangentsoft.net/wskfaq/
 *		* http://tangentsoft.net/wskfaq/articles/bsd-compatibility.html
 *
 *
 * Maybe a ping facility could be provided (see IPPROTO_ICMP).
 *
 */


using namespace Ceylan::Network ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;

using namespace std ;


NetworkException::NetworkException( const string & message ) throw() :
	Ceylan::Exception( message )
{

}


NetworkException::~NetworkException() throw()
{

}



const Ceylan::Uint16 Ceylan::Network::HostDNSEntry::HostNameMaxLength = 256 ;


#ifdef CEYLAN_USES_WS2TCPIP_H

/**
 * Avoid exposing system-dependent ws2tcpip.h in the headers :
 *
 */
struct HostDNSEntry::SystemSpecificHostEntry
{

	/*
	 * This will contain a linked list of addrinfo structures 
	 * containing address informations about the host :
	 *
	 */
	struct addrinfo* _entryList ;

	SystemSpecificHostEntry() throw():
		_entryList( 0 )
	{
	
	}
	
} ;


#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

#include <netdb.h>     // for hostent


/**
 * Avoid exposing system-dependent netdb.h in the headers :
 *
 * @note The hostent structure is described in 'man gethostbyname' :
 *
 */
struct HostDNSEntry::SystemSpecificHostEntry
{

	hostent * _entry ;
	
	SystemSpecificHostEntry() throw():
		_entry( 0 )
	{
	
	}
	
} ;

 
#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H


/*
 * Some of HostDNSEntry methods will return bogus values 
 * (ex : empty structures) whenever called whereas system-specific 
 * headers (ex : netdb.h or ws2tcpip.h) not available.
 *
 * This should not hurt, since the instances these methods should apply to 
 * cannot exist, since their constructors raise an exception in this context.
 *
 */
 
//FIXME

HostDNSEntry::HostDNSEntry( const std::string & hostName )
		throw( NetworkException ) :
	_internalEntry( 0 )
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	_internalEntry = new SystemSpecificHostEntry ;

	// Prepare hints about the type of socket the caller supports :
	struct addrinfo callerHints ;
	::memset( &callerHints, 0, sizeof(callerHints) ) ;
	callerHints.ai_family   = AF_INET ;
	callerHints.ai_socktype = /* any socket type */ 0 ;
	callerHints.ai_protocol = /* any protocol */    0 ;

	if ( ::getaddrinfo( hostName.c_str(), /* port */ 0, 
			&callerHints, &_internalEntry->_entryList ) != 0 ) 
		throw NetworkException( "HostDNSEntry constructor failed : "
			+ Network::explainSocketError() ) ;

	// Here _entryList should be correctly set.

	manageHostEntry() ;
	
	// Here the pointer in _internalEntry is not null for sure.

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	_internalEntry = new SystemSpecificHostEntry ;
	
	_internalEntry->_entry = ::gethostbyname( hostName.c_str() ) ;
	
	// Hard to factor code without creating a string most often useless :
	if ( _internalEntry->_entry == 0 )
		LogPlug::error( 
			"HostDNSEntry constructor failed for argument '"
			+ hostName + "'." ) ;
			
	manageHostEntry() ;
	
	// Here the pointer in _internalEntry is not null for sure.

	
#else // CEYLAN_USES_NETDB_H

	throw NetworkException( "HostDNSEntry constructor from hostname failed : "
		"not supported on this platform." ) ;

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}	
		
			
HostDNSEntry::HostDNSEntry( const IPAddress & ip ) throw( NetworkException )
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	_internalEntry = new SystemSpecificHostEntry ;

	// Prepare hints about the type of socket the caller supports :
	struct addrinfo callerHints ;
	::memset( &callerHints, 0, sizeof(callerHints) ) ;
	callerHints.ai_family   = AF_INET ;
	callerHints.ai_socktype = /* any socket type */ 0 ;
	callerHints.ai_protocol = /* any protocol    */ 0 ;

	if ( ::getaddrinfo( ip.toString().c_str(), /* port */ 0, 
			&callerHints, &_internalEntry->_entryList ) != 0 ) 
		throw NetworkException( "HostDNSEntry constructor failed : "
			+ Network::explainSocketError() ) ;

	// Here _entryList should be correctly set.

	manageHostEntry() ;

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H
	
	_internalEntry = new SystemSpecificHostEntry ;
	
	// Let's convert "82.225.152.215" in a binary form, for gethostbyaddr :
	struct in_addr binaryIp ;
	
	if ( ::inet_aton( ip.toString().c_str(), &binaryIp ) == 0 )
		throw NetworkException( "HostDNSEntry constructor from IP failed : "
			"the conversion of " + ip.toString() + " to binary IP failed." ) ;
	
	if ( ip.getType() == Network::IPv4 )
		_internalEntry->_entry = ::gethostbyaddr( 
			reinterpret_cast<const char *>( &binaryIp ), 
			sizeof(in_addr), AF_INET ) ;
	else
		throw NetworkException( "HostDNSEntry constructor from IP failed : "
			"address type not supported on this platform." ) ;
			
	// Hard to factor code without creating a string most often useless :
	if ( _internalEntry->_entry == 0 )
		LogPlug::error( 
			"HostDNSEntry constructor failed for argument '"
			+ ip.toString() + "'." ) ;
			
	manageHostEntry() ;
	
	
#else // CEYLAN_USES_NETDB_H

	throw NetworkException( "HostDNSEntry constructor from IP failed : "
		"not supported on this platform." ) ;

#endif // CEYLAN_USES_NETDB_H

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
	throw( NetworkException )
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	return string( _internalEntry->_entryList->ai_canonname ) ;

#else // CEYLAN_USES_WS2TCPIP_H

#ifdef CEYLAN_USES_NETDB_H

	return string( _internalEntry->_entry->h_name ) ;

#else // CEYLAN_USES_NETDB_H

	throw NetworkException( "HostDNSEntry::getOfficialHostName : "
			"not implemented yet." ) ;
	
#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}


list<string> & HostDNSEntry::getAliasList() const throw()
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	list<string> & res = * new list<string> ;

	addrinfo * currentAlias = _internalEntry->_entryList->ai_next ;

	while ( currentAlias != 0 )
	{
		res->push_back( string( currentAlias->ai_canonname ) ) ;
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


NetworkAddressType HostDNSEntry::getAddressType() const throw()
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
			LogPlug::error( "HostDNSEntry::getAddressType : "
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
			LogPlug::error( "HostDNSEntry::getAddressType : "
				"unknown address type, returning IPv4 instead." ) ;
			return IPv4 ;
			break ;
	
	}
	
#else // CEYLAN_USES_NETDB_H

	return IPv4 ;
	
#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}


list<IPAddress *> & HostDNSEntry::getAddresses() const throw()
{

#ifdef CEYLAN_USES_WS2TCPIP_H

	list<IPAddress *> & res = * new list<IPAddress *> ;

	// This structure is evil.

	addrinfo * currentAddressInfo = _internalEntry->_entryList ; 

	struct sockaddr_in * currentAddressStruct ;
	struct in_addr * currentEffectiveAddress ;

	do
	{

		currentAddressStruct = currentAddressInfo->ai_addr ;
		
		switch( currentAddressInfo->ai_family  )		
		{
	
			case IPv4:
				currentEffectiveAddress = & currentAddressStruct.sin_addr ;			
				char * decodedAddress = ::inet_ntoa( *currentEffectiveAddress ) ;
				if ( decodedAddress != 0 )
					res.push_back( 
						new IPAddressvFour( string( decodedAddress ) ) ) ;	
				else
					LogPlug::error( "HostDNSEntry::getAddresses : "
						"unable to decode address, skipping it." ) ;
				break ;	
						
			case IPv6:
				LogPlug::error( "HostDNSEntry::getAddresses : "
					"IPv6 not supported yet, skipping address entry." ) ;
			break ;
					
			default:
				LogPlug::error( "HostDNSEntry::getAddresses : "
					"unsupported address type, skipping address entry." ) ;
			break ;
					
		}

		currentAddressInfo = currentAddressInfo->ai_next ;

	}
	while ( currentAddressInfo != 0 )


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
			LogPlug::error( "HostDNSEntry::getAddresses : "
				"IPv6 not supported yet, returning empty list." ) ;
			break ;
					
		default:
			LogPlug::error( "HostDNSEntry::getAddresses : "
				"unexpected address type, returning empty list." ) ;
			break ;
					
	}

	return res ;


#else // CEYLAN_USES_NETDB_H

	return * new list<IPAddress *> ;

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}


const string HostDNSEntry::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res ;
	
	try
	{
		res = "The host '" + getOfficialHostName() + "' has " ;
	}
	catch( const NetworkException & e )
	{
		res = "HostDNSEntry::toString : "
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
		res += "following name alias : " + formatStringList( *alias ) ;
		
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
	
	res += ". Its known network addresses are : " ;
	
	list<IPAddress *> * addresses = & getAddresses() ;
	
	list<string> addressDescriptions ;
	
	for ( list<IPAddress *>::const_iterator	it = addresses->begin();
			it != addresses->end(); it++ )
		addressDescriptions.push_back( (*it)->toString( level ) ) ;
	
	delete addresses ;
		
	return res + formatStringList( addressDescriptions ) ;
	
}


void HostDNSEntry::manageHostEntry() throw( NetworkException )
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
				throw NetworkException( "HostDNSEntry constructor failed : "
					"the specified host is unknown." ) ;
				break ;
			
			case NO_ADDRESS /* equal to NO_DATA */ :
				throw NetworkException( "HostDNSEntry constructor failed : "
					"the requested name is valid but "
					"does not have an IP address." ) ;
				break ;
		
			case NO_RECOVERY:
				throw NetworkException( "HostDNSEntry constructor failed : "
					"a non-recoverable name server error occurred." ) ;
				break ;
	
			case TRY_AGAIN:
				throw NetworkException( "HostDNSEntry constructor failed : "
					"a temporary error occurred on "
					"an authoritative name server. Try again later" ) ;
				break ;
	
			default:
				throw NetworkException( "HostDNSEntry constructor failed : "
					"unexpected error code" ) ;
				break ;
	
		}
	}

#else // CEYLAN_USES_NETDB_H

	// Nothing special to do.

#endif // CEYLAN_USES_NETDB_H

#endif // CEYLAN_USES_WS2TCPIP_H

}


