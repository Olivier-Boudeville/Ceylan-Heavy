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
#if CEYLAN_USES_UNISTD_H
#include <unistd.h>
#endif // CEYLAN_USES_UNISTD_H

#if CEYLAN_USES_SYS_UTSNAME_H
#include <sys/utsname.h>      // for uname
#endif // CEYLAN_USES_SYS_UTSNAME_H

#if CEYLAN_USES_STRING_H
#include <string.h>           // for memcpy
#endif // CEYLAN_USES_STRING_H

#if CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>       // for inet_ntoa
#endif // CEYLAN_USES_SYS_SOCKET_H

#if CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>       // for inet_ntoa
#endif // CEYLAN_USES_NETINET_IN_H

#if CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>        // for inet_ntoa
#endif // CEYLAN_USES_ARPA_INET_H


}

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


#ifdef CEYLAN_USES_NETDB_H

#include <netdb.h>


// Avoid exposing system-dependent netdb.h in the headers :
struct HostDNSEntry::SystemSpecificHostEntry
{
	hostent * _entry ;
} ;


/*
 * Some methods return bogus values (ex : empty structures) whenever called
 * whereas netdb.h not available.
 *
 * This should hurt since the instances these methods should apply to cannot
 * exist, since their constructors raise an exception in this context.
 *
 */
 
 
#endif // CEYLAN_USES_NETDB_H



HostDNSEntry::HostDNSEntry( const std::string & hostName )
		throw( NetworkException ) :
	_internalEntry( 0 )
{

#ifdef CEYLAN_USES_NETDB_H

	_internalEntry = new SystemSpecificHostEntry ;
	
	_internalEntry->_entry = ::gethostbyname( hostName.c_str() ) ;
	
	manageHostEntry() ;
	

	
#else // CEYLAN_USES_NETDB_H

	throw NetworkException( "HostDNSEntry constructor from hostname failed : "
		"not supported on this platform." ) ;

#endif // CEYLAN_USES_NETDB_H

}	
		
			
HostDNSEntry::HostDNSEntry( const IPAddress & ip ) throw( NetworkException )
{

#ifdef CEYLAN_USES_NETDB_H
	
	_internalEntry = new SystemSpecificHostEntry ;
	
	
	// Let's convert "82.225.152.215" in a binary form, for gethostbyaddr :
	struct in_addr binaryIp ;
	
	if ( ::inet_aton( ip.toString().c_str(), &binaryIp ) == 0 )
		throw NetworkException( "HostDNSEntry constructor from IP failed : "
			"the conversion of " + ip.toString() + " to binary IP failed." ) ;
	
	if ( ip.getType() == Network::IPv4 )
		_internalEntry->_entry = ::gethostbyaddr( 
			reinterpret_cast<const char *>( &binaryIp), 
			sizeof(in_addr), AF_INET ) ;
	else
		throw NetworkException( "HostDNSEntry constructor from IP failed : "
			"address type not supported on this platform." ) ;
			
	manageHostEntry() ;
	
	
#else // CEYLAN_USES_NETDB_H

	throw NetworkException( "HostDNSEntry constructor from IP failed : "
		"not supported on this platform." ) ;

#endif // CEYLAN_USES_NETDB_H

}
					
												
HostDNSEntry::~HostDNSEntry() throw()
{

	// _internalEntry->_entry may point at static data, so not deallocated.
	if ( _internalEntry != 0 )
		delete _internalEntry ;
		
}	


string HostDNSEntry::getOfficialHostName() const throw()
{

#ifdef CEYLAN_USES_NETDB_H

	return string( _internalEntry->_entry->h_name ) ;

#else // CEYLAN_USES_NETDB_H

	return "" ;
	
#endif // CEYLAN_USES_NETDB_H

}


list<string> & HostDNSEntry::getAliasList() const throw()
{

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
	
}


NetworkAddressType HostDNSEntry::getAddressType() const throw()
{

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
	
}


list<IPAddress *> & HostDNSEntry::getAddresses() const throw()
{

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
				
				string a( "rr") ;
				
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
		
}


const string HostDNSEntry::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "The host '" + getOfficialHostName() + "' has " ;
	
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
	
#endif // CEYLAN_USES_NETDB_H

}



// Separate functions.



const string Ceylan::Network::getLocalHostName() throw( NetworkException )
{

#if CEYLAN_USES_GETHOSTNAME

	// HOST_NAME_MAX does not seem to be widely defined :
	char hostBuffer[ HostDNSEntry::HostNameMaxLength  + 1 ] ;

	if ( ::gethostname( hostBuffer, HostDNSEntry::HostNameMaxLength ) )
	throw NetworkException( "Ceylan::Network::getLocalHostName : "
		"unable to determine local host name : "
		+ explainError( getError() ) ) ;

	return string( hostBuffer ) ;

#else // CEYLAN_USES_GETHOSTNAME

	throw NetworkException( "Ceylan::Network::getLocalHostName : "
		"not available on this platform." ) ;
		
#endif // CEYLAN_USES_GETHOSTNAME

}


void Ceylan::Network::setLocalHostName( const string & newHostName )
	throw( NetworkException )
{

#if CEYLAN_USES_SETHOSTNAME

	if ( ::sethostname( newHostName.c_str(), newHostName.size() ) )
		throw NetworkException( "Ceylan::Network::setLocalHostName : "
			"unable to set local host name to "
			+ newHostName + " : " + explainError( getError() ) ) ;
			
#else // CEYLAN_USES_SETHOSTNAME

	throw NetworkException( "Ceylan::Network::setLocalHostName : "
		"not available on this platform." ) ;
		
#endif // CEYLAN_USES_SETHOSTNAME

}


const string Ceylan::Network::getLocalHostDomainName() throw( NetworkException )
{

#if CEYLAN_USES_GETDOMAINNAME
	
	char domainBuffer[ 256 ] ;

	if ( ::getdomainname( domainBuffer, 255 ) != 0 )
		throw NetworkException( "Unable to determine local host domain name : "
			+ explainError( getError() ) ) ;

	string res( domainBuffer ) ;
	
	if ( res == "(none)" )
		return "" ;
		
	return res ;
	
#else // CEYLAN_USES_GETDOMAINNAME

	throw NetworkException( "Ceylan::Network::getLocalHostDomainName : "
		"not available on this platform." ) ;
		
#endif // CEYLAN_USES_GETDOMAINNAME

}


void Ceylan::Network::setLocalHostDomainName( const string & newDomainName )
	throw( NetworkException )
{

#if CEYLAN_USES_SETDOMAINNAME
	
	if ( ::setdomainname( newDomainName.c_str(), newDomainName.size() ) )
		throw NetworkException( "Unable to set local host domain name to "
			+ newDomainName + " : "
			+ explainError( getError() ) ) ;
			
#else // CEYLAN_USES_SETDOMAINNAME

	throw NetworkException( "Ceylan::Network::setLocalHostDomainName : "
		"not available on this platform." ) ;
		
#endif // CEYLAN_USES_SETDOMAINNAME
			

}




const string Ceylan::Network::getMostPreciseLocalHostName() 
	throw( NetworkException )
{


	string guessedFullHostname ;
	
	
#if CEYLAN_USES_UNAME

	struct utsname buf ;

	if ( ::uname( & buf ) != 0 )
	{		
		LogPlug::error( "Ceylan::Network::getLocalHostFQDN : "
			"unable to determine fully qualified domain name "
			"of local host : " + explainError( getError() ) ) ;
	}		
	else
	{		
	
		guessedFullHostname = buf.nodename ;

		// Needing a FQDN, checking for dots in the host name :
		if ( Ceylan::countChars( guessedFullHostname, '.' ) != 0 )
			return guessedFullHostname ;
			
	}
						
#endif // CEYLAN_USES_UNAME 	
	
	
	/*
	 * Here we have no real FQDN, let's try with the DNS :
	 *
	 */

	/*
	 * Do not catch any exception, since we cannot succeed without the 
	 * basic hostname :
	 *
	 */
	string thisHostname = getLocalHostName() ;
	
	bool found = false ;
	
	try
	{

		guessedFullHostname = getFQDNFromHostname( thisHostname ) ;
		found = true ;
		
	}
	catch( const NetworkException & e )
	{
		LogPlug::debug( "Ceylan::Network::getMostPreciseLocalHostName : "
			"FQDN not available : " + e.toString() ) ;
	}

	if ( found )
		return guessedFullHostname ;

	/*
	 * Damned, not found.
	 * Last try : hostname.domainname, let's go for the domain name :
	 *
	 */
	
	string domain ;

	try
	{

		// This is the *domain* here :
		domain = getLocalHostDomainName() ;

	}
	catch( const NetworkException & e )
	{
		LogPlug::debug( "Ceylan::Network::getMostPreciseLocalHostName : "
			"domain name not available : " + e.toString() ) ;
		domain = "" ;

	}

	if ( ! domain.empty() )
	{
		// We seem to have a good domain name, let's keep it.
		return thisHostname + "." + domain ;
	}

	// Failed to have a FQDN, returning the mere hostname instead :
	return thisHostname ;

}




// Section for all hosts.
		
		
				
const string Ceylan::Network::getFQDNFromIP( const IPAddress & ip )
	throw( NetworkException )
{

	HostDNSEntry searched( ip ) ;
	
	return getFQDNFromDNSEntry( searched ) ;
	
}
			
			
const string Ceylan::Network::getFQDNFromIPv4( const std::string & ipString )
	throw( NetworkException ) 
{

	IPAddressvFour ip( ipString ) ;
	
	return getFQDNFromIP( ip ) ;
	
}
			
									
const string Ceylan::Network::getFQDNFromHostname( 
	const std::string & hostname ) throw( NetworkException )
{

	HostDNSEntry searched( hostname ) ;
	
	return getFQDNFromDNSEntry( searched ) ;
		
}


const string Ceylan::Network::getFQDNFromDNSEntry( const HostDNSEntry & entry )
	throw( NetworkException ) 
{

	string current = entry.getOfficialHostName() ;
	
	// Try the official name as FQDN :
	if ( Ceylan::countChars( current, '.' ) != 0 )
		return current ;
	
	// No, try alias :
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
	
	return current ;	
		
}


bool Ceylan::Network::isAValidHostName( const string & hostnameString ) throw()
{

#if CEYLAN_USES_REGEX
	
	Ceylan::RegExp target( hostnameString ) ;

	return target.matches(
		"^(([a-z]|[A-Z]{1,1})([a-z]|[A-Z]|[.]|[0-9]|[-]){0,})$" ) ;
	
#else // CEYLAN_USES_REGEX
	
	// Check disabled :
	return true ;
	
#endif // CEYLAN_USES_REGEX

}

