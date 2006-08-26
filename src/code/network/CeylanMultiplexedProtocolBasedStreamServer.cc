#include "CeylanMultiplexedProtocolBasedStreamServer.h"


#include "CeylanLogPlug.h"                            // for LogPlug
#include "CeylanStringUtils.h"                        // for formatStringList
#include "CeylanOperators.h"                          // for toString
#include "CeylanAnonymousProtocolAwareStreamSocket.h" // for this class 
#include "CeylanProtocolServer.h"                     // for ProtocolServer
#include "CeylanProtocolEndpoint.h"                   // for ProtocolException

#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;
using std::list ;
using std::set ;




MultiplexedProtocolBasedStreamServer::MultiplexedProtocolBasedStreamServer( 
	Port listeningPort, bool reuse )
		throw( SocketException ):
	MultiplexedServerStreamSocket( listeningPort, reuse )
{

	// Not anything special to do here.	

}


MultiplexedProtocolBasedStreamServer::~MultiplexedProtocolBasedStreamServer()
	throw()
{

	// Not anything special to do here.	
		
}

// Add handleConnection 

AnonymousStreamSocket * MultiplexedProtocolBasedStreamServer::accept() 
	throw( ServerStreamSocketException )
{

#if CEYLAN_USES_NETWORK
			
	if ( ! _bound )
		prepareToAccept() ;


	LogPlug::trace( "MultiplexedProtocolBasedStreamServer::accept : "
		"will accept now connections, state is : " + toString() ) ;
	
	AnonymousProtocolAwareStreamSocket * res ;
	
	try
	{
	
		/*
		 * Accepts the connection, by passing the listening file descriptor :
		 *
		 * No particular protocol server can be set here, see
		 * the customizeProtocolFor method.
		 * 
		 */
		res = new AnonymousProtocolAwareStreamSocket(
			getOriginalFileDescriptor() ) ;
			
	}
	catch( const SocketException & e )
	{
		throw MultiplexedServerStreamSocketException( 
			"MultiplexedProtocolBasedStreamServer::accept failed : "
			+ e.toString() ) ;
	}	

	_currentConnections.insert( res ) ;
		
	LogPlug::trace( "MultiplexedProtocolBasedStreamServer::accept : "
		"new connection accepted" ) ;
	
	/*
	 * The user-supplied accepted method must link a protocol server to the
	 * newly created AnonymousProtocolAwareStreamSocket :
	 *
	 */
	accepted( *res ) ;
	
	if ( ! res->hasProtocolServer() )
		throw  MultiplexedServerStreamSocketException( 
			"MultiplexedProtocolBasedStreamServer::accept : "
			"the user-overriden accepted() method did not set "
			"a protocol server to : " + res->toString() ) ;
			
			
	LogPlug::trace( "MultiplexedProtocolBasedStreamServer::accept : "
		"connection terminated, cleaning up afterwards" ) ;
	
	// No cleanAfterAccept() called, since connections are still alive here.

	return res ;


#else // CEYLAN_USES_NETWORK	


	throw ServerStreamSocketException( 
		"MultiplexedProtocolBasedStreamServer::accept : "
		"network feature not available." ) ;
		
		
#endif // CEYLAN_USES_NETWORK	
			
}


void MultiplexedProtocolBasedStreamServer::accepted( 
		AnonymousStreamSocket & newConnection )
	throw( ServerStreamSocketException )
{

	throw ServerStreamSocketException(
		"MultiplexedProtocolBasedStreamServer::accepted : "
		"this method must be overriden by the user "
		"(and it must set a protocol server of the new connection socket)." ) ;
		
}


bool MultiplexedProtocolBasedStreamServer::handleConnection( 
		AnonymousStreamSocket & connection )
	throw( MultiplexedServerStreamSocketException )
{

	AnonymousProtocolAwareStreamSocket * realSocket = 
		dynamic_cast<AnonymousProtocolAwareStreamSocket *>( & connection ) ;
		
	/*
	 * No test needed, it is really a protocol-aware socket, and it has
	 * a protocol server attached indeed.
	 *
	 */
	try
	{
		return realSocket->getProtocolServer().notifyDataAvailability() ;
	}
	catch( const AnonymousStreamSocket::AnonymousStreamSocketException & e )
	{
	
		throw MultiplexedServerStreamSocketException(
			"MultiplexedProtocolBasedStreamServer::handleConnection : "
			+ e.toString() ) ;
			 
	}	
	catch( const Middleware::ProtocolException & e )
	{
	
		// Do not kill the server for a connection-level error :
		LogPlug::error(
			"MultiplexedProtocolBasedStreamServer::handleConnection : "
			+ e.toString() + ". Killing the connection." ) ;
			
		// .. but kill the connection :
		return false ;	
			 
	}
		
}

	
const std::string MultiplexedProtocolBasedStreamServer::toString(
	Ceylan::VerbosityLevels level ) const throw()
{

#if CEYLAN_USES_NETWORK

	string res = "MultiplexedProtocolBasedStreamServer" ;
	
	if ( _currentConnections.empty() )
	{
		res += " currently not managing any connection" ;
	}	
	else
	{
		
		list<string> connectionDescriptions ;
	
		for ( set<AnonymousStreamSocket *>::const_iterator it =
				_currentConnections.begin(); it != _currentConnections.end(); 
					it++ )
			connectionDescriptions.push_back( (*it)->toString( level ) ) ;
	
		res += " managing currently following connection(s) : "
			+ Ceylan::formatStringList( connectionDescriptions ) ;
			
	}	
		
	if	( level == Ceylan::medium )
		return res ;
	
	return res + ". " + StreamSocket::toString( level ) ;

#else // CEYLAN_USES_NETWORK

	return "MultiplexedProtocolBasedStreamServer (no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
		

