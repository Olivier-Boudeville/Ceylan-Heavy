#include "CeylanMultiplexedServerStreamSocket.h"


#include "CeylanLogPlug.h"                // for LogPlug
#include "CeylanOperators.h"              // for toString
#include "CeylanThread.h"                 // for Sleep
#include "CeylanAnonymousStreamSocket.h"  // for AnonymousStreamSocket
#include "CeylanStringUtils.h"            // for formatStringList
#include "CeylanSignal.h"                 // for ignore
#include "CeylanInputStream.h"            // for Select


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#ifdef CEYLAN_USES_UNISTD_H
//#include <unistd.h>            // for FIXME
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for setsockopt, bind, accept
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_SYS_SOCKET_H
#include <sys/socket.h>        // for setsockopt, bind, listen, accept
#endif // CEYLAN_USES_SYS_SOCKET_H

#ifdef CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>         // for htonl
#endif // CEYLAN_USES_ARPA_INET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for htonl
#endif // CEYLAN_USES_NETINET_IN_H

}


using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;
using std::list ;
using std::set ;






MultiplexedServerStreamSocket::MultiplexedServerStreamSocketException::MultiplexedServerStreamSocketException( const std::string & reason ) throw():
	ServerStreamSocketException( reason )
{

}


MultiplexedServerStreamSocket::MultiplexedServerStreamSocketException::~MultiplexedServerStreamSocketException() throw()
{

}



MultiplexedServerStreamSocket::MultiplexedServerStreamSocket( 
	Port listeningPort, bool reuse )
		throw( SocketException ):
	ServerStreamSocket( listeningPort, reuse, /* blocking */ false ),
	_currentConnections()
{

#if CEYLAN_USES_NETWORK

	// No connection on creation.
	
#else // CEYLAN_USES_NETWORK

	throw MultiplexedServerStreamSocketException( 
		"MultiplexedServerStreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}


MultiplexedServerStreamSocket::~MultiplexedServerStreamSocket() throw()
{

#if CEYLAN_USES_NETWORK

	closeAcceptedConnections() ;
	
#endif // CEYLAN_USES_NETWORK
	
}


bool MultiplexedServerStreamSocket::isConnected() const throw()
{
	return ! _currentConnections.empty() ;
}


void MultiplexedServerStreamSocket::run() throw( ServerStreamSocketException )
{

#if CEYLAN_USES_NETWORK


#if CEYLAN_DEBUG_NETWORK_SERVERS
	LogPlug::trace( "Entering in MultiplexedServerStreamSocket::run" ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS


	// Records the total number of created connections :
	Ceylan::Uint32 connectionCount = 0 ;
	
	if ( ! _currentConnections.empty() )
		throw MultiplexedServerStreamSocketException( 
			"MultiplexedServerStreamSocket::run failed : "
			"there are already living connections." ) ; 
		
	// Loosing a client should not stop the server :
	Signal::ignore( Signal::BrokenPipe ) ;

	prepareToAccept() ;
	

	list<InputStream *> watchedSockets ;
	
	// Among all the watched sockets, the listening one is the first :
	watchedSockets.push_back( static_cast<InputStream*>( this ) ) ;
	 
	 
	list<AnonymousStreamSocket*> connectionsToRemove ;


#if CEYLAN_DEBUG_NETWORK_SERVERS
	LogPlug::trace( "MultiplexedServerStreamSocket::run : "
		"entering main listening loop." ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS

	
	while ( ! isRequestedToStop() )
	{	
	
		connectionsToRemove.clear() ;
		
		// Like a daemon, tries to resist to most common errors :
		
		try
		{
		
	
#if CEYLAN_DEBUG_NETWORK_SERVERS
			LogPlug::trace( "MultiplexedServerStreamSocket::run : "
				"waiting for selected sockets." ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
				
			/*
			 * Blocks until there is data to read somewhere :
			 *
			 * @note A time-out may be useful here.
			 *
			 */
			InputStream::Select( watchedSockets ) ;

#if CEYLAN_DEBUG_NETWORK_SERVERS
			LogPlug::trace( "MultiplexedServerStreamSocket::run : "
				"at least a socket is selected." ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
				

			// Searches for the input stream(s) that are selected :
			for ( list<InputStream*>::iterator it = watchedSockets.begin(); 
				it != watchedSockets.end(); it++ ) 
			{
			
				if ( (*it)->isFaulty() )
				{
					
					if ( (*it) != this )
					{
				
						LogPlug::warning( 
							"MultiplexedServerStreamSocket::run : "
							"a faulty connection socket will be removed : "
							+ (*it)->toString() ) ;
							
						connectionsToRemove.push_back(
							dynamic_cast<AnonymousStreamSocket*>( *it ) ) ;
						
					}
					else
					{
					
						LogPlug::error( "MultiplexedServerStreamSocket::run : "
							"listening socket is in faulty state : "
							+ toString() + ", trying to overcome..." ) ;
							
						// Trying to make as if everything was normal :	
						setFaulty( false ) ;	
							
					}
					
				}	
				
				
				if ( (*it)->isSelected() )
				{
				
					FileDescriptor selectedFD = (*it)->getInputStreamID() ;
			
			
					/*
					 * Depending on the selected stream, different actions
					 * have to be performed :
					 *
					 */
					if ( selectedFD == getOriginalFileDescriptor() )
					{
					
						/*
						 * A client is (most probably) knocking at the door,
						 * let's accept it and store its connection manager
						 * into the streams that should be watched :
						 *
						 */
#if CEYLAN_DEBUG_NETWORK_SERVERS
						LogPlug::trace( "MultiplexedServerStreamSocket::run : "
							"trying to accept a new connection." ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
							
						InputStream * newAnonymousSocket = accept() ;
						
						if ( newAnonymousSocket != 0 )
						{
							
							watchedSockets.push_back( newAnonymousSocket ) ;
					
							connectionCount++ ;		

#if CEYLAN_DEBUG_NETWORK_SERVERS
							LogPlug::trace( 
								"MultiplexedServerStreamSocket::run : "
								"connection #" 
								+ Ceylan::toString( connectionCount ) 
								+ " accepted." ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
						
						}
						else
						{
						
#if CEYLAN_DEBUG_NETWORK_SERVERS
							LogPlug::trace( 
								"MultiplexedServerStreamSocket::run : "
								"no connection could be accepted." ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
							
						}
											
					}
					else // not the listening socket
					{
						
						// Here, a connection-dedicated socket has data :
						
						AnonymousStreamSocket * connection = 
							dynamic_cast<AnonymousStreamSocket*>( *it ) ;
							
						if ( connection == 0 )	
						{
							LogPlug::error( 
								"MultiplexedServerStreamSocket::run : "
								"unexpected failure of conversion to "
								"an anonymous stream socket of : "
								+ (*it)->toString() ) ;
						}
						
						
#if CEYLAN_DEBUG_NETWORK_SERVERS
						LogPlug::trace( "MultiplexedServerStreamSocket::run : "
							"following connection socket has data available : "
							+ connection->toString() + "." ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
							
						if ( ! handleConnection( *connection ) )
							connectionsToRemove.push_back( connection ) ;
												
							
					} // connection managed
					
				
				} // if this stream is selected
				
				
			} // for it in watchedSockets...
			
			
			
#if CEYLAN_DEBUG_NETWORK_SERVERS

			LogPlug::trace( "MultiplexedServerStreamSocket::run : "
				"end of select scan." ) ;
		
			// Sleep for 0.2 second :
			Thread::Sleep( 0 /* second */, 200000 /* microsecond*/) ;
			
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
		
		
		   /*
		    * Now remove the dead connections since we are not iterating
		    * in watchedSockets anymore :
		    *
		    */
		   while ( ! connectionsToRemove.empty() )
		   {
		   		
				AnonymousStreamSocket & toDel = * connectionsToRemove.front() ;
				connectionsToRemove.pop_front() ;

#if CEYLAN_DEBUG_NETWORK_SERVERS
				LogPlug::trace( "MultiplexedServerStreamSocket::run : "
					"removing stopped connection : " + toDel.toString() ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
		       
				closeConnection( toDel ) ;
				watchedSockets.remove( & toDel ) ;  
			    
		   }
				 

		}
		catch( const Ceylan::Exception & e )
		{
			LogPlug::error ( "MultiplexedServerStreamSocket::run : "
				"error caught : " + e.toString() ) ;
		}
		
		
	}
	
#if CEYLAN_DEBUG_NETWORK_SERVERS
	LogPlug::trace( "Exiting from MultiplexedServerStreamSocket::run" ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS

	
#endif // CEYLAN_USES_NETWORK

}


AnonymousStreamSocket * MultiplexedServerStreamSocket::accept() 
	throw( ServerStreamSocketException )
{

#if CEYLAN_USES_NETWORK
			
	if ( ! _bound )
		prepareToAccept() ;


#if CEYLAN_DEBUG_NETWORK_SERVERS
	LogPlug::trace( "MultiplexedServerStreamSocket::accept : "
		"will accept now connections, state is : " + toString() ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
	
	AnonymousStreamSocket * res ;
	
	try
	{
	
		// Accepts the connection, by passing the listening file descriptor :
		res = new AnonymousStreamSocket( getOriginalFileDescriptor(),
			/* blocking */ false ) ;
			
	}
	catch( const AnonymousStreamSocket::NonBlockingAcceptException & e )
	{

		LogPlug::warning( "MultiplexedServerStreamSocket::accept "
			"cancelled : " + e.toString() ) ;
			
		return 0 ;
		
	}	
	catch( const SocketException & e )
	{
		throw MultiplexedServerStreamSocketException( 
			"MultiplexedServerStreamSocket::accept failed : "
			+ e.toString() ) ;
	}	

	_currentConnections.insert( res ) ;
		
#if CEYLAN_DEBUG_NETWORK_SERVERS
	LogPlug::trace( "MultiplexedServerStreamSocket::accept : "
		"new connection accepted" ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
		
	accepted( *res ) ;
	
#if CEYLAN_DEBUG_NETWORK_SERVERS
	LogPlug::trace( "MultiplexedServerStreamSocket::accept : "
		"connection terminated, cleaning up afterwards" ) ;
#endif // CEYLAN_DEBUG_NETWORK_SERVERS
	
	// No cleanAfterAccept() called, since connections are still alive here.

	return res ;


#else // CEYLAN_USES_NETWORK	


	throw ServerStreamSocketException( 
		"MultiplexedServerStreamSocket::accept : "
		"network feature not available." ) ;
		
		
#endif // CEYLAN_USES_NETWORK	
			
}


void MultiplexedServerStreamSocket::closeConnection( 
		AnonymousStreamSocket & connection )
	throw( MultiplexedServerStreamSocketException )
{

	if ( _currentConnections.find( &connection ) != _currentConnections.end() )
	{
					
		delete &connection ;
		_currentConnections.erase( &connection ) ;
		
	}
	else
		throw MultiplexedServerStreamSocketException( 
			"MultiplexedServerStreamSocket::closeConnection : "
			"unable to find following connection : " + connection.toString() ) ;
}


const std::string MultiplexedServerStreamSocket::toString(
	Ceylan::VerbosityLevels level ) const throw()
{

#if CEYLAN_USES_NETWORK

	string res = "MultiplexedServerStreamSocket" ;
	
	if ( _currentConnections.empty() )
	{
		res += " currently not managing any connection" ;
	}	
	else
	{
		
		list<string> connectionDescriptions ;
	
		for ( set<AnonymousStreamSocket *>::const_iterator it =
					_currentConnections.begin(); 
				it != _currentConnections.end(); it++ )
			connectionDescriptions.push_back( (*it)->toString( level ) ) ;
	
		res += " managing currently following connection(s) : "
			+ Ceylan::formatStringList( connectionDescriptions ) ;
			
	}	
		
	if	( level == Ceylan::medium )
		return res ;
	
	return res + ". " + StreamSocket::toString( level ) ;

#else // CEYLAN_USES_NETWORK

	return "MultiplexedServerStreamSocket (no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
		

bool MultiplexedServerStreamSocket::closeAcceptedConnections() 
	throw( Stream::CloseException )		
{

	if ( _currentConnections.empty() )
		return false ;
		
	for ( set<AnonymousStreamSocket *>::const_iterator it =
			_currentConnections.begin(); it != _currentConnections.end(); it++ )
	{	
						
		delete (*it) ;
			
	}
		
	_currentConnections.clear() ;
	
	return true ;
	
}
		
