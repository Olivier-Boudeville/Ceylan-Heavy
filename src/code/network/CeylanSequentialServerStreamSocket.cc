#include "CeylanSequentialServerStreamSocket.h"


#include "CeylanLogPlug.h"                // for LogPlug
#include "CeylanOperators.h"              // for toString
#include "CeylanThread.h"                 // for Sleep
#include "CeylanAnonymousStreamSocket.h"  // for AnonymousStreamSocket



#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



using namespace Ceylan::System ;
using namespace Ceylan::Network ;
using namespace Ceylan::Log ;


using std::string ;




SequentialServerStreamSocket::SequentialServerStreamSocketException::SequentialServerStreamSocketException( const std::string & reason ) throw():
	ServerStreamSocketException( reason )
{

}


SequentialServerStreamSocket::SequentialServerStreamSocketException::~SequentialServerStreamSocketException() throw()
{

}




SequentialServerStreamSocket::SequentialServerStreamSocket( Port listeningPort, 
		bool reuse ) throw( SocketException ):
	ServerStreamSocket( listeningPort, reuse ),
	_currentConnection( 0 )
{

#if CEYLAN_USES_NETWORK


#else // CEYLAN_USES_NETWORK

	throw SequentialServerStreamSocketException( 
		"SequentialServerStreamSocket constructor failed : "
		"network support not available." ) ; 
	
#endif // CEYLAN_USES_NETWORK
	
}


SequentialServerStreamSocket::~SequentialServerStreamSocket() throw()
{

#if CEYLAN_USES_NETWORK

	// The main listening socket is taken care of in mother classes.
	
	// No destructor should throw exception :
	try
	{
		closeAcceptedConnections() ;
	}
	catch( const Stream::CloseException	& e )
	{
		LogPlug::error( "SequentialServerStreamSocket destructor failed : " 
			+ e.toString() ) ;
	}
	
#endif // CEYLAN_USES_NETWORK
	
}


bool SequentialServerStreamSocket::isConnected() const throw()
{

	return _currentConnection != 0 ;
	
}


void SequentialServerStreamSocket::accept() 
	throw( ServerStreamSocketException )
{

#if CEYLAN_USES_NETWORK

	if ( _currentConnection != 0 )
		throw SequentialServerStreamSocketException( 
			"SequentialServerStreamSocket::accept : "
			"a connection is still active" ) ;
			
	if ( ! _bound )
		prepareToAccept() ;
	
	try
	{
	
		// Accepts the connection, by passing the listening file descriptor :
		_currentConnection = new AnonymousStreamSocket( 
			getOriginalFileDescriptor() ) ;
			
	}
	catch( const SocketException & e )
	{
		throw SequentialServerStreamSocketException( 
			"SequentialServerStreamSocket::accept failed : "
			+ e.toString() ) ;
	}	
		

	accepted() ;
	
	cleanAfterAccept() ;

#endif // CEYLAN_USES_NETWORK	
			
}


FileDescriptor SequentialServerStreamSocket::getFileDescriptorForTransport()
	const throw( SocketException, Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_NETWORK

	if ( _currentConnection != 0 )
		return _currentConnection->getOriginalFileDescriptor() ;
	else	
		throw SequentialServerStreamSocketException( 
			 "SequentialServerStreamSocket::getFileDescriptorForTransport : "
			 "no available connection." ) ;
		
			
#else // CEYLAN_USES_NETWORK

	throw Features::FeatureNotAvailableException( 
		"SequentialServerStreamSocket::getFileDescriptorForTransport : "
		"network support not available." ) ;
		
#endif // CEYLAN_USES_NETWORK

}



const std::string SequentialServerStreamSocket::toString(
	Ceylan::VerbosityLevels level ) const throw()
{

	string res = "SequentialServerStreamSocket " ;
	
#if CEYLAN_USES_NETWORK


	if ( _currentConnection != 0 )
		res += "with a running connection : " 
			+ _currentConnection->toString( level ) ;
	else
		res += "not connected to any peer" ;
	
	if ( level == Ceylan::low )
		return res ;
	
	return res + ". " + ServerStreamSocket::toString( level ) ;
	
	
#else // CEYLAN_USES_NETWORK

	return res + "(no network support not available)" ;
	
#endif // CEYLAN_USES_NETWORK	
	
}	
						

bool SequentialServerStreamSocket::closeAcceptedConnections() 
	throw( Stream::CloseException )
{

	if ( _currentConnection != 0 )
	{
		delete _currentConnection ;
		_currentConnection = 0 ;
		return true ;
		
	}
	
	return false ;

}
	
					
