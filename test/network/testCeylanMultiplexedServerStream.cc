#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Network ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <iostream>    // for cout, endl
using namespace std ;

#include <exception>

#include <string>





class MyTestMultiplexedStreamServer : 
	public Ceylan::Network::MultiplexedServerStreamSocket
{


	public:
	
	
		MyTestMultiplexedStreamServer( bool isBatch, 
				Ceylan::Uint32 targetConnectionCount ):
			MultiplexedServerStreamSocket( 6969, /* reuse */ true ),
			_batch( isBatch ),
			_targetConnectionCount( targetConnectionCount )
		{
		
			LogPlug::info( "MyTestMultiplexedStreamServer created : "
				+ toString() ) ;
				
		}
		
					
		bool handleConnection( AnonymousStreamSocket & connection )
			throw( MultiplexedServerStreamSocketException )		
		{
		
		
			LogPlug::info( "MyTestMultiplexedStreamServer::handleConnection : "
				"will read from connection " + connection.toString() ) ;
		       

			if ( ! _batch )
			{ 
				// Sleep for 0.1 second :	
				Thread::Sleep( 0, /* microseconds */ 100000 ) ;
			}
				
			char buffer[ 2 ] ;
			buffer[1] = 0 ;
				
			System::Size readCount ;
				
			try
			{
					
				readCount = connection.read( buffer, 1 ) ;
				
			}
			catch( const InputStream::ReadFailedException & e )
			{
				
				LogPlug::error( 
					"MyTestMultiplexedStreamServer::handleConnection "
					"failed : " + e.toString() ) ;
					
				// Kills that faulty connection :
				return false ;	
					
			}
				
				 
			LogPlug::trace( 
				"MyTestMultiplexedStreamServer::handleConnection : read "
				+ Ceylan::toString( readCount ) + " byte(s)." ) ;
				
				
			if ( readCount == 0 )
			{
			
				LogPlug::error( 
					"MyTestMultiplexedStreamServer::handleConnection : "
					"connection awoken but no data to read." ) ;
						
				return false ;
					
			}

			// 1 byte read here :
			
			if ( buffer[0] == 'X' )
			{
					
				LogPlug::trace(
					"MyTestMultiplexedStreamServer::handleConnection : "
					"acknowledging successful reception of end marker."	) ;
							
				cout << endl ;
				connection.write( "+", 2 ) ;
						
				// Connection terminated according to protocol :
				return false ;
				
			}
			else if ( buffer[0] == 'Q' )
			{
			
				LogPlug::debug( 
					"MyTestMultiplexedStreamServer::handleConnection : "
					"received 'Q', stopping server." ) ;
					
				cout << endl ;
				connection.write( "+", 2 ) ;
					
				requestToStop() ;
				return false ;
				
			}
			else
			{
					
				cout << buffer ;
				cout.flush() ;
				
				return true ;									
				
			}
			
		}
		
	
	private:
	
	
		bool _batch ;
		
		Ceylan::Uint32 _targetConnectionCount ;
		
} ;



/**
 * Test of Ceylan server stream.
 *
 * @see ServerStreamSocket.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;

	
	
    try
    {


        LogPlug::info( "Testing Ceylan's network implementation "
			"of stream socket for servers." ) ;


		if ( ! Features::isNetworkingSupported() )
		{
			LogPlug::info( 
				"No network support available, no test performed." ) ;
			return Ceylan::ExitSuccess ;
		}
		
		
		bool isBatch = false ;
		Ceylan::Uint32 targetConnectionCount = 1 ;
		
		
		std::string executableName ;
		std::list<std::string> options ;
		
		Ceylan::parseCommandLineOptions( executableName, options, argc, argv ) ;
		
		std::string token ;
		bool tokenEaten ;
		
		
		while ( ! options.empty() )
		{
		
			token = options.front() ;
			options.pop_front() ;

			tokenEaten = false ;
						
			if ( token == "--batch" )
			{
				LogPlug::info( "Batch mode selected" ) ;
				isBatch = true ;
				tokenEaten = true ;
			}

			if ( token == "--online" )
			{
				// Ignored :
				tokenEaten = true ;
			}
			
			if ( token == "--connection-count" )
			{
				if ( options.empty() )
					throw CommandLineParseException( "Option " + token 
						+ " expected one argument, none found." ) ;
				targetConnectionCount = static_cast<Ceylan::Uint32>(
					Ceylan::stringToUnsignedLong( options.front() ) ) ;
				options.pop_front() ;	
				LogPlug::info( "Will handle " + 
					Ceylan::toString( targetConnectionCount ) 
					+ " client requests before stopping" )  ;
				tokenEaten = true ;
			}
			
			if ( LogHolder::IsAKnownPlugOption( token ) )
			{
				// Ignores log-related (argument-less) options.
				tokenEaten = true ;
			}
			
			
			if ( ! tokenEaten )
			{
				throw CommandLineParseException( 
					"Unexpected command line argument : " + token ) ;
			}
		
		}
		
		MyTestMultiplexedStreamServer myServer( isBatch, 
			targetConnectionCount ) ;
	
        LogPlug::info( "Server created, waiting for connections : "
			+ myServer.toString() ) ;
		
		myServer.run() ;
		
        LogPlug::info( "Connection terminated, current server state is : "
			+ myServer.toString() ) ;
			
			
		/*
		 * In a test suite, we need to make the server wait a bit before
		 * returning, so that it lets enough time for the client to stop and
		 * then to wait for the server PID.
		 *
		 * @see playTests-local.sh
		 *
		 */
		if ( isBatch )
			Thread::Sleep( 0 /* second */, 500000 /* microseconds */ ) ;
			
        LogPlug::info( "End of network stream server test." ) ;


	}
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught : " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
