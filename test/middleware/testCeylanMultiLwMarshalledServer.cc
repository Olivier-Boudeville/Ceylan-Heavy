#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Network ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <iostream>    // for cout, endl
using namespace std ;

#include <exception>

#include <string>
#include <list>




/**
 * This test server uses light-weight marshalling.
 *
 * It is not really a multiplexed server though, as it manages at
 * most one marshaller at a time, and there is one connection
 * per marshaller.
 *
 * @see testCeylanClientStream.cc for test client.
 *
 * @see testCeylanSequentialServerStream.cc for a similar server with
 * no embedded marshaller.
 *
 * @see testCeylanMultiLwProtocolServer.cc for a full-blown server,
 * featuring multiplexed connections which are protocol-driven and
 * lightweight-marshaller-based.
 *
 */
class MyTestMultiLwMarshalledServer : 
	public Ceylan::Network::MultiplexedServerStreamSocket
{


	public:
	
	
		MyTestMultiLwMarshalledServer( bool isBatch, 
				Ceylan::Uint32 targetConnectionCount ):
			MultiplexedServerStreamSocket( 6969, /* reuse */ true ),
			_batch( isBatch ),
			_marshaller( 0 )
		{
		
			LogPlug::info( "MyTestMultiLwMarshalledServer created : "
				+ toString() ) ;
				
		}
		

		~MyTestMultiLwMarshalledServer() throw()
		{

			if ( _marshaller != 0 )
				delete _marshaller ;

		}


		bool handleConnection( AnonymousStreamSocket & connection )
			throw( MultiplexedServerStreamSocketException )		
		{
		
		
			LogPlug::info( "MyTestMultiLwMarshalledServer::handleConnection : "
				"will read from connection " + connection.toString() ) ;

			if ( _marshaller == 0 )
				_marshaller = new Middleware::LightWeightMarshaller( connection, 
					/* no buffer wanted */ 0 ) ;

			if ( ! _batch )
			{ 
				// Sleep for 0.1 second :	
				Thread::Sleep( 0, /* microseconds */ 100000 ) ;
			}
				
			
			// 1 byte read here :
			Ceylan::Uint8 readByte = _marshaller->decodeUint8() ;

			if ( readByte == 'X' )
			{
					
				LogPlug::trace(
					"MyTestMultiLwMarshalledServer::handleConnection : "
					"acknowledging successful reception of end marker."	) ;
							
				cout << endl ;
				_marshaller->encodeUint8( '+' ) ;
						
				// Connection terminated according to protocol :
				return false ;
				
			}
			else if ( readByte == 'Q' )
			{
			
				LogPlug::debug( 
					"MyTestMultiLwMarshalledServer::handleConnection : "
					"received 'Q', stopping server." ) ;
					
				cout << endl ;
				_marshaller->encodeUint8( '+' ) ;				

				requestToStop() ;

				return false ;
				
			}
			else
			{
					
				cout << readByte ;
				cout.flush() ;
				
				return true ;									
				
			}
			
		}
		
	
	private:
	
	
		bool _batch ;
				

		/*
		 * The internal marshaller.
		 *
		 * As it is a state-less marshaller, the same instance could be used
		 * by a set of connections. If it was stateful, then each connection
		 * would have to be associated with its own marshaller instance.
		 *
		 * However a marshaller depends on a specific I/O stream (connection),
		 * hence as there is only one marshaller here, at most one connection
		 * at a time can exist.
		 *
		 */
		Ceylan::Middleware::LightWeightMarshaller * _marshaller ;
		

} ;



/**
 * Test of Ceylan light-weight marshalling in the context of a server.
 *
 * @see MultiplexedServerStreamSocket
 * @see LightWeightMarshaller
 *
 */
int main( int argc, char * argv[] )
{

	
	LogHolder logger( argc, argv ) ;

	
    try
    {


        LogPlug::info( "Testing Ceylan's implementation "
			"of multiplexed server using lightweight marshalling." ) ;


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
		MyTestMultiLwMarshalledServer myServer( isBatch, 
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
		
			
        LogPlug::info( "End of test for multiplexed server "
			"using lightweight marshalling." ) ;

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
