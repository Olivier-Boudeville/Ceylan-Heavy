#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Network ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <iostream>    // for cout, endl
using namespace std ;

#include <exception>

#include <string>





class MyTestStreamServer : public Ceylan::Network::ServerStreamSocket
{


	public:
	
	
		MyTestStreamServer( bool isBatch, 
				Ceylan::Uint32 targetConnectionCount ):
			ServerStreamSocket( 6969, /* reuse */ true ),
			_batch( isBatch ),
			_targetConnectionCount( targetConnectionCount )
		{
		
			LogPlug::info( "MyTestStreamServer created : "
				+ toString() ) ;
				
		}
		
		
		void run() throw( ServerStreamSocketException )
		{

			/*
			 * Instead of relying on a connection count to know when to stop,
			 * the order could come from the network as well, with for
			 * example a bool _requestedToStop :
			 *
			 * while ( ! _requestedToStop )
			 *		accept() ;
			 *
			 */
			 
			for ( Ceylan::Uint32 i = 0; i < _targetConnectionCount; i++ )
			{	
				LogPlug::info( "Waiting for connection #" 
					+ Ceylan::toString( i+1 ) ) ;
				accept() ;
			}	
		
		}
			
		
		void accepted() throw( ServerStreamSocketException )
		{
		
			LogPlug::info( "MyTestStreamServer : connection accepted : "
				+ toString() ) ;
		       
			char buffer[ 2 ] ;
			buffer[1] = 0 ;
			
			while( true )
			{
			
				LogPlug::trace( 
					"MyTestStreamServer : will read from socket now." ) ;
				
				System::Size readCount ;
				
				try
				{
					
					readCount = read( buffer, 1 ) ;
				
				}
				catch( const InputStream::ReadFailedException & e )
				{
					throw ServerStreamSocketException( 
						"MyTestStreamServer::accepted failed : " 
						+ e.toString() ) ;
				}
				
				 
				LogPlug::trace( "MyTestStreamServer : read "
					+ Ceylan::toString( readCount ) + " bytes." ) ;
				
				if ( readCount > 0 )
				{

					if ( buffer[0] == 'Q' )
					{
						cout << endl;
						write( "+", 2 ) ;
						return;
					}
					else
					{
					
						cout << buffer ;
						cout.flush() ;
						
						if ( ! _batch )
						{
							// Sleep for 0.1 second :
							Thread::Sleep( 0, 100000 /* microseconds */ ) ;
						}
							
					}
					
				}
				else 
				{
					return ;
				}
				
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
		
		MyTestStreamServer myServer( isBatch, targetConnectionCount ) ;
	
        LogPlug::info( "Server created, waiting for connections : "
			+ myServer.toString() ) ;
		
		myServer.run() ;
		
        LogPlug::info( "Connection terminated, current server state is : "
			+ myServer.toString() ) ;
			
		
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
