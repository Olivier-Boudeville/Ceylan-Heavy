#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Network ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <iostream>    // for cout, endl
using namespace std ;

#include <exception>

#include <string>





class MyTestStreamClient : public Ceylan::Network::ClientStreamSocket
{


	public:
	
	
		MyTestStreamClient(): 
			ClientStreamSocket()
		{
		
			LogPlug::info( "MyTestStreamClient created : "
				+ toString() ) ;
				
		}
		

		void connected() throw( ClientStreamSocketException )
		{
		
			LogPlug::info( "Client connected !" ) ;
			
			string s = "This is a test line from client, quitting now ! Q\n";
			write( s ) ;
			
			LogPlug::info( "Client sent : '" + s + "'." ) ;
			
		}
		
		

} ;



/**
 * Test of Ceylan client stream.
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
			"of stream socket for clients." ) ;


		if ( ! Features::isNetworkingSupported() )
		{
			LogPlug::info( 
				"No network support available, no test performed." ) ;
			return Ceylan::ExitSuccess ;
		}


		MyTestStreamClient myClient ;

		string targetServer ;
		
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
				LogPlug::info( "Running in batch mode (ignored)" ) ;
				tokenEaten = true ;
			} else
			if ( token == "--interactive" )
			{
				LogPlug::info( "Running in interactive mode (ignored)" ) ;
				tokenEaten = true ;
			} else		
			if ( token == "--server" )
			{
				targetServer = options.front() ;
				options.pop_front() ;
				LogPlug::info( "Will try to connect to server '"
					+ targetServer + "'." ) ;
				tokenEaten = true ;
			}
			
			if ( ! tokenEaten )
			{
				LogPlug::error( "Unexpected command line argument : "
					+ token ) ;
			}
		
		}
	
	
		if ( targetServer.empty() )
			targetServer = "localhost" ;    
	
        LogPlug::info( "Client created : " + myClient.toString()
			+ ", will try to connect to '" + targetServer + "'." ) ;
		
			
		myClient.connect( targetServer, /* port */ 6969 ) ;

		LogPlug::info( "Client connected, its state is : "
			+ myClient.toString() ) ;

		char buffer[ 10 ] ;
	
		if ( myClient.read( buffer, 2 ) > 0 ) 
			cout << "Client read from server : '" << buffer 
				<< "'." << endl ;
		
		const char expectedAnswer = '+' ; 
		
		if ( buffer[0] != expectedAnswer )
			throw Ceylan::TestException( "Incorrect answer from server : "
				"received '" + string( buffer ) + "', instead of '" 
				+ toString( expectedAnswer ) + "'." ) ;
				
        LogPlug::info( "Connection terminated, current client state is : "
			+ myClient.toString() ) ;			
		
        LogPlug::info( "End of network stream client test." ) ;


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
