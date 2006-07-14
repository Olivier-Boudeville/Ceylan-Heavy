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
			
			string s = "This is a test line for server, quitting now : Q\n";
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

		MyTestStreamClient myClient ;

		string targetServer ;
		
		if ( argc < 2 )
			targetServer = "localhost" ;
		else	
			targetServer = argv[1] ;
	
        LogPlug::info( "Client created : " + myClient.toString()
			+ ", will try to connect to '" + targetServer + "'." ) ;
		
			
		myClient.connect( targetServer, /* port */ 6969 ) ;

		LogPlug::info( "Client connected, its state is : "
			+ myClient.toString() ) ;

		char buffer[ 10 ] ;
	
		if ( myClient.read( buffer, 2 ) > 0 ) 
			cout << "Client read from server : " << buffer << endl ;
		
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
