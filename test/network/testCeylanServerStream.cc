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
	
	
		MyTestStreamServer(): 
			ServerStreamSocket( 6969, /* reuse */ true )
		{
		
			LogPlug::info( "MyTestStreamServer created : "
				+ toString() ) ;
				
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
						
						// Sleep for 0.1 second :
						Thread::Sleep( 0, 100000 /* microseconds */ ) ;
					}
					
				}
				else 
				{
					return ;
				}
				
			}
			
		}
		

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

		MyTestStreamServer myServer ;
	
        LogPlug::info( "Server created, waiting for connections : "
			+ myServer.toString() ) ;
			
		myServer.accept() ;
		
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
