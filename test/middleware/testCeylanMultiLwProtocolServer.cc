#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Network ;
using namespace Ceylan::System ;
using namespace Ceylan::Middleware ;
using namespace Ceylan::Log ;


#include <iostream>    // for cout, endl
using namespace std ;

#include <exception>

#include <string>
#include <list>



/**
 * This specific protocol server manages the chosen test protocol :
 *   - first data received must be a Ceylan::Uint16 containing the request ID :
 *
 *       + ID #1 : it is a ping request, which must be answered with a
 * Ceylan::Uint8 equal to 120 (funny protocol, hey !). No further data is to be
 * sent for this ping request, and the connection is not terminated.
 *
 *       + ID #2 : it is a fixed-length requests, 80 bytes have to be read,
 * the (possibly overflowed) sum of all these bytes, stored in one byte, 
 * is returned. Connection is still alive afterwards.
 *
 *       + ID #3 : it is an reversed-echo request, first two bytes are the
 * length of the sent string (Uint16), then the string itself. The string 
 * that is sent back is the mirrored one. Connection is still alive afterwards.
 *
 *       + ID #4 : just quit (stop the connection).
 *
 * This example shows how to deal with variable messages, some of them being
 * themselves of variable length.
 *
 * This server is triggered by a protocol-aware connection socket, that
 * will call its notifyDataAvailability method.
 *
 * Between two data arrival notifications, the history of the server is saved
 * thanks to a state variable (_currentState).
 *
 */
class MyProtocolServer : public Ceylan::Middleware::ProtocolServer
{


	public:
	
	
	
		/**
		 * This protocol server supports only LightWeightMarshaller.
		 *
		 */
		MyProtocolServer( LightWeightMarshaller & lwMarshaller ) throw() :
			ProtocolServer( lwMarshaller ),
			_currentState( 0 )
		{
		
		}
		
		
		bool notifyDataAvailability() throw( ProtocolException )
		{
		
		
			LogPlug::info( "MyProtocolServer::notifyDataAvailability : "
				"server awoken while in state " 
				+ Ceylan::toString( _currentState ) ) ;
				
				
			// Winners take all :
			Size availableReadSize = 
				_marshaller->retrieveData( /* read everything you can */ 0 ) ;

			LogPlug::info( "MyProtocolServer::notifyDataAvailability : "
				"server fed with " 
				+ Ceylan::toString( availableReadSize) + " byte(s)." ) ;
			
			if ( availableReadSize == 0 )
			{
			
				LogPlug::error( "MyProtocolServer::notifyDataAvailability : "
					"awoken whereas no byte to read" ) ;
				return true ;
					
			}
				
			// We need to downcast our marshaller to use it :
			LightWeightMarshaller * lwMarshaller = 
				dynamic_cast<LightWeightMarshaller *>( _marshaller ) ;
				
			if ( lwMarshaller == 0 )
				throw ProtocolException(
					"MyProtocolServer::notifyDataAvailability : "
					"wrong type for marshaller." ) ;

			// Dispatch according to the current state :
			switch( _currentState )
			{
				
				case 0:
					return dispatchNewRequest( availableReadSize, 
						*lwMarshaller ) ;
					break ;
				
				case 1:
					throw ProtocolException(
						"MyProtocolServer::notifyDataAvailability : "
						"unexpected transient ping state encountered." ) ;
						
				case 2:
					return handleSum( availableReadSize, 
						*lwMarshaller ) ;
				
				case 3:
					return handleReversedEcho( availableReadSize, 
						*lwMarshaller ) ;
				
				default:
					throw ProtocolException(
						"MyProtocolServer::notifyDataAvailability : "
						"unexpected state." ) ;
				
			}
			
		}



		bool dispatchNewRequest( Size availableReadSize, 
			LightWeightMarshaller & lwMarshaller ) 
		{
		
			// Needing at least two bytes for the initial ID :			
			if ( availableReadSize > 1 )
			{
			
						
				Ceylan::Uint16 id = lwMarshaller.decodeUint16() ;
				availableReadSize -= 2 ;
					
				LogPlug::info( "MyProtocolServer::dispatchNewRequest : "
					"new request received, whose ID is " 
					+ Ceylan::toString( id ) + "." ) ;
						
										
				switch( id )
				{
				
					case 1:
						return handlePing( lwMarshaller ) ;
						break ;
								
					
					case 2:
						return handleSum( availableReadSize, 
							lwMarshaller ) ;
						break ;
						
						
					case 3:
						return handleReversedEcho( availableReadSize, 
							lwMarshaller ) ;
						break ;
						
						
					case 4:
						return handleQuit( availableReadSize, 
							lwMarshaller ) ;
						break ;
						
	
					default:
						return handleUnknownRequest( id ) ;
						break ;
										
				}
					
			}
			else
			{
			
				LogPlug::info( "MyProtocolServer::dispatchNewRequest : "
					"not enough data for ID, maybe next time will be fine, "
					"letting the connection live." ) ;
					
				return true ;
					
			}
				
			
		}
		
		
		bool handlePing( LightWeightMarshaller & marshaller ) 
			throw( ProtocolException )
		{
						
		    /*
		     * Changing the internal state here is useless but
		     * clearer :
		     *
		     */
			_currentState = 1 ;
			
			LogPlug::info( "MyProtocolServer::handlePing triggered." ) ;
						
			// This is a ping request, let's answer (nothing more to read) :
			marshaller.encodeUint8( 120 ) ;
			_currentState = 0 ;
			
			LogPlug::info( "MyProtocolServer::handlePing over." ) ;
			
			// Decide not to end the connection :
			return true ;
		
		}
		
		
		bool handleSum( Size availableReadSize, 
			LightWeightMarshaller & marshaller ) throw( ProtocolException )
		{
		
			_currentState = 2 ;

			LogPlug::info( "MyProtocolServer::handleSum triggered." ) ;
			
			/*
			 * We need 80 bytes, actually the buffer size has been chosen so
			 * that it cannot be achieved in one call, let's wait for it :
			 * (as the next 80-byte call will fit in buffer, no need to perform 
			 * partial sums)
			 *
			 */
			if ( availableReadSize < 80 )
			{
				LogPlug::info( "MyProtocolServer::handleSum : "
					"waiting for more data (got " 
					+ Ceylan::toString( availableReadSize ) + "/80 bytes)." ) ;
				return true ;
			}	
			
			LogPlug::info( "MyProtocolServer::handleSum : "
					"having enough data now (got " 
					+ Ceylan::toString( availableReadSize ) + "/80 bytes)." ) ;
			
			// Let's try a real 32-bit variable for endianess testing :
			Ceylan::Uint32 localSum = 0 ;
			Ceylan::Uint32 localValue ;
			Ceylan::Uint8  toAdd ;
			
			for ( Size i = 0; i < 80; i++ )
			{
				toAdd = marshaller.decodeUint8() ;
				localValue = static_cast<Ceylan::Uint32>(
					Maths::Pow( static_cast<Ceylan::Float32>( toAdd ), 3 ) * 
					( i + 1 ) ) ;

				LogPlug::debug( "Adding randomized value " 
					+ Ceylan::toString( localSum ) ) ;
					
				localSum += localValue ;
					
			}
			
				
			LogPlug::info( "MyProtocolServer::handleSum : "
					"returning sum : " + Ceylan::toString( localSum ) ) ;
								
			marshaller.encodeUint32( localSum ) ;	
			_currentState = 0 ;
			
			return true ;	
		
		}
		
		
		bool handleReversedEcho( Size availableReadSize, 
			LightWeightMarshaller & marshaller ) throw( ProtocolException )
		{
		
			LogPlug::info( "MyProtocolServer::handleReversedEcho triggered." ) ;
			
			/*
			 * Echoes strings can be more than 80-byte long, we need 
			 * intermediate storage :
			 *
			 * (we cannot use LightWeightMarshaller::decodeString here since
			 * we cannot know for sure whether all necessary bytes are already
			 * available)
			 *
			 */
			static string stringToEcho ;
			static Size targetLength ;
			static bool lengthRead ;
			
			
			if ( _currentState == 0 )
			{
			
				LogPlug::info( "MyProtocolServer::handleReversedEcho : "
					"starting operation" ) ;
					
				// Beginning of action, there is at last a byte to read :
				_currentState = 3 ;
				lengthRead = false ;
				stringToEcho.clear() ;
			
			}
			
			
			if ( ! lengthRead )
			{
				
				// Previously decoded request ID might have emptied the stream :
				if ( availableReadSize > 1 )
				{
				
					LogPlug::info( "MyProtocolServer::handleReversedEcho : "
						"reading string length." ) ;
						
					targetLength = marshaller.decodeUint16() ;
					availableReadSize -= 2 ;
					
					lengthRead = true ;
					
					LogPlug::info( "MyProtocolServer::handleReversedEcho : "
						"string length being read is " 
						+ Ceylan::toString( targetLength ) + "." ) ;
					
				}
				else
				{
				
					LogPlug::info( "MyProtocolServer::handleReversedEcho : "
						"not enough data to read string length, will wait." ) ;
				
					return true ;
					
				}	
			
			}
			
			Size toReadCount = Ceylan::Maths::Min( availableReadSize,
				targetLength ) ;
			
			LogPlug::info( "MyProtocolServer::handleReversedEcho : "
				"continuing operation, having " 
				+ Ceylan::toString( availableReadSize ) 
				+ " byte(s) in buffer, aiming at " 
				+ Ceylan::toString( targetLength ) + " byte(s), requesting "
				+ Ceylan::toString( toReadCount ) 
				+ " byte(s), pre-accumulated string is '" 
				+ stringToEcho + "'." ) ;
				
					
			for ( Size i = 0; i < toReadCount; i++ )
			{
				Ceylan::Uint8 readChar = marshaller.decodeUint8() ;
				LogPlug::debug( "MyProtocolServer::handleReversedEcho : "
					"read character '"
					+ Ceylan::toNumericalString( readChar ) + "'." ) ;
					
				stringToEcho += static_cast<char>( readChar ) ;
				
			}
			
			LogPlug::info( "MyProtocolServer::handleReversedEcho : "
				"post-accumulated string is now '" + stringToEcho + "'." ) ;
				
			
			targetLength -= toReadCount ;
			
			if ( targetLength == 0 )
			{
				
				LogPlug::info( "MyProtocolServer::handleReversedEcho over, "
					"request completed, with total string length being " 
					+ Ceylan::toString( stringToEcho.size() ) + "." ) ;
					
				string reversed = Ceylan::reverse( stringToEcho ) ;
					
				// We read the full string, we can now answer :	
				marshaller.encodeString( reversed  ) ;
				_currentState = 0 ;
				
				LogPlug::info( "MyProtocolServer::handleReversedEcho over, "
					"request completed by sending '" + reversed + "'." ) ;

			}
			else
			{
				LogPlug::info( "MyProtocolServer::handleReversedEcho over, "
					"still waiting for more data." ) ;
			}
							 
			return true ;
			
		}
		
		
		bool handleQuit( Size availableReadSize, 
			LightWeightMarshaller & marshaller ) throw( ProtocolException )
		{
		
			LogPlug::info( "MyProtocolServer::handleQuit triggered." ) ;
		
			askForShutdown() ;
			
			LogPlug::info( "MyProtocolServer::handleQuit over." ) ;
			
			// Decide to end the connection :
			return false ;
		
		}
		
		
		bool handleUnknownRequest( Ceylan::Uint16 id ) 
			throw( ProtocolException )
		{
		
			LogPlug::info( "MyProtocolServer::handleSum triggered." ) ;

			// Unknown request, let's kill the connection :
			LogPlug::error(
				"MyProtocolServer::handleUnknownRequest : "
				"unknown request ID (#" + Ceylan::toString( id )
				+ "), killing the connection." ) ;

			LogPlug::info( "MyProtocolServer::handleSum over." ) ;

			return false ;
		
		}
		
		
			
		
		
	private:
	
		/**
		 * The current state of progress for that connection.
		 *
		 * Here a state number corresponds roughly to the ID of the request
		 * being processed.
		 *
		 * The state 0 corresponds to no pending request.
		 * State 1 is for ping.
		 * State 2 is for sum.
		 * State 3 is for echo.
		 * State 4 is for quit.
		 * 
		 */
		Ceylan::Uint16 _currentState ;
		
} ;



/**
 * This test server is multiplexed and uses light-weight marshalling
 * thanks to a dedicated protocol server.
 *
 */
class MyTestMultiLwProtocolServer : 
	public Ceylan::Network::MultiplexedProtocolBasedStreamServer
{


	public:
	
	
		MyTestMultiLwProtocolServer( bool isBatch ):
			MultiplexedProtocolBasedStreamServer( 6969, /* reuse */ true ),
			_batch( isBatch )
		{
		
			LogPlug::info( "MyTestMultiLwProtocolServer created : "
				+ toString() ) ;
				
		}
		
		
		void accepted( AnonymousStreamSocket & newConnection )
			throw( ServerStreamSocketException )
		{
			
			AnonymousProtocolAwareStreamSocket * connectionSocket =
				dynamic_cast<AnonymousProtocolAwareStreamSocket *>(
					& newConnection ) ;
			
			if ( connectionSocket == 0 )
				throw TestException( "MyTestMultiLwProtocolServer::accepted : "
					"connection socket is not an "
					"AnonymousProtocolAwareStreamSocket instance." ) ;
			
			/*
			 * Let a (new) marshaller take control of data exchanges with the
			 * connection-dedicated socket. 
			 *
			 */
			LightWeightMarshaller * myMarshaller = 
				new LightWeightMarshaller( *connectionSocket,
					/* bufferedSize */ 80 ) ;
			
			/*
			 * Let's create the server-specific protocol manager which will
			 * use the specified marshaller to take care of lower-level
			 * data conversion :
			 *
			 */
			MyProtocolServer * myProtocolServer = 
				new MyProtocolServer( *myMarshaller ) ;
				
			/* 
			 * The socket takes ownership of the protocol server that itself
			 * owns the marshaller.
			 * Hence when the connection will be over, the connection-dedicated
			 * socket will be deallocated, which in turn will delete the 
			 * protocol server, which in turn will delete the marshaller.
			 *
			 */
			connectionSocket->setProtocolServer( *myProtocolServer ) ;


			// Ready to manage incoming requests !
					
		}
			
		
	
	private:
	
	
		bool _batch ;
				
		/*
		 * The internal marshaller.
		 *
		 * As it is a state-less marshaller, only one instance can be used
		 * by a set of connections. If it was stateful, then each connection
		 * would have to be associated with its o
		 *
		 */
		Ceylan::Middleware::LightWeightMarshaller * _marshaller ;
		
		
} ;



/**
 * Test of Ceylan light-weight marshalling in the context of a 
 * multiplexed server.
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


        LogPlug::info( "Testing Ceylan's network implementation "
			"of multiplexed server using an internal protocol server "
			"and a light-weight marshaller." ) ;


		if ( ! Features::isNetworkingSupported() )
		{
			LogPlug::info( 
				"No network support available, no test performed." ) ;
			return Ceylan::ExitSuccess ;
		}
		
		
		bool isBatch = false ;
		
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
		
		
		MyTestMultiLwProtocolServer myServer( isBatch ) ;
	
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
			
		
        LogPlug::info( 
			"End of multiplexed server with protocol and marshalling test." ) ;


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
