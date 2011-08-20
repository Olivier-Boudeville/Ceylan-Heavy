/*
 * Copyright (C) 2003-2011 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option)
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Network ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <iostream>    // for cout, endl
using namespace std ;

#include <exception>

#include <string>



/**
 * Test class of protocol clients, for protocol servers.
 *
 * Against a running server to compare implementations, one may use:
 *
 *   - telnet localhost 6969
 *
 *   - ./testCeylanProtocolClient --consolePlug localhost 6969
 *
 * and watch the result with: netstat -a --tcp -p
 *
 * @see testCeylanMultiLwProtocolServer.cc
 *
 */
class MyTestProtocolClient : public Ceylan::Network::ClientStreamSocket
{


public:


  MyTestProtocolClient( bool interactiveMode ):
	ClientStreamSocket(),
	_interactiveMode( interactiveMode )
  {

	LogPlug::info( "MyTestProtocolClient created: "
	  + toString() ) ;

  }


  void connected() throw( ClientStreamSocketException )
  {

	LogPlug::info( "Client connected! New state is: "
	  + toString() ) ;


	if ( _interactiveMode )
	{


	  bool keepAlive = true ;

	  do
	  {

		cout << endl << "Type any request ID number, end by 'q': "
			 << "( '1': ping, '2': sum, "
		  "'3': reversed-echo, '4': stop server)" << endl ;

		string toEcho ;

		char requestNumber = cin.get() ;

		// Eat the enter char (\n):
		cin.get() ;

		switch( requestNumber )
		{

		case '1':
		  sendPing() ;
		  break ;

		case '2':
		  sendSum() ;
		  break ;

		case '3':
		  cout << "Enter string to be reverse-echoed: " ;
		  getline( cin, toEcho ) ;
		  sendToEcho( toEcho ) ;
		  break ;

		case '4':
		  sendQuit() ;
		  break ;

		case 'q':
		  keepAlive = false ;
		  break ;

		default:
		  cerr << "Unexpected keypress, ignored" << endl ;

		}


	  }
	  while( keepAlive ) ;

	  cout << "Client exiting now." << endl ;

	}
	else
	{

	  // Chooses among ping, sum, echo, quit:
	  Ceylan::Maths::Random::WhiteNoiseGenerator myRequestChooser(
		1, 4 ) ;

	  Ceylan::Uint16 requestCount = 0 ;

	  while ( requestCount < 5 )
	  {

		requestCount++ ;

		LogPlug::info( "Sending now request #"
		  + Ceylan::toString( requestCount ) + "." ) ;


		switch( myRequestChooser.getNewValue() )
		{

		case 1:
		  sendPing() ;
		  break ;

		case 2:
		  sendSum() ;
		  break ;

		case 3:
		  sendToEcho( "Ceylan rocks, my friend !" ) ;
		  break ;

		default:
		  throw TestException(
			"MyTestProtocolClient:: connected: "
			"random generator returned value out of bounds"
							  ) ;

		}

	  }

	  // Finish by making the server shutdown:
	  sendQuit() ;

	}

  }


  void sendPing()
  {

	LogPlug::info( "MyTestProtocolClient::sendPing triggered" ) ;

	writeUint16( /* ping request ID */ 1 ) ;

	LogPlug::info( "MyTestProtocolClient::sendPing sent ping request, "
	  "waiting for ping answer (pong)." ) ;

	Ceylan::Uint8 res ;

	try
	{

	  res = readUint8() ;

	}
	catch( const InputStream::EOFException & e )
	{

	  LogPlug::info( "MyTestProtocolClient::sendPing: "
		"server not available (" + e.toString() + ")." ) ;

	  return ;

	}

	if ( res != 120 )
	  throw TestException( "MyTestProtocolClient::sendPing received "
		+ Ceylan::toNumericalString( res ) + " instead of 120." ) ;

	LogPlug::info( "MyTestProtocolClient::sendPing: "
	  "correct pong received." ) ;

	cout << "Pong OK!" ;


  }


  void sendSum()
  {

	LogPlug::info( "MyTestProtocolClient::sendSum triggered" ) ;

	writeUint16( /* sum request ID */ 2 ) ;

	// Generates random bytes (in [0;255]):
	Ceylan::Maths::Random::WhiteNoiseGenerator myRandomGen( 0, 256 ) ;

	Ceylan::Uint32 localSum = 0 ;
	Ceylan::Uint32 localValue ;

	Ceylan::Uint8 toAdd ;

	// 80 bytes needed:
	for ( Size i = 0; i < 80; i++ )
	{

	  toAdd = static_cast<Ceylan::Uint8>(
		myRandomGen.getNewValue() ) ;

	  writeUint8( toAdd ) ;

	  // To use the full 32-bit range (hopefully):
	  localValue = static_cast<Ceylan::Uint32>(
		Maths::Pow( static_cast<Ceylan::Float32>( toAdd ), 3 ) *
		( i + 1 ) ) ;

	  LogPlug::debug( "Adding randomized value "
		+ Ceylan::toString( localSum ) ) ;

	  localSum += localValue ;

	  if ( _interactiveMode )
	  {
		// Sleep for 0.1 second:
		Thread::Sleep( 0, 100000 /* microseconds */ ) ;
	  }

	}

	Ceylan::Uint32 receivedSum = readUint32() ;

	if ( receivedSum != localSum )
	  throw TestException( "MyTestProtocolClient::sendSum received "
		+ Ceylan::toNumericalString( receivedSum )
		+ " as a server-computed sum, whereas local sum is "
		+ Ceylan::toNumericalString( localSum ) + "." ) ;

	LogPlug::info( "MyTestProtocolClient::sendSum: "
	  "local and server-side sums match ("
	  + Ceylan::toString( receivedSum ) + ")." ) ;

	cout << "Sum OK!" ;

  }


  void sendToEcho( const std::string & toBeEchoed )
  {

	writeUint16( /* reversed echo request ID */ 3 ) ;

	writeString( toBeEchoed ) ;

	string mirrored = Ceylan::reverse( toBeEchoed ) ;

	LogPlug::info( "MyTestProtocolClient::sendToEcho: "
	  "will request '" + toBeEchoed
	  + "' to be mirrored, it should result in '"
	  + mirrored + "' (string length is "
	  + Ceylan::toString(
		static_cast<Ceylan::Uint32>( toBeEchoed.size() ) )
	  + " bytes)." ) ;

	string receivedEcho ;
	readString( receivedEcho ) ;

	if ( receivedEcho != mirrored )
	  throw TestException(
		"MyTestProtocolClient::sendToEcho received '"
		+ receivedEcho + "' as a server-mirrored string of '"
		+ toBeEchoed + "', whereas '" + mirrored
		+ "' was expected." ) ;

	LogPlug::info( "MyTestProtocolClient::sendToEcho: "
	  "local and server-side reversed echo match ("
	  + receivedEcho + ")." ) ;

	cout << "Echo OK!" ;

  }


  void sendQuit()
  {

	writeUint16( /* quit request ID */ 4 ) ;

  }



private:


  /// Tells whether the user is expected to type messages.
  bool _interactiveMode ;


} ;




/**
 * Test of Ceylan client stream.
 *
 * @see ServerStreamSocket.
 *
 */
int main( int argc, char * argv[] )
{

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


	  // Determines which server and port should be chosen:

	  string targetServer ;

	  std::string executableName ;
	  std::list<std::string> options ;

	  bool interactiveMode = false ;

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
		  LogPlug::info( "Running in batch mode." ) ;
		  interactiveMode = false ;
		  tokenEaten = true ;
		} else
		  if ( token == "--online" )
		  {
			// Ignored:
			tokenEaten = true ;
		  } else
			if ( token == "--interactive" )
			{
			  LogPlug::info( "Running in interactive mode." ) ;
			  interactiveMode = true ;
			  tokenEaten = true ;
			} else
			  if ( token == "--server" )
			  {
				targetServer = options.front() ;
				options.pop_front() ;
				LogPlug::info( "Will try to connect to server '"
				  + targetServer + "'." ) ;
				tokenEaten = true ;
			  } else
				if ( LogHolder::IsAKnownPlugOption( token ) )
				{
				  // Ignores log-related (argument-less) options.
				  tokenEaten = true ;
				}

		if ( ! tokenEaten )
		{
		  LogPlug::error( "Unexpected command line argument: "
			+ token ) ;
		}

	  }


	  MyTestProtocolClient myClient( interactiveMode ) ;


	  if ( targetServer.empty() )
		targetServer = "localhost" ;

	  LogPlug::info( "Client created: " + myClient.toString()
		+ ", will try to connect to '" + targetServer + "'." ) ;


	  // Now, connect and communicate:

	  myClient.connect( targetServer, /* port */ 6969 ) ;


	  LogPlug::info( "Connection terminated, current client state is: "
		+ myClient.toString() ) ;

	  LogPlug::info( "End of network stream client test." ) ;


	}

	catch ( const Ceylan::Exception & e )
	{

	  std::cerr << "Ceylan exception caught: "
				<< e.toString( Ceylan::high ) << std::endl ;

	  /*
	   * Added here, as Valgrind (through valgrindTest.sh) will run the client
	   * without the server, thus leading to a connection refused error ending
	   * up in this code path, which must be as a consequence preferably free of
	   * alarms:
	   *
	   */
	  Ceylan::shutdown() ;

	  return Ceylan::ExitFailure ;

	}

	catch ( const std::exception & e )
	{
	  std::cerr << "Standard exception caught: "
				<< e.what() << std::endl ;
	  return Ceylan::ExitFailure ;

	}

	catch ( ... )
	{
	  std::cerr << "Unknown exception caught" << std::endl ;
	  return Ceylan::ExitFailure ;

	}

  }

  Ceylan::shutdown() ;

  return Ceylan::ExitSuccess ;

}
