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
 * Test class of stream clients, for stream servers.
 *
 * Against a running server to compare implementations, one may use:
 *
 *   - telnet localhost 6969
 *   - ./testCeylanClientStream --consolePlug localhost 6969
 *
 * and watch the result with: netstat -a --tcp -p
 *
 * @see testCeylanSequentialServerStream.cc,
 * testCeylanMultiplexedServerStream.cc
 *
 */
class MyTestStreamClient : public Ceylan::Network::ClientStreamSocket
{


public:


  MyTestStreamClient( bool interactiveMode, bool keepServerAlive ):
	ClientStreamSocket(),
	_interactiveMode( interactiveMode ),
	_keepServerAlive( keepServerAlive )
  {

	LogPlug::info( "MyTestStreamClient created: "
	  + toString() ) ;

  }


  void connected() throw( ClientStreamSocketException )
  {


	LogPlug::info( "Client connected! New state is: "
	  + toString() ) ;


	if ( _interactiveMode )
	{

	  cout << "Type any number of characters, end them by 'Q': "
		   << endl ;

	  char c ;

	  do
	  {

		c = cin.get() ;
		write( &c, 1 ) ;
		//cout << c ;

	  }
	  while( c != 'Q' ) ;

	  cout << endl ;

	  // 'Q' is the latest sent character.

	}
	else
	{

	  string toSend = "This is a test line from client, "
		"stopping connection now! " ;

	  if ( _keepServerAlive )
		toSend += "X" ;
	  else
		toSend += "Q" ;


	  bool beSlow = false ;

	  if ( beSlow )
	  {

		for ( string::const_iterator it = toSend.begin() ;
			  it != toSend.end(); it++ )
		{

		  write( &( *it ), 1 ) ;

		  // Sleep for 0.1 second:
		  Thread::Sleep( 0, 100000 /* microseconds */ ) ;

		}
	  }
	  else
	  {
		write( toSend ) ;
	  }

	  LogPlug::info( "Client sent: '" + toSend + "'." ) ;

	}


	char buffer[ 10 ] ;

	System::Size readSize = read( buffer, 2 ) ;
	if ( readSize > 0 )
	{

	  cout << "Client read from server: '" << buffer
		   << "'." << endl ;

	}
	else
	{

	  throw ClientStreamSocketException(
		"Unexpected size read for server: "
		+ Ceylan::toString( readSize ) + " bytes." ) ;

	}

	const char expectedAnswer = '+' ;

	if ( buffer[0] != expectedAnswer )
	  throw ClientStreamSocketException(
		"Incorrect answer from server: received '"
		+ string( buffer ) + "', instead of '"
		+ Ceylan::toString( expectedAnswer ) + "'." ) ;

  }


private:


  /// Tells whether the user is expected to type messages.
  bool _interactiveMode ;


  /**
   * Tells whether the client should request the server to shutdown after
   * connection (Q is sent) or to stay alive (X is sent).
   *
   */
  bool _keepServerAlive ;

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

	  bool interactiveMode     = false ;
	  bool keepServerAliveMode = false ;

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
			  if ( token == "--keep-alive" )
			  {
				LogPlug::info( "Running in keep-alive mode." ) ;
				keepServerAliveMode = true ;
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


	  MyTestStreamClient myClient( interactiveMode, keepServerAliveMode ) ;


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
