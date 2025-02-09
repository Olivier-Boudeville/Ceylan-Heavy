/*
 * Copyright (C) 2003-2013 Olivier Boudeville
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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Maths ;
using namespace Ceylan::System ;


#include <string>
using std::string ;




/**
 * Test of Ceylan memory stream management.
 *
 * @see Ceylan::System::MemoryStream.
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{


	  LogPlug::info( "Testing Ceylan's memory stream support." ) ;

	  Size streamSize = 20 ;

	  MemoryStream myMemoryStream( streamSize /* bytes */ ) ;

	  Ceylan::Uint8 toWrite = 0 ;

	  // First, fill up the buffer and check everything fit in it:
	  for ( Size s = 0; s < streamSize; s++ )
	  {
		LogPlug::info( "Fill test: writing '"
		  + Ceylan::toNumericalString( toWrite ) + "'." ) ;
		myMemoryStream.writeUint8( toWrite ) ;
		toWrite++ ;
	  }

	  Ceylan::Uint8 toRead  = 0 ;
	  Ceylan::Uint8 readValue ;

	  // Empties the buffer:
	  for ( Size s = 0; s < streamSize; s++ )
	  {
		readValue = myMemoryStream.readUint8() ;
		if ( readValue != toRead )
		  throw TestException( "Fill test: expected to read '"
			+ Ceylan::toNumericalString( toRead ) + "', read '"
			+ Ceylan::toNumericalString( readValue ) + "'." ) ;
		else
		  LogPlug::info( "Fill test: read '"
			+ Ceylan::toNumericalString( toRead ) + "'." ) ;

		toRead++ ;
	  }


	  LogPlug::info( "Fill test succeeded, now trying disordered test." ) ;


	  // Second, disordered read/write operations:
	  Ceylan::Uint32 io_count = 0 ;

	  Ceylan::Uint32 inBuffer = 0 ;

	  bool shouldRead ;


	  // Can draw 0 or 1:
	  Random::WhiteNoiseGenerator myGenerator( 0, 2 ) ;

	  // Read/write in disorder and wrap-around the full buffer:

	  while ( io_count < 2000 )
	  {

		if ( ( io_count % 50 ) == 0 )
		  LogPlug::debug( "State of memory stream: "
			+ myMemoryStream.toString() ) ;


		if ( inBuffer == 0 )
		{

		  // Order to write (only possibility):
		  LogPlug::debug( "Forced writing." ) ;
		  shouldRead = false ;

		}
		else if ( inBuffer == streamSize )
		{

		  // Can only read:
		  LogPlug::debug( "Forced reading." ) ;
		  shouldRead = true ;

		}
		else
		{

		  // Can write or read:

		  if ( myGenerator.getNewValue() == 0 )
		  {

			// 0: write
			shouldRead = false ;

		  }
		  else
		  {

			// 1: read
			shouldRead = true ;

		  }
		}

		if ( shouldRead )
		{

		  readValue = myMemoryStream.readUint8() ;
		  if ( readValue != toRead )
			throw TestException( "Disordered test: expected to read '"
			  + Ceylan::toNumericalString( toRead ) + "', read '"
			  + Ceylan::toNumericalString( readValue ) + "'." ) ;
		  toRead++ ;
		  inBuffer-- ;

		}
		else
		{

		  myMemoryStream.writeUint8( toWrite ) ;
		  toWrite++ ;
		  inBuffer++ ;

		}

		io_count++ ;

	  }

	  LogPlug::info( "Disordered test succeeded." ) ;




	  LogPlug::info( "Now testing automatic buffer reorganization, "
		"first with only one whole block to move." ) ;

	  myMemoryStream.blank() ;

	  Size wroteBytes = 10 ;

	  for ( Size i = 0; i < wroteBytes; i++ )
		myMemoryStream.writeUint8( static_cast<Uint8>( i ) ) ;

	  // Buffer should be: 0|1|2|3|4|5|6|7|8|9|XXXXXX...

	  Size readBytes = 3 ;

	  // Ignored reads:
	  for ( Size i = 0; i < readBytes; i++ )
		myMemoryStream.readUint8() ;

	  Size remainingBytes = wroteBytes - readBytes ;

	  /*
	   * Buffer should be now: X|X|X|3|4|5|6|7|8|9|XXXXXX...
	   * Filled index should be 3, length 7:
	   *
	   */
	  if ( myMemoryStream.getBlockIndex() != readBytes )
		throw TestException( "Wrong filled index: expected to be "
		  + Ceylan::toString( static_cast<Ceylan::Uint32>( readBytes ) )
		  + ", is "
		  + Ceylan::toString( static_cast<Ceylan::Uint32>(
			  myMemoryStream.getBlockIndex() ) ) + "." ) ;

	  if ( myMemoryStream.getBlockLength() != remainingBytes )
		throw TestException( "Wrong filled length: expected to be "
		  + Ceylan::toString(
			static_cast<Ceylan::Uint32>( remainingBytes ) ) + ", is "
		  + Ceylan::toString( static_cast<Ceylan::Uint32>(
			  myMemoryStream.getBlockLength() ) )
		  + "." ) ;

	  myMemoryStream.moveFilledBlockToBufferStart() ;

	  if ( myMemoryStream.getBlockIndex() != 0 )
		throw TestException(
		  "Wrong filled index after move: expected to be 0, is "
		  + Ceylan::toString( static_cast<Ceylan::Uint32>(
			  myMemoryStream.getBlockIndex() ) ) + "." ) ;

	  if ( myMemoryStream.getBlockLength() != remainingBytes )
		throw TestException(
		  "Wrong filled length after move: expected to be "
		  + Ceylan::toString(
			static_cast<Ceylan::Uint32>( remainingBytes ) ) + ", is "
		  + Ceylan::toString( static_cast<Ceylan::Uint32>(
			  myMemoryStream.getBlockLength() ) )
		  + "." ) ;


	  // We should have here: 3|4|5|6|7|8|9|XXXXXX

	  Ceylan::Uint8 newRead = myMemoryStream.getElementAt( 0 ) ;

	  if ( newRead != 3 )
		throw TestException( "Wrong element read at index 0 after move ("
		  + Ceylan::toNumericalString( newRead )
		  + "), 3 was expected." ) ;

	  newRead = myMemoryStream.getElementAt( 6 ) ;

	  if ( newRead != 9 )
		throw TestException( "Wrong element read at index 6 after move ("
		  + Ceylan::toNumericalString( newRead )
		  + "), 9 was expected." ) ;


	  for ( Size i = readBytes; i < wroteBytes; i++ )
	  {

		newRead = myMemoryStream.readUint8() ;
		if ( newRead != i )
		  throw TestException( "Wrong element read ("
			+ Ceylan::toNumericalString( newRead )
			+ "), " + Ceylan::toNumericalString(
			  static_cast<Ceylan::Uint8>( i ) )
			+ " was expected." ) ;

	  }




	  LogPlug::info( "Now testing automatic buffer reorganization, "
		"second with two chunks to move." ) ;


	  myMemoryStream.blank() ;

	  wroteBytes = 18 ;

	  for ( Size i = 0; i < wroteBytes; i++ )
		myMemoryStream.writeUint8(
		  static_cast<Ceylan::Uint8>( i ) ) ;

	  // Buffer should be: 0|1|2|3|4|5|6|7|8|9|..|16|17|X|X

	  readBytes = 15 ;

	  // Ignored reads:
	  for ( Size i = 0; i < readBytes; i++ )
		myMemoryStream.readUint8() ;

	  // Buffer should be: X|X|X|..X|15|16|17|X|X

	  wroteBytes = 5 ;
	  for ( Size i = 0; i < wroteBytes; i++ )
		myMemoryStream.writeUint8(
		  static_cast<Ceylan::Uint8>( i ) ) ;


	  /*
	   * Buffer should be now: 2|3|4|X|X|X|..X|15|16|17|0|1.
	   * Filled index should be 14, length 8:
	   *
	   */
	  if ( myMemoryStream.getBlockIndex() != readBytes )
		throw TestException(
		  "Wrong filled index: expected to be "
		  + Ceylan::toString( 14 ) + ", is "
		  + Ceylan::toString( static_cast<Ceylan::Uint32>(
			  myMemoryStream.getBlockIndex() ) ) + "." ) ;

	  remainingBytes = 8 ;

	  if ( myMemoryStream.getBlockLength() != remainingBytes )
		throw TestException(
		  "Wrong filled length: expected to be "
		  + Ceylan::toString(
			static_cast<Ceylan::Uint32>( remainingBytes ) ) + ", is "
		  + Ceylan::toString( static_cast<Ceylan::Uint32>(
			  myMemoryStream.getBlockLength() ) ) + "." ) ;


	  myMemoryStream.moveFilledBlockToBufferStart() ;

	  // We should now have here: 15|16|17|0|1|2|3|4|X|X|X|..X|

	  if ( myMemoryStream.getBlockIndex() != 0 )
		throw TestException(
		  "Wrong filled index after move: expected to be 0, is "
		  + Ceylan::toString( static_cast<Ceylan::Uint32>(
			  myMemoryStream.getBlockIndex() ) ) + "." ) ;

	  if ( myMemoryStream.getBlockLength() != remainingBytes )
		throw TestException(
		  "Wrong filled length after move: expected to be "
		  + Ceylan::toString(
			static_cast<Ceylan::Uint32>( remainingBytes ) )
		  + ", is "
		  + Ceylan::toString( static_cast<Ceylan::Uint32>(
			  myMemoryStream.getBlockLength() ) )
		  + "." ) ;


	  newRead = myMemoryStream.getElementAt( 0 ) ;

	  // To debug: Ceylan::checkpoint( myMemoryStream.toString() ) ;

	  if ( newRead != 15 )
		throw TestException( "Wrong element read at index 0 ("
		  + Ceylan::toNumericalString( newRead )
		  + ") after move, 15 was expected." ) ;


	  newRead = myMemoryStream.getElementAt( 7 ) ;

	  if ( newRead != 4 )
		throw TestException( "Wrong element read at index 7 ("
		  + Ceylan::toNumericalString( newRead )
		  + ") after move, 4 was expected." ) ;

	  for ( Size i = 15; i < 18; i++ )
	  {

		newRead = myMemoryStream.readUint8() ;
		if ( newRead != i )
		  throw TestException( "Wrong element read ("
			+ Ceylan::toNumericalString( newRead )
			+ ") after move, "
			+ Ceylan::toNumericalString(
			  static_cast<Ceylan::Uint8>( i ) )
			+ " was expected." ) ;

	  }

	  for ( Size i = 0; i < 5; i++ )
	  {

		newRead = myMemoryStream.readUint8() ;
		if ( newRead != i )
		  throw TestException( "Wrong element read ("
			+ Ceylan::toNumericalString( newRead )
			+ ") after move, "
			+ Ceylan::toNumericalString(
			  static_cast<Ceylan::Uint8>( i ) )
			+ " was expected." ) ;

	  }


	  LogPlug::info( "End of memory stream test." ) ;


	}

	catch ( const Ceylan::Exception & e )
	{
	  std::cerr << "Ceylan exception caught: "
				<< e.toString( Ceylan::high ) << std::endl ;
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
