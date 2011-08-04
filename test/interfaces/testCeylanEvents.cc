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
using namespace Ceylan::Log ;


#include <iostream>    // for cerr, endl
#include <exception>

#include <string>
using std::string ;




class TestEvent : public Ceylan::Event
{


	public:

		TestEvent( const std::string & message, EventSource & source ) throw() :
			Event( source ),
			_message( message )
		{


		}


		virtual ~TestEvent() throw()
		{

		}


		virtual const std::string toString( Ceylan::VerbosityLevels level
			= Ceylan::high ) const throw()
		{
			return "Test event: " + _message ;
		}


	protected:

		std::string _message ;

} ;


class FirstListener : public Ceylan::EventListener
{

	public:

		virtual void beNotifiedOf( const Event & newEvent ) throw()
		{
			LogPlug::info( "I am a FirstListener and "
				"I received following event: " + newEvent.toString() ) ;
		}

} ;


class SecondListener : public Ceylan::EventListener
{

	public:

		virtual void beNotifiedOf( const Event & newEvent ) throw()
		{
			LogPlug::info( "I am a SecondListener and "
				"I received following event: " + newEvent.toString() ) ;
		}

} ;



class FirstSource : public Ceylan::EventSource
{

	public:

		FirstSource()
		{

		}


		void sendEvent()
		{

			TestEvent newEvent( "I am an event of first source.", * this ) ;

			notifyAllListeners( newEvent ) ;

		}

} ;


class SecondSource : public Ceylan::EventSource
{

	public:

		SecondSource()
		{

		}


		void sendEvent()
		{
			TestEvent newEvent( "I am an event of second source.", * this ) ;

			notifyAllListeners( newEvent ) ;

		}

} ;




/**
 * Test for Event framework implementation.
 *
 * @see Event, EventSource, EventListener
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;


	try
	{

		LogPlug::info( "Starting testing event framework." ) ;

		FirstSource firstSource ;

		LogPlug::info( "Sending an event to newly created first event source "
			"whereas no listener is registered, displaying source: "
			+ firstSource.toString() ) ;

		firstSource.sendEvent() ;

		LogPlug::info( "Adding a listener to this source." ) ;

		FirstListener firstListener ;
		SecondListener secondListener ;

		firstListener.subscribeTo( firstSource ) ;

		LogPlug::info( "Sending a new event." ) ;
		firstSource.sendEvent() ;

		LogPlug::info( "Again." ) ;
		firstSource.sendEvent() ;

		LogPlug::info( "Adding another listener to this source." ) ;

		secondListener.subscribeTo( firstSource ) ;

		LogPlug::info( "Sending a new event." ) ;
		firstSource.sendEvent() ;

		LogPlug::info( "Subscribing the same new listener "
			"to another source." ) ;

		SecondSource secondSource ;

		secondListener.subscribeTo( secondSource ) ;

		LogPlug::info( "Second source sending a new event." ) ;
		secondSource.sendEvent() ;

		LogPlug::info( "Unsubscribing first listener of first source." ) ;
		firstListener.unsubscribeFrom( firstSource ) ;

		LogPlug::info( "Sending a new event to first source." ) ;
		firstSource.sendEvent() ;


		LogPlug::info( "Unsubscribing first listener from second source, "
			"whereas it had not subscribed to it." ) ;


		bool caught = false ;

		try
		{

			firstListener.unsubscribeFrom( secondSource ) ;

		}
		catch( const EventException & e )
		{
			caught = true ;
			LogPlug::info( "Correct, EventException thrown as expected: "
				+ e.toString() ) ;
		}

		if ( ! caught )
			LogPlug::error( "No exception raised after first listener "
				"unsubscribed from second source whereas "
				"it had not subscribed to it." ) ;


		LogPlug::info( "Unsubscribing second listener from all its sources, "
			"which means from the two existing sources." ) ;

		secondListener.unsubscribeFromAllSources() ;

		LogPlug::info( "New state of second subscriber: "
			+ secondListener.toString() ) ;

		LogPlug::info( "New state of first source: "
			+ firstListener.toString() ) ;

		LogPlug::info( "New state of second source: "
			+ secondListener.toString() ) ;

		LogPlug::info( "Sending a new event to both sources." ) ;
		firstSource.sendEvent() ;
		secondSource.sendEvent() ;

		LogPlug::info( "Adding back the two listeners to first source "
			"just before deallocation to test faulty life cycles." ) ;

		firstListener.subscribeTo( firstSource ) ;
		secondListener.subscribeTo( firstSource ) ;

		LogPlug::info( "End of event framework test." ) ;

		/*
		 * Sources and listeners are deleted in unspecified order, the framework
		 * must be robust.
		 *
		 */

		Ceylan::shutdown() ;

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

	Ceylan::shutdown() ;

	return Ceylan::ExitSuccess ;

}
