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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "CeylanEventSource.h"


#include "CeylanLogPlug.h"         // for LogPlug
#include "CeylanOperators.h"
#include "CeylanEventListener.h"   // for EventListener::toString
#include "CeylanStringUtils.h"     // for formatStringList


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"          // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;



EventSource::EventSource() :
	_listeners()
	//,_events()
{

}



EventSource::EventSource( EventListener & listener ) :
	_listeners()
	//,_events()
{

	add( listener ) ;
	
}



EventSource::~EventSource() throw()
{

	if ( ! _listeners.empty() )
	{
	
		LogPlug::error( "EventSource destructor: " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( _listeners.size() ) )
			+ " listener(s) still registered: faulty life cycle, "
			"temporary parachute deployed, "
			"the listeners should have been unsubscribed before "
			"(for example they may have to be deallocated before "
			"their sources)." ) ;
				
		/*
		 * Allows to avoid a crash when this/these listener(s) will be
		 * deallocated, since 'this' will not exist any more 
		 *
		 * @note One should <b>not</b> rely on this last-chance security.
		 *
		 */
			
		for ( list<EventListener *>::iterator it = _listeners.begin(); 
				it != _listeners.end(); it++ )
		{
			(*it)->forgetSource( * this ) ;
		}
		
		// _listeners not owned.
		
	}
	
	_listeners.clear() ;
	
	
	// Events are to be deleted if stored (which is not the case):
	
	/*
	for ( std::list<Event *>::iterator it = _events.begin(); 
			it != _events.end(); it ++ )
		delete (*it) ;
	 */
	 	
}



void EventSource::add( EventListener & listener )
{

	for ( list<EventListener *>::const_iterator it = _listeners.begin();
		it != _listeners.end(); it++ )
	{
			if  ( (*it) == & listener )
				throw EventException( "EventSource::add: event listener (" 
					+ listener.toString() + ") is already registered." ) ;
		
	}
					
#if CEYLAN_DEBUG_EVENTS		
	LogPlug::debug( "A new event listener registered: " 
		+ listener.toString() ) ;
#endif // CEYLAN_DEBUG_EVENTS
		
	_listeners.push_back( & listener ) ;

			
}



void EventSource::remove( const EventListener & listener ) 
{

	/*
	 * This seems to be one of the only correct forms for iterating on a
	 * list, examining elements, destroying the ones that match some criteria,
	 * while keeping the record of matching elements.
	 *
	 * Most other forms are hazardous usages of the STL.
	 *
	 */
	bool found = false ;

	list<EventListener *>::iterator it = _listeners.begin() ;
	
	while ( it != _listeners.end() )
	{
	
		if ( (*it) == & listener )
		{
	
			//(*it)->detachedFromSource( * this ) ;
				
			/*
			 * At most one pointer to this source should be registered.
			 * 
			 * @see add
			 *
			 */
			 
			if ( ! found )
				found = true ;
			else
				LogPlug::error( "EventSource::remove: this event source "
					"was linked more than once with listener " 
					+ (*it)->toString() ) ;

#if CEYLAN_DEBUG_EVENTS		
			LogPlug::debug( "EventSource::remove: "
				"this event source acknowledges "
				"unsubscription from listener " + (*it)->toString() ) ;
#endif // CEYLAN_DEBUG_EVENTS

			// Directly points to the next element:
			it = _listeners.erase( it ) ;
						
		}	
		else
		{
			++it ;	
		}
		
	}

	if ( ! found )
		throw EventException( "EventSource::remove: event listener " 
			+ listener.toString() + " was not already registered." ) ;
	
}



void EventSource::removeAllListeners()
{

	// Avoid the use of iterators that could be invalidated:
	while ( ! _listeners.empty() )
		_listeners.front()->unsubscribeFrom( *this ) ;
				
}



const string EventSource::toString( Ceylan::VerbosityLevels level ) const
{

	if ( _listeners.empty() )
	{
		return "Event source has currently no registered listener" ;
	}	
	else
	{
	
		if ( level == Ceylan::low )
			return "Event source has currently " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( _listeners.size() ) )
			+ " registered listener(s)" ; 
		else
		{
			
			list<string> listenersDescription  ;
				
			for ( list<EventListener *>::const_iterator it = _listeners.begin();
					it != _listeners.end(); it++ )
				listenersDescription.push_back( (*it)->toString() ) ;
			
			return "Event source has currently " 
				+ Ceylan::toString( 
					static_cast<Ceylan::Uint32>( _listeners.size() ) )
				+ " registered listener(s): " 
				+ Ceylan::formatStringList( listenersDescription ) ;
	
		}
			
	}
}



bool EventSource::isRegistered( const EventListener & listener )
{

	for ( list<EventListener *>::const_iterator it = _listeners.begin();
		it != _listeners.end(); it++ )
	{
		 
		if ( (*it) == & listener ) 
			return true ;
		 
	}	
		 
	return false ;	 
	
}



void EventSource::notifyAllListeners( const Event & newEvent )
{

	// _events.push_back( & newEvent ) ;
	
	for ( list<EventListener *>::iterator it = _listeners.begin(); 
		it != _listeners.end(); it++ )
	{
		(*it)->beNotifiedOf( newEvent ) ;	
	}
	
}

