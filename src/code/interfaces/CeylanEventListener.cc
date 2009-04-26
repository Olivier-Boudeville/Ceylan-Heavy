/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#include "CeylanEventListener.h"

#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanOperators.h"    // for Ceylan::toString
#include "CeylanEventSource.h"  // for EventSource::toString


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for CEYLAN_DEBUG_EVENTS
#endif // CEYLAN_USES_CONFIG_H


using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;




EventListener::EventListener() :
	_sources()
{

}



EventListener::EventListener( EventSource & source ) :
	_sources()
{

	subscribeTo( source ) ;	
	
}



EventListener::~EventListener() throw()
{

#if CEYLAN_DEBUG_EVENTS
	if ( ! _sources.empty() )		
		LogPlug::debug( "EventListener destructor: "
			"listener was still registered to " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( _sources.size() ) )
			+ " source(s), unsubscribing it." ) ;
#endif // CEYLAN_DEBUG_EVENTS

	unsubscribeFromAllSources() ;
	
	// _sources not owned.
	
}



void EventListener::subscribeTo( EventSource & source )
{

	for ( list<EventSource *>::const_iterator it = _sources.begin();
		it != _sources.end(); it ++ )
	{
			if  ( (*it) == & source )
				throw EventException( 
					"EventListener::subscribeTo: event source " 
					+ source.toString() + " is already registered." ) ;
		
	}
							
	_sources.push_back( & source ) ;

	source.add( * this ) ;

#if CEYLAN_DEBUG_EVENTS		
	LogPlug::debug( "EventListener subscribed to source '" 
		+ source.toString() + "'." ) ;
#endif // CEYLAN_DEBUG_EVENTS
			
}



void EventListener::unsubscribeFrom( EventSource & source ) 
{

	list<EventSource *>::iterator it ;
	
	for ( it = _sources.begin(); it != _sources.end(); it++ )
	{
	
		if ( (*it) == & source )
		{
			
			// Ensure that the source forgot this listener:
			(*it)->remove( *this ) ;

#if CEYLAN_DEBUG_EVENTS		

			LogPlug::debug( 
				"EventListener unsubscribed from following source: " 
				+ (*it)->toString() ) ;

#endif // CEYLAN_DEBUG_EVENTS

			/*
			 * Removes the source, invalidates the iterator but it will 
			 * not be used any more:
			 *
			 */
			_sources.remove( & source ) ;
				

			// At most one pointer to this source registered!
						
			return ;
			
		}	
						
	}
					
	if ( it == _sources.end() )
		throw EventException( "EventListener::unsubscribeFrom: "
			"listener was not already registered to following event source: " 
			+ source.toString() ) ;
	
}



void EventListener::unsubscribeFromAllSources()
{
		
	for ( list<EventSource *>::iterator it = _sources.begin(); 
		it != _sources.end() ; it++ )
	{
	
		/*
		 * Ensure that this source forgot this listener:
		 *
		 * @note The rule of not deleting elements while iterating is 
		 * respected.
		 *
		 */
		(*it)->remove( * this ) ;
	
#if CEYLAN_DEBUG_EVENTS	

		LogPlug::debug( "EventListener unsubscribed from following source: " 
			+ (*it)->toString() ) ;

#endif // CEYLAN_DEBUG_EVENTS
										
	}
					
	_sources.clear() ;
	
}



void EventListener::forgetSource( EventSource & source ) 
{


#if CEYLAN_DEBUG_EVENTS		
	
	// Check that the source was already known of this listener:
	
	LogPlug::warning( "EventListener had to forgot source " 
		+ source.toString() ) ;
	
	list<EventSource *>::iterator it ;
		
	for ( it = _sources.begin(); it != _sources.end() ; it++ )
	{
	
		if ( (*it) == & source )
			break ;	
						
	}
					
	if ( it == _sources.end() )
		throw EventException( "EventListener::unsubscribeFrom: "
			"listener was not already registered to following event source: " 
			+ source.toString() ) ;

#endif // CEYLAN_DEBUG_EVENTS	

	// Do not use iterator for deleting tasks!	
	_sources.remove( & source ) ;

}



list<EventSource *> EventListener::getSources() const
{

	return _sources ;
	
}



const string EventListener::toString( Ceylan::VerbosityLevels level ) const
{

	if ( _sources.empty() )
		return "Event listener not currently subscribed to any event source" ;
	else
		return "Event listener subscribed currently to " 
			+ Ceylan::toString( 
				static_cast<Ceylan::Uint32>( _sources.size() ) )
			+ " different event source(s)" ; 
}

