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


#ifndef CEYLAN_EVENT_LISTENER_H_
#define CEYLAN_EVENT_LISTENER_H_


#include "CeylanEvent.h"            // for EventException
#include "CeylanTextDisplayable.h"  // for inheritance


#include <string>
#include <list>



namespace Ceylan
{


	// Listeners listen to events.
	class Event ;


	// Listeners are linked to event sources.	
	class EventSource ;



    /**
     * Interface which should be implemented by all objects that are able to
	 * listen to events from at least one source.
	 *
	 * Event listeners are linked to event sources, but do not own them:
	 * sources have independent life cycle, even though a vanishing source
	 * should inform its listener that it is disappearing.
	 *
	 * An event listener may listen to multiple event sources simultaneously.
	 *
	 * @see http://www.research.ibm.com/designpatterns/example.htm
	 *
     */
    class CEYLAN_DLL EventListener : public TextDisplayable
    {


        public:
					
					
					
			/// Basic constructor, not registered to any event source.
			EventListener() ;
			
			
			
			/**
			 * Listener constructor automatically performing a subscription 
			 * to the specified event source.
			 *
			 */
			explicit EventListener( EventSource & source ) ;
			
				
						
			/**
			 * Virtual destructor, performs automatic unsubscribing from all
			 * event sources this listener was registered to.
			 *
			 */
			virtual ~EventListener() throw() ;
			
			
			
			/**
			 * Subscribes this listener to the specified source, so that
			 * forthcoming events will be sent to it as well.
			 *
			 * @note A listener can be subscribed to multiple sources at once.
			 *
			 * @throw EventException if listener is already registered to the
			 * specified event source.
			 *
			 */
			virtual void subscribeTo( EventSource & source ) ;
			
			
			
			/**
			 * Unsubscribes this listener from specified source, no 
			 * forthcoming event will be received from it. 
			 *
			 * This is the usual method to call when one wants that this
			 * listener to not listen any more to the specified source.
			 *
			 * It is in fact implemented by notifying this source 
			 * of the unsubscription of this listener, and by forgetting it.
			 *
			 * @throw EventException if listener was not already registered 
			 * to the event source.
			 *
			 */
			virtual void unsubscribeFrom( EventSource & source ) ;
				
						
			
			/**
			 * Unsubscribes this listener from all registered sources, no
			 * forthcoming event will be received from any.
			 *
			 */
			virtual void unsubscribeFromAllSources() ;



			/**
			 * Forgets the specified source, but does not notify it that the
			 * unsubscription occured on the listener side. 
			 * Therefore, if the source were to send new events, this listener
			 * would be notified, and may not be aware it received an event 
			 * from a source it is not registered to any more.
			 *
			 * @see beNotifiedOf
			 *
			 * This method should mostly be used by event sources being
			 * deallocated, having realized that some listeners were still
			 * subscribed, and wanting nevertheless that the listeners remove
			 * these sources from their list. 
			 * Otherwise, on the source side, should unsubscribeFrom be used
			 * instead, the listeners list would be modified in turn while
			 * iterating on it in the source destructor, which may cause a
			 * crash.
			 *
			 * Alternatively, on these listener deallocations, if nothing was
			 * done in the case of a deallocated source still having at least
			 * one listener left, a call to the remove method of these source
			 * instances would be tempted, and may cause a crash since sources
			 * could be already deallocated.
			 *
			 * @note This method is therefore a parachute which should not be
			 * used by the application. It is used too when the source has to
			 * manage the life cycle of its listeners, and has to deallocate
			 * them.
			 *
			 * This listener can be registered multiple times to this source.
			 *
			 * @throw EventException if listener was not already registered to
			 * the event source, and if debug mode for events is activated.
			 *
			 */
			virtual void forgetSource( EventSource & source ) ;
			
			
			
			/**
			 * Notifies this listener of a new event.
			 *
			 * @note The event is not by default checked to testify that this
			 * listener received an event from a source it is, at the time of
			 * this call, registered to.
			 *
			 * @note This event remains property of the EventSource, which 
			 * will take care of its life cycle.
			 *
			 */
			 virtual void beNotifiedOf( const Event & newEvent ) = 0 ;


				
			/**
			 * Returns a shallow copy of the sources that listener is 
			 * registered to.
			 *
			 * @note The returned list contains the same pointer values as 
			 * the internal one.
			 *
			 */
			std::list<EventSource *> getSources() const ;
			
			 	
				
			/**
			 * Returns a user-friendly description of the state of this object.
			 *
			 * @see TextDisplayable, Displayable
			 * @see Ceylan::VerbosityLevels
			 *
			 */
			virtual const std::string toString( 
				Ceylan::VerbosityLevels level = Ceylan::high ) const ;
					
		
		
		
		protected:
	
/*
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks.
 *
 */
#pragma warning( push )
#pragma warning( disable : 4251 )
		
			/**
			 * The event sources to which this listener is subscribed to.
			 *
			 */
			std::list<EventSource *> _sources ;

#pragma warning( pop ) 



		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will 
			 * never be called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			EventListener( const EventListener & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it 
			 * will never be called.
			 *
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			EventListener & operator = ( const EventListener & source ) ;
				
				 			
    } ;	

}



#endif // CEYLAN_EVENT_LISTENER_H_

