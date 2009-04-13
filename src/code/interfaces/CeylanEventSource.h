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


#ifndef CEYLAN_EVENT_SOURCE_H_
#define CEYLAN_EVENT_SOURCE_H_


#include "CeylanEvent.h"              // for EventException, Event
#include "CeylanTextDisplayable.h"    // for inheritance


#include <string>
#include <list>


namespace Ceylan
{

	// Events are sent by sources.
	class Event ;

	// Sources send events to listeners.
	class EventListener ;

}


namespace Ceylan
{


    /**
     * Interface which should be implemented for all objects that 
	 * should be able to send events to listeners.
	 *
	 * Event sources are supposed to always be able to answer to any 
	 * listener asking for their latest event. 
	 *
	 * To do so, they may keep their last message, forge a new 
	 * summary event on purpose, etc.
	 *
	 * Any listener can be registered at most one time to an event source :
	 * attempt of multiple registrations result in an exception.
	 *
	 * @note The event source / listener framework is also known as 
	 * the Observer pattern.
	 *
	 * @see http://www.research.ibm.com/designpatterns/example.htm
	 *
     */
    class CEYLAN_DLL EventSource : public TextDisplayable
    {


        public:
					
					
			/// Basic constructor with no listener registered.
			EventSource() throw() ;
				
				
			/**
			 * Constructs an event source with already one listener
			 * registered.
			 *
			 */
			explicit EventSource( EventListener & listener ) throw() ;
				
									
			/// Basic virtual destructor.
			virtual ~EventSource() throw() ;
			
			
			/**
			 * Registers the specified listener to this source, so that
			 * forthcoming events will be sent to it as well.
			 *
			 * @throw EventException if listener is already registered 
			 * to the event source.
			 *
			 */
			virtual void add( EventListener & listener ) 
				throw( EventException ) ;
			
			
			/**
			 * Unregisters the specified listener from this source, no
			 * forthcoming event will be sent to the listener.
			 *
			 * @throw EventException if listener was not already 
			 * registered to the event source.
			 *
			 * @note This method does nothing special with regard to the 
			 * listener instance, it acts only on the source. The reason for
			 * that is that the unsubscriptions should be initiated on the
			 * listener side, not on the source one.
			 *
			 * @see Ceylan::EventListener::unsubscribeFrom
			 
			 */
			virtual void remove( const EventListener & listener ) 
				throw( EventException ) ;


			/**
			 * Unsubscribes all registered listeners from this source,
			 * no forthcoming event will be sent. 
			 *
			 * This source will request each of its listeners to unsubscribe
			 * from it, then the source will forget them in turn.
			 *
		 	 * The links will be removed, but no instance will be deleted 
			 * by this call.
			 *
			 * @see Ceylan::EventListener::unsubscribeFromAllSources
			 *
			 */
			virtual void removeAllListeners() throw() ;
							
				
            /**
             * Returns a user-friendly description of the state of 
			 * this object.
             *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall 
			 * settings.
			 *
			 * @see TextDisplayable
             *
             */
			virtual const std::string toString( 
				Ceylan::VerbosityLevels level = Ceylan::high ) const throw() ;
			
			
					
		
		protected:


			/**
			 * Notifies all currently registered listeners of a new event.
			 *
			 * @note This source takes ownership of this event.
			 *
			 */
			 virtual void notifyAllListeners( const Event & newEvent ) 
			 	throw() ;


			/**
			 * Tells whether specified listener is registered to this 
			 * source.
			 *
			 */
			virtual bool isRegistered( const EventListener & listener ) 
				throw() ;
			
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
			 * The registered event listeners.
			 *
			 */
			std::list<EventListener *> _listeners ;

#pragma warning( pop ) 
									 	


		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, 
			 * implicitly or not.
			 * 
			 */			 
			EventSource( const EventSource & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			EventSource & operator = ( const EventSource & source ) throw() ;
					
						
    } ;	

}



#endif // CEYLAN_EVENT_SOURCE_H_
