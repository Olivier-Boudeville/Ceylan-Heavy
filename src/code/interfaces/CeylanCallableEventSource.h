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


#ifndef CEYLAN_CALLABLE_EVENT_SOURCE_H_
#define CEYLAN_CALLABLE_EVENT_SOURCE_H_


#include "CeylanEventSource.h"      // for inheritance


#include <string>
#include <list>


namespace Ceylan
{



	// Event sources send events.
	class Event ;


	// Sources are talking to Listeners.	
	class EventListener ;


	// Special Listeners can call sources to retrieve events.	
	class CallerEventListener ;
	
	

    /**
     * Specialized event source which can be interrogated notably by 
	 * specialized listeners, CallerEventListener, so that an event is
	 * propagated according to the pace chosen by the listener rather than 
	 * by the one of the source. 
	 *
	 * The returned event can moreover be adapted by the source to the
	 * particular caller, if needed. 
	 * This may prove useful for example with the MVC framework, where video
	 * Views may be interested in different kinds of informations than audio
	 * Views.
	 *
	 * At each call, one and only one event is propagated this way. 
	 * Therefore this event is meant to summarize all past events until the
	 * moment the source is called.
	 *
	 * For example, the CallableEventSource may return an event which 
	 * describes its state, i.e. the  result of its initial state changed
	 * according to all the events this source sent, so that the listener is
	 * given accurate, specific but full informations about the source.
	 *
	 * @see getEventFor
	 *
     */
    class CEYLAN_DLL CallableEventSource : public EventSource
    {


        public:
				
					
					
			/// Basic constructor with no listener registered.
			CallableEventSource() ;
				
				
				
			/**
			 * Constructs a callable event source with already one listener
			 * registered.
			 *
			 * @param listener the first and only listener registered for the
			 * moment.
			 *
			 */
			explicit CallableEventSource( EventListener & listener ) ;
			
				
									
			/// Basic virtual destructor.
			virtual ~CallableEventSource() throw() ;
			
			
			
			/**
			 * Returns the event this source has to propagate to the specific
			 * listener caller, in order to summarize all past events to this
			 * specific listener. 
			 *
			 * @param listener the caller event listener for which the event 
			 * is forged.
			 *
			 * This method is mostly to be used by the caller listener itself
			 * on situations where the communication needs to be
			 * listener-driven. 
			 *
			 * More precisely, with basic event sources and listeners, the
			 * source triggers the notifications, and the listener has to 
			 * adapt itself, by storing messages for later use or by acting
			 * immediately, accordingly. It is therefore source-driven.
			 *
			 * On cases where the listener activation is not to be ruled by
			 * sources (ex: if listeners are specifically scheduled already,
			 * or if sources, from a listener point of view, would send
			 * unnecessary notifications as long as listener is not ready),
			 * caller listeners can be used.
			 *
			 * They are in charge of driving the communication process: 
			 * whenever they deem it useful, they ask their sources to return
			 * an event describing their current state.
			 *
			 * This allows to propagate only fresh and required events for
			 * listeners, which is the appropriate way of handling events 
			 * in all the cases where listeners drive the pace of the pair.
			 *
			 * @return an event that can be the latest sent event (if any), 
			 * or an event especially created for this specific call of this
			 * specific listener. 
			 * The purpose of this event is to make so that the listener is
			 * given the information it would have received if all the 
			 * previous events had been sent to it. 
			 * This returned event is 'const' so that the same event can be
			 * instanciated once and shown multiple times to the same ad/or
			 * to different listeners, if needed.
			 *
			 * @note This listener-driven event propagation is especially 
			 * useful for memory-less sources such as most controllers,
			 * according to the MVC framework: for models, the only 
			 * interesting information is their current state, thus the
			 * propagated source event could be used to summarize to the model
			 * what is the current controller state, without having to take
			 * into account any past information.
			 *
			 * @throw EventException if the caller event listener is not 
			 * already registered to this source.
			 *
			 * @see isRegistered the helper method to check whether specified
			 * listener is registered.
			 *
			 */
			virtual const Event & getEventFor( 
				const CallerEventListener & listener ) = 0 ;
				
				
				
            /**
             * Returns a user-friendly description of the state of this object.
             *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see TextDisplayable
             *
             */
			virtual const std::string toString( 
				Ceylan::VerbosityLevels level = Ceylan::high ) const ;
			


				 
		private:
		
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 *
			 */			 
			CallableEventSource( const CallableEventSource & source ) ;
			
			
			
			/**
			 * Assignment operator made private to ensure that it will never
			 * be called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			CallableEventSource & operator = ( 
				const CallableEventSource & source ) ;
				
						
    } ;	

}



#endif // CEYLAN_CALLABLE_EVENT_SOURCE_H_

