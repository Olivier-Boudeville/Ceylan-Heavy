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


#ifndef CEYLAN_CALLER_EVENT_LISTENER_H_
#define CEYLAN_CALLER_EVENT_LISTENER_H_


#include "CeylanEventListener.h"   // for inheritance


#include <string>


namespace Ceylan
{


	// Events are propagated from sources to listeners.
	class Event ;


	// Sources emit events.	
	class EventSource ;



    /**
     * This specialized event listener does not provide any more method than
	 * basic event listeners do. 
	 * Instead, it allows to define a specialized type which can be the only
	 * one used in calls to the CallableEventSource <code>getEventFor</code>
	 * method. This ensures stronger typing.
	 *
	 * @note Sources will check that the caller is an already registered 
	 * caller event listener indeed when asked for events.
	 *
     */
    class CEYLAN_DLL CallerEventListener : public EventListener
    {


        public:
					
					
			/// Basic constructor, not registered to any event source.
			CallerEventListener() ;
			
			
			
			/**
			 * Listener constructor automatically performing subscription 
			 * to the event source.
			 *
			 */
			explicit CallerEventListener( EventSource & source ) ;
			
			
						
			/**
			 * Virtual destructor, performs automatic unsubscribing from all
			 * event sources this listener is subscribed to.
			 *
			 */
			virtual ~CallerEventListener() throw() ;
			
				
					 	
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
			CallerEventListener( const CallerEventListener & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be 
			 * never called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 *
			 */			 
			CallerEventListener & operator = ( 
				const CallerEventListener & source ) ;
				
				 			
    } ;	

}



#endif // CEYLAN_CALLER_EVENT_LISTENER_H_

