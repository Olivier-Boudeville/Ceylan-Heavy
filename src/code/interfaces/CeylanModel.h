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


#ifndef CEYLAN_MODEL_H_
#define CEYLAN_MODEL_H_


#include "CeylanCallableEventSource.h"    // for inheritance
#include "CeylanCallerEventListener.h"    // for inheritance

#include <string>



namespace Ceylan
{



	/**
	 * Specialized event exchanged through Model-View-Controller (MVC)
	 * framework.
	 *
	 * Note that the communication between the MVC components could also
	 * be performed thanks to direct method call, if no distributed operation
	 * is targeted.
	 *
	 * @see also the generic alternative MVC framework, for more lightweight and
	 * flexible exchanges: Ceylan::BaseView, Ceylan::BaseModel and
	 * Ceylan::BaseController.
	 *
	 */
	class CEYLAN_DLL MVCEvent : public Event
	{
	
	
		public:
		
		
			/// Source should be either a Controller or a Model.
			explicit MVCEvent( EventSource & source ) ;
			
			
			/// Virtual destructor.
			virtual ~MVCEvent() throw() ;
	
	
			
		private:
			
			
			/**
			 * Copy constructor made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			MVCEvent( const MVCEvent & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 *
			 */			 
			MVCEvent & operator = ( const MVCEvent & source ) ;
				
	} ;
	
	


	// Models may have views.
	class View ;


	// Models may listen to controllers.	
	class Controller ;



    /**
     * Model of the Model-View-Controller (MVC) design pattern.
	 *
	 * The model of an object describes its internal state, and is able 
	 * to make it behave according to its logic.
	 *
	 * The controller(s) this model may be listening to will send events
	 * thanks to this model's inherited <b>beNotifiedOf</b> method, or
	 * will be requested to do so when their model will call their 
	 * <b>getEventFor</b> method.
	 *
	 * @note Model is a (callable) source of events for its views, and 
	 * a (caller) event listener for its controllers.
	 *
	 * @note The link between a model and its views could take into 
	 * account various aspects, themes: not all views are interested in each
	 * and every event.
	 *
	 * @note Models have to implement both the <code>beNotifiedOf</code> method 
	 * (so that they can listen to controller) and the
	 * <code>getSourceEvent</code> virtual method (since they must be
	 * able to be triggered, notably by Views).
	 *
     */
    class CEYLAN_DLL Model : public CallableEventSource, 
		public CallerEventListener
    {


        public:
					
					
			/// Basic constructor.
			explicit Model() ;
					
									
			/// Basic virtual destructor.
			virtual ~Model() throw() ;
			
			
			
			/**
			 * Registers a new view, to which MVC events will be sent.
			 *
			 * @throw EventException if view was already registered.
			 *
			 */
			virtual void addView( View & newView ) ;
			
			
			
			/**
			 * Unregisters specified registered view.
			 *
			 * @throw EventException if view was not already registered.
			 *
			 */
			virtual void removeView( View & view ) ;
			
			
			
			/**
			 * Unregisters all registered views.
			 *
			 * Each of its views will be requested to unsubscribe from this 
			 * model.
			 *
		 	 * The links will be removed, but no instance will be deleted 
			 * by this call.
			 *
			 */
			virtual void removeAllViews() ;
			
			
			
			/**
			 * Subscribes to a new controller, from which MVC events 
			 * will be received.
			 *
			 * @throw EventException if model was already registered.
			 *
			 */
			virtual void subscribeToController( Controller & newController ) ;
			
			
			
			/**
			 * Unsubscribes from specified controller.
			 *
			 * @throw EventException if model was not already registered.
			 *
			 */
			virtual void unsubscribeFromController( Controller & controller ) ;
			
			
			
			/**
			 * Unregisters all registered controllers.
			 *
			 */
			virtual void unsubscribeFromAllControllers() ;
			

				
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
					
					
		
		protected:

			
			/**
			 * Notifies all registered views of a new event.
			 *
			 * @note This event remains property of this model, which 
			 * will take care of its life cycle.
			 *
			 */
			 virtual void notifyAllViews( const MVCEvent & newMVCEvent ) ;
		



		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 *
			 */			 
			Model( const Model & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			Model & operator = ( const Model & source ) ;
			
					 			
    } ;	

}



#endif // CEYLAN_MODEL_H_

