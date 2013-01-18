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


#ifndef CEYLAN_CONTROLLER_H_
#define CEYLAN_CONTROLLER_H_


#include "CeylanCallableEventSource.h" // for inheritance

#include <string>



namespace Ceylan
{


	// Controllers send MVC events.
	class MVCEvent ;
	
	
	// Models received MVC events from controllers.	
	class Model ;



    /**
     * Controller of the Model-View-Controller (MVC) design pattern.
	 *
	 * The controller of an object sends events to its model, which in turn
	 * change its state.
	 *
	 * Alternatively, since a Controller is a Callable Event Source, any
	 * listener can call the Controller <code>getSourceEvent</code> virtual
	 * method, so that this listener can drive the controller, and not the
	 * contrary.
	 *
	 * @note Controller is mainly a source of events aimed at a model, it can
	 * however have multiple objects listening to its events.
	 *
	 * Controllers are to receive notifications from one or more input devices.
	 * Controllers for the moment do not have to know their input devices.
	 *
	 * Input devices and controller have separate life cycle, no one is owned
	 * by the other.
	 *
	 * @see InputDevice
	 *
	 * @note Controllers have to implement the <code>getSourceEvent</code>
	 * virtual method, since they must be able to be triggered, notably by
	 * Models.
	 *
	 * @see also the generic alternative MVC framework, for more lightweight and
	 * flexible exchanges: Ceylan::BaseView, Ceylan::BaseModel and
	 * Ceylan::BaseController.
	 *
     */
    class CEYLAN_DLL Controller : public CallableEventSource
    {


        public:
			
					
			/**
			 * Constructs a controller not linked to any model.
			 *
			 */
			Controller() ;
						
					
													
			/**
			 * Constructs a controller that will send events to the specified
			 * model.
			 *
			 */
			explicit Controller( Model & model ) ;
						
							
									
			/// Basic virtual destructor.
			virtual ~Controller() throw() ;
			
			
						
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
			 * Copy constructor made private to ensure that it will 
			 * never be called.
			 *
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Controller( const Controller & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will 
			 * never be called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			Controller & operator = ( const Controller & source ) ;
		
			
					 			
    } ;	

}



#endif // CEYLAN_CONTROLLER_H_

