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
     */
    class Controller : public CallableEventSource
    {


        public:
			
					
			/**
			 * Constructs a controller not linked to any model.
			 *
			 */
			Controller() throw() ;
						
													
			/**
			 * Constructs a controller that will send events to the specified
			 * model.
			 *
			 */
			explicit Controller( Model & model ) throw() ;
						
									
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
				Ceylan::VerbosityLevels level = Ceylan::high ) const throw() ;
					
					
					
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will 
			 * never be called.
			 *
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Controller( const Controller & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will 
			 * never be called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			Controller & operator = ( const Controller & source ) throw() ;
		
			
					 			
    } ;	

}



#endif // CEYLAN_CONTROLLER_H_
