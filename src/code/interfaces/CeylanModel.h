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
	 */
	class MVCEvent : public Event
	{
	
		public:
		
		
			/// Source should be either a Controller or a Model.
			explicit MVCEvent( EventSource & source ) throw() ;
			
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
			MVCEvent( const MVCEvent & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 *
			 */			 
			MVCEvent & operator = ( const MVCEvent & source ) throw() ;
				
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
	 * account various aspects : not all views are interested in each
	 * and every event.
	 *
	 * @note Models have to implement both the <code>beNotifiedOf</code> method 
	 * (so that they can listen to controller) and the
	 * <code>getSourceEvent</code> virtual method (since they must be
	 * able to be triggered, notably by Views).
	 *
     */
    class Model : public CallableEventSource, public CallerEventListener
    {


        public:
					
					
			/// Basic constructor.
			explicit Model() throw() ;
					
									
			/// Basic virtual destructor.
			virtual ~Model() throw() ;
			
			
			/**
			 * Registers a new view, to which MVC events will be sent.
			 *
			 * @throw EventException if view was already registered.
			 *
			 */
			virtual void addView( View & newView ) throw( EventException ) ;
			
			
			/**
			 * Unregisters specified registered view.
			 *
			 * @throw EventException if view was not already registered.
			 *
			 */
			virtual void removeView( View & view ) throw( EventException ) ;
			
			
			/**
			 * Unregisters all registered views.
			 *
			 * Should not be used under normal circumstances.
			 *
			 * @note This is not how things should be done, since 
			 * registered views will not be informed this model unlisted
			 * them. The process should be done the other way round :
			 * each view should have unsubscribed itself instead from this
			 * model.
		 	 *
			 */
			virtual void removeAllViews() throw() ;
			
			
			
			/**
			 * Subscribes to a new controller, from which MVC events 
			 * will be received.
			 *
			 * @throw EventException if model was already registered.
			 *
			 */
			virtual void subscribeToController( Controller & newController ) 
				throw( EventException ) ;
			
			
			/**
			 * Unsubscribes from specified controller.
			 *
			 * @throw EventException if model was not already registered.
			 *
			 */
			virtual void unsubscribeFromController( Controller & controller ) 
				throw( EventException ) ;
			
			
			/**
			 * Unregisters all registered controllers.
			 *
			 */
			virtual void unsubscribeFromAllControllers() throw() ;
			

				
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
				Ceylan::VerbosityLevels level = Ceylan::high ) 
					const throw() ;
					
					
		
		protected:

			
			/**
			 * Notifies all registered views of a new event.
			 *
			 * @note This event remains property of this model, which 
			 * will take care of its life cycle.
			 *
			 */
			 virtual void notifyAllViews( const MVCEvent & newMVCEvent ) 
			 	throw() ;
		


		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 *
			 */			 
			Model( const Model & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			Model & operator = ( const Model & source ) throw() ;
			
					 			
    } ;	

}



#endif // CEYLAN_MODEL_H_
