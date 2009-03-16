#ifndef CEYLAN_VIEW_H_
#define CEYLAN_VIEW_H_


#include "CeylanCallerEventListener.h"          // for inheritance

#include <string>



namespace Ceylan
{


	// Forward declaration.
	class MVCEvent ;
	
	// Forward declaration.	
	class Model ;


    /**
     * View of the Model-View-Controller (MVC) design pattern.
	 *
	 * The view of an object reflects the state of its model(s).
	 *
	 * Such view is meant to communicate with its model(s) only by the means
	 * of exchanged events.
	 *
	 * @note View is mainly a listener of events from its model, it can 
	 * however listen to other objects as well, helping the view in its task.
	 *
	 * @note The link between a model and its views could take into account
	 * various aspects: not all views are interested in each and every event.
	 *
	 * @note Views might be shared between multiple models.
	 *
	 * @see also the generic alternative MVC framework, for more lightweight and
	 * flexible exchanges: Ceylan::BaseView, Ceylan::BaseModel and
	 * Ceylan::BaseController.
	 *
     */
    class CEYLAN_DLL View : public CallerEventListener
    {


        public:
					
							
			/**
			 * Constructs a view, not linked to any model.
			 *
			 */
			View() throw() ;
				
													
			/**
			 * Constructs a view linked to a model.
			 *
			 * @note model cannot be 'const' since the underlying event 
			 * listener not always deals with 'const' sources.
			 *
			 */
			explicit View( Model & model ) throw() ;
					
									
			/// Basic virtual destructor.
			virtual ~View() throw() ;
			
			
			/**
			 * Requests that view to generate its interpretation of the model
			 * it is linked to.
			 *
			 */
			virtual void renderModel() throw() = 0 ;
			
				
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
				
		
		protected:
		
		
			/**
			 * Returns the model presumably associated to this view.
			 *
			 * @note A view might not be associated with any model (yet) or
			 * it might be associated to more than one model.
			 *
			 * @throw EventException if there is not exactly one model
			 * registered by this view.
			 *
			 */
			virtual Model &	getModel() throw( EventException ) ;		
		
		
			/*
			 * The models (usually just one) corresponding to that view are
			 * stored in the _sources member, a list of EventSource pointers,
			 * inherited from the EventListener mother class.
			 *
			 */


		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 */			 
			View( const View & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			View & operator = ( const View & source ) throw() ;

			
					 			
    } ;	

}



#endif // CEYLAN_VIEW_H_

