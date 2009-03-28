#ifndef CEYLAN_GENERIC_MODEL_H_
#define CEYLAN_GENERIC_MODEL_H_

#include "CeylanGenericDefines.h"           // for GenericMVCException
#include "CeylanTextDisplayable.h"          // for inheritance

#include <string>


/*
 * Here the base model and some generic ones are defined: 
 *  - a model with one view and no controller: SingleViewGenericModel
 *  - a model with no view and a templated controller:
 * SingleControllerNoViewGenericModel<Controller>
 *
 */
 
 

namespace Ceylan
{


	// A model may know its view(s):
	class BaseView ;
	
	
	// A model knows its controller(s):
	class BaseController ;
	
	

    /**
     * Interface class for all models of the lightweight generic
	 * Model-View-Controller (MVC) design pattern.
	 *
	 * The model manages the state of an object. It might be affected by
	 * controllers and rendered by views.
	 *
	 * A model needs to know its controller(s), as it will have to obtain
	 * directly informations from them, rather than relying on controllers
	 * to send regular (potentially useless) updates: we are in a context 
	 * where models, views and controllers might be scheduled independently,
	 * therefore the most efficient scheme is to have each scheduled 
	 * component requesting synchronously fresh informations from the 
	 * components it depends on whenever (if and when) they are needed.
	 *
	 * @note The classical MVC framework (with Ceylan::Model, Ceylan::View
	 * and Ceylan::Controller) is an entirely separated framework.
	 * It is event-based, it uses more resources and is generally deemed less
	 * flexible than this generic one. Choose the one you prefer.
	 *
	 * @see testCeylanGenericMVC.cc for examples.
	 *
     */
    class CEYLAN_DLL BaseModel : public TextDisplayable
    {


        public:
					
							
			/**
			 * Constructs a model, not linked to any view or controller.
			 *
			 */
			BaseModel() throw() ;
				
									
			/// Basic virtual destructor.
			virtual ~BaseModel() throw() ;


			/**
			 * Adds specified view to that model.
			 *
			 * @throw GenericMVCException if the operation failed.
			 *
			 * @note Pure virtual method so that all model child classes have
			 * to choose how many views can be registered and whether they 
			 * are owned by the model.
			 *
			 */			
			virtual void addView( const BaseView & view ) 
				throw( GenericMVCException ) = 0 ;
				
				
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
			 * Copy constructor made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 */			 
			BaseModel( const BaseModel & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			BaseModel & operator = ( const BaseModel & source ) throw() ;

			
					 			
    } ;	



	/*
	 * All necessary templated variations of models should be defined below:
	 *   - SingleViewGenericModel: a model with a single view
	 *   - SingleControllerNoViewGenericModel: a model with a single controller
	 * and no view
	 *
	 */


	// Implementation of the SingleViewGenericModel template.



	/**
	 * Generic model template, for models specifically linked to a given
	 * view, which the model owns, and with no known specific controller.
	 *
	 * This generic model is linked to exactly one view, and knowing it is
	 * only useful to manage its lifecycle.
	 *
	 * @note Not a template, as no specific operation besides deletion is 
	 * to be performed on the owned view.
	 *
	 * @see testCeylanGenericMVC.cc for an example of such implementation.
	 *
	 */
	class SingleViewGenericModel: public BaseModel
	{
	
		public:
		
		
			/**
			 * Creates a new generic model, which will be linked to specified
			 * view, whose ownership is taken, and to no controller.
			 *
			 * @note Generally cannot be used because of the chicken-and-egg
			 * problem: on creation the view needs its model, and this model
			 * needs its view, both cannot be satisfied.
			 *
			 */
			explicit SingleViewGenericModel( const BaseView & view ) ;
	
	
			/**
			 * Creates a new generic model, which will be linked afterwards 
			 * to specified view, whose ownership will be taken, and to no
			 * controller.
			 *
			 * @note Generally used because of the chicken-and-egg
			 * problem: on creation the view needs its model, so this view
			 * cannot be specified directly when creating this model.
			 *
			 */
			explicit SingleViewGenericModel() ;
	
			
			/**
			 * Virtual destructor.
			 *
			 * @note Any associated view will be removed.
			 *
			 */
			virtual ~SingleViewGenericModel() throw() ;
			
			
			
			/**
			 * Sets the unique view this model should have.
			 *
			 * @param view the view, whose ownership is taken.
			 *
			 * @throw GenericMVCException if a view was already registered.
			 *
			 * @note Can be understood as 'setView'.
			 *
			 */
			virtual void addView( const BaseView & view )
				 throw( GenericMVCException ) ;
			
			
				
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
		
		
			/// The associated (unique and owned) view.
			const BaseView * _view ;
						
			
				
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			SingleViewGenericModel( 
				const SingleViewGenericModel & source )	throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			SingleViewGenericModel & operator = ( 
				const SingleViewGenericModel & source ) throw() ;

    } ;
	




	/**
	 * Generic model template, for models specifically linked to a given
	 * controller (whose class is the typename) and with no known specific 
	 * view (therefore unable to manage their lifecycle).
	 *
	 * This generic model is linked to exactly one controller.
	 *
	 * @note No CEYLAN_DLL declaration for templates.
	 *
	 * @see testCeylanGenericMVC.cc for an example of such implementation.
	 *
	 */
	template <typename BaseController>
	class SingleControllerNoViewGenericModel: public BaseModel
	{
	
		public:
		
		
			/**
			 * Creates a new generic model, which will be linked to specified
			 * controller, and to no view.
			 *
			 */
			explicit SingleControllerNoViewGenericModel( 
				BaseController & controller ) ;
	
			
			/**
			 * Virtual destructor.
			 *
			 */
			virtual ~SingleControllerNoViewGenericModel() throw() ;
				
				
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
		
		
			/// The associated (unique) controller.
			BaseController * _controller ;
						
			
				
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			SingleControllerNoViewGenericModel( 
				const SingleControllerNoViewGenericModel & source )	throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			SingleControllerNoViewGenericModel & operator = ( 
				const SingleControllerNoViewGenericModel & source ) throw() ;

    } ;
	
	
	
	// Implementation of the SingleControllerNoViewGenericModel template.
	
	
	template <typename BaseController>
	SingleControllerNoViewGenericModel<BaseController>::SingleControllerNoViewGenericModel(	BaseController & controller ) :
		_controller( & controller )		 				
	{

	}
	
	
	template <typename BaseController>
	SingleControllerNoViewGenericModel<BaseController>::~SingleControllerNoViewGenericModel() throw()
	{
	
		// Nothing to do, controller not owned.
							
	}
	
	
	template <typename BaseController>
	const std::string SingleControllerNoViewGenericModel<BaseController>::toString( 
		Ceylan::VerbosityLevels level ) const throw()
	{
	
		return "Single-controller generic model associated to: " 
			+ _controller->toString() ;
	
	}	
	
}




#endif // CEYLAN_GENERIC_MODEL_H_

