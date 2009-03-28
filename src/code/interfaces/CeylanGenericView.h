#ifndef CEYLAN_GENERIC_VIEW_H_
#define CEYLAN_GENERIC_VIEW_H_


#include "CeylanTextDisplayable.h"       // for inheritance
#include "CeylanGenericModel.h"          // for inheritance

#include <string>



/*
 * See the CeylanGenericMVCDefines.h file for detailed design explanations
 * about this light-weight generic (template-based) MVC framework.
 *
 */



/*
 * Here the base view (BaseView) and all necessary templated variations of
 * views are defined: 
 *  - a view with one templated model: SingleModelGenericView<Model>
 *
 * On the contrary of the model counterpart file, no non-templated class is
 * implemented, as a view needs to know the actual model class it is linked
 * with (whereas the opposite is not true).
 *
 */
 
 
 
namespace Ceylan
{


	namespace MVC
	{
    
	
	
		/**
    	 * Interface class for all views of the lightweight generic
		 * Model-View-Controller (MVC) design pattern.
		 *
		 * The view of an object reflects the state of its model(s).
		 *
		 * A view needs to know its model(s), as it will have to obtain directly
		 * informations from them, rather than relying on models to send regular
		 * (potentially useless) updates: we are in a context where models,
		 * views and controllers might be scheduled independently, therefore 
		 * the most efficient scheme is to have each scheduled component
		 * requesting synchronously fresh informations from the components it
		 * depends on whenever (if and when) they are needed.
		 *
		 * @note The classical MVC framework (with Ceylan::Model, Ceylan::View
		 * and Ceylan::Controller) is an entirely separated framework.
		 * It is event-based, it uses more resources and is generally deemed
		 * less flexible than this generic one. Choose the one you prefer.
		 *
		 * @see testCeylanGenericMVC.cc for examples.
		 *
	     */
	    class CEYLAN_DLL BaseView : public TextDisplayable
	    {


	        public:
	
	
				/**
				 * Constructs a view, not linked to any model.
				 *
				 */
				BaseView() throw() ;
	
	
				/// Basic virtual destructor.
				virtual ~BaseView() throw() ;
	
	
				/**
				 * Requests that view to generate its interpretation 
				 * of the model(s) it is linked to.
				 *
				 * @note There is intentionally no throw specification.
				 *
				 */
				virtual void render() = 0 ;
	
	
	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
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
	
	


			private:
	
	
				/**
				 * Copy constructor made private to ensure that it will
				 *  never be called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				BaseView( const BaseView & source ) throw() ;
	
	
				/**
				 * Assignment operator made private to ensure that it
				 * will never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				BaseView & operator = ( const BaseView & source ) throw() ;

	
	
	    } ;





		/*
		 * SingleModelGenericView templated class.
		 *
		 */



		/**
		 * Generic view template, for views specifically linked to a given
		 * model, whose class is the typename.
		 *
		 * This generic view is linked to exactly one model and is owned by that
		 * model.
		 *
		 * @note No CEYLAN_DLL declaration for templates.
		 *
		 * @see testCeylanGenericMVC.cc for an example of such implementation.
		 *
		 */
		template <typename ActualModel>
		class SingleModelGenericView: public BaseView
		{
	
			public:
	
	
				/**
				 * Creates a new generic view, which will be linked to specified
				 * model.
				 *
				 * @param model the model that view will render.
				 *
				 * @note This view will automatically registers to the model,
				 * which will take ownership of that view.
				 *
				 */
				explicit SingleModelGenericView( const ActualModel & model ) ;
	
	
	
				/**
				 * Virtual destructor.
				 *
				 */
				virtual ~SingleModelGenericView() throw() ;
	
	
	
	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
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
	
	
				/// The associated (unique) model.
				const ActualModel * _model ;
	
	
	
			private:
	
	
				/**
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				SingleModelGenericView( const SingleModelGenericView & source )
					throw() ;
	
	
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				SingleModelGenericView & operator = (
					const SingleModelGenericView & source ) throw() ;

	    } ;
	
	
	
		// Implementation of the SingleModelGenericView templated class.
	
	
		template <typename ActualModel>
		SingleModelGenericView<ActualModel>::SingleModelGenericView(
				const ActualModel & model ) :
			_model( & model )
		{

			_model->addView( *this ) ;
	
		}
	
	
		template <typename ActualModel>
		SingleModelGenericView<ActualModel>::~SingleModelGenericView() throw()
		{
	
			// Nothing to do, model not owned.
	
		}
	
	
		template <typename ActualModel>
		const std::string SingleModelGenericView<ActualModel>::toString(
			Ceylan::VerbosityLevels level ) const throw()
		{
	
			return "Single-model generic view associated to "
				+ this->_model->ActualModel::toString( Ceylan::low ) ;
	
		}


	}	
	
	
}



#endif // CEYLAN_GENERIC_VIEW_H_

