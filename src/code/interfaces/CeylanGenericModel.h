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


#ifndef CEYLAN_GENERIC_MODEL_H_
#define CEYLAN_GENERIC_MODEL_H_


#include "CeylanGenericMVCDefines.h"        // for GenericMVCException
#include "CeylanTextDisplayable.h"          // for inheritance
#include "CeylanOperators.h"                // for toString
#include "CeylanStringUtils.h"              // for formatStringList


#include <string>
#include <list>



/*
 * See the CeylanGenericMVCDefines.h file for detailed design explanations
 * about this light-weight generic (template-based) MVC framework.
 *
 */


/*
 * Here the base model (BaseModel) and all necessary templated variations of
 * models are defined: 
 *
 *  - a model with no view and no controller: NoViewGenericModel
 *  - a model with a single view and no controller: SingleViewGenericModel<View>
 *  - a model with multiple (any number of) views and no controller:
 * MultipleViewGenericModel<View>
 *  - a model with no view and a single controller:
 * SingleControllerNoViewGenericModel<Controller>
 *
 * 
 */
 
 
/* 
 * Actually there is apparently little need for a model to know the
 * specific class of its view(s), as a model will just manage their life-cycle.
 * Hence view-templated models are not strictly necessary: they just deal with
 * (not necessarily known) BaseView instances.
 *
 * On the contrary, a view has to know the actual model class it corresponds
 * to (i.e. a view has to be model-templated), as the view needs, to perform
 * its rendering, to call class-specific (const) methods from its model(s)
 * (to know its state).
 *
 * Therefore here view-templated models have non-template counterparts, which
 * are easier to integrate in user's code and should be nevertheless suitable
 * for most usages:
 *
 *  - a model with a single view and no controller: SingleViewGenericModel
 *  - a model with multiple (any number of) views and no controller:
 * MultipleViewGenericModel
 *  
 * @note SingleViewModel could not be named SingleViewGenericModel as the
 * compiler would find an ambiguity with the SingleViewGenericModel template.
 * However SingleViewModel and al should not by confused with the
 * Ceylan::MVC::Model class, which belongs to a different (event-based) MVC
 * framework.
 *
 */
  




namespace Ceylan
{

	
	namespace MVC
	{
	
	

		// A model may know its view(s):
		class BaseView ;
	
	
		// A model knows its controller(s):
		class BaseController ;
	




		// BaseModel mother class.
	

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
		 * It is event-based, it uses more resources and is generally 
		 * deemed less flexible than this generic one. Choose the one you
		 * prefer.
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
				 * Pure virtual method so that all model child 
				 * classes have to choose how many views can be registered 
				 * and whether they are owned by the model.
				 *
				 * @note This is nevertheless a 'const' method, as adding 
				 * a view is deemed not to change the state of the model
				 * itself; moreover if it was non-const, then views (which
				 * have const references to models) could not auto-register
				 * themselves to the models).
				 *
				 */
				virtual void addView( const BaseView & view )
					const throw( GenericMVCException ) = 0 ;
	
	
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
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				BaseModel( const BaseModel & source ) throw() ;
	
	
				/**
				 * Assignment operator made private to ensure that it will
				 * never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				BaseModel & operator = ( const BaseModel & source ) throw() ;

	
	
	    } ;






		// NoViewModel class. No template issues here.



		/**
		 * Pure model, not linked to any view.
		 *
		 * @see testCeylanGenericMVC.cc for examples.
		 *
		 */
	    class CEYLAN_DLL NoViewModel : public BaseModel
	    {


	        public:
	
	
				/**
				 * Constructs a model, not linked to any view or controller.
				 *
				 */
				NoViewModel() throw() ;
	
	
				/// Basic virtual destructor.
				virtual ~NoViewModel() throw() ;


				/**
				 * @throw GenericMVCException in all cases, as no view is
				 * supported.
				 *
				 * @note This is nevertheless a 'const' method, as adding 
				 * a view is deemed not to change the state of the model
				 * itself; moreover if it was non-const, then views (which
				 * have const references to models) could not auto-register
				 * themselves to the models).
				 *
				 */
				virtual void addView( const BaseView & view )
					const throw( GenericMVCException ) ;
	
	
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
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				NoViewModel( const NoViewModel & source ) throw() ;
	
	
				/**
				 * Assignment operator made private to ensure that it will
				 * never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				NoViewModel & operator = ( const NoViewModel & source )
					throw() ;

	
	    } ;







		/*
		 * SingleViewGenericModel templated class.
		 *
		 * @note Usually using this view-templated class is overkill, as 
		 * most if not all models do not really need to know the actual class
		 * of the views they are associated with.
		 *
		 */




		/**
		 * Generic model template, for models specifically linked to a given
		 * view, which the model owns, and with no known specific controller.
		 *
		 * This generic model is linked to exactly one view, and knowing it is
		 * only useful to manage its lifecycle.
		 *
		 * Note that using the non-templated version SingleViewModel is enough
		 * in most cases, as a model generally does not need to trigger 
		 * methods on its view, whereas the contrary is true (a view requests
		 * the model for its current state at rendering time).
		 *
		 * @see testCeylanGenericMVC.cc for examples.
		 *
		 */
		template <typename ActualView> 
		class SingleViewGenericModel: public BaseModel
		{
	
			public:
	
	
				/**
				 * Creates a new generic model, which will be linked to
				 * specified view, whose ownership is taken, and to no
				 * controller.
				 *
				 * @note Generally cannot be used because of the chicken-and-egg
				 * problem: on creation the view needs its model, and this model
				 * needs its view, both cannot be satisfied.
				 *
				 */
				explicit SingleViewGenericModel( const ActualView & view ) ;
	
	
				/**
				 * Creates a new generic model, which will be linked afterwards
				 * to a view and to no controller.
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
				 * If a view was already registered, it will be deallocated
				 * first.
				 *
				 * @param view the view, whose ownership is taken.
				 *
				 * @throw GenericMVCException if a view was already registered.
				 *
				 */
				virtual void setView( const ActualView & view )
					 throw( GenericMVCException ) ;
	
	
	
				/**
				 * Sets the unique view this model should have.
				 *
				 * If a view was already registered, it will be deallocated
				 * first.
				 *
				 * @param view the view, whose ownership is taken.
				 *
				 * @throw GenericMVCException if a view was already registered.
				 *
				 * @note This is nevertheless a 'const' method, as adding 
				 * a view is deemed not to change the state of the model
				 * itself; moreover if it was non-const, then views (which
				 * have const references to models) could not auto-register
				 * themselves to the models).
				 *
				 * @note The use of the setView method should be preferred,
				 * as its name is clearer than this inherited one.
				 *
				 */
				virtual void addView( const ActualView & view )
					 const throw( GenericMVCException ) ;
	
	
	
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
	
	
				/**
				 * The associated (unique, if any, and owned) view.
				 *
				 * @note A model should a priori never change its view(s), 
				 * (the views are expected to retrieve information directly
				 * from their model), hence the const reference.
				 *
				 */
				const ActualView * _view ;
	
	
	
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
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				SingleViewGenericModel & operator = (
					const SingleViewGenericModel & source ) throw() ;

	    } ;
	



		// Implementation of the SingleViewGenericModel templated class.
	
	
		template <typename ActualView>
		SingleViewGenericModel<ActualView>::SingleViewGenericModel( 
				const ActualView & view ) :
			_view( & view )
		{

		}
	
	
	
		template <typename ActualView>
		SingleViewGenericModel<ActualView>::SingleViewGenericModel() :
			_view( 0 )
		{

		}
	
	
	
		template <typename ActualView>
		SingleViewGenericModel<ActualView>::~SingleViewGenericModel() throw()
		{
	
			if ( _view != 0 )
			{
			
				// View owned here:
				delete _view ;
				
			}
			
		}
	
	
	
		template <typename ActualView>
		void
		SingleViewGenericModel<ActualView>::setView( const ActualView & view )
			throw( GenericMVCException )
		{
		
			addView( view ) ;
		
		}
		
		
		
		template <typename ActualView>
		void
		SingleViewGenericModel<ActualView>::addView( const ActualView & view )
			const throw( GenericMVCException )
		{
		
			if ( _view != 0 )
				throw GenericMVCException( 
					"SingleViewGenericModel<ActualView>::addView failed: "
					"a view was already registered." ) ;
			
			// Do as if this instance had not to be 'const':
			const_cast< SingleViewGenericModel<ActualView> *>(this)->_view =
				& view ;
		
		}
		
		
		
		template <typename ActualView>
		const std::string
		SingleViewGenericModel<ActualView>::toString(
			Ceylan::VerbosityLevels level ) const throw()
		{
	
			if ( _view != 0 )
			{
			
				/*
				 * Beware to infinite recursion:
				 * (the view may display in turn this model)
				 *
				 */

				if ( level == Ceylan::low )
					return "Single-view controller-less generic model "
						"owning a view" ;
				else
					return "Single-view controller-less generic model "
						"associated to " + _view->toString( level ) ;

			}		
			else
			{
			
				return "Single-view controller-less generic model "
					"not associated to any view" ;
	
			}
			
		}
	
	
	
	

		/*
		 * SingleViewModel non-templated class.
		 *
		 * Simpler than the SingleViewGenericModel view-templated 
		 * counterpart, but deals with BaseView instances instead of an actual
		 * view child class.
		 *
		 * Once the actual relationships between a model and its views
		 * are taken into account, using a non-templated model should not be
		 * a problem in general.
		 *
		 */




		/**
		 * Generic model class, linked to a given generic view (BaseView), 
		 * which the model owns, and with no known specific controller.
		 *
		 * This generic model is linked to exactly one view, and knowing it is
		 * only useful to manage its lifecycle.
		 *
		 * @see testCeylanGenericMVC.cc for examples.
		 *
		 */
		class CEYLAN_DLL SingleViewModel: public BaseModel
		{
	
			public:
	
	
				/**
				 * Creates a new generic model, which will be linked to
				 * specified view, whose ownership is taken, and to no
				 * controller.
				 *
				 * @note Generally cannot be used because of the chicken-and-egg
				 * problem: on creation the view needs its model, and this model
				 * needs its view, both cannot be satisfied.
				 *
				 */
				explicit SingleViewModel( const BaseView & view ) ;
	
	
				/**
				 * Creates a new generic model, which will be linked afterwards
				 * to a view and to no controller.
				 *
				 * @note Generally used because of the chicken-and-egg
				 * problem: on creation the view needs its model, so this view
				 * cannot be specified directly when creating this model.
				 *
				 */
				explicit SingleViewModel() ;
	
	
	
				/**
				 * Virtual destructor.
				 *
				 * @note Any associated view will be removed.
				 *
				 */
				virtual ~SingleViewModel() throw() ;
	
	
	
				/**
				 * Sets the unique view this model should have.
				 *
				 * If a view was already registered, it will be deallocated
				 * first.
				 *
				 * @param view the view, whose ownership is taken.
				 *
				 * @throw GenericMVCException if a view was already registered.
				 *
				 * @note This is nevertheless a 'const' method, as adding 
				 * a view is deemed not to change the state of the model
				 * itself; moreover if it was non-const, then views (which
				 * have const references to models) could not auto-register
				 * themselves to the models).
				 *
				 */
				virtual void setView( const BaseView & view )
					 throw( GenericMVCException ) ;
	
	
	
				/**
				 * Sets the unique view this model should have.
				 *
				 * If a view was already registered, it will be deallocated
				 * first.
				 *
				 * @param view the view, whose ownership is taken.
				 *
				 * @throw GenericMVCException if a view was already registered.
				 *
				 * @note This is nevertheless a 'const' method, as adding 
				 * a view is deemed not to change the state of the model
				 * itself; moreover if it was non-const, then views (which
				 * have const references to models) could not auto-register
				 * themselves to the models).
				 *
				 * @note The use of the setView method should be preferred,
				 * as its name is clearer than this inherited one.
				 *
				 */
				virtual void addView( const BaseView & view )
					const throw( GenericMVCException ) ;
	
	
	
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
	
	
				/**
				 * The associated (unique, if any, and owned) view.
				 *
				 * @note A model should a priori never change its view(s), 
				 * (the views are expected to retrieve information directly
				 * from their model), hence the const reference.
				 *
				 */
				const BaseView * _view ;
	
	
	
			private:
	
	
				/**
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				SingleViewModel( const SingleViewModel & source ) throw() ;
	
	
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				SingleViewModel & operator = (
					const SingleViewModel & source ) throw() ;

	    } ;
	





		/*
		 * MultipleViewGenericModel templated class.
		 *
		 * @note Usually using this view-templated class is overkill, as 
		 * most if not all models do not really need to know the actual class
		 * of the views they are associated with.
		 *
		 */



		/**
		 * Generic model template, for models specifically linked to 
		 * multiple (i.e. any number of) views, which the model owns, and
		 * with no known specific controller.
		 *
		 * @see testCeylanGenericMVC.cc for examples.
		 *
		 */
		template <typename ActualView> 
		class MultipleViewGenericModel: public BaseModel
		{
	
			public:
	
	
				/**
				 * Creates a new generic model, which will be linked to
				 * specified view, whose ownership is taken, and to no
				 * controller.
				 *
				 * @note Generally cannot be used because of the chicken-and-egg
				 * problem: on creation the view needs its model, and this model
				 * needs its view, both cannot be satisfied.
				 *
				 */
				explicit MultipleViewGenericModel( const ActualView & view ) ;
	
	
				/**
				 * Creates a new generic model, which will be linked to
				 * specified views, whose ownership is taken, and to no
				 * controller.
				 *
				 * @note Generally cannot be used because of the chicken-and-egg
				 * problem: on creation the views need their model, and this
				 * model needs its views, both cannot be satisfied.
				 *
				 */
				explicit MultipleViewGenericModel( 
					const std::list<const ActualView *> & views ) ;
	
	
				/**
				 * Creates a new generic model, which will be linked afterwards
				 * to views, and to no controller.
				 *
				 * @note Generally used because of the chicken-and-egg
				 * problem: on creation the views need their model, so 
				 * these views cannot be specified directly when creating 
				 * this model.
				 *
				 */
				explicit MultipleViewGenericModel() ;
	
	
				/**
				 * Virtual destructor.
				 *
				 * @note All associated views will be removed.
				 *
				 */
				virtual ~MultipleViewGenericModel() throw() ;
	
	
	
				/**
				 * Sets the list of views this model should be linked.
				 *
				 * @param views the list of views; the list is copied 
				 * (ownership not taken), but the ownership of views is taken.
				 *
				 * @throw GenericMVCException if at least a view was already
				 * registered.
				 *
				 */
				virtual void setViews( 
						const std::list<const ActualView *> & views )
					 throw( GenericMVCException ) ;
	
	
	
				/**
				 * Adds specified view to the already registered views.
				 *
				 * @param view the view, whose ownership is taken.
				 *
				 * @throw GenericMVCException if the operation failed.
				 *
				 * @note This is nevertheless a 'const' method, as adding 
				 * a view is deemed not to change the state of the model
				 * itself; moreover if it was non-const, then views (which
				 * have const references to models) could not auto-register
				 * themselves to the models).
				 *
				 */
				virtual void addView( const ActualView & view )
					 const throw( GenericMVCException ) ;
	
	
	
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
	
	
				/// Deletes all referenced views.
				virtual void deleteViews() ;
				
	
				/// The associated views.
				std::list<const ActualView *> _views ;
	
	
	
			private:
	
	
				/**
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				MultipleViewGenericModel(
					const MultipleViewGenericModel & source )	throw() ;
	
	
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				MultipleViewGenericModel & operator = (
					const MultipleViewGenericModel & source ) throw() ;


	    } ;
	
	


		// Implementation of the MultipleViewGenericModel templated class.
	
	
		template <typename ActualView>
		MultipleViewGenericModel<ActualView>::MultipleViewGenericModel( 
			const ActualView & view )
		{

			_views.push_back( view ) ;
			
		}
	
	
		template <typename ActualView>
		MultipleViewGenericModel<ActualView>::MultipleViewGenericModel( 
			const std::list<const ActualView *> & views )
		{

			_views = views ;
			
		}
	
	
	
		template <typename ActualView>
		MultipleViewGenericModel<ActualView>::MultipleViewGenericModel()
		{

		}
	
	
	
		template <typename ActualView>
		MultipleViewGenericModel<ActualView>::~MultipleViewGenericModel()
			throw()
		{
	
			deleteViews() ;
						
		}


	
		template <typename ActualView>
		void MultipleViewGenericModel<ActualView>::setViews( 
				const std::list<const ActualView *> & views )
			throw( GenericMVCException )
		{
			
			if ( ! _views.empty() )
				throw GenericMVCException(
					"MultipleViewGenericModel<ActualView>::setViews failed: "
					"there was at least one view registered." ) ;
					
			_views = views ;
		
		}
			
	
	
		template <typename ActualView>
		void
		MultipleViewGenericModel<ActualView>::addView( const ActualView & view )
			const throw( GenericMVCException )
		{

			_views.push_back( & view ) ;
		
		}
		
				
		
		template <typename ActualView>
		const std::string
		MultipleViewGenericModel<ActualView>::toString(
			Ceylan::VerbosityLevels level ) const throw()
		{
	
			if ( _views.empty() )
				return "Multiple-view controller-less generic model "
					"not associated to any view" ;
			
			/*
			 * Beware to infinite recursion:
			 * (the view may display in turn this model)
			 *
			 */

			if ( level == Ceylan::low )
				return "Multiple-view controller-less generic model "
					"owning " + Ceylan::toString( _views.size() ) + " view(s)" ;


			std::list<std::string> res ;
			
			for ( typename std::list<const ActualView *>::const_iterator it =
				_views.begin(); it != _views.end() ; it++ )
			{
			
				res.push_back( (*it)->toString( level ) ) ;
				
			}
						
			return "Multiple-view controller-less generic model "
				"associated to following view(s): "
				+ Ceylan::formatStringList( res ) ;
				
		}
	
	

		template <typename ActualView>
		void MultipleViewGenericModel<ActualView>::deleteViews()
		{

			for ( typename std::list<const ActualView *>::const_iterator it =
				_views.begin(); it != _views.end(); it++ )
			{
			
				delete *it ;
				
			}
			
			_views.clear() ;
		
		}
		




		/*
		 * MultipleViewModel non-templated class.
		 *
		 * Simpler than the MultipleViewGenericModel view-templated 
		 * counterpart, but deals with BaseView instances instead of an actual
		 * view child class.
		 *
		 * Once the actual relationships between a model and its views
		 * are taken into account, using a non-templated model should not be
		 * a problem in general.
		 *
		 */



		/**
		 * Generic model class, for models specifically linked to 
		 * multiple (i.e. any number of) views, which the model owns, and
		 * with no known specific controller.
		 *
		 * @see testCeylanGenericMVC.cc for examples.
		 *
		 */
		class CEYLAN_DLL MultipleViewModel: public BaseModel
		{
	
			public:
	
	
				/**
				 * Creates a new generic model, which will be linked to
				 * specified view, whose ownership is taken, and to no
				 * controller.
				 *
				 * @note Generally cannot be used because of the chicken-and-egg
				 * problem: on creation the view needs its model, and this model
				 * needs its view, both cannot be satisfied.
				 *
				 */
				explicit MultipleViewModel( const BaseView & view ) ;
	
	
	
				/**
				 * Creates a new generic model, which will be linked to
				 * specified views, whose ownership is taken, and to no
				 * controller.
				 *
				 * @note Generally cannot be used because of the chicken-and-egg
				 * problem: on creation the views need their model, and this
				 * model needs its views, both cannot be satisfied.
				 *
				 */
				explicit MultipleViewModel( 
					const std::list<const BaseView *> & views ) ;
	
	
	
				/**
				 * Creates a new generic model, which will be linked afterwards
				 * to views, and to no controller.
				 *
				 * @note Generally used because of the chicken-and-egg
				 * problem: on creation the views need their model, so 
				 * these views cannot be specified directly when creating 
				 * this model.
				 *
				 */
				explicit MultipleViewModel() ;
	
	
	
				/**
				 * Virtual destructor.
				 *
				 * @note All associated views will be removed.
				 *
				 */
				virtual ~MultipleViewModel() throw() ;
	
	
	
				/**
				 * Sets the list of views this model should be linked.
				 *
				 * @param views the list of views; the list is copied 
				 * (ownership not taken), but the ownership of views is taken.
				 *
				 * @throw GenericMVCException if at least a view was already
				 * registered.
				 *
				 */
				virtual void setViews( 
						const std::list<const BaseView *> & views )
					 throw( GenericMVCException ) ;
	
	
	
				/**
				 * Adds specified view to the already registered views.
				 *
				 * @param view the view, whose ownership is taken.
				 *
				 * @throw GenericMVCException if the operation failed.
				 *
				 * @note This is nevertheless a 'const' method, as adding 
				 * a view is deemed not to change the state of the model
				 * itself; moreover if it was non-const, then views (which
				 * have const references to models) could not auto-register
				 * themselves to the models).
				 *
				 */
				virtual void addView( const BaseView & view )
					 const throw( GenericMVCException ) ;
	
	
	
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
	
	
				/// Deletes all referenced views.
				virtual void deleteViews() ;
				
	
				/// The associated views.
				std::list<const BaseView *> _views ;
	
	
	
			private:
	
	
				/**
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				MultipleViewModel(
					const MultipleViewModel & source )	throw() ;
	
	
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				MultipleViewModel & operator = (
					const MultipleViewModel & source ) throw() ;


	    } ;
	
	





		// SingleControllerNoViewGenericModel templated class.



		/**
		 * Generic model template, for models specifically linked to a given
		 * controller (whose class is the typename) and with no known specific
		 * view (therefore unable to manage their lifecycle).
		 *
		 * This generic model is linked to exactly one controller.
		 *
		 * @note No CEYLAN_DLL declaration for templates.
		 *
		 * @see testCeylanGenericMVC.cc for examples.
		 *
		 */
		template <typename ActualController>
		class SingleControllerNoViewGenericModel: public BaseModel
		{
	
			public:
	
	
				/**
				 * Creates a new generic model, which will be linked 
				 * to specified controller, whose ownership is taken, and
				 * to no view.
				 *
				 * @param controller the controller this model will reference.
				 *
				 * @note Generally cannot be used because of the chicken-and-egg
				 * problem: on creation the controller needs its model, 
				 * and this model needs its controller, both cannot be
				 * satisfied.
				 *
				 */
				explicit SingleControllerNoViewGenericModel(
					const ActualController & controller ) ;
	
	
				/**
				 * Creates a new generic model, which will be linked afterwards
				 * to a controller and to no view.
				 *
				 * @note Generally used because of the chicken-and-egg
				 * problem: on creation the controller needs its model, so 
				 * this controller cannot be specified directly when creating 
				 * this model.
				 *
				 */
				explicit SingleControllerNoViewGenericModel() ;
		
		
				/**
				 * Virtual destructor.
				 *
				 */
				virtual ~SingleControllerNoViewGenericModel() throw() ;
	
	
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
	
		
				/**
				 * The associated (unique, if any, and owned) controller.
				 *
				 * @note A model should a priori never change its controller(s),
				 * (the model is expected to retrieve information directly
				 * from their model), hence the const reference.
				 *
				 */
				const ActualController * _controller ;
	
	
	
			private:
	
	
				/**
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				SingleControllerNoViewGenericModel(
						const SingleControllerNoViewGenericModel & source )	
					throw() ;
	
	
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				SingleControllerNoViewGenericModel & operator = (
						const SingleControllerNoViewGenericModel & source ) 
					throw() ;

	    } ;
	
	
	
		/* 
		 * Implementation of the SingleControllerNoViewGenericModel templated
		 * class.
		 *
		 */
	
	
		template <typename ActualController>
		SingleControllerNoViewGenericModel<ActualController>::SingleControllerNoViewGenericModel( const ActualController & controller ) :
			_controller( & controller )
		{

		}
	
	
	
		template <typename ActualController>
		SingleControllerNoViewGenericModel<ActualController>::SingleControllerNoViewGenericModel() :
			_controller( 0 )
		{

		}
	
	
	
		template <typename ActualController>
		SingleControllerNoViewGenericModel<ActualController>::~SingleControllerNoViewGenericModel() throw()
		{
	
			if ( _controller != 0 )
			{
			
				// Controller owned here:
				delete _controller ;
				
			}
			
		}
	
	
	
		template <typename ActualController>
		const std::string
		SingleControllerNoViewGenericModel<ActualController>::toString(
			Ceylan::VerbosityLevels level ) const throw()
		{
	
			if ( _controller != 0 )
			{
			
				/*
				 * Beware to infinite recursion:
				 * (the controller may display in turn this model)
				 *
				 */

				if ( level == Ceylan::low )
					return "Single-controller view-less generic model "
						"owning a view" ;
				else
					return "Single-controller view-less generic model "
						"associated to " + _controller->toString( level ) ;

			}		
			else
				return "Single-controller view-less generic model "
					"not associated to any controller" ;
	
		}
	
	

	} // MVC namespace


} // Ceylan namespace



#endif // CEYLAN_GENERIC_MODEL_H_

