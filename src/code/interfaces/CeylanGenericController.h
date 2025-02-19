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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef CEYLAN_GENERIC_CONTROLLER_H_
#define CEYLAN_GENERIC_CONTROLLER_H_


#include "CeylanTextDisplayable.h"          // for inheritance

#include <string>



/*
 * See the CeylanGenericMVCDefines.h file for detailed design explanations
 * about this light-weight generic (template-based) MVC framework.
 *
 */



/*
 * Here the base controller (BaseController) is defined.
 *
 * A controller will be asked for information by a model, thus a model has
 * to know the actual class of its controller(s), but a controller does not
 * have to know specifically which BaseModel child class it is interacting
 * will.
 *
 * Therefore no templated versions of controllers are needed here.
 *
 */
 
 

namespace Ceylan
{

	
	namespace MVC
	{



	    /**
    	 * Interface class for all controllers of the lightweight generic
		 * Model-Controller-Controller (MVC) design pattern.
		 *
		 * The controller of an object may modify the state of the model(s)
		 * it is linked with.
		 *
		 * @see testCeylanGenericMVC.cc for examples.
		 *
	     */
	    class CEYLAN_DLL BaseController : public TextDisplayable
	    {


	        public:
	
	
	
				/**
				 * Constructs a controller, not linked to any model.
				 *
				 */
				BaseController() ;
	
	
	
				/// Basic virtual destructor.
				virtual ~BaseController() throw() ;
	
	
	
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
						Ceylan::VerbosityLevels level = Ceylan::high ) const ;
	
	


			private:
	
	
				/**
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				BaseController( const BaseController & source ) ;
	
	
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				BaseController & operator = ( const BaseController & source ) ;

	
	
	    } ;


	}


}


#endif // CEYLAN_GENERIC_CONTROLLER_H_

