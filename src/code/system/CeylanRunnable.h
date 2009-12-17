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


#ifndef CEYLAN_RUNNABLE_H_
#define CEYLAN_RUNNABLE_H_


#include "CeylanTextDisplayable.h"   // for inheritance
#include "CeylanSystem.h"            // for SystemException

#include <string>



namespace Ceylan 
{


	namespace System
	{
	
	
		/// Exception class for runnable concerns.
		class CEYLAN_DLL RunnableException : public SystemException
		{
		
			public:
			
				explicit RunnableException( const std::string message ) ;
				
				virtual ~RunnableException() throw() ; 			
							
		} ;
		
		
	
		/**
		 * Signature of an classical callback function.
		 *
		 * They can be useful so that functions can change behaviour
		 * by calling  such generic callbacks. 
		 * These caller functions do not have to know their callbacks
		 * a priori.
		 *
		 * The data parameter allows to customize the callback, 
		 * which will have to cast the given argument to the specific
		 * data it is actually looking for.
		 *
		 * Even though no exception specification can be specified,
		 * the callback should not throw any exception whatsoever.
		 *
		 * See also: Ceylan::Functor.
		 *
		 */
		typedef void (*Callback) ( void * data ) /* throw() */ ;
	
	
	
		/** 
 		 * Basic runnable interface.
		 *
		 * @see Thread, Process.
		 *
		 */
		class CEYLAN_DLL Runnable : public Ceylan::TextDisplayable
		{


			public:
			
			
			
				/// Constructs an anonymous Runnable.
				Runnable() ;



				// Constructs a Runnable whose name is the one specified.
				explicit Runnable( const std::string & name ) ;
				
				
				
				// Basic virtual destructor.
				virtual ~Runnable() throw() ;



				/**
				 * Start point method.
				 *
				 * @throw RunnableException if the runnable could not
				 * be run.
				 *
				 */
				virtual void run() = 0 ;
				
				
				
				/// Returns the name string.
				const std::string & getName() const ;
				
				

            	/**
            	 * Returns a user-friendly description of the state of 
				 * this object.
            	 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall 
				 * settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) const = 0 ;

				
				
			private:
			

				/// The name of the Runnable.
				std::string _name ;


				/**
				 * Copy constructor made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				Runnable( const Runnable & source ) ;
			
			
			
				/**
				 * Assignment operator made private to ensure that
				 * it will be never called.
				 *
				 * The compiler should complain whenever this 
				 * undefined operator is called, implicitly or not.
				 *
				 */			 
				Runnable & operator = ( const Runnable & source ) ;
				
				
		} ;
		

	}
	
}



#endif // CEYLAN_RUNNABLE_H_

