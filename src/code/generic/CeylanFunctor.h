/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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


#ifndef CEYLAN_FUNCTOR_H_
#define CEYLAN_FUNCTOR_H_


#include "CeylanTextDisplayable.h"  // for TextDisplayable


#include <string>



namespace Ceylan
{		



		/**
		 * A functor is an object that behaves as a function, thanks 
		 * to the overloading of operator (). 
		 *
		 * This allows the encapsulated function to have a state, as opposed
		 * to mere C function pointers: this state allows to first setup
		 * and parametrize the functor (thanks to its constructor), then to
		 * use it as "a lambda function".
		 *
		 * @note It is especially useful when one has to specify a 
		 * function A as a parameter for another function B. If B is set
		 * so that its argument shall be a functor, then it can take
		 * indifferently a simple function or a functor as parameter. The
		 * reciprocal would not be true.
		 *
		 * @note Functors are great when one wants to make use of 
		 * parametrized functions: thanks to the functor's constructor,
		 * one can generate numerous different functions at runtime.
		 *
		 * @example addFunctor( int myAdd ) could be constructed as:
		 * <code>Functor * myAdd = new addFunctor( 5 )</code>: myAdd 
		 * is a dynamically created function.
		 * 
		 * @see Ceylan::Maths::IntToIntFunctor for a complete example.
		 *
		 * @note Each child class should define following operator:
		 * virtual 'returned type' operator() ('parameter') throw()
		 *
		 */
		class CEYLAN_DLL Functor : public Ceylan::TextDisplayable
		{
		
		
			public:
			
			
				/// Do-nothing constructor.
				Functor() ;
				
				
				/// Virtual destructor.
				virtual ~Functor() throw() ;
				

				/*
				 * The operator a functor is defined for:

				/// The callable method.
				virtual X operator() ( Y y ) = 0 ;

				 *
				 */


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
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;
 
 
 
			private:
			
			
				/**
				 * Copy constructor cannot be private since would 
				 * prevent calls such as:
				 * 'CreateFrom( Linear::Rotation2DFunctor( angle )'.
				 *
				 *	Functor( const Functor & source ) ;
				 *
				 */
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				Functor & operator = ( const Functor & source ) ;
			
							
		} ;
		

}


#endif // CEYLAN_FUNCTOR_H_		

