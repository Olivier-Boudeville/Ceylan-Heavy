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


#ifndef CEYLAN_VISITOR_H_
#define CEYLAN_VISITOR_H_


#include "CeylanTextDisplayable.h"  // for TextDisplayable
#include "CeylanException.h"        // for Ceylan::Exception

#include <string>


namespace Ceylan
{


	
	// A Visitable is visited by Visitor instances.
	class Visitable ;



	/**
	 * A Visitor is a class designed to be subclassed.
	 * Each child class can visit a structure of Visitable
	 * instances and perform dedicated actions, that depend
	 * on the actual visitor <b>and</b> on the actual visitable
	 * being visited.
	 *
	 * @see http://en.wikipedia.org/wiki/Visitor_pattern
	 *
	 * @note This code is less meant for code reuse than for
	 * didactic purpose, as seldom any generic code can be 
	 * shared for visitors.
	 *
	 */
	class CEYLAN_DLL Visitor : public Ceylan::TextDisplayable
	{
		
		public:


			/// Default empty constructor.
			Visitor() ;


			/**
			 * Visits a concrete Visitable.
			 *
			 * @note Should be declared on the actual visitor (ex: a 
			 * XMLVisitor) as it must specify the actual visitables it can
			 * visit (ex: XML markup, XML text, etc.), and not a more generic
			 * type.
			 *
			 
			virtual void visit( MyFirstConcreteVisitable & concreteVisitable ) ;
	
			 */


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
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 * @note Made to avoid unwanted hidden clone of the Singleton.
			 *
			 */			 
			Visitor( const Visitor & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Visitor & operator = ( const Visitor & source ) ;
			

	} ;

}



#endif // CEYLAN_VISITOR_H_

