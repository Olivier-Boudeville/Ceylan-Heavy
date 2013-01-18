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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#ifndef CEYLAN_VISITABLE_H_
#define CEYLAN_VISITABLE_H_


#include "CeylanException.h"        // for Ceylan::Exception

#include <string>



namespace Ceylan
{



	/**
	 * Exception to be raised when a visit operation failed.
	 *
	 */
	class CEYLAN_DLL VisitException : public Ceylan::Exception
	{
	
		public:
		
			explicit VisitException( const std::string & reason ) ;
			
			virtual ~VisitException() throw() ;
			
	} ;




	// A Visitable is visited by Visitor instances.
	class Visitor ;



	/**
	 * A Visitable instance is made to be subclassed.
	 * Each child class can be visited by any Visitor.
	 *
	 * @see http://en.wikipedia.org/wiki/Visitor_pattern
	 *
	 */
	class CEYLAN_DLL Visitable
	{
		
		public:



			/// Default empty constructor.
			Visitable() ;



			/// Virtual destructor.
			virtual ~Visitable() throw() ;



			/**
			 * Allows given visitor to visit this object, thanks to a 
			 * callback: 'visitor.visit( *this ) ;'
			 *
			 * @throw VisitException if the visit failed.
			 *
			 * @note This method cannot be implemented here, as the visitor 
			 * must declare its 'visit' method which must accept a specific 
			 * datatype, not a generic one such as Visitable. 
			 * Otherwise, there would be ambiguous calls. 
			 *
			 */
			virtual void accept( Visitor & visitor ) = 0 ;



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
			Visitable( const Visitable & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Visitable & operator = ( const Visitable & source ) ;


	} ;

}



#endif // CEYLAN_VISITABLE_H_

