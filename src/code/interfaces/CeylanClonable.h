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


#ifndef CEYLAN_CLONABLE_H_
#define CEYLAN_CLONABLE_H_


#include "CeylanException.h"   // for inheritance

#include <string>



namespace Ceylan
{



	/// Exception to be raised whenever cloning fails.
	class CEYLAN_DLL ClonableException: public Ceylan::Exception
	{
	
		public:
		
			explicit ClonableException( const std::string & message ) ;
			
			virtual ~ClonableException() throw() ;
			
	
	} ;
	
	
	
    /**
     * Interface that every object which can be cloned should implement. 
     *
     */
    class CEYLAN_DLL Clonable
    {

        public:
		

			/// Basic constructor.
			Clonable()
			{
			
			}
			
			
			/// Virtual destructor.
			virtual ~Clonable() throw()
			{
			
			}
			
			
            /**
			 * Returns a clone of this object. 
			 *
			 * The ownership of the clone is transferred to the caller, who
			 * therefore shall delete the clone when appropriate.
			 *
			 * @throw ClonableException whenever the cloning fails.
			 *
			 */
            virtual Clonable & clone() const = 0 ;
		
		
			
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Clonable( const Clonable & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Clonable & operator = ( const Clonable & source ) ;
		
			

    } ;

}


#endif // CEYLAN_CLONABLE_H_

