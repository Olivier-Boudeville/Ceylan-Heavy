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


#ifndef CEYLAN_MEASURABLE_H_
#define CEYLAN_MEASURABLE_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanSystem.h"           // for Size

#include <string>



namespace Ceylan
{


    /**
     * Interface which should be implemented for all objects whose size 
	 * can be dynamically evaluated (at runtime).
	 *
	 * Measurable objects are notably able to evaluate their memory footprint.
	 *
     */
    class CEYLAN_DLL Measurable : public TextDisplayable
    {


        public:

		
			/// Creates a new Measurable resource.
			Measurable() ;
			
			
			/// Virtual destructor.
			virtual ~Measurable() throw() ;
			
			
			/**
			 * Returns the approximate size in memory, in bytes, currently
			 * taken by this object.
			 *
			 * The general method for implementing this interface is to 
			 * evaluate the static size of the object (thanks to 'sizeof()') 
			 * and to evaluate recursively all owned dynamically allocated
			 * members. The overall sum is the size to return.
			 * 
			 */
			virtual Ceylan::System::Size getSizeInMemory() const = 0 ;
			
			
			
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
			 * Copy constructor made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Measurable( const Measurable & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Measurable & operator = ( const Measurable & source ) ;


	} ;


}



#endif // CEYLAN_MEASURABLE_H_

