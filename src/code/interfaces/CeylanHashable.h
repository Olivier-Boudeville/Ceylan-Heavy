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


#ifndef CEYLAN_HASHABLE_H_
#define CEYLAN_HASHABLE_H_


#include "CeylanTypes.h"           // for Uint8, Uint32
#include "CeylanTextDisplayable.h" // for inheritance

#include <string>


namespace Ceylan
{


    /**
	 * Definition of a weak hash code.
	 *
	 * This hash code is described as weak since it can take only 256 
	 * different values.
	 *
	 * It is especially useful for hash tables that could not have thousands 
	 * of buckets.
	 *
	 */
    typedef Ceylan::Uint8 WeakHashCode ;


    /**
	 * Definition of a strong hash code.
	 *
	 * This hash code is described as strong since it can take 2^32 
	 * different values.
	 *
	 * It is especially useful in order to compare for equality two objects,
	 * since the probability they have the same hash code should be low.
	 *
	 */
    typedef Ceylan::Uint32 StrongHashCode ;



    /**
     * Interface that every object able to compute a hash value should
	 * implement, to ease hashtable handling.
     *
	 * @note It should be specified whether the hashcodes depend on the state 
	 * of the Hashable. At least in theory, they should not.
	 *
     * @see Object.
     *
     */
    class CEYLAN_DLL Hashable : public TextDisplayable
    {

        public:


			/// Basic constructor.
			Hashable() throw() ;
			
			/// Basic virtual destructor.
			virtual ~Hashable() throw() ;

			
			/// Returns the current weak hash code for this Hashable.
            virtual WeakHashCode getWeakHashCode() const throw() = 0 ;


			/// Returns the current strong hash code for this Hashable.
            virtual StrongHashCode getStrongHashCode() const throw() = 0 ;


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
			virtual const std::string toString( VerbosityLevels level = high )
				const throw() ;


			/**
			 * Returns the weak hash code corresponding to the specified 
			 * string.
			 *
			 */
			static WeakHashCode GetWeakHashCode( 
				const std::string & stringToHash ) throw() ;
			
			
			/**
			 * Returns the strong hash code corresponding to the specified
			 * string.
			 *
			 */
			static StrongHashCode GetStrongHashCode( 
				const std::string & stringToHash ) throw() ;



		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 *
			 */			 
			Hashable( const Hashable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be
			 * never called.
			 *
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 *
			 */			 
			Hashable & operator = ( const Hashable & source ) throw() ;
		

	
			
    } ;

}


#endif // CEYLAN_HASHABLE_H_
