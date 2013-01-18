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


#ifndef CEYLAN_COUNTABLE_H_
#define CEYLAN_COUNTABLE_H_



#include "CeylanDisplayable.h"     // for Ceylan::VerbosityLevels
#include "CeylanTypes.h"           // for Ceylan:Uint32


#include <string>



namespace Ceylan
{
	

	/**
	 * This very simple class allows instance counting for all objects
	 * that inherit from it. 
	 * Its main use is to check that every Countable is rightly deallocated
	 * when appropriate, i.e. each program should end up with a null instance
	 * count.
	 *
	 * @note This can only tell how many objects implementing this interface 
	 * are alive at any given time, no matter what their actual class is: one
	 * cannot tell which they are, what is their type. 
	 * So it gives the number of elements of a set, not its elements.
	 * Some more work is therefore needed to track down objects that should be 
	 * deallocated but are not. 
	 * Using Ceylan::Object with trackInstance set to true or Valgrind would
	 * be a solution.
	 *
	 * @see Ceylan::Object
	 *
	 * @see http://valgrind.kde.org
	 *
	 */
	class CEYLAN_DLL Countable
	{
		
		
		public:



			/// Records rhe number of instances.
			typedef Ceylan::Uint32 InstanceCount ;
			
			
			
			/**
			 * Constructs a Countable, increments the global reference count.
			 *
			 * @param verbose tells whether LogLight should be used to publish
			 * changes of instances counts.
			 *
			 * @note LogLight has to be enabled at compile-time
			 * (CEYLAN_DEBUG_LOG flag) to have those informations displayed.
			 *
			 */
			explicit Countable( bool verbose = true ) ;
			
			
			
			/// The virtual destructor decrements the reference count.
			virtual ~Countable() throw() ;
			
			
			
         	/**
             * Returns informations about the global reference count.
             *
             * @see TextDisplayable, Displayable
             * @see Ceylan::VerbosityLevels
             *
             */
            static const std::string ToString( 
				Ceylan::VerbosityLevels level = Ceylan::high ) ;	
			
			
			
			/// Returns the current instance count.
			static InstanceCount GetInstanceCount() ;
			
			
			
			/**
			 * Returns the current maximum instance count ever reached in
			 * this run.
			 *
			 */
			static InstanceCount GetMaximumInstanceCount() ;
			
				
				
		private:
		
		
			/**
			 * Tells whether count changes should be recorded in LightLog.
			 *
			 * @note This is a per-instance member.
			 *
			 */
			bool _verbose ;
		
		
			/// The current reference count.
			static InstanceCount ReferenceCount ;	
			
			
			/// The maximum reference count that has been already reached.
			static InstanceCount MaximumReferenceCount ;	
			
			
			/// The prefix used when instance counts are logged.
			static const std::string LogPrefix ;	

		
			/**
			 * Copy constructor made private to ensure that it will never
			 * be called.
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Countable( const Countable & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never
			 * be called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Countable & operator = ( const Countable & source ) ;
				
	} ; 


}


#endif // CEYLAN_COUNTABLE_H_

