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


#ifndef CEYLAN_SMART_RESOURCE_H_
#define CEYLAN_SMART_RESOURCE_H_


#include "CeylanResource.h"         // for inheritance
#include "CeylanMeasurable.h"       // for inheritance
#include "CeylanClonable.h"         // for inheritance



namespace Ceylan
{


	/**
	 * Smart Resources are advanced resources, able to determine their size 
	 * and clone themselves at will, for example so that they can be cached
	 * by a SmartResourceManager.
	 *
	 * All resources implement the Measurable interface, and therefore should
	 * be able to evaluate the size in memory they are occupying, so that the
	 * cache can adapt its policy with regard to the memory it uses for them.
	 *
	 * All resources implement the Clonable interface too, since the cache may
	 * have to store only copies of submitted resources, and may have to give
	 * the caller a copy (a clone) of them.
	 * Otherwise the cached resource would be modified too as the original one
	 * is used afterwards, or could be even silently deallocated while still 
	 * in cache, depending of their use by the caller.
	 *
	 */
	class CEYLAN_DLL SmartResource: 
		public Resource, public Measurable, public Clonable
	{
	
	
		public:
		
			
			/// Creates a new resource.
			SmartResource() ;
			
			
			/// Virtual destructor.
			virtual ~SmartResource() throw() ;
	
			
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
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			SmartResource( const SmartResource & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 *
			 */			 
			SmartResource & operator = ( const SmartResource & source ) ;
				
			
	} ;

}



#endif // CEYLAN_SMART_RESOURCE_H_

