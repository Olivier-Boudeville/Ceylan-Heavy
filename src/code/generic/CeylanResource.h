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


#ifndef CEYLAN_RESOURCE_H_
#define CEYLAN_RESOURCE_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanException.h"        // for inheritance
#include "CeylanTypes.h"            // for Ceylan::Uint32


#include <string>




namespace Ceylan
{



	/// Exception to be raised when a Resource encounters an abnormal situation.
	class CEYLAN_DLL ResourceException : public Ceylan::Exception
	{
	
		public:
			explicit ResourceException( const std::string & reason ) ;
			virtual ~ResourceException() throw() ;
	
	} ;




	/// Resources can be designated based on a resource identifier.
	typedef Ceylan::Uint32 ResourceID ;
	
	
	

	/**
	 * Void interface that has to be implemented by all Resources, so that
	 * they can be cached by a Resource manager.
	 *
	 * All Resource instances have to implement the TextDisplayable interface
	 * so that they can be asked for a description of their state
	 * (toString method).
	 *
	 */
	class CEYLAN_DLL Resource : public Ceylan::TextDisplayable
	{
	
	
	
		public:
		
			
			
			/// Creates a new resource.
			Resource() ;
			
			
			/// Virtual destructor.
			virtual ~Resource() throw() ;
			

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
			Resource( const Resource & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 * 
			 */			 
			Resource & operator = ( const Resource & source ) ;
			
			
	} ;
	
}



#endif // CEYLAN_RESOURCE_H_

