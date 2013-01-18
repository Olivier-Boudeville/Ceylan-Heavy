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


#ifndef CEYLAN_SINGLETON_H_
#define CEYLAN_SINGLETON_H_


#include "CeylanException.h"

#include <string>



namespace Ceylan
{



	/// Exception raised by the Singleton class.
	class CEYLAN_DLL SingletonException: public Ceylan::Exception
	{

		public:
	
			explicit SingletonException( const std::string & reason ) ;
			virtual ~SingletonException() throw() ;
			
	} ;
	
	

	/**
	 * This helper class ensures that one and only one instance of a particular
	 * class exists, so that this instance is necessarily shared by all its
	 * users.
	 *
	 * This class should be templated so that it can be used with any class 
	 * that has to have only one instance at any time.
	 *
	 * Another possible implementation would be:
	 *
	 * static AClass::singleton()
	 * {
 	 *   static AClass singleton ;
	 *   return singleton ;
	 * }
	 *
	 * @note This implementation is mainly for explanation purpose.
	 *
	 */
	class CEYLAN_DLL Singleton
	{


	    public:


	        /**
	         * Returns the one and only one Singleton instance available.
	         *
	         * The returned value is a reference and not a pointer, to avoid 
			 * any abnormal deallocation by its users, that should never
			 * deallocate the Singleton.
	         *
	         */
	        static Singleton & GetSingleton() ;


			/// Removes the shared Singleton.
	        static void DeleteSingleton() ;



	    protected:


	        /// Basic constructor.
	        Singleton() ;


	        /// Basic virtual destructor.
	        virtual ~Singleton() throw() ;



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
			Singleton( const Singleton & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Singleton & operator = ( const Singleton & source ) ;


	        /// The internal single instance.
	        static Singleton * _InternalSingleton ;



	} ;

}



#endif // CEYLAN_SINGLETON_H_

