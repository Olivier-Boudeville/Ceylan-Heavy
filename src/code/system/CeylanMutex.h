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


#ifndef CEYLAN_MUTEX_H_
#define CEYLAN_MUTEX_H_


#include "CeylanSystem.h"            // for ErrorCode
#include "CeylanLockable.h"          // for inheritance
#include "CeylanFeatures.h"          // for FeatureNotAvailableException

#include <string>


				


namespace Ceylan
{



	namespace System
	{



		/**
		 * Simple mutual exclusion device.
		 *
		 * Mostly encapsulates POSIX default initialized mutex.
		 *
		 * @note Mutex can be successfully instanciated if and only if
		 * the multithreading feature was enabled during the configuration
		 * step of the Ceylan library being used at run-time.
		 *
		 * @see Feature::isMultithreadingSupported
		 *
		 */
		class CEYLAN_DLL Mutex : public Ceylan::Lockable
		{



			/**
			 * Opaque handle for forward-declared but undefined struct
			 * pointer to system mutex, used to avoid including 
			 * system-specific headers such as pthread.
			 *
			 * Otherwise the API exposed by Ceylan would depend on these
			 * headers, then on a config.h that should then be installed 
			 * but may clash with others, and so on.
			 *
			 */
			struct SystemSpecificMutexType ;


			public:



				/**
				 * Basic constructor.
				 *
				 * @throw Features::FeatureNotAvailableException if the 
				 * multithreading feature is available.
				 *
				 */
				Mutex() ;


				/// Basic virtual destructor.
				virtual ~Mutex() throw() ;



				/**
				 * Locks the mutex. 
				 *
				 * This method is to be called by the user of the class
				 * instances.
				 *
				 * It will handle the locking process and will call the
				 * 'postLock' method afterwards.
				 *
				 * @throw LockException if the operation failed.
				 *
				 * @see isLocked, postLock, unlock
				 *
				 */
				virtual void lock() ;



				/**
				 * Unlocks the mutex.
				 *
				 * This method is to be called by the user of the class
				 * instances.
				 *
				 * It will handle the unlocking process and will call the
				 * 'preUnlock' method afterwards.
				 *
				 * @throw LockException if the operation failed.
				 *
				 * @see isLocked, preUnlock, lock
				 *
				 */
				virtual void unlock() ;



	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
	             */
	            virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) const  ;



			protected:



				/**
				 * Effective locking of the mutex.
				 *
				 * @throw LockException if the effective locking failed.
				 *
				 */
				virtual void postLock() ;



				/**
				 * Effective locking of the mutex.
				 *
				 * @throw LockException if the effective locking failed.
				 *
				 */
			 	virtual void preUnlock() ;



				/// Returns the reference on the mutex itself.
				SystemSpecificMutexType & getMutexReference() ;



			private:



				/**
				 * Copy constructor made private to ensure that 
				 * it will be never called.
				 *
				 * @note If this copy constructor had to be defined, then
				 * if would have to be explicitly defined since the version
				 * defined by the compiler would imply memberwise (shallow)
				 * copy, whereas the _internalMutex should be duplicated, 
				 * as it is owned (not used) by its Mutex object.
				 *
				 */
				Mutex( const Mutex & source ) ;



				/**
				 * Assignment operator made private to ensure that 
				 * it will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 *
				 */
				Mutex & operator = ( const Mutex & source ) ;


				/// The internal mutex type, depending on the platform.
		 		SystemSpecificMutexType * _internalMutex ;


		} ;
		

	}

}



#endif // CEYLAN_MUTEX_H_

