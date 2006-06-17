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
		class Mutex : public Ceylan::Lockable
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
				Mutex() throw( Features::FeatureNotAvailableException ) ;


				/// Basic virtual destructor.
				virtual ~Mutex() throw() ;


				/**
				 * Effective locking of the mutex.
				 *
				 * @throw LockException if the effective locking failed.
				 *
				 */
				 virtual void postLock() throw( LockException ) ;


				/**
				 * Effective locking of the mutex.
				 *
				 * @throw LockException if the effective locking failed.
				 *
				 */
			 	virtual void preUnlock() throw( LockException ) ;


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
						Ceylan::VerbosityLevels level = Ceylan::high )
					const throw() ;



			protected:


				/// Returns the reference on the mutex itself.
				SystemSpecificMutexType & getMutexReference() throw() ;



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
				Mutex( const Mutex & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that 
				 * it will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 *
				 */
				Mutex & operator = ( const Mutex & source ) throw() ;


				/// The internal mutex type, depending on the platform.
		 		SystemSpecificMutexType * _internalMutex ;



		} ;

	}

}


#endif // CEYLAN_MUTEX_H_

