#ifndef CEYLAN_SYNCHRONIZED_H_
#define CEYLAN_SYNCHRONIZED_H_


#include "CeylanMutex.h"             // for Mutex



namespace Ceylan
{


	namespace System
	{


		/**
		 * Template defining generically objects (notably numerical 
		 * datatypes) protected by a mutex.
		 *
		 * Any write operation first obtains a lock, then modifies 
		 * the value, and unlocks the mutex, thus synchronizing the 
		 * access to the value.
		 *
		 * @note This template still ought to be thoroughfully tested.
		 *
		 * @see Mutex, POSIX threads
		 *
		 */
		template <class X>
		class Synchronized
		{

			public:


				/// Default constructor.
				Synchronized() throw() : 
					_value() 
				{
				
				}


				/**
				 * Value assigned constructor.
				 *
				 * @note This constructor would not be convenient if it
				 * used the 'explicit' keyword.
				 *
				 */
				Synchronized( X value ) throw() :
					_value( value ) 
				{
				
				}



				/// Sets value.
				Synchronized & set( const volatile X & value ) 
					throw( Lockable::LockException )
				{
				
					 _mutex.lock() ; 
					 _value = value ; 
					 _mutex.unlock() ; 
					 
					 return *this ; 
					 
				}


				/// Gets value.
				const volatile X & get() const volatile throw()
				{ 
					return _value ; 
				}


				/// Assignment operator.
				Synchronized & operator = ( const X & value )
					throw( Lockable::LockException )
				{ 
					return set( value ) ; 
				}


				/// Conversion operator.
				operator const volatile X () const volatile throw()
				{ 
					return _value ; 
				}


				/// Increment operator.
				const volatile X operator ++() throw( Lockable::LockException )
				{ 
				
					_mutex.lock() ; 
					++_value ; 
					_mutex.unlock() ; 
					
					return _value ; 
					
				}


				/// Increment operator.
				const volatile X operator ++(int) 
					throw( Lockable::LockException )
				{ 
					return operator ++() ; 
				}


				/// Decrement operator.
				const volatile X operator --() throw( Lockable::LockException )
				{ 
				
					_mutex.lock() ; 
					--_value ; 
					_mutex.unlock() ; 
					return _value ; 
					
				}


				/// Decrement operator.
				const volatile X operator --(int) 
					throw( Lockable::LockException )
				{ 
					return operator --() ; 
				}



			private:



				/**
				 * Copy constructor made private to ensure that it 
				 * will be never called.
				 *
				 * Calls such as : <code>Synchronized<int> number = 0 ;</code>
				 * should be rewritten in : 
				 * <code>Synchronized<int> number( 0 ) ;</code> otherwise
				 * a copy constructor would be needed.
				 *
				 */
				Synchronized( const Synchronized & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 *
				 */
				Synchronized & operator = ( const Synchronized & source )
					throw() ;


				/// The synchronized resource.
				volatile X _value ;


				/// The protecting mutex.
				Mutex _mutex ;
				

		} ;

	}

}


#endif // CEYLAN_SYNCHRONIZED_H_
