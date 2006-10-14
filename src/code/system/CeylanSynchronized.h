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
		template <typename X>
		class /* CEYLAN_DLL */ Synchronized
		{

			public:


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

				/*
				~Synchronized() throw() 
				{

				}
				*/

				/// Sets value.
				Synchronized & setValue( const volatile X & value )
					throw( Lockable::LockException )
				{

					 _mutex.lock() ;
					 _value = value ;
					 _mutex.unlock() ;

					 return *this ;

				}


				/// Returns the current value.
				const volatile X & getValue() const volatile throw()
				{
					return _value ;
				}

				
				// Operators.


				/// Assignment operator.
				Synchronized & operator = ( const X & value )
					throw( Lockable::LockException )
				{
					return setValue( value ) ;
				}


				/// Conversion operator.
				operator X () const volatile throw()
				{
					return _value ;
				}


				/// Prefixed increment operator  (ex : ++x).
				X operator ++() throw( Lockable::LockException )
				{

					_mutex.lock() ;
					++_value ;
					_mutex.unlock() ;

					return _value ;

				}


				/// Postfixed increment operator (ex : x++).
				X operator ++(int) throw( Lockable::LockException )
				{
					return operator ++() ;
				}


				/// Prefixed decrement operator (ex : --x).
				X operator --() throw( Lockable::LockException )
				{

					_mutex.lock() ;
					--_value ;
					_mutex.unlock() ;
					return _value ;

				}


				/// Postfixed decrement operator (ex : x--).
				X operator --(int)
					throw( Lockable::LockException )
				{
					return operator --() ;
				}



			private:


				/// The synchronized resource.
				volatile X _value ;


				/// The protecting mutex.
				Mutex _mutex ;



				/**
				 * Default constructor made private to ensure that it
				 * will be never called.
				 *
				 */
				Synchronized() throw() ;


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


		} ;



		/**
		 * Partial specialization of the Synchonized template for bool.
		 *
		 * Needed since bool do not implement natively ++ and -- operators.
		 *
		 * @note Although its is a partial specialization, at least with some
		 * compilers (Visual C++ 2005), unchanged methods are not "inherited", 
		 * hence must be duplicated verbatim.
		 *
		 */
		template<>
		class /* CEYLAN_DLL */ Synchronized<bool>
		{

			public:


				/**
				 * Value assigned constructor.
				 *
				 * @note This constructor would not be convenient if it
				 * used the 'explicit' keyword.
				 *
				 */
				Synchronized( bool value ) throw() :
					_value( value )
				{

				}


				/// Sets value.
				Synchronized & setValue( const volatile bool & value )
					throw( Lockable::LockException )
				{

					 _mutex.lock() ;
					 _value = value ;
					 _mutex.unlock() ;

					 return *this ;

				}


				/// Returns the current value.
				const volatile bool & getValue() const volatile throw()
				{
					return _value ;
				}

				
				// Operators.


				/// Assignment operator.
				Synchronized & operator = ( const bool & value )
					throw( Lockable::LockException )
				{
					return setValue( value ) ;
				}


				/// Conversion operator.
				operator bool () const volatile throw()
				{
					return _value ;
				}


				/**
				 * Prefixed increment operator  (ex : ++x).
				 * 
				 * Here, reverses the logical value of the bool.
				 *
				 */
				bool operator ++() throw( Lockable::LockException )
				{

					_mutex.lock() ;
					_value = ! _value ;
					_mutex.unlock() ;

					return _value ;

				}


				/// Postfixed increment operator (ex : x++).
				bool operator ++(int) throw( Lockable::LockException )
				{
					return operator ++() ;
				}


				/**
				 * Prefixed decrement operator  (ex : --x).
				 * 
				 * Here, reverses the logical value of the bool.
				 *
				 */
				bool operator --() throw( Lockable::LockException )
				{

					_mutex.lock() ;
					_value = ! _value ;
					_mutex.unlock() ;
					return _value ;

				}


				/// Postfixed decrement operator (ex : x--).
				bool operator --(int)
					throw( Lockable::LockException )
				{
					return operator --() ;
				}


		private:


				/// The synchronized resource.
				volatile bool _value ;

				/// The protecting mutex.
				Mutex _mutex ;


				/**
				 * Default constructor made private to ensure that it
				 * will be never called.
				 *
				 */
				Synchronized() throw() ;


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

		} ;

	}

}


#endif // CEYLAN_SYNCHRONIZED_H_
