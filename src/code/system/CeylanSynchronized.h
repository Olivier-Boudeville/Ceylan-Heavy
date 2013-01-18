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


#ifndef CEYLAN_SYNCHRONIZED_H_
#define CEYLAN_SYNCHRONIZED_H_


#include "CeylanMutex.h"             // for Mutex




namespace Ceylan
{



	namespace System
	{



		/**
		 * Template defining generically objects (notably numerical datatypes)
		 * protected by a mutex.
		 *
		 * Any write operation first obtains a lock, then modifies the value,
		 * and unlocks the mutex, thus synchronizing the access to the value.
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
				 * @note This constructor would not be convenient if it used the
				 * 'explicit' keyword.
				 *
				 */
				Synchronized( X value ) :
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
				{

					 _mutex.lock() ;
					 _value = value ;
					 _mutex.unlock() ;

					 return *this ;

				}



				/// Returns the current value.
				const volatile X & getValue() const volatile
				{

					return _value ;

				}




				// Operators.



				/// Assignment operator.
				Synchronized & operator = ( const X & value )
				{

					return setValue( value ) ;

				}



				/// Conversion operator.
				operator X () const volatile
				{

					return _value ;

				}



				/// Prefixed increment operator  (ex: ++x).
				X operator ++()
				{

					_mutex.lock() ;
					++_value ;
					_mutex.unlock() ;

					return _value ;

				}



				/// Postfixed increment operator (ex: x++).
				X operator ++(int)
				{

					return operator ++() ;

				}



				/// Prefixed decrement operator (ex: --x).
				X operator --()
				{

					_mutex.lock() ;
					--_value ;
					_mutex.unlock() ;
					return _value ;

				}



				/// Postfixed decrement operator (ex: x--).
				X operator --(int)
				{
					return operator --() ;
				}




			private:



				/// The synchronized resource.
				volatile X _value ;


				/// The protecting mutex.
				Mutex _mutex ;



				/**
				 * Default constructor made private to ensure that it will be
				 * never called.
				 *
				 */
				Synchronized() ;


				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * Calls such as: <code>Synchronized<int> number = 0 ;</code>
				 * should be rewritten in: <code>Synchronized<int> number( 0 )
				 * ;</code> otherwise a copy constructor would be needed.
				 *
				 */
				Synchronized( const Synchronized & source ) ;



				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				Synchronized & operator = ( const Synchronized & source ) ;


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
				 * @note This constructor would not be convenient if it used the
				 * 'explicit' keyword.
				 *
				 */
				Synchronized( bool value ) :
					_value( value )
				{

				}



				/// Sets value.
				Synchronized & setValue( const volatile bool & value )
				{

					 _mutex.lock() ;
					 _value = value ;
					 _mutex.unlock() ;

					 return *this ;

				}



				/// Returns the current value.
				const volatile bool & getValue() const volatile
				{

					return _value ;

				}



				// Operators.


				/// Assignment operator.
				Synchronized & operator = ( const bool & value )
				{

					return setValue( value ) ;

				}



				/// Conversion operator.
				operator bool () const volatile
				{

					return _value ;

				}



				/**
				 * Prefixed increment operator  (ex: ++x).
				 *
				 * Here, reverses the logical value of the bool.
				 *
				 */
				bool operator ++()
				{

					_mutex.lock() ;
					_value = ! _value ;
					_mutex.unlock() ;

					return _value ;

				}



				/// Postfixed increment operator (ex: x++).
				bool operator ++(int)
				{

					return operator ++() ;

				}



				/**
				 * Prefixed decrement operator  (ex: --x).
				 *
				 * Here, reverses the logical value of the bool.
				 *
				 */
				bool operator --()
				{

					_mutex.lock() ;
					_value = ! _value ;
					_mutex.unlock() ;
					return _value ;

				}



				/// Postfixed decrement operator (ex: x--).
				bool operator --(int)
				{

					return operator --() ;

				}




		private:


				/// The synchronized resource.
				volatile bool _value ;


				/// The protecting mutex.
				Mutex _mutex ;



				/**
				 * Default constructor made private to ensure that it will be
				 * never called.
				 *
				 */
				Synchronized() ;



				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * Calls such as: <code>Synchronized<int> number = 0 ;</code>
				 * should be rewritten in: <code>Synchronized<int> number( 0 )
				 * ;</code> otherwise a copy constructor would be needed.
				 *
				 */
				Synchronized( const Synchronized & source ) ;



				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				Synchronized & operator = ( const Synchronized & source ) ;


		} ;


	}


}



#endif // CEYLAN_SYNCHRONIZED_H_
