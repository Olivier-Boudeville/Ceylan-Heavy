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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef CEYLAN_COUNTED_POINTER_H_
#define CEYLAN_COUNTED_POINTER_H_


#include "CeylanTypes.h"              // for Uint32
#include "CeylanTextDisplayable.h"    // for TextDisplayable
#include "CeylanOperators.h"          // for Ceylan::toString



#include <string>



/**
 * Advanced pointer template, counting its references and deleting the 
 * contained resource as soon as no one refers to it. 
 *
 * Significantly simpler than shared pointers of Boost (see CeylanSmartPointer
 * in unstable section), these counted pointers do not deal with multithread
 * (therefore, no lightweight mutex needed!) nor with circular references 
 * (no weak_ptr counterpart).
 *
 * Advanced raw pointer, but basic shared pointer. Mimics the STL names.
 *
 * Taken from http://c2.com/cgi/wiki?CppCountedPointerImplementation
 *
 * Thanks to Robert Di Falco.
 *
 * @see CeylanSmartPointer
 *
 */
 
 
/*
 * Optimization of copy, made only when necessary, currently disabled for the
 * sake of simplicity.
 *
 * Moreover, the code seems to make the assumption that a copy constructor 
 * for the templated type exists, which is not always true.
 *
 */
#define CEYLAN_COUNTED_POINTER_USE_COPY_ON_WRITE 0 
//#define CEYLAN_COUNTED_POINTER_USE_COPY_ON_WRITE 1


// Set to 1 if wanting refcount-related debug displays:
#define CEYLAN_COUNTED_POINTER_DEBUG 0
//#define CEYLAN_COUNTED_POINTER_DEBUG 1


#if CEYLAN_COUNTED_POINTER_DEBUG

	#include <iostream>

	#define CEYLAN_DISPLAY_REFCOUNT(message) std::cout << "[CountedPointer] " << message << std::endl ;

#else // CEYLAN_COUNTED_POINTER_DEBUG

	#define CEYLAN_DISPLAY_REFCOUNT(message)

#endif // CEYLAN_COUNTED_POINTER_DEBUG




namespace Ceylan
{



	/// The reference count type.
	typedef Ceylan::Uint32 ReferenceCount ;



	/**
	 * Pointers with reference counting.
	 *
	 * @example:
	 * <pre>
	 * CountedPointer<AnObjectType> p = new AnObjectType(...) ;
	 * p->aMethod() ;
	 * </pre>
	 *
	 *
	 * This pointer will track the references to the specified resource,
	 * which will be deallocated only when nobody references it any more.
	 *
	 * For each counted resource, exactly one referent will exist.
	 *
 	 * Original name: CountedPointer.h
	 * Original date: 07/24/99
	 * @author Robert Di Falco (http://c2.com/cgi/wiki?RobertDiFalco)
	 *	 
	 * Implementation of the CountedPointer specification. Uses a 'static 
	 * default' referent to improve the performance when being default
	 * constructed by an array or collection.
	 *
	 * #define CEYLAN_COUNTED_POINTER_USE_COPY_ON_WRITE
	 *
	 * Define the above constant if you wish to enable the
	 * CountedPointer::isolate member, which is useful for using 
	 * CountedPointer as the base for a string class that uses copyOnWrite
	 * for its non-const members.
	 *
	 * @note Not for multithread use.
	 *
	 * @note Does not deal with circular references.
	 *
	 * Another use of this pointer is to hold local resources so that they get
	 * deallocated whatever happens in a method (return, exceptions, etc.).
	 *
	 * @example: for a mutex, which is a kind of resource that has to be
	 * specifically deallocated under such circumstances:
	 * <pre>
	 * bool aMethod()
	 * {
	 *    CountedPointer<Mutex> p = new Mutex() ;
	 * 
	 *    if ( condition )
	 *    	throw Exception() ;  // no problemo!
	 *    else
	 *      return false ;
	 * }
	 * </pre>
	 *
	 * The mode of operation is to create for each shared resource any number
	 * of counter pointer instances, which all share (point to) the same
	 * referent structure, which is the one tracking the reference count and
	 * taking care of the life-cycle of that resource.
	 *
	 */
	template< typename T >
	class CountedPointer : public Ceylan::TextDisplayable
	{
	
	
		// First: interface.
			
			
		public:


			/// The counted object (wrapped resource) type.
			typedef T ElementType ;    



			/**
			 * Assignment constructor.
			 *
			 * Creates a counted pointer from a raw pointer.
			 *
			 * @example CountedPointer<Foo> foo_ref = new Foo() ;
			 *
			 * @param resourcePointer the unique pointer to the resource that
			 * should exist.
			 * If null (0), the counter pointer will refer to a void pointer
			 * common to all ElementType instances. 
			 * This is useful when an array of counted pointers is created,
			 * since their default constructor will be called as many times as
			 * there will be elements.
			 *
			 */
	        CountedPointer( ElementType * resourcePointer = 0 )
	        {
		 
	            /*
				 * Speeds default creation for arrays (stack gets a reference).
				 *
				 * Therefore, for each class templated with CountedPointer,
				 * as soon as a first counter pointer is created, the 
				 * following static referent will be itself created (and 
				 * deallocated at program ending).
				 *
				 */
	            static Referent staticRef( /* null pointer */ 0, 
					/* ref count */ 1 ) ; 

	            setReferent( ( resourcePointer == 0 ) ? 
					&staticRef : new Referent( resourcePointer ) ) ;
					
	        }



			/**
			 * Copy constructor.
			 *
			 * The two instances of counted pointer will refer to the same
			 * referent.
			 *
			 */
			CountedPointer( const CountedPointer<T> & source ) :
				Ceylan::TextDisplayable()
			{
			
				setReferent( source._referent ) ;
				
			}



			/**
			 * Assignment operator.
			 *
			 * @example:
			 * CountedPointer<Foo> foo_ref1 = new Foo() ;
			 * CountedPointer<Foo> foo_ref2 = new Foo() ;
			 * foo_ref2 = foo_ref1 ;
			 * which is equivalent to: foo_ref2.operator=( foo_ref1 ) ;
			 *			
			 */
			CountedPointer<T> & operator=( const CountedPointer<T> & source )
			{
			
				if ( _referent != source._referent )
				{
					// Sets own referent to the source one:
					reset( source._referent ) ;
				}
					
				/*
				 * else do nothing, for auto-assignment (foo_ref1 = foo_ref1 ;)
				 *
				 */
				
				// For assignment chaining:
				return * this ;
				
			}



			/**
			 * Destructor of a counted pointer, decreases the reference count
			 * which may lead to deallocation of resource.
			 *
			 * @note Should not specifically be called explicitly under normal
			 * use.
			 *
			 */
			~CountedPointer() throw()
			{
			
				release() ;
				
			}

			
			
			/**
			 * Dereferences the counted pointer to access the wrapped resource,
			 * and returns a reference to this resource.
			 *
			 */
			ElementType & operator*() const
			{
			
				return * get() ;
				
			}

			
			
			/**
			 * Returns the wrapped resource pointer.
			 *
			 */
			ElementType * operator->() const
			{
			
				return get() ;
				
			}



			/**
			 * Returns the wrapped resource pointer.
			 *
			 */
			ElementType * get() const
			{
			
				return _referent->_resourcePointer ;
				
			}



			/**
			 * Tells whether the reference count for the wrapped resource is
			 * exactly 1.
			 *
			 */
			bool isUnique() const
			{
			
				return ( _referent->_refCount == 1 ) ;
				
			}


			
			/**
			 * Returns the reference count for this counted pointer.
			 *
			 */
			ReferenceCount getReferenceCount() const
			{
			
				return _referent->_refCount ;
							
			}



           	/**
             * Returns a user-friendly description of this counted pointer.
             *
             * @see TextDisplayable.
             *
           	 */
            virtual const std::string toString( VerbosityLevels level = high )
				const
			{
			
				if ( _referent != 0 )
				{
					
					std::string res = "Counted pointer keeping track of " 
						+ Ceylan::toString( _referent->_refCount ) 
						+ " reference(s) to its wrapped resource ("
						+ Ceylan::toString( _referent->_resourcePointer ) 
						+ ")" ;
					
					if ( level == Ceylan::low )
						return res ;
					
					if ( _referent->_resourcePointer == 0 )
						return res + ", which is a null pointer (abnormal)" ;
					else
						return res + ", which is: " + 
							_referent->_resourcePointer->toString( level ) ;
								
				}		
				else
				{
					return "Counted pointer with no resource referenced" ;
				}	
				
			}	


#if CEYLAN_COUNTED_POINTER_USE_COPY_ON_WRITE


			/**
			 * Creates a deep clone of the shared pointer, including its 
			 * wrapped instance.
			 *
			 * If needed (i.e. if reference count is not one), this clones 
			 * the shared pointer too with its copy constructor and then
			 * isolates it from the rest of its references. 
			 *
			 * After the call, in all cases, the current shared pointer should
			 * be the only holder of reference to the resource.
			 * 
			 */
			void isolate()
			{
			
				if ( isUnique() )
				{
				
				    // Nothing to do.
					return ;
					
				}	


				/* 
				 * @note: if this fails, the state of the object must continue
				 * to be sound. 
				 * The only problem areas are the new operations (and possibly
				 * the delete called by release). 
				 * Basically, we treat the code like a transaction. 
				 *
				 */


				/*
				 * Throws out of isolate, if error creating element occurs.
				 *
				 * Supposes a copy constructor for the templated type exists!
				 *
				 */
				ElementType * newElement = new ElementType( * get() ) ;


				try
				{
			 
					/* 
					 * @note: This is safe if the new operation fails. 
					 * If success, it does a simple release and reference call. 
					 */

					reset( new Referent( newElement ) ) ;
				 
				}
				catch( ... )
				{
				
					delete newElement ;
					
					// Propagate the exception:
					throw ;   
					     
				}
				
 	        }


#endif // CEYLAN_COUNTED_POINTER_USE_COPY_ON_WRITE





     /// Second: implementation.


     private:


         /// The referenced representation of the resource.
         struct Referent
         {
		 
		 
		 	// Method members.
		 
		 
			/**
			 * Creates a new referent.
			 *
			 * @param resourcePointer the pointer to the resource to be wrapped.
			 *
			 * @param initialCount the initial reference count.
			 *
			 */
             Referent( ElementType * resourcePointer = 0, 
			 		ReferenceCount initialCount = 0 ):
                 _resourcePointer( resourcePointer ),
                 _refCount( initialCount )
             {
			
				CEYLAN_DISPLAY_REFCOUNT( 
					"after referent construction for resource pointer "
					<< Ceylan::toString( _resourcePointer )
					<< ", refcount = " << _refCount ) ;
			 
             }


             ~Referent() throw()
             {
			 
			 	CEYLAN_DISPLAY_REFCOUNT( 
					"referent destructor deallocating its resource "
					<< Ceylan::toString( _resourcePointer ) ) ;

			 	// The actual only place where the wrapped resource is deleted:
                delete _resourcePointer ;
				
             }
			 


			// Data members.
			
			
			/// Pointer to the actual object.
            ElementType * _resourcePointer ;     
			
			
			/// The reference count for the wrapped resource.
            ReferenceCount _refCount ;
			
			
         } * _referent ;                



		 /**
		  * Each CountedPointer has therefore a member: 
		  * Referent * _referent ; 
		  *
		  */


         // Simple methods to reference and release representations.



         /*
		  * @note The following methods should use critical sections if they
		  * were to be used in a multithreaded context.
		  *
		  */

		 
		 /// Releases one reference and sets the referent to the specified one.
         void reset( Referent * refPointer )
         {
		 
             // Enter critical section: auto_lock< lock_type > lock ;

             release() ;
             setReferent( refPointer ) ;
			 
         }



		/**
		 * Sets the referent for this counter pointer.
		 * Increments the reference count.
		 *
		 */
        void setReferent( Referent * refPointer )
        {

            // Enter critical section: auto_lock< lock_type > lock ;
			
            ( _referent = refPointer )->_refCount++ ;
			
			CEYLAN_DISPLAY_REFCOUNT( "after setReferent, refcount = " 
				<< _referent->_refCount << " for "
				<< Ceylan::toString( _referent->_resourcePointer ) ) ;
				
        }



		/**
		 * Release a reference to the wrapped resource.
		 * If no reference left, deallocate this unused resource.
		 *
		 */
        void release()
        {

            // Enter critical section: auto_lock< lock_type > lock ;

            if ( --_referent->_refCount == 0 )
			{
			
				CEYLAN_DISPLAY_REFCOUNT( "after release, refcount is null, "
					"deallocating resource "
					<< Ceylan::toString( _referent->_resourcePointer ) ) ;
					
				// Will trigger the resource deletion too:
                delete _referent ;
				_referent = 0 ;
				
			}
			else
			{
			
				CEYLAN_DISPLAY_REFCOUNT( "after release, refcount = " 
					<< _referent->_refCount << " for " 
					<< Ceylan::toString( _referent->_resourcePointer ) ) ;
			}
			
        }
		 
		 
		 
	 } ;  // Definition of CountedPointer template.
 
 
 
} // namespace Ceylan



#endif // CEYLAN_COUNTED_POINTER_H_

