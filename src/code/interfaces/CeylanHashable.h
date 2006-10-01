#ifndef CEYLAN_HASHABLE_H_
#define CEYLAN_HASHABLE_H_


#include "CeylanTypes.h"           // for Uint8, Uint32
#include "CeylanTextDisplayable.h" // for inheritance

#include <string>


namespace Ceylan
{


    /**
	 * Definition of a weak hash code.
	 *
	 * This hash code is described as weak since it can take only 256 
	 * different values.
	 *
	 * It is especially useful for hash tables that could not have thousands 
	 * of buckets.
	 *
	 */
    typedef Ceylan::Uint8 WeakHashCode ;


    /**
	 * Definition of a strong hash code.
	 *
	 * This hash code is described as strong since it can take 2^32 
	 * different values.
	 *
	 * It is especially useful in order to compare for equality two objects,
	 * since the probability they have the same hash code should be low.
	 *
	 */
    typedef Ceylan::Uint32 StrongHashCode ;



    /**
     * Interface that every object able to compute a hash value should
	 * implement, to ease hashtable handling.
     *
	 * @note It should be specified whether the hashcodes depend on the state 
	 * of the Hashable. At least in theory, they should not.
	 *
     * @see Object.
     *
     */
    class CEYLAN_DLL Hashable : public TextDisplayable
    {

        public:


			/// Basic constructor.
			Hashable() throw() ;
			
			/// Basic virtual destructor.
			virtual ~Hashable() throw() ;

			
			/// Returns the current weak hash code for this Hashable.
            virtual WeakHashCode getWeakHashCode() const throw() = 0 ;


			/// Returns the current strong hash code for this Hashable.
            virtual StrongHashCode getStrongHashCode() const throw() = 0 ;


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
			virtual const std::string toString( VerbosityLevels level = high )
				const throw() ;


			/**
			 * Returns the weak hash code corresponding to the specified 
			 * string.
			 *
			 */
			static WeakHashCode GetWeakHashCode( 
				const std::string & stringToHash ) throw() ;
			
			
			/**
			 * Returns the strong hash code corresponding to the specified
			 * string.
			 *
			 */
			static StrongHashCode GetStrongHashCode( 
				const std::string & stringToHash ) throw() ;



		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 *
			 */			 
			Hashable( const Hashable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be
			 * never called.
			 *
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 *
			 */			 
			Hashable & operator = ( const Hashable & source ) throw() ;
		

	
			
    } ;

}


#endif // CEYLAN_HASHABLE_H_
