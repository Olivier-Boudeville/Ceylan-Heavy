#ifndef CEYLAN_MEASURABLE_H_
#define CEYLAN_MEASURABLE_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanSystem.h"           // for Size

#include <string>



namespace Ceylan
{


    /**
     * Interface which should be implemented for all objects whose size 
	 * can be dynamically evaluated (at runtime).
	 *
	 * Measurable objects are notably able to evaluate their memory footprint.
	 *
     */
    class CEYLAN_DLL Measurable : public TextDisplayable
    {


        public:

		
			/// Creates a new Measurable resource.
			Measurable() throw() ;
			
			
			/// Virtual destructor.
			virtual ~Measurable() throw() ;
			
			
			/**
			 * Returns the approximate size in memory, in bytes, currently
			 * taken by this object.
			 *
			 * The general method for implementing this interface is to 
			 * evaluate the static size of the object (thanks to 'sizeof()') 
			 * and to evaluate recursively all owned dynamically allocated
			 * members. The overall sum is the size to return.
			 * 
			 */
			virtual Ceylan::System::Size getSizeInMemory() const throw() = 0 ;
			
			
			
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
				Ceylan::VerbosityLevels level = Ceylan::high ) const throw() ;


		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Measurable( const Measurable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Measurable & operator = ( const Measurable & source ) throw() ;


	} ;


}

#endif // CEYLAN_MEASURABLE_H_
