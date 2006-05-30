#ifndef CEYLAN_CLONABLE_H_
#define CEYLAN_CLONABLE_H_


#include "CeylanException.h"   // for inheritance

#include <string>



namespace Ceylan
{



	/// Exception to be raised whenever cloning fails.
	class ClonableException : public Ceylan::Exception
	{
	
		public:
		
			explicit ClonableException( const std::string & message ) throw() ;
			
			virtual ~ClonableException() throw() ;
			
	
	} ;
	
	
	
    /**
     * Interface that every object which can be cloned should implement. 
     *
     */
    class Clonable
    {

        public:
		

			/// Basic constructor.
			Clonable() throw()
			{
			
			}
			
			
			/// Basic virtual destructor.
			virtual ~Clonable() throw()
			{
			}
			
			
            /**
			 * Returns a clone of this object. 
			 *
			 * The ownership of the clone is transferred to the caller, who
			 * therefore shall delete the clone when appropriate.
			 *
			 * @throw ClonableException whenever the cloning fails.
			 *
			 */
            virtual Clonable & clone() const throw( ClonableException ) = 0 ;
		
		
			
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Clonable( const Clonable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Clonable & operator = ( const Clonable & source ) throw() ;
		
			

    } ;

}


#endif // CEYLAN_CLONABLE_H_
