#ifndef CEYLAN_GENERIC_CONTROLLER_H_
#define CEYLAN_GENERIC_CONTROLLER_H_


#include "CeylanTextDisplayable.h"          // for inheritance

#include <string>


/*
 * Here the base controller is defined.
 *
 */
 
 

namespace Ceylan
{



    /**
     * Interface class for all controllers of the lightweight generic
	 * Model-Controller-Controller (MVC) design pattern.
	 *
	 * The controller of an object may modify the state of the model(s)
	 * it is linked with.
	 *
	 * @see testCeylanGenericMVC.cc for examples.
	 *
     */
    class CEYLAN_DLL BaseController : public TextDisplayable
    {


        public:
					
							
			/**
			 * Constructs a controller, not linked to any model.
			 *
			 */
			BaseController() throw() ;
				
									
			/// Basic virtual destructor.
			virtual ~BaseController() throw() ;
			
							
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
			 * Copy constructor made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 */			 
			BaseController( const BaseController & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			BaseController & operator = ( const BaseController & source )
				throw() ;

			
					 			
    } ;	


}


#endif // CEYLAN_GENERIC_CONTROLLER_H_

