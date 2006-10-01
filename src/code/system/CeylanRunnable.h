#ifndef CEYLAN_RUNNABLE_H_
#define CEYLAN_RUNNABLE_H_


#include "CeylanTextDisplayable.h"   // for inheritance
#include "CeylanSystem.h"            // for SystemException

#include <string>



namespace Ceylan 
{


	namespace System
	{
	
	
		/// Exception class for runnable concerns.
		class CEYLAN_DLL RunnableException : public SystemException
		{
		
			public:
			
				explicit RunnableException( const std::string message ) 
					throw() ;
				virtual ~RunnableException() throw() ; 			
							
		} ;
		
	
		/**
		 * Signature of an classical callback function.
		 *
		 * They can be useful so that functions can change behaviour
		 * by calling  such generic callbacks. 
		 * These caller functions do not have to know their callbacks
		 * a priori.
		 *
		 * The data parameter allows to customize the callback, 
		 * which will have to cast the given argument to the specific
		 * data it is actually looking for.
		 *
		 * Even though no exception specification can be specified,
		 * the callback should not throw any exception whatsoever.
		 *
		 */
		typedef void (*Callback) ( void * data ) /* throw() */ ;
	
	
	
		/** 
 		 * Basic runnable interface.
		 *
		 * @see Thread, Process.
		 *
		 */
		class CEYLAN_DLL Runnable : public Ceylan::TextDisplayable
		{


			public:
			
			
				/// Constructs an anonymous Runnable.
				Runnable() throw() ;


				// Constructs a Runnable whose name is <b>name</b>.
				explicit Runnable( const std::string & name ) throw() ;
				
				
				// Basic virtual destructor.
				virtual ~Runnable() throw() ;


				/**
				 * Start point method.
				 *
				 * @throw RunnableException if the runnable could not
				 * be run.
				 *
				 */
				virtual void run() throw( RunnableException ) = 0 ;
				
				
				/// Returns the name string.
				const std::string & getName() const throw() ;
				

            	/**
            	 * Returns a user-friendly description of the state of 
				 * this object.
            	 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall 
				 * settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) 
						const throw() = 0 ;

				
				
			private:
			

				/**
				 * Copy constructor made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				Runnable( const Runnable & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that
				 * it will be never called.
				 *
				 * The compiler should complain whenever this 
				 * undefined operator is called, implicitly or not.
				 *
				 */			 
				Runnable & operator = ( const Runnable & source ) throw() ;
				
				
				/// The name of the Runnable.
				std::string _name ;
				
				
		} ;

	}
	
}


#endif // CEYLAN_RUNNABLE_H_

