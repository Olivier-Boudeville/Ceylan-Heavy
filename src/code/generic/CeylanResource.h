#ifndef CEYLAN_RESOURCE_H_
#define CEYLAN_RESOURCE_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanException.h"        // for inheritance

#include <string>



namespace Ceylan
{


	/// Exception to be raised when a Resource encounters an abnormal situation.
	class CEYLAN_DLL ResourceException : public Ceylan::Exception
	{
	
		public:
			explicit ResourceException( const std::string & reason ) throw() ;
			virtual ~ResourceException() throw() ;
	
	} ;



	/**
	 * Void interface that has to be implemented by all Resources, so that
	 * they can be cached by a Resource manager.
	 *
	 * All Resource instances have to implement the TextDisplayable interface
	 * so that they can be asked for a description of their state
	 * (toString method).
	 *
	 */
	class CEYLAN_DLL Resource : public Ceylan::TextDisplayable
	{
	
	
		public:
		
			
			/// Creates a new resource.
			Resource() throw() ;
			
			/// Virtual destructor.
			virtual ~Resource() throw() ;
			

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
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Resource( const Resource & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 * 
			 */			 
			Resource & operator = ( const Resource & source ) throw() ;
			
			
	} ;
	
}


#endif // CEYLAN_RESOURCE_H_
