#ifndef CEYLAN_VISITABLE_H_
#define CEYLAN_VISITABLE_H_


#include "CeylanTextDisplayable.h"  // for TextDisplayable
#include "CeylanException.h"        // for Ceylan::Exception
#include "CeylanVisitor.h"          // for Visitor definition

#include <string>


namespace Ceylan
{


	/**
	 * Exception to be raised when a visit operation failed.
	 *
	 */
	class CEYLAN_DLL VisitException : public Ceylan::Exception
	{
	
		public:
		
			explicit VisitException( const std::string & reason ) throw() ;
	
	} ;


	// A Visitable is visited by Visitor instances.
	class Visitor ;


	/**
	 * A Visitable instance is made to be subclassed.
	 * Each child class can be visited by any Visitor.
	 *
	 * @see http://en.wikipedia.org/wiki/Visitor_pattern
	 *
	 */
	class CEYLAN_DLL Visitable : public Ceylan::TextDisplayable
	{
		
		public:


			/// Default empty constructor.
			Visitable() throw() ;

			/**
			 * Allows given visitor to visit this object, thanks to a 
			 * callback : 'visitor.visit( *this ) ;'
			 *
			 */
			void accept( Visitor & visitor ) throw( VisitException ) ;



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


		protected:


		private:

			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 * @note Made to avoid unwanted hidden clone of the Singleton.
			 *
			 */			 
			Visitable( const Visitable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Visitable & operator = ( const Visitable & source ) throw() ;

	} ;

}


#endif // CEYLAN_VISITABLE_H_
