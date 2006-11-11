#ifndef CEYLAN_VISITOR_H_
#define CEYLAN_VISITOR_H_


#include "CeylanTextDisplayable.h"  // for TextDisplayable
#include "CeylanException.h"        // for Ceylan::Exception
#include "CeylanVisitor.h"          // for Visitor definition

#include <string>


namespace Ceylan
{


	

	// A Visitable is visited by Visitor instances.
	class Visitable ;


	/**
	 * A Visitor is a class designed to be subclassed.
	 * Each child class can visit a structure of Visitable
	 * instances and perform dedicated actions, that depend
	 * on the actual visitor <b>and</b> on the actual visitable
	 * being visited.
	 *
	 * @see http://en.wikipedia.org/wiki/Visitor_pattern
	 *
	 * @note This code is less meant for code reuse than for
	 * didactic purpose, as seldom any generic code can be 
	 * shared.
	 *
	 */
	class CEYLAN_DLL Visitor : public Ceylan::TextDisplayable
	{
		
		public:


			/// Default empty constructor.
			Visitor() throw() ;


			/**
			 * Visits a concrete Visitable.
			 *
			 */
			//void visit( MyFirstConcreteVisitable & concreteVisitable ) throw( VisitException ) ;
	

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
			Visitor( const Visitable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Visitor & operator = ( const Visitable & source ) throw() ;

	} ;

}


#endif // CEYLAN_VISITOR_H_
