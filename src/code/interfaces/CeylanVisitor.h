#ifndef CEYLAN_VISITOR_H_
#define CEYLAN_VISITOR_H_


#include "CeylanTextDisplayable.h"  // for TextDisplayable
#include "CeylanException.h"        // for Ceylan::Exception

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
	 * shared for visitors.
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
			 * @note Should be declared on the actual visitor (ex : a 
			 * XMLVisitor) as it must specify the actual visitables it can
			 * visit (ex : XML markup, XML text, etc.), and not a more generic
			 * type.
			 *
			 
			virtual void visit( MyFirstConcreteVisitable & concreteVisitable ) 
				throw( VisitException ) ;
	
			 */


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
			 * @note Made to avoid unwanted hidden clone of the Singleton.
			 *
			 */			 
			Visitor( const Visitor & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Visitor & operator = ( const Visitor & source ) throw() ;
			

	} ;

}


#endif // CEYLAN_VISITOR_H_
