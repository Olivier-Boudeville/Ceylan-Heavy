#ifndef CEYLAN_VISITABLE_H_
#define CEYLAN_VISITABLE_H_


#include "CeylanException.h"        // for Ceylan::Exception

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
			
			virtual ~VisitException() throw() ;
			
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
	class CEYLAN_DLL Visitable
	{
		
		public:


			/// Default empty constructor.
			Visitable() throw() ;


			/// Virtual destructor.
			virtual ~Visitable() throw() ;


			/**
			 * Allows given visitor to visit this object, thanks to a 
			 * callback : 'visitor.visit( *this ) ;'
			 *
			 * @note This method cannot be implemented here, as the visitor 
			 * must declare its 'visit' method which must accept a specific 
			 * datatype, not a generic one such as Visitable. 
			 * Otherwise, there would be ambiguous calls. 
			 *
			 */
			virtual void accept( Visitor & visitor ) 
				throw( VisitException ) = 0 ;





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
