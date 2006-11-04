#ifndef CEYLAN_IDENTIFIER_H_
#define CEYLAN_IDENTIFIER_H_


#include "CeylanException.h"         // for Exception
#include "CeylanTextDisplayable.h"   // for inheritance

#include <string>



namespace Ceylan
{


    /**
	 * Mother class of all identifiers.
	 *
	 * @note Non-abstract child classes should implement :
	 * bool operator==( const ChildIdentifier & otherIdentifier ) throw() ; 
	 *
     * @see IdentifierOwner.
     *
     */
    class CEYLAN_DLL Identifier : public TextDisplayable
    {

        public:


			/// Exception for identifier issues.
			class IdentifierException : public Exception
			{
			
			
				public:
				
				
					IdentifierException( const std::string message ) :
						Exception( message )
					{
					
					}	
					
					
					virtual ~IdentifierException() throw()
					{
					
					}
					
			} ;


            /// Basic void constructor.
            Identifier() throw() ;

            /// Basic destructor, to ensure it remains virtual.
            virtual ~Identifier() throw() ;
			
			
            /**
             * Returns a user-friendly description of the state of this 
			 * object.
             *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see TextDisplayable
             *
             */
            virtual const std::string toString( Ceylan::VerbosityLevels 
				level = Ceylan::high ) const throw() = 0 ;



		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Identifier( const Identifier & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			Identifier & operator = ( const Identifier & source ) throw() ;
			
	
    } ;

}


#endif // CEYLAN_IDENTIFIER_H_
