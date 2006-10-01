#ifndef CEYLAN_TEXT_IDENTIFIER_H_
#define CEYLAN_TEXT_IDENTIFIER_H_


#include "CeylanIdentifier.h"

#include <string>



namespace Ceylan
{


    /**
	 * Text-only identifiers.
	 *
     * @see Identifier.
     *
     */
    class CEYLAN_DLL TextIdentifier : public Identifier
    {


        public:


            /// Basic void constructor.
            explicit TextIdentifier() throw() ;


            /**
			 * Constructs a TextIdentifier whose identifier is the provided
			 * string.
			 *
			 */
            explicit TextIdentifier( const std::string & id ) throw() ;
			
						
           /// Basic destructor, to ensure it remains virtual.
            virtual ~TextIdentifier() throw() ;
			
			
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


			/// Compares to text identifiers.
			bool operator==( const TextIdentifier & otherIdentifier ) throw() ;
			
			
			
		protected:	
			
			
			/// The text identifier itself.	
			std::string _id ;



		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 */			 
			TextIdentifier( const TextIdentifier & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			TextIdentifier & operator = ( const TextIdentifier & source )
				throw() ;
				
				
    } ;

}


#endif // CEYLAN_TEXT_IDENTIFIER_H_
