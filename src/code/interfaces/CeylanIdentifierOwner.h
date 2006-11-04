#ifndef CEYLAN_IDENTIFIER_OWNER_H_
#define CEYLAN_IDENTIFIER_OWNER_H_


#include "CeylanException.h"        // for Ceylan::Exception
#include "CeylanTextDisplayable.h"  // for inheritance

#include <string>



namespace Ceylan
{



	// An IdentifierOwner owns an Identifier.
	class Identifier ;


	/// Exception to be raised when no identifier is available.
	class CEYLAN_DLL IdentifierNotAvailableException: public Ceylan::Exception
	{

		public:
				
			explicit IdentifierNotAvailableException( 
				const std::string & reason ) throw() ;
				
			virtual ~IdentifierNotAvailableException() throw() ;
					
	} ;



    /**
     * Interface that every object owning an identifier should 
	 * implement. 
     *
     * An identifier, a primary key, is a way of surely distinguishing
	 * between two references to know whether they point towards the 
	 * same object or not. 
     *
	 * Such identifiers are meant to be unique, among all possible 
	 * instances, classes, processes and hosts, at a particular moment. 
	 *
     * @see Object.
     * @see Identifier.
     *
     */
    class CEYLAN_DLL IdentifierOwner : public TextDisplayable
    {


        public:



            /**
			 * Basic constructor, does not assign internal identifier.
			 *
			 */
            IdentifierOwner() throw() ;


            /**
             * Common constructor, assigns internal identifier.
             *
             * @param id the identifier this IdentifierOwner should have.
             */
            explicit IdentifierOwner( const Identifier & id ) throw() ;


            /**
			 * Deletes this IdentifierOwner and, if necessary, its
			 * internal identifier.
			 *
			 */
            virtual ~IdentifierOwner() throw() ;


            /// Returns this IdentifierOwner's identifier.
            Identifier & getIdentifier() const 
				throw( IdentifierNotAvailableException ) ;


            /**
			 * Sets this IdentifierOwner's identifier.
			 *
			 * @note This IdentifierOwner takes ownership of 
			 * provided identifier.
			 *
			 */
            void setIdentifier( Identifier & id ) 
				throw( IdentifierNotAvailableException ) ;


            /// Returns whether this IdentifierOwner has a stored identifier.
            bool hasIdentifier() const throw() ;

            /// Deletes this IdentifierOwner's identifier.
            void deleteIdentifier() throw() ;


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
				Ceylan::VerbosityLevels level = Ceylan::high ) const throw() ;

	

		private:


			/// The owned identifier.
            Identifier * _id ;
		
		
			/**
			 * Copy constructor made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			IdentifierOwner( const IdentifierOwner & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 *
			 */			 
			IdentifierOwner & operator = ( 
				const IdentifierOwner & source ) throw() ;
		
			

    } ;

}


#endif // CEYLAN_IDENTIFIER_OWNER_H_
