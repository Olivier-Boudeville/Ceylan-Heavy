#ifndef CEYLAN_EXCEPTION_H_
#define CEYLAN_EXCEPTION_H_


#include "CeylanTextDisplayable.h"  // for inheritance


#include <string>
#include <exception>                // for std::exception
#include <iosfwd>                   // for ostream



namespace Ceylan
{


    /**
     * This Exception class should be the mother of all exceptions raised by 
	 * the Ceylan library.
     * 
     * Exception subclasses standard exception, and should be the root of the
	 * whole Ceylan inheritance tree for exceptions.
     *
	 * @todo Redefine set_unexpected to avoid unclear messages when 
	 * unexpected exceptions occur.
	 *
     */
    class CEYLAN_DLL Exception : public std::exception, public TextDisplayable
    {


        public:


            /**
             * Basic constructor.
             *
             * @param reason an explanation for this exception being raised.
             *
             */
            explicit Exception( const std::string & reason ) throw() ;


            /// Basic virtual destructor.
            virtual ~Exception() throw() ;


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
            virtual const std::string toString( VerbosityLevels level = high )
				const throw() ;


            /**
             * Returns a user-friendly description of the exception.
             * Uses high level of detail for the exception description.
             *
             * This methods is made for backward compability with
			 * std::exception.
             * 
             * @see toString the recommended way of having the exception
			 * description.
             *
             */
            virtual const char * what() const throw() ;



        protected:


            /**
			 * Contains the message giving more accurate feedback once the
			 * exception is raised.
			 *
			 */
            std::string _reason ;



		private:
		
		
			/*
			 * Copy constructor could not be made private, since it has to be
			 * called whenever any Exception is being thrown. 
			 *
			 * Exception( const Exception & source ) throw() ;
			 *
			 */
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 *
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Exception & operator = ( const Exception & source ) throw() ;


    } ;

}


/**
 * Operator used to display easily an exception's message into an output
 * stream.
 *
 * The message is the one returned by toString, with high level of detail
 * selected.
 *
 * @see toString.
 * 
 */
std::ostream & operator << ( std::ostream & os, const Ceylan::Exception & e )
	throw() ;



#endif // CEYLAN_EXCEPTION_H_
