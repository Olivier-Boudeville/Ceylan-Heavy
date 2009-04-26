/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


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
            explicit Exception( const std::string & reason ) ;


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
				const  ;



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
			 * Exception( const Exception & source )  ;
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
			Exception & operator = ( const Exception & source ) ;


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
std::ostream & operator << ( std::ostream & os, const Ceylan::Exception & e ) ;



#endif // CEYLAN_EXCEPTION_H_

