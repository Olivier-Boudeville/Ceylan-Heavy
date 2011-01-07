/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
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
	 * @note Non-abstract child classes should implement:
	 * 'bool operator==( const ChildIdentifier & otherIdentifier )'
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
            Identifier() ;


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
				level = Ceylan::high ) const = 0 ;



		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Identifier( const Identifier & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			Identifier & operator = ( const Identifier & source ) ;
			
	
    } ;

}



#endif // CEYLAN_IDENTIFIER_H_

