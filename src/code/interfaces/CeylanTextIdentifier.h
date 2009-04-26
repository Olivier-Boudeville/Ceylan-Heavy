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
            TextIdentifier() ;



            /**
			 * Constructs a TextIdentifier whose identifier is the provided
			 * string.
			 *
			 */
            explicit TextIdentifier( const std::string & id ) ;
			
						
						
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
				Ceylan::VerbosityLevels level = Ceylan::high ) const ;


			/// Compares two text identifiers.
			bool operator==( const TextIdentifier & otherIdentifier ) ;
			
			
			
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
			TextIdentifier( const TextIdentifier & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			TextIdentifier & operator = ( const TextIdentifier & source ) ;
				
				
    } ;

}



#endif // CEYLAN_TEXT_IDENTIFIER_H_

