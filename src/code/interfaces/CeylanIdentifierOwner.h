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
	class CEYLAN_DLL IdentifierNotAvailableException : public Ceylan::Exception
	{

		public:

			explicit IdentifierNotAvailableException(
				const std::string & reason ) ;

			virtual ~IdentifierNotAvailableException() throw() ;

	} ;



	/**
	 * Interface that every object owning an identifier should implement.
	 *
	 * An identifier, a primary key, is a way of surely distinguishing between
	 * two references to know whether they point towards the same object or not.
	 *
	 * Such identifiers are meant to be unique, among all possible instances,
	 * classes, processes and hosts, at a particular moment.
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
			IdentifierOwner() ;


			/**
			 * Common constructor, assigns internal identifier.
			 *
			 * @param id the identifier this IdentifierOwner should have.
			 */
			explicit IdentifierOwner( const Identifier & id ) ;



			/**
			 * Deletes this IdentifierOwner and, if necessary, its internal
			 * identifier.
			 *
			 */
			virtual ~IdentifierOwner() throw() ;



			/**
			 * Returns this IdentifierOwner's identifier.
			 *
			 * @throw IdentifierNotAvailableException if the operation failed.
			 *
			 */
			Identifier & getIdentifier() const ;



			/**
			 * Sets this IdentifierOwner's identifier.
			 *
			 * @throw IdentifierNotAvailableException if the operation failed.
			 *
			 * @note This IdentifierOwner takes ownership of provided
			 * identifier.
			 *
			 */
			void setIdentifier( Identifier & id ) ;



			/// Returns whether this IdentifierOwner has a stored identifier.
			bool hasIdentifier() const ;



			/// Deletes this IdentifierOwner's identifier.
			void deleteIdentifier() ;



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



		private:



			/// The owned identifier.
			Identifier * _id ;


			/**
			 * Copy constructor made private to ensure that it will be never
			 * called.
			 *
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 *
			 */
			IdentifierOwner( const IdentifierOwner & source ) ;



			/**
			 * Assignment operator made private to ensure that it will be never
			 * called.
			 *
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 *
			 */
			IdentifierOwner & operator = ( const IdentifierOwner & source ) ;


	} ;


}



#endif // CEYLAN_IDENTIFIER_OWNER_H_
