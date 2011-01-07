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


#ifndef CEYLAN_UNICODE_H_
#define CEYLAN_UNICODE_H_


#include "CeylanTextDisplayable.h"   // for inheritance
#include "CeylanStringUtils.h"       // for Ceylan::Latin1Char, StringSize



/**
 * This part of the Ceylan namespace gathers very limited Unicode generic
 * facilities, including UTF-8s.
 *
 */

namespace Ceylan
{
	

	/**
	 * Describes a 16-bit UNICODE value.
	 *
	 */
	typedef Ceylan::Uint16 Unicode ;



	/**
	 * Encapsulates a Unicode string to facilitate its management.
	 * 
	 * A first UTF-8 (Unicode Transformation Format-8) encoding support is
	 * provided as well.
	 *
	 * UTF-8 is the default encoding for XML.
	 *
	 * @see http://wikipedia.org/wiki/UTF-8
	 * @see http://www.utf-8.com/
	 *
	 * @note Unicode services still have to be tested.
	 *
	 */
	class CEYLAN_DLL UnicodeString : public Ceylan::TextDisplayable
	{
	
	
		public:
		
		
			/**
			 * Constructs an empty Unicode string.
			 *
			 */
			UnicodeString() ;
			
			
			
			/// Virtual destructor.
			virtual ~UnicodeString() throw() ;
			
			
			
			/// Returns the number of characters in this Unicode string.
			virtual StringSize size() const ;
			
			
			
			/**
			 * Creates a new Unicode string which is the copy of this Unicode
			 * string.
			 *
			 * Ownership of the new string is transferred to the caller, 
			 * which will have to deallocate it when not useful anymore.
			 *
			 */
			virtual UnicodeString & copy( bool swap = false ) const ;
		
		
		
			/**
			 * Sets this string from the specified Latin-1 encoded string.
			 * 
			 * @param latin1String the Latin-1 encoded string which will be
			 * copied and encoded in Unicode in this string.
			 *
			 */
			virtual void setFromLatin1( const std::string & latin1String ) ;
			
			
			
			/**
			 * Sets this string from the specified UTF-8 encoded string.
			 *
			 * @param utf8String the UTF-8 encoded string which will be 
			 * copied and encoded in Unicode in this string.
			 *
			 * @param size the number of characters encoded in the UTF-8 
			 * string (not the string size, since a UTF-8 character may be
			 * encoded in more than one byte).
			 *
			 */
			virtual void setFromUTF8( const std::string & utf8String, 
				StringSize characterCount ) ;
			
			
			
			/**
	         * Returns an user-friendly description of the state of this object.
	         *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see Ceylan::TextDisplayable
	         *
	         */
		 	virtual const std::string toString( 
				Ceylan::VerbosityLevels level = Ceylan::high ) const ;
			
			
			
			// Static section.
			
			
			/// Converts specified Latin-1 encoded character to Unicode.
			static Unicode ConvertFromLatin1( Ceylan::Latin1Char latin1Char ) ;
			
			
			
			
		protected:
		
		
		
			/**
			 * Sets the internal character buffer so that it has the specified
			 * capacity.
			 *
			 * @note The memory is allocated but the content is undefined.
			 *
			 */
			virtual void setCapacity( StringSize newSize ) ;
			
			
			
			/// The buffer storing Unicode characters.
			Unicode * _buffer ;
			
			
			
			
		private:	
	
	
			/**
			 * Copy constructor made private to ensure that it will never be
			 * called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			UnicodeString( const UnicodeString & source ) ;
		
		
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 *
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			UnicodeString & operator = ( const UnicodeString & source ) ;
			
	
	} ;
	

}



#endif // CEYLAN_UNICODE_H_

