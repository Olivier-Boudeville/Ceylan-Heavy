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


#ifndef CEYLAN_OPERATORS_H_
#define CEYLAN_OPERATORS_H_


#include "CeylanException.h"      // for Ceylan::Exception
#include "CeylanDisplayable.h"    // for Ceylan::VerbosityLevels
#include "CeylanTypes.h"          // for Ceylan::Uint8, etc.


#include <string>
#include <list>



/**
 * This module offers the operators lacking in C++ in order to perform 
 * appending of numerical values to standard strings, and basic string
 * conversion.
 *
 * Various conversions are provided, from numerical to string and the other 
 * way round.
 *
 * Each operator has its counterpart, which allows to add a numerical value 
 * to a string in both orders: 'numerical + string' and also 
 * 'string + numerical'.
 * 
 * Warning: a C++ compiler evaluates '"Four means " + 4' as 'char *' + 'int'
 * so it does pointer arithmetic and returns unexpected bytes: actually, it
 * returns the 'char *' starting with the space before "means", 4 characters
 * after the initial F.
 *
 * Please consider using 'string( "Four means " ) + 4', which would result in
 * the expected string "Four means 4".
 *
 * @note Operators should not be defined in a namespace.
 *
 * @see CeylanStringUtils.h for other string manipulations.
 *
 * Most operators can throw Ceylan::Exception.
 *
 */

CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Sint8 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Sint8 i, const std::string & s ) ;


CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Uint8 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Uint8 i, const std::string & s ) ;



CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Sint16 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Sint16 i, const std::string & s ) ;


CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Uint16 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Uint16 i, const std::string & s ) ;



CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Sint32 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Sint32 i, const std::string & s ) ;


CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Uint32 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Uint32 i, const std::string & s ) ;



/*
 * No serialization of Uint64 and Sint64 for the moment, since
 * on some platforms they are just fake datatypes.
 *
 
CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Sint64 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Sint64 i, const std::string & s ) ;


CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Uint64 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Uint64 i, const std::string & s ) ;
 
 *
 */



CEYLAN_DLL std::string operator + ( const std::string & s,
	Ceylan::SignedLongInteger i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::SignedLongInteger i, 
	const std::string & s ) ;


CEYLAN_DLL std::string operator + ( const std::string & s,
	Ceylan::UnsignedLongInteger i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::UnsignedLongInteger i,
	const std::string & s ) ;



CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Float32 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Float32 i, const std::string & s ) ;


CEYLAN_DLL std::string operator + ( const std::string & s, Ceylan::Float64 i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::Float64 i, const std::string & s ) ;



/*
 * See CeylanTypes.h to understand why Ceylan::Float80 is not defined anymore.
 *
 */
  
CEYLAN_DLL std::string operator + ( const std::string & s, 
	Ceylan::LongFloat i ) ;
	
CEYLAN_DLL std::string operator + ( Ceylan::LongFloat i, 
	const std::string & s ) ;



// Pointer size depends on the platform.

CEYLAN_DLL std::string operator + ( const std::string & s, const void * p )	;
	
CEYLAN_DLL std::string operator + ( const void * p, const std::string & s )	;


CEYLAN_DLL std::string operator + ( const std::string & a, const char * b ) ;

CEYLAN_DLL std::string operator + ( const char * a, const std::string & b ) ;




namespace Ceylan
{


	/**
	 * This section focuses on conversion from various numerical types to
	 * standard strings.
	 *
	 */


	/**
	 * Determines the <b>maximum</b> number of digits that shall be output
	 * on string insertion operations to express floating-point values, 
	 * counting both the digits before and after the decimal point.
	 *
	 * @note 40, the actual value, seems to be largely enough, even for
	 * Ceylan::Float64.
	 *
	 */
	extern CEYLAN_DLL const Ceylan::Uint16 DigitOutputPrecision ;
	
	
	
    /// Returns a user-friendly representation of a pointer.
    CEYLAN_DLL std::string toString( const void * pointer ) ;



    /**
	 * Returns a user-friendly textual representation of a bool 
	 * ("true" or "false").
	 *
	 */
    CEYLAN_DLL std::string toString( bool value ) ;



	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::Sint8 value, 
		bool bitField = false ) ;
	
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::Uint8 value, 
		bool bitField = false ) ;
	
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::Sint16 value, 
		bool bitField = false ) ;
	
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::Uint16 value, 
		bool bitField = false ) ;
	
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::Sint32 value, 
		bool bitField = false ) ;
	
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::Uint32 value, 
		bool bitField = false ) ;
	
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value prefixed by '0b'.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::UnsignedLongInteger value, 
		bool bitField = false ) ;
		
		
		
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value prefixed by '0b'.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::SignedLongInteger value, 
		bool bitField = false ) ;
		
	
	
	/// The hexadecimal digits: 0123456789ABCDEF.
	extern CEYLAN_DLL const std::string HexDigits ;
	
	
	
	/**
	 * Converts the numerical value to a string with the value in 
	 * hexadecimal format.
	 *
	 * @param value the value to convert in hexadecimal.
	 *
	 * @param prefix tells whether the hexadecimal prefix ("0x") 
	 * should be added.
	 *
	 * @param minDigits tells which minimal number of digits should be put 
	 * in the returned string (prefix not included). Leading zeroes are 
	 * added to reach that number.
	 *
	 */
	CEYLAN_DLL std::string toHexString( Ceylan::UnsignedLongInteger value, 
		bool prefix = true, Ceylan::Uint8 minDigits = 1 ) ;	
	
	
	
	// Conversions from floating point values to text.
	
	

	/**
	 * Converts the numerical value to a string, with specified precision,
	 * in fixed (not scientific) notation.
	 *
	 * @param value the value to convert to text.
	 *
	 * @param precision the requested precision, i.e. the number of digits
	 * that is to be displayed after the dot. 
	 *
	 * @throw Exception if the conversion failed.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::Float32 value, 
		Ceylan::Uint8 precision = Ceylan::DigitOutputPrecision ) ;
		
	
	
	/**
	 * Converts the numerical value to a string, with specified precision,
	 * in fixed (not scientific) notation.
	 *
	 * @param value the value to convert to text.
	 *
	 * @param precision the requested precision, i.e. the number of digits
	 * that is to be displayed after the dot. 
	 *
	 * @throw Exception if the conversion failed.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::Float64 value,
			Ceylan::Uint8 precision = Ceylan::DigitOutputPrecision ) ;

		
		
	/**
	 * Converts the numerical value to a string, with specified precision,
	 * in fixed (not scientific) notation.
	 *
	 * @param value the value to convert to text.
	 *
	 * @param precision the requested precision, i.e. the number of digits
	 * that is to be displayed after the dot. 
	 *
	 * @throw Exception if the conversion failed.
	 *
	 * @see CeylanTypes.h to understand why Ceylan::Float80 is not defined
	 * anymore.
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::LongFloat value, 
			Ceylan::Uint8 precision = Ceylan::DigitOutputPrecision ) ;
		

	
	/**
	 * Converts a verbosity level into a string.
	 *
	 * @see Displayable
	 *
	 */
    CEYLAN_DLL std::string toString( Ceylan::VerbosityLevels level ) ;
	


	/**
	 * Converts a list of integers into a string, separated by a dash.
	 *
	 * @example Returns "33 - 54 - 1".
	 *
	 * @see Displayable
	 *
	 */
    CEYLAN_DLL std::string toString( 
		const std::list<Ceylan::Uint32> & intList ) ;
	
	
	
	/**
	 * Converts a list of pointers into a string, separated by a dash.
	 *
	 * @example Returns "0x95dd268 - 0x5dd22333".
	 *
	 * @see Displayable
	 *
	 */
    CEYLAN_DLL std::string toString( 
		const std::list<const void *> & pointerList ) ;
	


	/**
	 * Converts the numerical value to a string.
	 *
	 * Alternative to the toString method, so that the parameter is always
	 * interpreted as a numerical value, and not as a character, as it
	 * would happen if toString was used.
	 *
	 * @example: a value of zero would display as 0, not as a non printable
	 * character.
	 *
	 */
	CEYLAN_DLL std::string toNumericalString( Uint8 number ) ;
	
	
	
	/**
	 * Constructs a string from a single char.
	 *
	 * @example converts 'a' into "a".
	 */
	CEYLAN_DLL std::string toString( char character ) ;
	
	
	
	/**
	 * This section focuses on conversion from standard strings to various 
	 * numerical types.
	 *
	 */


    /**
	 * Converts, if possible, a string containing an integer number into
	 * that number.
	 *
	 * @throw Ceylan::Exception if the conversion failed.
	 *
	 */
    CEYLAN_DLL int stringToUnsignedLong( const std::string & numericalString ) ;
		
		
    /// Converts, if possible, a string containing a pointer into that pointer.
    CEYLAN_DLL void * stringToAddress( const std::string & addressString ) ;

	
}



#endif // CEYLAN_OPERATORS_H_

