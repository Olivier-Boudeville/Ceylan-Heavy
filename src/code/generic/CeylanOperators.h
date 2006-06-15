#ifndef CEYLAN_OPERATORS_H_
#define CEYLAN_OPERATORS_H_


#include "CeylanException.h"      // for Ceylan::Exception
#include "CeylanTypes.h"          // for Ceylan::Uint8, etc.

#include <string>



/**
 * This module offers the operators lacking in C++ in order to perform 
 * appending of numerical values to standard strings, and basic string
 * conversion.
 *
 * Various conversions are provided, from numerical to string and the other 
 * way round.
 *
 * Each operator has its counterpart, which allows to add a numerical value 
 * to a string in both orders : 'numerical + string' and also 
 * 'string + numerical'.
 * 
 * Warning : a C++ compiler evaluates '"Four means " + 4' as 'char *' + 'int'
 * so it does pointer arithmetic and returns unexpected bytes : actually, it
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
 */

std::string operator + ( const std::string & s, Ceylan::Sint8 i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Sint8 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, Ceylan::Uint8 i ) 
	throw ( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Uint8 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;



std::string operator + ( const std::string & s, Ceylan::Sint16 i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Sint16 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, Ceylan::Uint16 i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Uint16 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;



std::string operator + ( const std::string & s, Ceylan::Sint32 i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Sint32 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, Ceylan::Uint32 i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Uint32 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


/*
 * No serialization of Uint64 and Sint64 for the moment, since
 * in some cases they are just fake datatypes.
 
std::string operator + ( const std::string & s, Ceylan::Sint64 i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Sint64 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, Ceylan::Uint64 i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Uint64 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;
 
 
 *
 */



std::string operator + ( const std::string & s, Ceylan::SignedLongInteger i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::SignedLongInteger i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, Ceylan::UnsignedLongInteger i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::UnsignedLongInteger i, const std::string & s ) 
	throw( Ceylan::Exception ) ;



std::string operator + ( const std::string & s, Ceylan::Float32 i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Float32 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, Ceylan::Float64 i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::Float64 i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


/*
 * See CeylanTypes.h to understand why Ceylan::Float80 is not defined anymore.
 *
 */
  
std::string operator + ( const std::string & s, Ceylan::LongFloat i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( Ceylan::LongFloat i, const std::string & s ) 
	throw( Ceylan::Exception ) ;



// Pointer size depends on the platform.

std::string operator + ( const std::string & s, const void * p ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( const void * p, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & a, const char * b ) ;

std::string operator + ( const char * a, const std::string & b ) ;




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
	 * @note 40 seems to be largely enough, even for Ceylan::Float64.
	 *
	 */
	const Ceylan::Uint16 DigitOutputPrecision = 40 ;
	
	
    /// Returns a user-friendly representation of a pointer.
    std::string toString( const void * pointer ) throw( Exception ) ;


    /**
	 * Returns a user-friendly textual representation of a bool 
	 * ("true" or "false").
	 *
	 */
    std::string toString( bool value ) throw() ;



	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( Ceylan::Sint8 value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( Ceylan::Uint8 value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( Ceylan::Sint16 value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( Ceylan::Uint16 value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( Ceylan::Sint32 value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( Ceylan::Uint32 value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value prefixed by '0b'.
	 *
	 */
    std::string toString( Ceylan::UnsignedLongInteger value, bool bitField = false ) 
		throw( Exception ) ;
		
		
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value prefixed by '0b'.
	 *
	 */
    std::string toString( Ceylan::SignedLongInteger value, bool bitField = false ) 
		throw( Exception ) ;
		
	
	
	/// The hexadecimal digits.
	const std::string hexDigits = "0123456789ABCDEF" ;
	
	
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
	std::string toHexString( Ceylan::UnsignedLongInteger value, 
		bool prefix = true, Ceylan::Uint8 minDigits = 1 ) throw() ;	
	
	
	
	// Conversions from Ceylan::Float32ing points values to text.
	

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
    std::string toString( Ceylan::Float32 value, 
			Ceylan::Uint8 precision = Ceylan::DigitOutputPrecision ) 
		throw( Exception ) ;
		
	
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
    std::string toString( Ceylan::Float64 value,
			Ceylan::Uint8 precision = Ceylan::DigitOutputPrecision ) 
		throw( Exception ) ;

		
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
    std::string toString( Ceylan::LongFloat value, 
			Ceylan::Uint8 precision = Ceylan::DigitOutputPrecision ) 
		throw( Exception ) ;
		
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * Alternative to the toString method, so that the parameter is always
	 * interpreted as a numerical value, and not as a character, as it
	 * would happen if toString was used.
	 *
	 * @example : a value of zero would display as 0, not as a non printable
	 * character.
	 *
	 */
	std::string toNumericalString( Uint8 number ) throw( Exception ) ;
	
	
	
	/**
	 * This section focuses on conversion from standard strings to various 
	 * numerical types.
	 *
	 */


    /**
	 * Converts, if possible, a string containing an integer number into
	 * that number.
	 *
	 */
    UnsignedLongInteger stringToUnsignedLong( 
		const std::string & numericalString ) throw( Exception ) ;
		
		
    /// Converts, if possible, a string containing a pointer into that pointer.
    void * stringToAddress( const std::string & addressString ) 
		throw( Exception ) ;


}


#endif // CEYLAN_OPERATORS_H_

