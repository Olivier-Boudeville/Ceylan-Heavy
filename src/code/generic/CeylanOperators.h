#ifndef CEYLAN_OPERATORS_H_
#define CEYLAN_OPERATORS_H_

#include "CeylanException.h"      // for Ceylan::Exception
#include "CeylanTypes.h"          // for Ceylan::Uint8

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
 * to a string in both orders : numerical + string and also string + numerical.
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

std::string operator + ( const std::string & s, signed char i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( signed char i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, unsigned char i ) 
	throw ( Ceylan::Exception ) ;
	
std::string operator + ( unsigned char i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, signed short i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( signed short i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, unsigned short i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( unsigned short i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, signed int i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( signed int i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, unsigned int i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( unsigned int i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, long i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( long i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, float i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( float i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


std::string operator + ( const std::string & s, double i ) 
	throw( Ceylan::Exception ) ;
	
std::string operator + ( double i, const std::string & s ) 
	throw( Ceylan::Exception ) ;


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
	 * Determines the <b>maximum</b> number of digits that shall be output on
	 * string insertion operations to express floating-point values, counting
	 * both the digits before and after the decimal point.
	 *
	 * @note 40 seems to be largely enough, even for long doubles.
	 *
	 */
	const unsigned short DigitOutputPrecision = 40 ;
	
	
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
    std::string toString( signed char value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( unsigned char value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( signed short value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( unsigned short value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( signed int value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value, prefixed by '0b'.
	 *
	 */
    std::string toString( unsigned int value, bool bitField = false ) 
		throw( Exception ) ;
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value prefixed by '0b'.
	 *
	 */
    std::string toString( unsigned long value, bool bitField = false ) 
		throw( Exception ) ;
		
		
	/**
	 * Converts the numerical value to a string.
	 *
	 * @param bitField if true, returns a binary representation of the 
	 * specified value prefixed by '0b'.
	 *
	 */
    std::string toString( signed long value, bool bitField = false ) 
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
	std::string Ceylan::toHexString( unsigned long value, bool prefix = true, 
		unsigned short minDigits = 1 ) throw() ;	
	
	
	
	// Conversions from floating points values to text.
	

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
    std::string toString( float value, 
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
    std::string toString( double value,
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
    std::string toString( long double value, 
			Ceylan::Uint8 precision = Ceylan::DigitOutputPrecision ) 
		throw( Exception ) ;
	
	
	
	/**
	 * Converts the numerical value to a string.
	 *
	 * Alternative to the toString method, so that the parameter is always
	 * interpreted as a numerical value, and not as an unsigned char, as it
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
    unsigned long int stringToUnsignedLong( const std::string numericalString ) 
		throw( Exception ) ;
		
		
    /// Converts, if possible, a string containing a pointer into a pointer.
    void * stringToAddress( const std::string addressString ) 
		throw( Exception ) ;


}


#endif // CEYLAN_OPERATORS_H_

