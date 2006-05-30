#include "CeylanOperators.h"

#include "CeylanStringUtils.h"      // for reverse


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"         // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <sstream>                  // for ostringstream
using std::ostringstream ;
using std::istringstream ;




using std::string ;


/**
 * For all these operators, using an internal static string instead of a string
 * would be faster, but the code would then not be reentrant : a mutex should be
 * added in this case.
 *
 * @see MutexHolder.
 *
 */


string operator + ( const string & s, signed char i ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;
	
    oss << static_cast<short>( i ) ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + signed char -> string." ) ;
	}

#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( signed char i, const string & s ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << static_cast<short>( i ) ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"signed char + const string & -> string." ) ;
	}

#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, unsigned char i ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << static_cast<short>( i ) ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + unsigned char -> string." ) ;

	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( unsigned char i, const string & s ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << static_cast<short>( i ) ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"unsigned char + const string & +-> string." ) ;

	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, signed short i ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + signed short -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( signed short i, const string & s ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"signed short + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, unsigned short i ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + unsigned short -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( unsigned short i, const string & s ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"unsigned short + const string & -> string." ) ;

	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, signed int i ) throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + signed int -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( signed int i, const string & s ) throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"signed int + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, unsigned int i ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + unsigned int  -> string." ) ;
	}

#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( unsigned int i, const string & s ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"unsigned int + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, long i ) throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + long -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( long i, const string & s ) throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"long + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, float i ) throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + float -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( float i, const string & s ) throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"float + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, double i ) throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + double -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( double i, const string & s ) throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << i ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"double + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, const void * p ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << p ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + const void * -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const void * p, const string & s ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << p ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const void * + const string & + -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}



string operator + ( const string & a, const char * b )
{
    return a + string( b ) ;
}


string operator+( const char * a, const string & b )
{
    return string( a ) + b ;
}



string Ceylan::toString( const void * pointer ) throw ( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << pointer ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : const void * -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( bool value ) throw ()
{

    if ( value )
	{
        return "true" ;
	}	
    else
	{
        return "false" ;
	}
	
}


string Ceylan::toString( signed char value, bool bitField ) 
	throw ( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( static_cast<unsigned long>( value ), true ) ;

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;
	
	if ( bitField ) 
		oss << 
    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : signed char -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( unsigned char value, bool bitField ) 
	throw ( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( static_cast<unsigned long>( value ), true ) ;

    string res ;
	
    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : unsigned char -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( signed short value, bool bitField ) 
	throw ( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( static_cast<unsigned long>( value ), true ) ;

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : signed short -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( unsigned short value, bool bitField ) 
	throw ( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( static_cast<unsigned long>( value ), true ) ;

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : unsigned short -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( signed int value, bool bitField ) 
	throw ( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( static_cast<unsigned long>( value ), true ) ;

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : signed int -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( unsigned int value, bool bitField ) 
	throw ( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( static_cast<unsigned long>( value ), true ) ;
		
    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : unsigned int -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( unsigned long value, bool bitField ) 
	throw ( Ceylan::Exception )
{

    string res ;

	if ( bitField )
	{
				
		while ( value )
		{						
			res += Ceylan::toString( value % 2 ) ;
			value /= 2 ;			
		}
			
		
		if ( res.empty() )
			return "0b0" ;
						
		return "0b" + Ceylan::reverse( res ) ;
		
	}

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : unsigned long -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( signed long value, bool bitField ) 
	throw ( Ceylan::Exception )
{

    string res ;

	if ( bitField )
	{
		
		string begin ;
		
		if ( value < 0 )
		{
			value = -value ;
			begin = "-" ;
		}
					
		while ( value )
		{						
			res += Ceylan::toString( value % 2 ) ;
			value /= 2 ;			
		}
			
		
		if ( res.empty() )
			return "0b0" ;
						
		return begin + "0b" + Ceylan::reverse( res ) ;
		
	}

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : signed long -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toHexString( unsigned long value, bool prefix, 
	unsigned short minDigits ) throw()
{

	string res ;
	
	/*

	bool inverted = false ;
	
	if ( value < 0 )
	{
		value = - value ;
		inverted = true ;
	}	

	*/
	
	while ( value )
	{	
		res += hexDigits[ value % 16 ] ;
		value /= 16 ;			
	}
			
	while ( res.size() < minDigits )
		res += "0" ;
				
	return /* ( inverted ? string( "-" ) : string( "" ) ) + */ 
		( prefix ? "0x" : "" ) + Ceylan::reverse( res ) ;

}
 

string Ceylan::toString( float value, Ceylan::Uint8 precision ) 
	throw ( Ceylan::Exception )
{

    ostringstream oss ;

	oss << std::fixed ;
	oss.precision( precision ) ;

    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : float -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( double value, Ceylan::Uint8 precision ) 
	throw ( Ceylan::Exception )
{
    ostringstream oss ;

	oss << std::fixed ;
	oss.precision( precision ) ;
    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : double -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( long double value, Ceylan::Uint8 precision ) 
	throw ( Ceylan::Exception )
{
    ostringstream oss ;

	oss << std::fixed ;
	oss.precision( precision ) ;

    string res ;

    oss << std::fixed << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : long double -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}



string Ceylan::toNumericalString( Uint8 number ) throw ( Ceylan::Exception )
{

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;

    oss << static_cast<unsigned short>( number ) ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toNumericalString : Uint8 -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}

unsigned long int Ceylan::stringToUnsignedLong( const string numericalString )
	throw( Exception )
{
	
	unsigned long int result ;
	
	istringstream iss( numericalString ) ;
    iss >> result ;


    if ( iss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::stringToUnsignedLong, while operating on " 
			+ numericalString + "." ) ;
	}
	
	return result ;

}


void * Ceylan::stringToAddress( const string addressString ) throw( Exception )
{
	
	void * result ;
	
	istringstream iss( addressString ) ;
    iss >> result ;


    if ( iss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::stringToAddress, while operating on " 
			+ addressString + "." ) ;
	}
	
	return result ;

}

