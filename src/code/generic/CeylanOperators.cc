#include "CeylanOperators.h"

#include "CeylanStringUtils.h"      // for reverse


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"         // for CEYLAN_DEBUG
#endif // CEYLAN_USES_CONFIG_H


#include <sstream>                  // for ostringstream
using std::ostringstream ;
using std::istringstream ;




using std::string ;


const Ceylan::Uint16 Ceylan::DigitOutputPrecision = 40 ;

const std::string Ceylan::HexDigits = "0123456789ABCDEF" ;



/**
 * For all these operators, using an internal static string instead of a string
 * would be faster, but the code would then not be reentrant : a mutex should be
 * added in this case.
 *
 * @see MutexHolder.
 *
 */


string operator + ( const string & s, Ceylan::Sint8 i ) 
	throw( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;
	
	/*
	 * 'Ceylan::Sint16' chosen so that the stream operator does not take
	 * the parameter as a character but as a numerical value.
	 *
	 */
    oss << static_cast<Ceylan::Sint16>( i ) ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + Ceylan::Sint8 -> string." ) ;
	}

#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::Sint8 i, const string & s ) 
	throw( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

	/*
	 * 'Ceylan::Sint16' chosen so that the stream operator does not take
	 * the parameter as a character but as a numerical value.
	 *
	 */
    oss << static_cast<Ceylan::Sint16>( i ) ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"Ceylan::Sint8 + const string & -> string." ) ;
	}

#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, Ceylan::Uint8 i ) 
	throw( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

	/*
	 * 'Ceylan::Uint16' chosen so that the stream operator does not take
	 * the parameter as a character but as a numerical value.
	 *
	 */
    oss << static_cast<Ceylan::Uint16>( i ) ;
    res = s + oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"const string & + Ceylan::Uint8 -> string." ) ;

	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::Uint8 i, const string & s ) 
	throw( Ceylan::Exception )
{

    ostringstream oss ;
    string res ;

	/*
	 * 'Ceylan::Uint16' chosen so that the stream operator does not take
	 * the parameter as a character but as a numerical value.
	 *
	 */
	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << static_cast<Ceylan::Uint16>( i ) ;
    res = oss.str() + s ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in Ceylan operator "
			"Ceylan::Uint8 + const string & +-> string." ) ;

	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}




string operator + ( const string & s, Ceylan::Sint16 i ) 
	throw( Ceylan::Exception )
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
			"const string & + Ceylan::Sint16 -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::Sint16 i, const string & s ) 
	throw( Ceylan::Exception )
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
			"Ceylan::Sint16 + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, Ceylan::Uint16 i ) 
	throw( Ceylan::Exception )
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
			"const string & + Ceylan::Uint16 -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::Uint16 i, const string & s ) 
	throw( Ceylan::Exception )
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
			"Ceylan::Uint16 + const string & -> string." ) ;

	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}




string operator + ( const string & s, Ceylan::Sint32 i ) 
	throw( Ceylan::Exception )
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
			"const string & + Ceylan::Sint32 -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::Sint32 i, const string & s ) 
	throw( Ceylan::Exception )
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
			"Ceylan::Sint32 + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, Ceylan::Uint32 i ) 
	throw( Ceylan::Exception )
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
			"const string & + Ceylan::Uint32  -> string." ) ;
	}

#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::Uint32 i, const string & s ) 
	throw( Ceylan::Exception )
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
			"Ceylan::Uint32 + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}



/*
 * No serialization of Uint64 and Sint64 for the moment.
 
 
string operator + ( const std::string & s, Ceylan::Sint64 i ) 
	throw( Ceylan::Exception )
{

}	
	
string operator + ( Ceylan::Sint64 i, const std::string & s ) 
	throw( Ceylan::Exception )
{

}	
	


string operator + ( const std::string & s, Ceylan::Uint64 i ) 
	throw( Ceylan::Exception )
{

}	
	
	
string operator + ( Ceylan::Uint64 i, const std::string & s ) 
	throw( Ceylan::Exception )
{

}	
	
 
 *
 */




string operator + ( const string & s, Ceylan::SignedLongInteger i ) 
	throw( Ceylan::Exception )
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
			"const string & + Ceylan::SignedLongInteger -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::SignedLongInteger i, const string & s ) 
	throw( Ceylan::Exception )
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
			"Ceylan::SignedLongInteger + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, Ceylan::UnsignedLongInteger i ) 
	throw( Ceylan::Exception )
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
			"const string & + Ceylan::UnsignedLongInteger -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::UnsignedLongInteger i, const string & s ) 
	throw( Ceylan::Exception )
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
			"Ceylan::UnsignedLongInteger + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}





string operator + ( const string & s, Ceylan::Float32 i ) 
	throw( Ceylan::Exception )
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
			"const string & + Ceylan::Float32 -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::Float32 i, const string & s ) 
	throw( Ceylan::Exception )
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
			"Ceylan::Float32 + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}



string operator + ( const string & s, Ceylan::Float64 i ) 
	throw( Ceylan::Exception )
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
			"const string & + Ceylan::Float64 -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::Float64 i, const string & s ) 
	throw( Ceylan::Exception )
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
			"Ceylan::Float64 + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}



/*
 * See CeylanTypes.h to understand why Ceylan::Float80 is not defined anymore.
 *
 */

string operator + ( const string & s, Ceylan::LongFloat i ) 
	throw( Ceylan::Exception )
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
			"const string & + Ceylan::LongFloat -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( Ceylan::LongFloat i, const string & s ) 
	throw( Ceylan::Exception )
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
			"Ceylan::LongFloat + const string & -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string operator + ( const string & s, const void * p ) 
	throw( Ceylan::Exception )
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
	throw( Ceylan::Exception )
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






// In namespace Ceylan, conversions to string :



string Ceylan::toString( const void * pointer ) throw( Ceylan::Exception )
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


string Ceylan::toString( bool value ) throw()
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


string Ceylan::toString( Ceylan::Sint8 value, bool bitField ) 
	throw( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( 
			static_cast<UnsignedLongInteger>( value ), true ) ;

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
			"Ceylan::toString : Ceylan::Sint8 -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( Ceylan::Uint8 value, bool bitField ) 
	throw( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( 
			static_cast<UnsignedLongInteger>( value ), true ) ;

    string res ;
	
    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : Ceylan::Uint8 -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( Ceylan::Sint16 value, bool bitField ) 
	throw( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( 
			static_cast<UnsignedLongInteger>( value ), true ) ;

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : Ceylan::Sint16 -> string." ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( Ceylan::Uint16 value, bool bitField ) 
	throw( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( 
			static_cast<UnsignedLongInteger>( value ), true ) ;

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : Ceylan::Uint16 -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( Ceylan::Sint32 value, bool bitField ) 
	throw( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( 
			static_cast<UnsignedLongInteger>( value ), true ) ;

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : Ceylan::Sint32 -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( Ceylan::Uint32 value, bool bitField ) 
	throw( Ceylan::Exception )
{

	if ( bitField )
		return Ceylan::toString( 
			static_cast<UnsignedLongInteger>( value ), true ) ;
		
    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;

    oss << value ;
    res = oss.str() ;

#if CEYLAN_DEBUG

    if ( oss.fail() )
	{
        throw Ceylan::Exception( "Conversion error in method "
			"Ceylan::toString : Ceylan::Uint32 -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( UnsignedLongInteger value, bool bitField ) 
	throw( Ceylan::Exception )
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
			"Ceylan::toString : UnsignedLongInteger -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( SignedLongInteger value, bool bitField ) 
	throw( Ceylan::Exception )
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
			"Ceylan::toString : SignedLongInteger -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toHexString( Ceylan::UnsignedLongInteger value, bool prefix, 
	Ceylan::Uint8 minDigits ) throw()
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
		res += HexDigits[ value % 16 ] ;
		value /= 16 ;			
	}
			
	while ( res.size() < minDigits )
		res += "0" ;
				
	return /* ( inverted ? string( "-" ) : string( "" ) ) + */ 
		( prefix ? "0x" : "" ) + Ceylan::reverse( res ) ;

}
 

string Ceylan::toString( Ceylan::Float32 value, Ceylan::Uint8 precision ) 
	throw( Ceylan::Exception )
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
			"Ceylan::toString : Ceylan::Float32 -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( Ceylan::Float64 value, Ceylan::Uint8 precision ) 
	throw( Ceylan::Exception )
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
			"Ceylan::toString : Ceylan::Float64 -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}



/*
 * See CeylanTypes.h to understand why Ceylan::Float80 is not defined anymore.
 *
 */
 
string Ceylan::toString( Ceylan::LongFloat value, Ceylan::Uint8 precision ) 
	throw( Ceylan::Exception )
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
			"Ceylan::toString : Ceylan::LongFloat -> string."
        ) ;
	}
	
#endif // CEYLAN_DEBUG

    return ( res ) ;

}


string Ceylan::toString( Ceylan::VerbosityLevels level ) throw( Exception )
{

	switch( level )
	{
	
		case low:
			return "low" ;
			break ;
				
		case medium:
			return "medium" ;
			break ;
				
		case high:
			return "high" ;
			break ;
				
		default:
			throw Ceylan::Exception( "Conversion error in method "
				"Ceylan::toString : Ceylan::VerbosityLevels -> string : "
				"unknown verbosity level." ) ;
			break ;
				
	}
	
}


string Ceylan::toNumericalString( Uint8 number ) throw( Ceylan::Exception )
{

    ostringstream oss ;

	oss.precision( Ceylan::DigitOutputPrecision ) ;

    string res ;
	
    oss << static_cast<Ceylan::Uint16>( number ) ;
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


Ceylan::UnsignedLongInteger Ceylan::stringToUnsignedLong( 
	const string & numericalString ) throw( Exception )
{
	
	UnsignedLongInteger result ;
	
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


void * Ceylan::stringToAddress( const string & addressString ) 
	throw( Exception )
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

