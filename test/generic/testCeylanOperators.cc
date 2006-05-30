#include "Ceylan.h"

#include <iostream>

#include <string>
using std::string ;

#include <exception>


using namespace Ceylan ;
using namespace Ceylan::Log ;



/**
 * Test for conversion operators services.
 *
 * @see Operators.
 *
 */
int main( int argc, char * argv[] )
{

	
	LogHolder logger( argc, argv ) ;
	
	
    try
    {
		
		LogPlug::info( "Testing extended string operators." ) ;

        signed char i1   = -11 ;
        unsigned char i2 = 2 ;

        signed short i3   = -4 ;
        unsigned short i4 = 112 ;

        signed int i5   = -34 ;
        unsigned int i6 = 58 ;
        long i7         = -782 ;

        float i8  = -12.342 ;
        double i9 = -1026.23 ;

        const char * i10 = "Ceylan is good" ;

        const void * p1 = static_cast<const void *>( i10 ) ;

        string s1 = string( "signed char i1 = -11 ---> " ) + i1 ;
        LogPlug::info( s1 ) ;
        string s2 = string( "unsigned char i2 = 2 ---> " ) + i2 ;
        LogPlug::info( s2 ) ;

        string s3 = string( "signed short i3 = -4 ---> " ) + i3 ;
        LogPlug::info( s3 ) ;

        string s4 = string( "unsigned short i4 = 112 ---> " ) + i4 ;
        LogPlug::info( s4 ) ;

        string s5 = string( "signed int i5 = -34 ---> " ) + i5 ;
        LogPlug::info( s5 ) ;
        string s6 = string( "unsigned int i6 = 58 ---> " ) + i6 ;
        LogPlug::info( s6 ) ;

        string s7 = string( "long  i7 = -782 ---> " ) + i7 ;
        LogPlug::info( s7 ) ;

        string s8 = string( "float i8 = -12.342 ---> " ) + i8 ;
        LogPlug::info( s8 ) ;

        string s9 = string( "double i9 = -1026.23 ---> " ) + i9 ;
        LogPlug::info( s9 ) ;

        string s10 = string( "char * i10 = Ceylan is good ---> " ) + i10 ;
        LogPlug::info( s10 ) ;

        string s11 = string( "void * p1 ---> " ) + p1 ;
        LogPlug::info( s11 ) ;

        const char * c = " made of a mix of " ;
        string s12 = "This string is" + string( c ) + "all strings" + "." ;
        LogPlug::info( s12 ) ;


        LogPlug::info( "And now, the same thing, but,"
			" er, in reverse order, er, whizzz !\n" ) ;

        string r1 = i1 + string( "---> signed char i1 = -11" ) ;
        LogPlug::info( r1 ) ;

        string r2 = i2 + string( "---> unsigned char i2 = 2" ) ;
        LogPlug::info( r2 ) ;

        string r3 = i3 + string( "---> signed short i3 = -4" ) ;
        LogPlug::info( r3 ) ;

        string r4 = i4 + string( "---> unsigned short i4 = 112" ) ;
        LogPlug::info( r4 ) ;

        string r5 = i5 + string( "---> signed int i5 = -34" ) ;
        LogPlug::info( r5 ) ;
        string r6 = i6 + string( "---> unsigned int i6 = 58" ) ;
        LogPlug::info( r6 ) ;

        string r7 = i7 + string( "---> long  i7 = -782" ) ;
        LogPlug::info( r7 ) ;

        string r8 = i8 + string( "---> float i8 = -12.342" ) ;
        LogPlug::info( r8 ) ;

        string r9 = i9 + string( "---> double i9 = -1026.23" ) ;
        LogPlug::info( r9 ) ;

        string r10 = i10 + string( "---> char * i10 = Ceylan is good" ) ;
        LogPlug::info( r10 ) ;

        string r11 = p1 + string( "---> void * p1" ) ;
        LogPlug::info( r11 ) ;

		Ceylan::Uint8 precision = 3 ;
		
		LogPlug::info( "Testing the display of 1/3 "
			"with a precision of only " 
			+ Ceylan::toNumericalString( precision ) + " digits : " 
			+ Ceylan::toString( 1.0f / 3, precision ) + "." ) ;
			
		LogPlug::info( "Testing the display of 4/3 "
			"with a precision of only " 
			+ Ceylan::toNumericalString( precision ) + " digits : " 
			+ Ceylan::toString( 4.0f / 3, precision ) + "." ) ;
		
		LogPlug::info( "Testing the display of float 75312 "
			"with a precision of only " 
			+ Ceylan::toNumericalString( precision ) + " digits : " 
			+ Ceylan::toString( 75312.0f, precision ) + "." ) ;
			
		unsigned char aChar = 0 ;
	      LogPlug::info( "Testing Ceylan::toNumericalString "
		  	"for eight bit numbers : the number 0 is translated in : '" 
			+ Ceylan::toNumericalString( aChar ) 
			+ "', and not '" + Ceylan::toString( aChar ) + "'." ) ;
			

        int * p = new int[ 3 ] ;

        LogPlug::info( "Finally, testing output for a pointer : " 
			+ Ceylan::toString( p ) ) ;
		
        LogPlug::info( "and for a boolean, which may be : " 
			+ Ceylan::toString( true )
			+ " (true) or " 
			+ Ceylan::toString( false )
			+ " (false)." ) ;

        LogPlug::info( "Yep, this kind of programming is "
			"a dull task, man." ) ;

		LogPlug::info( "Now, testing bit fields display." ) ;
		
		
		for ( Ceylan::Uint8 count = 0; count < 21 ; count++ )
		{
		
			// Converted to Ceylan::Uint16 not to be taken for a char :
			LogPlug::info( "The number " 
				+ Ceylan::toString( static_cast<Ceylan::Uint16>( count ) ) 
				+ " displays, as a bitfield, as "
				+ Ceylan::toString( count, /* bitfield */ true ) ) ;
	
		}
		
		
		for ( Ceylan::Uint8 count = 0; count < 21 ; count++ )
		{
		
			// Converted to Ceylan::Uint16 not to be taken for a char :
			LogPlug::info( "The number " 
				+ Ceylan::toString( static_cast<Ceylan::Uint16>( count ) ) 
				+ " displays, in hexadecimal, as "
				+ Ceylan::toHexString( count ) ) ;	
							
		}
		
		// Actually this number would be -16777216 with signed long :
		Ceylan::UnsignedLongInteger bigNumber = 0xFF000000 ;
		
		LogPlug::info( "The number " 
			+ Ceylan::toString( 
				static_cast<Ceylan::SignedLongInteger>( bigNumber ) ) 
			+ " displays, in hexadecimal, as "
			+ Ceylan::toHexString( bigNumber ) ) ;				
		
        LogPlug::info( "End of extended string operators test." ) ;

		delete p ;

    }
   
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught : " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}
