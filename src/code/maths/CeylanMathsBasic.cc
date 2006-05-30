#include "CeylanMathsBasic.h"


#include "CeylanOperators.h"   // for string + operator

#include <cstdlib>   // for abs etc.
#include <cmath>     // for fabsf etc.



using std::string ;

using namespace Ceylan::Maths ;



MathsException::MathsException( const std::string & message ) throw()
	: Ceylan::Exception( "Maths exception : " + message )
{

}

	
MathsException::~MathsException() throw()
{

}


bool Ceylan::Maths::IsNull( float x, long double epsilon ) throw()
{
	return Abs( x ) < epsilon ;
}


bool Ceylan::Maths::IsNull( double x, long double epsilon ) throw()
{
	return Abs( x ) < epsilon ;
}


bool Ceylan::Maths::IsNull( long double x, long double epsilon ) throw()
{
	return Abs( x ) < epsilon ;
}



bool Ceylan::Maths::AreEqual( float x, float y, 
	long double epsilon ) throw()
{
	return Abs( x - y ) < epsilon ;
}


bool Ceylan::Maths::AreEqual( double x, double y, 
	long double epsilon ) throw()
{
	return Abs( x - y  ) < epsilon ;
}


bool Ceylan::Maths::AreEqual( long double x, long double y, 
	long double epsilon ) throw()
{
	return Abs( x - y  ) < epsilon ;
}



float Ceylan::Maths::Floor( float x ) throw()
{
	return ::floorf( x ) ;
}


double Ceylan::Maths::Floor( double x ) throw()
{
	return ::floor( x ) ;
}


long double Ceylan::Maths::Floor( long double x ) throw()
{
	return ::floorl( x ) ;
}



float Ceylan::Maths::Ceil( float x ) throw()
{
	return ::ceilf( x ) ;
}


double Ceylan::Maths::Ceil( double x ) throw()
{
	return ::ceil( x ) ;
}


long double Ceylan::Maths::Ceil( long double x ) throw()
{
	return ::ceill( x ) ;
}



// Round section.


float Ceylan::Maths::Round( float x ) throw()
{
	return ::roundf( x ) ;
}


float Ceylan::Maths::Round( float x, Ceylan::Uint8 precision ) throw()
{
	
	double offset = ::pow( 10, precision ) ;
	
	return Round( offset * x ) / offset ;
}



double Ceylan::Maths::Round( double x ) throw()
{
	return ::round( x ) ;
}


double Ceylan::Maths::Round( double x, Ceylan::Uint8 precision ) throw()
{
	
	double offset = ::pow( 10, precision ) ;
	
	return Round( offset * x ) / offset ;
}



long double Ceylan::Maths::Round( long double x ) throw()
{
	return ::roundl( x ) ;
}


long double Ceylan::Maths::Round( long double x, Ceylan::Uint8 precision ) throw()
{
	
	long double offset = ::powl( 10, precision ) ;
	
	return Round( offset * x ) / offset ;
}



char Ceylan::Maths::Abs( char x ) throw()
{
	return ( x > 0 ) ? x : -x ;
}


short Ceylan::Maths::Abs( short x ) throw()
{
	return ( x > 0 ) ? x : -x ;
}
	
		
int Ceylan::Maths::Abs( int x ) throw()
{
	return ::abs( x ) ;
}
		
		
long Ceylan::Maths::Abs( long x ) throw()
{
	return ::labs( x ) ;
}


/* 
 * @note Disabled since ISO C++ does not support `long long'.

long long int Ceylan::Maths::Abs( long long int x ) throw()
{
	return ::llabs( x ) ;
}

 */
 

float Ceylan::Maths::Abs( float x ) throw()
{
	return ::fabsf( x ) ;
}


double Ceylan::Maths::Abs( double x ) throw()
{
	return ::fabs( x ) ;
}


long double Ceylan::Maths::Abs( long double x ) throw() 
{
	return ::fabsl( x ) ;
}




char Ceylan::Maths::Min( char x, char y ) throw()
{
	return ( x < y ) ? x : y ;
}


unsigned char Ceylan::Maths::Min( unsigned char x, 
	unsigned char y ) throw()
{
	return ( x < y ) ? x : y ;
}


short Ceylan::Maths::Min( short x, short y ) throw()
{
	return ( x < y ) ? x : y ;
}


unsigned short Ceylan::Maths::Min( unsigned short x, 
	unsigned short y ) throw()
{
	return ( x < y ) ? x : y ;
}



int Ceylan::Maths::Min( int x, int y ) throw()
{
	return ( x < y ) ? x : y ;
}


unsigned int Ceylan::Maths::Min( unsigned int x, 
	unsigned int y ) throw()
{
	return ( x < y ) ? x : y ;
}



long Ceylan::Maths::Min( long x, long y ) throw()
{
	return ( x < y ) ? x : y ;
}


unsigned long Ceylan::Maths::Min( unsigned long x, 
	unsigned long y ) throw()
{
	return ( x < y ) ? x : y ;
}


/* 
 * @note Disabled since ISO C++ does not support `long long'.


long long int Ceylan::Maths::Min( long long int x, long long int y ) throw()
{
	return ( x < y ) ? x : y ;
}

 */

float Ceylan::Maths::Min( float x, float y ) throw()
{
	return ( x < y ) ? x : y ;
}

double Ceylan::Maths::Min( double x, double y ) throw()
{
	return ( x < y ) ? x : y ;
}


long double Ceylan::Maths::Min( long double x, long double y ) throw()
{
	return ( x < y ) ? x : y ;
}




char Ceylan::Maths::Max( char x, char y ) throw()
{
	return ( x > y ) ? x : y ;
}


unsigned char Ceylan::Maths::Max( unsigned char x, unsigned char y ) throw()
{
	return ( x > y ) ? x : y ;
}


short Ceylan::Maths::Max( short x, short y ) throw()
{
	return ( x > y ) ? x : y ;
}


unsigned short Ceylan::Maths::Max( unsigned short x, unsigned short y ) throw()
{
	return ( x > y ) ? x : y ;
}



int Ceylan::Maths::Max( int x, int y ) throw()
{
	return ( x > y ) ? x : y ;
}


unsigned int Ceylan::Maths::Max( unsigned int x, unsigned int y ) throw()
{
	return ( x > y ) ? x : y ;
}


long Ceylan::Maths::Max( long x, long y ) throw()
{
	return ( x > y ) ? x : y ;
}


unsigned long Ceylan::Maths::Max( unsigned long x, unsigned long y ) throw()
{
	return ( x > y ) ? x : y ;
}


/* 
 * @note Disabled since ISO C++ does not support `long long'.

long long int Ceylan::Maths::Max( long long int x, long long int y ) throw()
{
	return ( x > y ) ? x : y ;
}

 */
 

float Ceylan::Maths::Max( float x, float y ) throw()
{
	return ( x > y ) ? x : y ;
}

double Ceylan::Maths::Max( double x, double y ) throw()
{
	return ( x > y ) ? x : y ;
}


long double Ceylan::Maths::Max( long double x, long double y ) throw()
{
	return ( x > y ) ? x : y ;
}



float Ceylan::Maths::Exp( float x ) throw() 
{
	return ::expf( x ) ;
}


double Ceylan::Maths::Exp( double x ) throw()
{
	return ::exp( x ) ;
}


long double Ceylan::Maths::Exp( long double x ) throw()
{
	return ::expl( x ) ;
}


float Ceylan::Maths::Pow( float x, float y ) throw() 
{
	return ::powf( x, y ) ;
}


double Ceylan::Maths::Pow( double x, double y ) throw() 
{
	return ::pow( x, y ) ;
}


long double Ceylan::Maths::Pow( long double x, long double y ) throw() 
{
	return ::powl( x, y ) ;
}



float Ceylan::Maths::Pow2( float x ) throw() 
{
	return x * x ;
}


double Ceylan::Maths::Pow2( double x ) throw() 
{
	return x * x ;
}


long double Ceylan::Maths::Pow2( long double x ) throw() 
{
	return x * x ;
}



float Ceylan::Maths::Sqrt( float x ) throw( MathsException )
{
	if ( x < 0 )
		throw MathsException( 
			"Sqrt( float x ) : parameter is negative ("
			+ Ceylan::toString( x ) + ")." ) ; 
	return ::sqrtf( x ) ;		
}


double Ceylan::Maths::Sqrt( double x ) throw( MathsException )
{
	if ( x < 0 )
		throw MathsException( 
			"Sqrt( double x ) : parameter is negative ("
			+ Ceylan::toString( x ) + ")." ) ; 
	return ::sqrt( x ) ;		
}


long double Ceylan::Maths::Sqrt( long double x ) throw( MathsException )
{
	if ( x < 0 )
		throw MathsException( 
			"Sqrt( long double x ) : parameter is negative ("
			+ Ceylan::toString( x ) + ")." ) ; 
	return ::sqrtl( x ) ;		
}



float Ceylan::Maths::Cos( float x ) throw()
{
	return ::cosf( x ) ;
}


double Ceylan::Maths::Cos( double x ) throw()
{
	return ::cos( x ) ;
}


long double Ceylan::Maths::Cos( long double x ) throw() 
{
	return ::cosl( x ) ;
}



float Ceylan::Maths::Sin( float x ) throw()
{
	return ::sinf( x ) ;
}



double Ceylan::Maths::Sin( double x ) throw()
{
	return ::sin( x ) ;
}


long double Ceylan::Maths::Sin( long double x ) throw() 
{
	return ::sinl( x ) ;
}



AngleInRadians Ceylan::Maths::DegreeToRadian( 
	AngleInDegrees angleInDegrees ) throw()
{
	return ( angleInDegrees * Pi / 180.0 ) ;
}


Ceylan::Uint16 Ceylan::Maths::NextPowerOfTwo( Ceylan::Uint16 value ) throw()	
{

	// No overflow checking.
	
	Ceylan::Uint16 result = 1 ;
	
	// Could have been : result <<= 1
	while ( result < value )
		result *= 2 ;
	
	return result ;		
}


bool Ceylan::Maths::IsAPowerOfTwo( Ceylan::Uint16 value ) throw()
{
	return ( value == NextPowerOfTwo( value ) ) ;
}



Ceylan::Maths::Functor::Functor() throw()
{

}


Ceylan::Maths::Functor::~Functor() throw()
{

}


const string Ceylan::Maths::Functor::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{
	return "Basic functor" ;
}



Ceylan::Maths::IntToIntFunctor::IntToIntFunctor( int creationParameter ) 
		throw()
	: _creationParameter( creationParameter )
{

}


Ceylan::Maths::IntToIntFunctor::~IntToIntFunctor() throw()
{

}


const string Ceylan::Maths::IntToIntFunctor::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{
	return "IntToIntFunctor functor" ;
}


