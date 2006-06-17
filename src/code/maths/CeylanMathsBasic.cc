#include "CeylanMathsBasic.h"


#include "CeylanOperators.h"   // for string + operator

#include <cstdlib>   // for abs, etc.
#include <cmath>     // for fabsf, etc.



using std::string ;

using namespace Ceylan::Maths ;



MathsException::MathsException( const std::string & message ) throw() :
	Ceylan::Exception( "Maths exception : " + message )
{

}

	
MathsException::~MathsException() throw()
{

}




// IsNull section.



bool Ceylan::Maths::IsNull( Ceylan::Float32 x ) throw()
{
	return Abs( x ) < EpsilonFloat32 ;
}


bool Ceylan::Maths::IsNull( Ceylan::Float32 x, Ceylan::Float32 epsilon ) 
	throw()
{
	return Abs( x ) < epsilon ;
}



bool Ceylan::Maths::IsNull( Ceylan::Float64 x ) throw()
{
	return Abs( x ) < EpsilonFloat64 ;
}


bool Ceylan::Maths::IsNull( Ceylan::Float64 x, Ceylan::Float64 epsilon ) throw()
{
	return Abs( x ) < epsilon ;
}



bool Ceylan::Maths::IsNull( Ceylan::LongFloat x ) throw()
{
	return Abs( x ) < EpsilonLongFloat ;
}


bool Ceylan::Maths::IsNull( Ceylan::LongFloat x, Ceylan::LongFloat epsilon )
	throw()
{
	return Abs( x ) < epsilon ;
}



// AreEqual section.


bool Ceylan::Maths::AreEqual( Ceylan::Float32 x, Ceylan::Float32 y ) throw()
{
	return Abs( x - y ) < EpsilonFloat32 ;
}


bool Ceylan::Maths::AreEqual( Ceylan::Float32 x, Ceylan::Float32 y, 
	Ceylan::Float32 epsilon ) throw()
{
	return Abs( x - y ) < epsilon ;
}



bool Ceylan::Maths::AreEqual( Ceylan::Float64 x, Ceylan::Float64 y ) throw()
{
	return Abs( x - y  ) < EpsilonFloat64 ;
}


bool Ceylan::Maths::AreEqual( Ceylan::Float64 x, Ceylan::Float64 y, 
	Ceylan::Float64 epsilon ) throw()
{
	return Abs( x - y  ) < epsilon ;
}



bool Ceylan::Maths::AreEqual( Ceylan::LongFloat x, Ceylan::LongFloat y ) 
	throw()
{
	return Abs( x - y  ) < EpsilonLongFloat ;
}


bool Ceylan::Maths::AreEqual( Ceylan::LongFloat x, Ceylan::LongFloat y, 
	Ceylan::LongFloat epsilon ) throw()
{
	return Abs( x - y  ) < epsilon ;
}




Ceylan::Float32 Ceylan::Maths::Floor( Ceylan::Float32 x ) throw()
{
	return ::floorf( x ) ;
}


Ceylan::Float64 Ceylan::Maths::Floor( Ceylan::Float64 x ) throw()
{
	return ::floor( x ) ;
}


Ceylan::LongFloat Ceylan::Maths::Floor( Ceylan::LongFloat x ) throw()
{
	return ::floorl( x ) ;
}



Ceylan::Float32 Ceylan::Maths::Ceil( Ceylan::Float32 x ) throw()
{
	return ::ceilf( x ) ;
}


Ceylan::Float64 Ceylan::Maths::Ceil( Ceylan::Float64 x ) throw()
{
	return ::ceil( x ) ;
}


Ceylan::LongFloat Ceylan::Maths::Ceil( Ceylan::LongFloat x ) throw()
{
	return ::ceill( x ) ;
}



// Round section.


Ceylan::Float32 Ceylan::Maths::Round( Ceylan::Float32 x ) throw()
{
	return ::roundf( x ) ;
}


Ceylan::Float32 Ceylan::Maths::Round( Ceylan::Float32 x, Ceylan::Uint8 precision ) throw()
{
	
	Ceylan::Float64 offset = ::pow( 10, precision ) ;
	
	return Round( offset * x ) / offset ;
}



Ceylan::Float64 Ceylan::Maths::Round( Ceylan::Float64 x ) throw()
{
	return ::round( x ) ;
}


Ceylan::Float64 Ceylan::Maths::Round( Ceylan::Float64 x, Ceylan::Uint8 precision ) throw()
{
	
	Ceylan::Float64 offset = ::pow( 10, precision ) ;
	
	return Round( offset * x ) / offset ;
}



Ceylan::LongFloat Ceylan::Maths::Round( Ceylan::LongFloat x ) throw()
{
	return ::roundl( x ) ;
}


Ceylan::LongFloat Ceylan::Maths::Round( Ceylan::LongFloat x, Ceylan::Uint8 precision ) throw()
{
	
	Ceylan::LongFloat offset = ::powl( 10, precision ) ;
	
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
 

Ceylan::Float32 Ceylan::Maths::Abs( Ceylan::Float32 x ) throw()
{
	return ::fabsf( x ) ;
}


Ceylan::Float64 Ceylan::Maths::Abs( Ceylan::Float64 x ) throw()
{
	return ::fabs( x ) ;
}


Ceylan::LongFloat Ceylan::Maths::Abs( Ceylan::LongFloat x ) throw() 
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

Ceylan::Float32 Ceylan::Maths::Min( Ceylan::Float32 x, Ceylan::Float32 y ) throw()
{
	return ( x < y ) ? x : y ;
}

Ceylan::Float64 Ceylan::Maths::Min( Ceylan::Float64 x, Ceylan::Float64 y ) throw()
{
	return ( x < y ) ? x : y ;
}


Ceylan::LongFloat Ceylan::Maths::Min( Ceylan::LongFloat x, Ceylan::LongFloat y ) throw()
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
 

Ceylan::Float32 Ceylan::Maths::Max( Ceylan::Float32 x, Ceylan::Float32 y ) throw()
{
	return ( x > y ) ? x : y ;
}

Ceylan::Float64 Ceylan::Maths::Max( Ceylan::Float64 x, Ceylan::Float64 y ) throw()
{
	return ( x > y ) ? x : y ;
}


Ceylan::LongFloat Ceylan::Maths::Max( Ceylan::LongFloat x, Ceylan::LongFloat y ) throw()
{
	return ( x > y ) ? x : y ;
}



Ceylan::Float32 Ceylan::Maths::Exp( Ceylan::Float32 x ) throw() 
{
	return ::expf( x ) ;
}


Ceylan::Float64 Ceylan::Maths::Exp( Ceylan::Float64 x ) throw()
{
	return ::exp( x ) ;
}


Ceylan::LongFloat Ceylan::Maths::Exp( Ceylan::LongFloat x ) throw()
{
	return ::expl( x ) ;
}


Ceylan::Float32 Ceylan::Maths::Pow( Ceylan::Float32 x, Ceylan::Float32 y ) throw() 
{
	return ::powf( x, y ) ;
}


Ceylan::Float64 Ceylan::Maths::Pow( Ceylan::Float64 x, Ceylan::Float64 y ) throw() 
{
	return ::pow( x, y ) ;
}


Ceylan::LongFloat Ceylan::Maths::Pow( Ceylan::LongFloat x, Ceylan::LongFloat y ) throw() 
{
	return ::powl( x, y ) ;
}



Ceylan::Float32 Ceylan::Maths::Pow2( Ceylan::Float32 x ) throw() 
{
	return x * x ;
}


Ceylan::Float64 Ceylan::Maths::Pow2( Ceylan::Float64 x ) throw() 
{
	return x * x ;
}


Ceylan::LongFloat Ceylan::Maths::Pow2( Ceylan::LongFloat x ) throw() 
{
	return x * x ;
}



Ceylan::Float32 Ceylan::Maths::Sqrt( Ceylan::Float32 x ) throw( MathsException )
{
	if ( x < 0 )
		throw MathsException( 
			"Sqrt( Ceylan::Float32 x ) : parameter is negative ("
			+ Ceylan::toString( x ) + ")." ) ; 
	return ::sqrtf( x ) ;		
}


Ceylan::Float64 Ceylan::Maths::Sqrt( Ceylan::Float64 x ) throw( MathsException )
{
	if ( x < 0 )
		throw MathsException( 
			"Sqrt( Ceylan::Float64 x ) : parameter is negative ("
			+ Ceylan::toString( x ) + ")." ) ; 
	return ::sqrt( x ) ;		
}


Ceylan::LongFloat Ceylan::Maths::Sqrt( Ceylan::LongFloat x ) throw( MathsException )
{
	if ( x < 0 )
		throw MathsException( 
			"Sqrt( Ceylan::LongFloat x ) : parameter is negative ("
			+ Ceylan::toString( x ) + ")." ) ; 
	return ::sqrtl( x ) ;		
}



Ceylan::Float32 Ceylan::Maths::Cos( Ceylan::Float32 x ) throw()
{
	return ::cosf( x ) ;
}


Ceylan::Float64 Ceylan::Maths::Cos( Ceylan::Float64 x ) throw()
{
	return ::cos( x ) ;
}


Ceylan::LongFloat Ceylan::Maths::Cos( Ceylan::LongFloat x ) throw() 
{
	return ::cosl( x ) ;
}



Ceylan::Float32 Ceylan::Maths::Sin( Ceylan::Float32 x ) throw()
{
	return ::sinf( x ) ;
}



Ceylan::Float64 Ceylan::Maths::Sin( Ceylan::Float64 x ) throw()
{
	return ::sin( x ) ;
}


Ceylan::LongFloat Ceylan::Maths::Sin( Ceylan::LongFloat x ) throw() 
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


