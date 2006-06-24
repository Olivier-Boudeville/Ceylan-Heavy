#include "CeylanMathsBasic.h"

#include "CeylanOperators.h"   // for string + operator


#include <cstdlib>             // for abs, etc.
#include <cmath>               // for fabsf, etc.


extern "C"
{

#ifdef CEYLAN_USES_MATH_H
//#include <math.h>              // for cos, ceil, etc. 
#endif // CEYLAN_USES_MATH_H

}


using std::string ;

using namespace Ceylan::Maths ;



MathsException::MathsException( const std::string & message ) throw() :
	Ceylan::Exception( "Maths exception : " + message )
{

}

	
MathsException::~MathsException() throw()
{

}



// Definitions of most common constants.


const Ceylan::LongFloat Ceylan::Maths::E =
	2.7182818284590452353602874713526625L ;
	
const Ceylan::LongFloat Ceylan::Maths::Log2E = 
	1.4426950408889634073599246810018922L ;

const Ceylan::LongFloat Ceylan::Maths::Log10E = 
	0.4342944819032518276511289189166051L ;

const Ceylan::LongFloat Ceylan::Maths::LogE2 = 
	0.6931471805599453094172321214581766L ;
	
const Ceylan::LongFloat Ceylan::Maths::LogE10 = 
	2.3025850929940456840179914546843642L ;


const Ceylan::LongFloat Ceylan::Maths::Pi = 
	3.1415926535897932384626433832795029L ;
	
const Ceylan::LongFloat Ceylan::Maths::Pi_div_2 = 
	1.5707963267948966192313216916397514L ;
	
const Ceylan::LongFloat Ceylan::Maths::Pi_div_4 = 
	0.7853981633974483096156608458198757L ;
	
const Ceylan::LongFloat Ceylan::Maths::One_div_Pi = 
	0.3183098861837906715377675267450287L ;
	
const Ceylan::LongFloat Ceylan::Maths::Two_div_Pi = 
	0.6366197723675813430755350534900574L ;
	
const Ceylan::LongFloat Ceylan::Maths::Two_div_sqrt_Pi	= 
	1.1283791670955125738961589031215452L ;
			
			
const Ceylan::LongFloat Ceylan::Maths::Sqrt_2 = 
	1.4142135623730950488016887242096981L ;
	
const Ceylan::LongFloat Ceylan::Maths::One_div_sqrt_2 = 
	0.7071067811865475244008443621048490L ;
	
	
const Ceylan::LongFloat Ceylan::Maths::EpsilonFloat32   = 1.0e-7 ;			
const Ceylan::LongFloat Ceylan::Maths::EpsilonFloat64   = 1.0e-9 ;
const Ceylan::LongFloat Ceylan::Maths::EpsilonLongFloat = 1.0e-11 ;
const Ceylan::LongFloat Ceylan::Maths::Epsilon          = EpsilonFloat32 ;



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
	return ::floor( x ) ;
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
	return ::ceil( x ) ;
}



// Round section.


Ceylan::Float32 Ceylan::Maths::Round( Ceylan::Float32 x ) throw()
{
	return ::roundf( x ) ;
}


Ceylan::Float32 Ceylan::Maths::Round( Ceylan::Float32 x, 
	Ceylan::Uint8 precision ) throw()
{
	
	Ceylan::Float64 offset = ::pow( 10, precision ) ;
	
	return Round( offset * x ) / offset ;
}



Ceylan::Float64 Ceylan::Maths::Round( Ceylan::Float64 x ) throw()
{
	return ::round( x ) ;
}


Ceylan::Float64 Ceylan::Maths::Round( Ceylan::Float64 x, 
	Ceylan::Uint8 precision ) throw()
{
	
	Ceylan::Float64 offset = ::pow( 10, precision ) ;
	
	return Round( offset * x ) / offset ;
}



Ceylan::LongFloat Ceylan::Maths::Round( Ceylan::LongFloat x ) throw()
{
	return ::round( x ) ;
}


Ceylan::LongFloat Ceylan::Maths::Round( Ceylan::LongFloat x, 
	Ceylan::Uint8 precision ) throw()
{
	
	Ceylan::LongFloat offset = ::pow( 10, precision ) ;
	
	return Round( offset * x ) / offset ;
}



Ceylan::Sint8 Ceylan::Maths::Abs( Ceylan::Sint8 x ) throw()
{
	return ( x > 0 ) ? x : -x ;
}


Ceylan::Sint16 Ceylan::Maths::Abs( Ceylan::Sint16 x ) throw()
{
	return ( x > 0 ) ? x : -x ;
}
	
		
Ceylan::Sint32 Ceylan::Maths::Abs( Ceylan::Sint32 x ) throw()
{
	return ::abs( x ) ;
}
		
		
Ceylan::SignedLongInteger Ceylan::Maths::Abs( Ceylan::SignedLongInteger x )
	throw()
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
	return ::fabs( x ) ;
}




Ceylan::Sint8 Ceylan::Maths::Min( Ceylan::Sint8 x, Ceylan::Sint8 y ) throw()
{
	return ( x < y ) ? x : y ;
}


Ceylan::Uint8 Ceylan::Maths::Min( Ceylan::Uint8 x, 
	Ceylan::Uint8 y ) throw()
{
	return ( x < y ) ? x : y ;
}


Ceylan::Sint16 Ceylan::Maths::Min( Ceylan::Sint16 x, Ceylan::Sint16 y ) throw()
{
	return ( x < y ) ? x : y ;
}


Ceylan::Uint16 Ceylan::Maths::Min( Ceylan::Uint16 x, 
	Ceylan::Uint16 y ) throw()
{
	return ( x < y ) ? x : y ;
}



Ceylan::Sint32 Ceylan::Maths::Min( Ceylan::Sint32 x, Ceylan::Sint32 y ) throw()
{
	return ( x < y ) ? x : y ;
}


Ceylan::Uint32 Ceylan::Maths::Min( Ceylan::Uint32 x, 
	Ceylan::Uint32 y ) throw()
{
	return ( x < y ) ? x : y ;
}



Ceylan::SignedLongInteger Ceylan::Maths::Min( Ceylan::SignedLongInteger x,
	Ceylan::SignedLongInteger y ) throw()
{
	return ( x < y ) ? x : y ;
}


Ceylan::UnsignedLongInteger Ceylan::Maths::Min( Ceylan::UnsignedLongInteger x, 
	Ceylan::UnsignedLongInteger y ) throw()
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
 

Ceylan::Float32 Ceylan::Maths::Min( Ceylan::Float32 x, Ceylan::Float32 y )
	throw()
{
	return ( x < y ) ? x : y ;
}

Ceylan::Float64 Ceylan::Maths::Min( Ceylan::Float64 x, Ceylan::Float64 y )
	throw()
{
	return ( x < y ) ? x : y ;
}


Ceylan::LongFloat Ceylan::Maths::Min( Ceylan::LongFloat x, Ceylan::LongFloat y )
	throw()
{
	return ( x < y ) ? x : y ;
}




Ceylan::Sint8 Ceylan::Maths::Max( Ceylan::Sint8 x, Ceylan::Sint8 y ) throw()
{
	return ( x > y ) ? x : y ;
}


Ceylan::Uint8 Ceylan::Maths::Max( Ceylan::Uint8 x, Ceylan::Uint8 y ) throw()
{
	return ( x > y ) ? x : y ;
}


Ceylan::Sint16 Ceylan::Maths::Max( Ceylan::Sint16 x, Ceylan::Sint16 y ) throw()
{
	return ( x > y ) ? x : y ;
}


Ceylan::Uint16 Ceylan::Maths::Max( Ceylan::Uint16 x, Ceylan::Uint16 y ) throw()
{
	return ( x > y ) ? x : y ;
}



Ceylan::Sint32 Ceylan::Maths::Max( Ceylan::Sint32 x, Ceylan::Sint32 y ) throw()
{
	return ( x > y ) ? x : y ;
}


Ceylan::Uint32 Ceylan::Maths::Max( Ceylan::Uint32 x, Ceylan::Uint32 y ) throw()
{
	return ( x > y ) ? x : y ;
}


Ceylan::SignedLongInteger Ceylan::Maths::Max( Ceylan::SignedLongInteger x,
	Ceylan::SignedLongInteger y ) throw()
{
	return ( x > y ) ? x : y ;
}


Ceylan::UnsignedLongInteger Ceylan::Maths::Max( Ceylan::UnsignedLongInteger x,
	Ceylan::UnsignedLongInteger y ) throw()
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
 

Ceylan::Float32 Ceylan::Maths::Max( Ceylan::Float32 x, Ceylan::Float32 y )
	throw()
{
	return ( x > y ) ? x : y ;
}

Ceylan::Float64 Ceylan::Maths::Max( Ceylan::Float64 x, Ceylan::Float64 y )
	throw()
{
	return ( x > y ) ? x : y ;
}


Ceylan::LongFloat Ceylan::Maths::Max( Ceylan::LongFloat x, Ceylan::LongFloat y )
	throw()
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
	return ::exp( x ) ;
}


Ceylan::Float32 Ceylan::Maths::Pow( Ceylan::Float32 x, Ceylan::Float32 y )
	throw() 
{
	return ::powf( x, y ) ;
}


Ceylan::Float64 Ceylan::Maths::Pow( Ceylan::Float64 x, Ceylan::Float64 y )
	throw() 
{
	return ::pow( x, y ) ;
}


Ceylan::LongFloat Ceylan::Maths::Pow( Ceylan::LongFloat x, Ceylan::LongFloat y )
	throw() 
{
	return ::pow( x, y ) ;
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


Ceylan::LongFloat Ceylan::Maths::Sqrt( Ceylan::LongFloat x ) 
	throw( MathsException )
{
	if ( x < 0 )
		throw MathsException( 
			"Sqrt( Ceylan::LongFloat x ) : parameter is negative ("
			+ Ceylan::toString( x ) + ")." ) ; 
	return ::sqrt( x ) ;		
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
	return ::cos( x ) ;
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
	return ::sin( x ) ;
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



Ceylan::Maths::IntToIntFunctor::IntToIntFunctor( 
		Ceylan::Sint32 creationParameter ) throw() :
	Functor(),	 
	_creationParameter( creationParameter )
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


