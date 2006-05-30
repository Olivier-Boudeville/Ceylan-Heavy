#ifndef CEYLAN_MATH_BASIC_H_
#define CEYLAN_MATH_BASIC_H_


#include "CeylanTextDisplayable.h"  // for TextDisplayable
#include "CeylanException.h"        // for Exception
#include "CeylanTypes.h"            // for Float32, etc.


#include <string>



namespace Ceylan
{


	namespace Maths
	{
	
	
		/*
		 * Numerical constants commonly used for mathematic computations.
		 *
		 * Instead of standard variables (ex : M_E), their GNU counterparts
		 * (ex : M_El) are used, since their accuracy (precision) is better :
		 * they are adequate for computation using `long double'.
		 * 
		 * Since depending on a GNU set of constants would not be portable
		 * enough, their values are explicitly set here, one time for all.
		 *
		 * @note Curiously, the overall accuracy is not that great, for 
		 * example with E : 
		 * 	- original  : 2.7182818284590452353602874713526625
		 *  - displayed : 2.7182818284590450908
		 * even with a precision set to 40 digits.
		 *
		 * From that comparison, we defined epsilon.
		 *
		 * @note For floating point numbers, tests for equality should be
		 * performed thanks to the AreEqual method, since limited precision
		 * generates tiny numerical errors.
		 *
		 * @see GNU extension of math.h 
		 *
		 * @todo Maybe add signal handler or similar device to catch 
		 * floating point exceptions.
		 *
		 */

		/// Generic signed integer data.
		typedef Ceylan::Sint32 IntegerData ;
		
		
		/**
		 * Real is the floating-point coordinate used in Ceylan's 
		 * linear services.
		 *
		 * @note Float32 corresponds also to GLfloat and GLclampf.
		 *
		 */
		typedef Ceylan::Float32 Real ;
		
		
		/// Angles, expressed in degrees.
		typedef Ceylan::Float32 AngleInDegrees ;
		
		/// Angles, expressed in radians.
		typedef Ceylan::Float32 AngleInRadians ;
		
		
		/// Percentage values, which should range from 0 to 100, both included.
		typedef Ceylan::Uint8 Percentage ;
		
		
		/// Exception for Maths-related issues.
		class MathsException : public Ceylan::Exception
		{
		
			public:
			
				explicit MathsException( const std::string & message ) throw() ;
				virtual ~MathsException() throw() ;
		
		} ;
		
		
		
		// Section related to the e constant.
		
		
		/**
		 * The number e. 
		 * (used to be the M_E maths.h constant).
		 *
		 */
		const long double E = 2.7182818284590452353602874713526625L ;


		/**
		 * The number log_2(e).
		 * (used to be the M_LOG2E maths.h constant).
		 *
		 */
		const long double Log2E = 1.4426950408889634073599246810018922L ;
	
	
		/**
		 * The number log_10(e). 
		 * (used to be the M_LOG10E maths.h constant).
		 *
		 */
		const long double Log10E = 0.4342944819032518276511289189166051L ;
	
	
		/**
		 * The number log_e(2) = ln(2). 
		 * (used to be the M_LN2 maths.h constant).
		 *
		 */
		const long double LogE2 = 0.6931471805599453094172321214581766L ;
	
	
		/**
		 * The number log_e(10) = ln(10) 
		 * (used to be the M_LN10 maths.h constant).
		 *
		 */
		const long double LogE10 = 2.3025850929940456840179914546843642L ;



		// Section related to the Pi constant.
		
	
		/** 
		 * The number pi.
		 * (used to be the M_PI maths.h constant).
		 *
		 */
		const long double Pi = 3.1415926535897932384626433832795029L ;
	
	
		/** 
		 * The number pi/2. 
		 * (used to be the M_PI_2 maths.h constant).
		 *
		 */
		const long double Pi_div_2 = 1.5707963267948966192313216916397514L ;
	
	
		/** 
		 * The number pi/4. 
		 * (used to be the M_PI_4 maths.h constant).
		 *
		 */
		const long double Pi_div_4 = 0.7853981633974483096156608458198757L ;
	
	
		/** 
		 * The number 1/pi. 
		 * (used to be the M_1_PI maths.h constant).
		 *
		 */
		const long double One_div_Pi = 0.3183098861837906715377675267450287L ;
	
	
		/** 
		 * The number 2/pi.
		 * (used to be the M_2_PI maths.h constant).
		 *
		 */
		const long double Two_div_Pi = 0.6366197723675813430755350534900574L ;
	
	
		/** 
		 * The number 2/sqrt(pi). 
		 * (used to be the M_2_SQRTPI maths.h constant).
		 *
		 */
		const long double Two_div_sqrt_Pi 
			= 1.1283791670955125738961589031215452L ;
	
	
	
	
		// Section related to the sqrt(2) constant.
		
	
		/** 
		 * The number sqrt(2). 
		 * (used to be the M_SQRT2 maths.h constant).
		 *
		 */
		const long double Sqrt_2 = 1.4142135623730950488016887242096981L ;
	
	
		/** 
		 * The number 1/sqrt(2). 
		 * (used to be the M_SQRT1_2 maths.h constant).
		 *
		 */
		const long double One_div_sqrt_2 
			= 0.7071067811865475244008443621048490L ;
	
	
		/**
		 * A very small number, used for floating point comparisons :
		 * two numbers x and y are deemed equal if | x - y | < Epsilon
		 *
		 */
		const long double EpsilonFloat = 1.0e-7 ;


		/**
		 * A very small number, used for floating point comparisons : 
		 * two numbers x and y are deemed equal if | x - y | < Epsilon
		 *
		 */
		const long double EpsilonDouble = 1.0e-9 ;


		/**
		 * A very small number, used for floating point comparisons : 
		 * two numbers x and y are deemed equal if | x - y | < Epsilon
		 *
		 */
		const long double EpsilonLongDouble = 1.0e-11 ;


		/**
		 * A very small number, used for floating point comparisons :
		 * two numbers x and y are deemed equal if | x - y | < Epsilon
		 *
		 */
		const long double Epsilon = EpsilonFloat ;

		
		
		// Templates could have been more widely used.


		/** 
		 * Tells whether x is null as regard to floating point numbers :
		 * | x | < epsilon.
		 *
		 */
		bool IsNull( float x, 
			long double epsilon = EpsilonFloat ) throw() ;
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers :
		 * | x | < epsilon.
		 *
		 */
		bool IsNull( double x, 
			long double epsilon = EpsilonDouble ) throw() ;
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers :
		 * | x | < epsilon.
		 *
		 */
		bool IsNull( long double x,
			long double epsilon = EpsilonLongDouble ) throw() ;
		
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers :
		 * | x - y | < epsilon.
		 *
		 */
		bool AreEqual( float x, float y, 
			long double epsilon = EpsilonFloat ) throw() ;
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers :
		 * | x - y | < epsilon.
		 *
		 */
		bool AreEqual( double x, double y, 
			long double epsilon = EpsilonDouble ) throw() ;
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers : 
		 * | x - y | < epsilon.
		 *
		 */
		bool AreEqual( long double x, long double y, 
			long double epsilon = EpsilonLongDouble ) throw() ;
		
	
	
		/**
		 * Relative comparison.
		 *
		 * @note Not that useful, since compilers usually complain about
		 * an ambiguous call of overloaded `Abs(...)', which should be 
		 * templated too.
		 *
		 * @param x the first value to compare.
		 *
		 * @param y the second value to compare.
		 *
		 * @param epsilon the double of the criterion of relative equality.
		 *
		 */
		template <typename T> bool AreRelativelyEqual( T x, T y, 
			long double epsilon = EpsilonFloat ) throw()
		{
		
		
			/*
			 * One goal, avoid division by zero on final formula :
			 * x + y == 0 iff x == -y, in this case :  
			 *   - if x == 0 then y == 0 and they are deemed equal
			 *   - otherwise x != 0 implies x != y (since x == -y),
			 * hence they are not deemed equal.
			 *
			 */
			if ( IsNull( x + y ) )
				return IsNull( x ) ;
				
				
			/*
			 * Here x + y != 0.
			 * 
			 * Relative comparison : compare their difference (|x-y|) 
			 * to their mean value ((x+y)/2), here using Epsilon/2 as 
			 * equality criterion.
			 *
			 */
			return ( Abs( x - y ) / ( x + y ) < epsilon ) ;
			
		}
		
		
		
		/*
		 * Floor, Ceil and Round section.
		 *
		 * These functions are designed to help computations.
		 *
		 * In order to display floating-point values with a limited number
		 * of digits, one should prefer toString methods, whose second
		 * parameter allows to select the precision.
		 *
		 * @see Ceylan::toString
		 *
		 */
		
		
		/**
		 * Returns largest integral value not greater than argument.
		 *
		 * @param x the floating-point value to floor.
		 *
		 * @return A floating point value containing an integer.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @example Floor(-2)   = -2
		 * @example Floor(-1.5) = -2
		 * @example Floor(-0.8) = -1
		 * @example Floor(0.5)  =  0
		 * @example Floor(1.6)  =  1
		 *
		 */
		float Floor( float x ) throw() ;

		
		/**
		 * Returns largest integral value not greater than argument.
		 *
		 * @param x the floating-point value to floor.
		 *
		 * @return A floating point value containing an integer.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @example Floor(-2)   = -2
		 * @example Floor(-1.5) = -2
		 * @example Floor(-0.8) = -1
		 * @example Floor(0.5)  =  0
		 * @example Floor(1.6)  =  1
		 *
		 */
		double Floor( double x ) throw() ;
		
		
		/**
		 * Returns largest integral value not greater than argument.
		 *
		 * @param x the floating-point value to floor.
		 *
		 * @return A floating point value containing an integer.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @example Floor(-2)   = -2
		 * @example Floor(-1.5) = -2
		 * @example Floor(-0.8) = -1
		 * @example Floor(0.5)  =  0
		 * @example Floor(1.6)  =  1
		 *
		 */
		long double Floor( long double x ) throw() ;
		
		
		
		
		/**
		 * Returns smallest integral value not less than argument.
		 *
		 * @param x the floating-point value to ceil.
		 *
		 * @return A floating point value containing an integer.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @example Ceil(-2)   = -2
		 * @example Ceil(-1.5) = -1
		 * @example Ceil(-0.8) =  0
		 * @example Ceil(0.5)  =  1
		 * @example Ceil(1.6)  =  2
		 *
		 */
		float Ceil( float x ) throw() ;
		
		
		/**
		 * Returns smallest integral value not less than argument.
		 *
		 * @param x the floating-point value to ceil.
		 *
		 * @return A floating point value containing an integer.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @example Ceil(-2)   = -2
		 * @example Ceil(-1.5) = -1
		 * @example Ceil(-0.8) =  0
		 * @example Ceil(0.5)  =  1
		 * @example Ceil(1.6)  =  2
		 *
		 */
		double Ceil( double x ) throw() ;

		
		/**
		 * Returns smallest integral value not less than argument.
		 *
		 * @param x the floating-point value to ceil.
		 *
		 * @return A floating point value containing an integer.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @example Ceil(-2)   = -2
		 * @example Ceil(-1.5) = -1
		 * @example Ceil(-0.8) =  0
		 * @example Ceil(0.5)  =  1
		 * @example Ceil(1.6)  =  2
		 *
		 */
		long double Ceil( long double x ) throw() ;
		
		
		
		// Round section.
		
		
		/**
		 * Returns nearest integer value from argument. 
		 * When half-way (ex : 0.5 or -1.5), will choose, among the 
		 * two nearest integers, the one with the greater absolute value 
		 * (i.e. rounds away from zero).
		 *
		 * @param x the floating-point value to round.
		 *
		 * @return A floating point value containing an integer.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @example Round(-2)   = -2
		 * @example Round(-1.5) = -2
		 * @example Round(-0.8) = -1
		 * @example Round(0.5)  =  1
		 * @example Round(1.6)  =  2
		 *
		 */
		float Round( float x ) throw() ;


		/**
		 * Returns nearest float value, with specified fractional 
		 * precision, from argument. 
		 *
		 * When half-way (ex : 0.5 or -1.5), will choose, among the 
		 * two nearest floats, the one with the greater absolute value
		 * (i.e. rounds away from zero).
		 *
		 * @param x the floating-point value to round.
		 *
		 * @param precision the number of digits after the dot to round at.
		 *
		 * @return A floating point value rounded appropriately.
		 *
		 * As floating values have limited accuracy, rounding '1.676' 
		 * (which translates into an actual number '1.6759999...') to 
		 * 2 digits after the dot may result, instead of the expected 
		 * '1.68', to '1.679999...'. 
		 * If the reason for the round is to have a text with not too
		 * many useless digits after the dot, then use Ceylan::toString 
		 * with two parameters, the number and the output precision.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @see Ceylan::toString
		 *
		 * @example Round(1.676, 2) = 1.68 or 1.67999999...
		 * @example Round(-3,23456, 3 ) = -3,235 or -3.2349999...
		 *
		 */
		float Round( float x, Ceylan::Uint8 precision ) throw() ;

		
		
		/**
		 * Returns nearest integer value from argument. 
		 * When half-way (ex : 0.5 or -1.5), will choose, among the 
		 * two nearest integers, the one with the greater absolute value 
		 * (i.e. rounds away from zero).
		 *
		 * @param x the floating-point value to round.
		 *
		 * @return A floating point value containing an integer.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @example Round(-2)   = -2
		 * @example Round(-1.5) = -2
		 * @example Round(-0.8) = -1
		 * @example Round(0.5)  =  1
		 * @example Round(1.6)  =  2
		 *
		 */
		double Round( double x ) throw() ;


		/**
		 * Returns nearest double value, with specified fractional 
		 * precision, from argument. 
		 *
		 * When half-way (ex : 0.5 or -1.5), will choose, among the
		 * two nearest floats, the one 
		 * with the greater absolute value (i.e. rounds away from zero).
		 *
		 * @param x the floating-point value to round.
		 *
		 * @param precision the number of digits after the dot to round at.
		 *
		 * @return A floating point value rounded appropriately.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 * As floating values have limited accuracy, rounding '1.676'
		 * (which translates into an actual number '1.6759999...') to 
		 * 2 digits after the dot may result, instead of the expected 
		 * '1.68', to '1.679999...'. 
		 * If the reason for the round is to have a text with not too 
		 * many useless digits after the dot, then use Ceylan::toString 
		 * with two parameters, the number and the output precision.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @see Ceylan::toString
		 *
		 * @example Round(1.676, 2) = 1.68 or 1.67999999...
		 * @example Round(-3,23456, 3 ) = -3,235 or -3.2349999...
		 *
		 */
		double Round( double x, Ceylan::Uint8 precision ) throw() ;
				
				
		
		/**
		 * Returns nearest integer value from argument. 
		 * When half-way (ex : 0.5 or -1.5), will choose, among the 
		 * two nearest integers, the one with the greater absolute value 
		 * (i.e. rounds away from zero).
		 *
		 * @param x the floating-point value to round.
		 *
		 * @return A floating point value containing an integer.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @example Round(-2)   = -2
		 * @example Round(-1.5) = -2
		 * @example Round(-0.8) = -1
		 * @example Round(0.5)  =  1
		 * @example Round(1.6)  =  2
		 *
		 */
		long double Round( long double x ) throw() ;


		/**
		 * Returns nearest float value, with specified fractional 
		 * precision, from argument. 
		 *
		 * When half-way (ex : 0.5 or -1.5), will choose, among the 
		 * two nearest floats, the one with the greater absolute value
		 * (i.e. rounds away from zero).
		 *
		 * @param x the floating-point value to round.
		 *
		 * @param precision the number of digits after the dot to round at.
		 *
		 * @return A floating point value rounded appropriately.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 * As floating values have limited accuracy, rounding 
		 * '1.676' (which translates into an actual number '1.6759999...') 
		 * to 2 digits after the dot may result, instead of the expected
		 * '1.68', to '1.679999...'. 
		 * If the reason for the round is to have a text with not too 
		 * many useless digits after the dot, then use Ceylan::toString 
		 * with two parameters, the number and the output precision.
		 *
		 * @note A floating-point point value is returned to avoid 
		 * overflow of integer types.
		 *
		 * @see Ceylan::toString
		 *
		 * @example Round(1.676, 2) = 1.68 or 1.67999999...
		 * @example Round(-3,23456, 3 ) = -3,235 or -3.2349999...
		 *
		 */
		long double Round( long double x, Ceylan::Uint8 precision ) throw() ;
	
			
				
		/// Absolute value, polluted by ambiguous calls :
		
		
		// Integer signed types :

		/// Computes the absolute value of specified argument.
		char Abs( char x ) throw() ;		
		
		/// Computes the absolute value of specified argument.
		short Abs( short x ) throw() ;
				
		/// Computes the absolute value of specified argument.
		int Abs( int x ) throw() ;
				
		/// Computes the absolute value of specified argument.
		long Abs( long x ) throw() ;
		
		
		/**
		 * Computes the absolute value of specified argument.
		 *
		 * @note Disabled since ISO C++ does not support `long long'.
		 *
		 */
		//long long int Abs( long long int x ) throw() ;
		
		
		// Floating point types :
		 
		/// Computes the absolute value of specified argument.
		float Abs( float x ) throw() ;
		
		/// Computes the absolute value of specified argument.
		double Abs( double x ) throw() ;
		
		/// Computes the absolute value of specified argument.
		long double Abs( long double x ) throw() ;
		
		
		/// Min :
		

		/**
		 * Min operator template available, since using only specialized
		 * Min functions lead to way too many ambiguities and implies 
		 * several uneasy static_cast.
		 *
		 */
		template<class T>
		T Min( T x, T y ) throw()
		{
			return ( ( x < y ) ? x : y ) ;
				
		}

		
		// Integer signed and unsigned types :

		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		char Min( char x, char y ) throw() ;		

		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		unsigned char Min( unsigned char x, unsigned char y ) throw() ;		
		
		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		short Min( short x, short y ) throw() ;
			
			
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		unsigned short Min( unsigned short x, unsigned short y ) throw() ;
				
				
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		int Min( int x, int y ) throw() ;
	
		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		unsigned int Min( unsigned int x, unsigned int y ) throw() ;
		
				
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		long Min( long x, long y ) throw() ;

				
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		unsigned long Min( unsigned long x, unsigned long y ) throw() ;
	
		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 * @note Disabled since ISO C++ does not support `long long'.
		 *
		 */
		//long long int Min( long long int x, long long int y ) throw() ;
		
		
		// Floating point types :

		 
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		float Min( float x, float y ) throw() ;

		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		double Min( double x, double y ) throw() ;

		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		long double Min( long double x, long double y ) throw() ;
		
		
		
	
		/// Max :

		
		/**
		 * Max operator template available, since using only 
		 * specialized Max functions lead to way too many ambiguities 
		 * and implies several uneasy static_cast.
		 *
		 */
		template<class T>
		T Max( T x, T y ) throw()
		{
			return ( ( x > y ) ? x : y ) ;
				
		}

		
		// Integer signed and unsigned types :

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		char Max( char x, char y ) throw() ;		

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		unsigned char Max( unsigned char x, unsigned char y ) throw() ;		
		
		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		short Max( short x, short y ) throw() ;
			
			
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		unsigned short Max( unsigned short x, unsigned short y ) throw() ;
				
				
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		int Max( int x, int y ) throw() ;
	
		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		unsigned int Max( unsigned int x, unsigned int y ) throw() ;
		
				
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		long Max( long x, long y ) throw() ;

				
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		unsigned long Max( unsigned long x, unsigned long y ) throw() ;
	
		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 * @note Disabled since ISO C++ does not support `long long'.
		 *
		 */
		//long long int Max( long long int x, long long int y ) throw() ;
		
		
		// Floating point types :

		 
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		float Max( float x, float y ) throw() ;

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		double Max( double x, double y ) throw() ;

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		long double Max( long double x, long double y ) throw() ;
		
			

				
		// Exponentials :
		
		
		/**
		 * Computes the value of e (the base of natural logarithms) 
		 * raised to the power of x.
		 *
		 */
		float Exp( float x ) throw() ;
		
		
		/**
		 * Computes the value of e (the base of natural logarithms) 
		 * raised to the power of x.
		 *
		 */
		double Exp( double x ) throw() ;
		
		
		/**
		 * Computes the value of e (the base of natural logarithms)
		 * raised to the power of x.
		 *
		 */
		long double Exp( long double x ) throw() ;
		
		
		
		// Power :
		
		/// Computes the value of x raised to the power of y.
		float Pow( float x, float y ) throw() ;
		
		/// Computes the value of x raised to the power of y.		
		double Pow( double x, double y ) throw() ;
		
		/// Computes the value of x raised to the power of y.		
		long double Pow( long double x, long double y ) throw() ;
		
		
		
		// Power of two :
		
		/// Computes the value of x² (x*x).
		float Pow2( float x ) throw() ;
		
		/// Computes the value of x² (x*x).		
		double Pow2( double x ) throw() ;
		
		/// Computes the value of x² (x*x).		
		long double Pow2( long double x ) throw() ;
		
			
				
		// Square root :
		
		/**
		 * Computes the non-negative square root of x, throw an 
		 * exception if x is negative.
		 *
		 */
		float Sqrt( float x ) throw( MathsException ) ;
		
		
		/**
		 * Computes the non-negative square root of x, throw an 
		 * exception if x is negative.
		 *
		 */
		double Sqrt( double x ) throw( MathsException ) ;
		
		
		/**
		 * Computes the non-negative square root of x, throw an 
		 * exception if x is negative.
		 *
		 */
		long double Sqrt( long double x ) throw( MathsException ) ;
			
			
		
		// Some trigonometry.
			
				
				
		// Cosine :

		
		/**
		 * Computes the cosine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		float Cos( float x ) throw() ;
		
		
		/**
		 * Computes the cosine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		double Cos( double x ) throw() ;
		
		
		/**
		 * Computes the cosine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		long double Cos( long double x ) throw() ;
		

		
		// Sine :
		
		
		/**
		 * Computes the sine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		float Sin( float x ) throw() ;
		
		
		/**
		 * Computes the sine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		double Sin( double x ) throw() ;
		
		
		/**
		 * Computes the sine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		long double Sin( long double x ) throw() ;
		
		
		
		/**
		 * Converts an angle expressed in degrees into an angle 
		 * expressed in radians.
		 *
		 */		
		AngleInRadians DegreeToRadian( AngleInDegrees angleInDegrees ) 
			throw() ;
		
		
		/**
		 * Returns the first power of 2 greater or equal to the 
		 * specified value.
		 *
		 * Useful for example to compute OpenGL texture sizes.
		 *
		 * @param value for which the smallest power of two must be 
		 * returned.
		 *
		 * @return the smallest power of 2 greater or equal to the 
		 * specified value.
		 *
		 * @note No overflow checking is done.
		 *
		 */
		Uint16 NextPowerOfTwo( Uint16 value ) throw() ;
		
		
		/**
		 * Tells whether the specified value is a power of two.
		 *
		 *
		 */
		bool IsAPowerOfTwo( Uint16 value ) throw() ;	
		
		
		
		/**
		 * A functor is an object that behaves as a function, thanks 
		 * to the overloading of operator (). 
		 *
		 * This allows the encapsulated function to have a state.
		 *
		 * @note It is especially useful when one has to specify a 
		 * function A as a parameter for another function B. If B is set
		 * so that its argument shall be a functor, then it can take
		 * indifferently a simple function or a functor as parameter. The
		 * reciprocal would not be true.
		 *
		 * @note Functors are great when one wants to make use of 
		 * parametrized functions : thanks to the functor's constructor,
		 * one can generate numerous different functions at runtime.
		 *
		 * @example addFunctor( int myAdd ) could be constructed as :
		 * <code>Functor * myAdd = new addFunctor( 5 )</code> : myAdd 
		 * is a dynamically created function.
		 * 
		 * @see IntToIntFunctor for a complete example.
		 *
		 * @note Each child should define following operator :
		 * virtual 'returned type' operator() ('parameter') throw()
		 *
		 */
		class Functor : public Ceylan::TextDisplayable
		{
		
		
			public:
			
			
				/// Do-nothing constructor.
				Functor() throw() ;
				
				/// Basic virtual destructor;
				virtual ~Functor() throw() ;
				
								 
	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall
				 * settings.
				 *
				 * @see TextDisplayable
				 *
	             */
		 		virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) 
						const throw() ;
 
 
			private:
			
			
				/**
				 * Copy constructor cannot be private since would 
				 * prevent calls such as :
				 * 'CreateFrom( Linear::Rotation2DFunctor( angle )'.
				 *
				 *	Functor( const Functor & source ) throw() ;
				 *
				 */
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				Functor & operator = ( const Functor & source ) throw() ;
							
		} ;
		
		
		
		/**
		 * A functor dealing with integers (one integer as parameter, 
		 * returns integer).
		 *
		 */
		class IntToIntFunctor : public Functor
		{
		
		
			public:
			
			
				/// Basic constructor.
				explicit IntToIntFunctor( int creationParameter ) throw() ;
				
				/// Basic virtual destructor;
				virtual ~IntToIntFunctor() throw() ;
				
				/// The callable method.
				virtual int operator() ( int callParameter ) throw() = 0 ;
	
		
	            /**
	             * Returns a user-friendly description of the state of
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall
				 * settings.
				 *
				 * @see TextDisplayable
				 *
	             */
		 		virtual const std::string toString( 
						Ceylan::VerbosityLevels level = Ceylan::high ) 
					const throw() ;
		
		
			protected:
			
			
				/** 
				 * Internal integer, which should be used when evaluating 
				 * the functor.
				 *
				 */
				int _creationParameter ;
			
 		} ;
				
	}

}


#endif // CEYLAN_MATH_BASIC_H_
