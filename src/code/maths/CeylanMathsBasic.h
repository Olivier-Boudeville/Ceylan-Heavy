#ifndef CEYLAN_MATHS_BASIC_H_
#define CEYLAN_MATHS_BASIC_H_


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
		 * they are adequate for computation using `Ceylan::LongFloat'.
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
		
		
		/**
		 * Integer percentage values, which should range from 0 to 100, 
		 * both included.
		 *
		 */
		typedef Ceylan::Uint8 Percentage ;
		
		
		/// Exception for Maths-related issues.
		class MathsException : public Ceylan::Exception
		{
		
			public:
			
				explicit MathsException( const std::string & message ) throw() ;
				virtual ~MathsException() throw() ;
		
		} ;
		
		
		
		/*
		 * Section related to most useful constants.
		 *
		 * @note The LongFloat type is used to decrease numerical errors.
		 *
		 */
		
		
		/**
		 * The number e. 
		 * (used to be the M_E maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat E ;


		/**
		 * The number log_2(e).
		 * (used to be the M_LOG2E maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat Log2E ;
	
	
		/**
		 * The number log_10(e). 
		 * (used to be the M_LOG10E maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat Log10E ;
	
	
		/**
		 * The number log_e(2) = ln(2). 
		 * (used to be the M_LN2 maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat LogE2 ;
	
	
		/**
		 * The number log_e(10) = ln(10) 
		 * (used to be the M_LN10 maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat LogE10 ;



		// Section related to the Pi constant.
		
	
		/** 
		 * The number pi.
		 * (used to be the M_PI maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat Pi ;
	
	
		/** 
		 * The number pi/2. 
		 * (used to be the M_PI_2 maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat Pi_div_2 ;
	
	
		/** 
		 * The number pi/4. 
		 * (used to be the M_PI_4 maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat Pi_div_4 ;
	
	
		/** 
		 * The number 1/pi. 
		 * (used to be the M_1_PI maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat One_div_Pi ;
	
	
		/** 
		 * The number 2/pi.
		 * (used to be the M_2_PI maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat Two_div_Pi ;
	
	
		/** 
		 * The number 2/sqrt(pi). 
		 * (used to be the M_2_SQRTPI maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat Two_div_sqrt_Pi ;
	
	
	
	
		// Section related to the sqrt(2) constant.
		
	
		/** 
		 * The number sqrt(2). 
		 * (used to be the M_SQRT2 maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat Sqrt_2 ;
	
	
		/** 
		 * The number 1/sqrt(2). 
		 * (used to be the M_SQRT1_2 maths.h constant).
		 *
		 */
		extern const Ceylan::LongFloat One_div_sqrt_2 ;
	
	
		/**
		 * A very small number, used for floating point comparisons :
		 * two numbers x and y are deemed equal if | x - y | < EpsilonFloat32
		 *
		 */
		extern const Ceylan::LongFloat EpsilonFloat32 ;


		/**
		 * A very small number, used for floating point comparisons : 
		 * two numbers x and y are deemed equal if | x - y | < EpsilonFloat64
		 *
		 */
		extern const Ceylan::LongFloat EpsilonFloat64 ;


		/**
		 * A very small number, used for floating point comparisons : 
		 * two numbers x and y are deemed equal if | x - y | < EpsilonLongFloat
		 *
		 */
		extern const Ceylan::LongFloat EpsilonLongFloat ;


		/**
		 * A very small number, used for floating point comparisons :
		 * two numbers x and y are deemed equal if | x - y | < Epsilon
		 *
		 */
		extern const Ceylan::LongFloat Epsilon ;

		
		
		
		// Templates could have been more widely used.



		// IsNull section.
		

		/** 
		 * Tells whether x is null as regard to floating point numbers :
		 * | x | < EpsilonFloat32.
		 *
		 */
		bool IsNull( Ceylan::Float32 x ) throw() ;

		
		/** 
		 * Tells whether x is null as regard to floating point numbers :
		 * | x | < epsilon.
		 *
		 */
		bool IsNull( Ceylan::Float32 x, Ceylan::Float32 epsilon ) throw() ;
		
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers :
		 * | x | < EpsilonFloat64.
		 *
		 */
		bool IsNull( Ceylan::Float64 x ) throw() ;
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers :
		 * | x | < epsilon.
		 *
		 */
		bool IsNull( Ceylan::Float64 x, Ceylan::Float64 epsilon ) throw() ;
		
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers :
		 * | x | < EpsilonLongFloat.
		 *
		 */
		bool IsNull( Ceylan::LongFloat x ) throw() ;
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers :
		 * | x | < epsilon.
		 *
		 */
		bool IsNull( Ceylan::LongFloat x, Ceylan::LongFloat epsilon ) throw() ;
		
		
		
		
		// AreEqual section.
		
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers :
		 * | x - y | < EpsilonFloat32.
		 *
		 */
		bool AreEqual( Ceylan::Float32 x, Ceylan::Float32 y ) throw() ;
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers :
		 * | x - y | < epsilon.
		 *
		 */
		bool AreEqual( Ceylan::Float32 x, Ceylan::Float32 y, 
			Ceylan::Float32 epsilon ) throw() ;
		
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers :
		 * | x - y | < EpsilonFloat64.
		 *
		 */
		bool AreEqual( Ceylan::Float64 x, Ceylan::Float64 y ) throw() ;
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers :
		 * | x - y | < epsilon.
		 *
		 */
		bool AreEqual( Ceylan::Float64 x, Ceylan::Float64 y, 
			Ceylan::Float64 epsilon ) throw() ;
		

		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers : 
		 * | x - y | < EpsilonLongFloat.
		 *
		 */
		bool AreEqual( Ceylan::LongFloat x, Ceylan::LongFloat y ) throw() ;

		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers : 
		 * | x - y | < epsilon.
		 *
		 */
		bool AreEqual( Ceylan::LongFloat x, Ceylan::LongFloat y, 
			Ceylan::LongFloat epsilon ) throw() ;
		
	
	
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
		 * @note The chosen epsilon, the double of the criterion of relative 
		 * equality, is the default epsilon used for the templated type for
		 * tests against zero, and EpsilonFloat32 for the relative test.
		 *
		 */
		template <typename T> bool AreRelativelyEqual( T x, T y ) throw()
		{
		
		
			/*
			 * One goal, avoid division by zero on final formula :
			 * x + y == 0 iff x == -y, in this case :  
			 *   - if x == 0 then y == 0 and they are deemed equal
			 *   - otherwise x != 0 implies x != y (since x == -y),
			 * hence they are not deemed equal.
			 *
			 * EpsilonFloat32 not used, otherwise the IsNull calls are
			 * deemed ambiguous.
			 * 
			 */
			if ( IsNull( x + y ) )
				return IsNull( x ) ;
				
				
			/*
			 * Here x + y != 0.
			 * 
			 * Relative comparison : compare their difference (|x-y|) 
			 * to their mean value ((x+y)/2), here using 
			 * EpsilonFloat32/2 as equality criterion.
			 *
			 */
			return ( Abs( x - y ) / ( x + y ) < EpsilonFloat32 ) ;
			
		}
		
		
		
		/**
		 * Relative comparison with user-supplied epsilon.
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
			Ceylan::LongFloat epsilon ) throw()
		{
		
		
			/*
			 * One goal, avoid division by zero on final formula :
			 * x + y == 0 iff x == -y, in this case :  
			 *   - if x == 0 then y == 0 and they are deemed equal
			 *   - otherwise x != 0 implies x != y (since x == -y),
			 * hence they are not deemed equal.
			 *
			 */
			if ( IsNull( x + y, epsilon ) )
				return IsNull( x, epsilon ) ;
				
				
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
		Ceylan::Float32 Floor( Ceylan::Float32 x ) throw() ;

		
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
		Ceylan::Float64 Floor( Ceylan::Float64 x ) throw() ;
		
		
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
		Ceylan::LongFloat Floor( Ceylan::LongFloat x ) throw() ;
		
		
		
		
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
		Ceylan::Float32 Ceil( Ceylan::Float32 x ) throw() ;
		
		
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
		Ceylan::Float64 Ceil( Ceylan::Float64 x ) throw() ;

		
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
		Ceylan::LongFloat Ceil( Ceylan::LongFloat x ) throw() ;
		
		
		
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
		 * @note On some platforms (at least OpenBSD 3.8), round operation 
		 * is not available, floor will be used instead.
		 *
		 */
		Ceylan::Float32 Round( Ceylan::Float32 x ) throw() ;


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
		 * @note On some platforms (at least OpenBSD 3.8), round operation 
		 * is not available, floor will be used instead.
		 *
		 */
		Ceylan::Float32 Round( Ceylan::Float32 x, Ceylan::Uint8 precision )
			throw() ;

		
		
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
		 * @note On some platforms (at least OpenBSD 3.8), round operation 
		 * is not available, floor will be used instead.
		 *
		 */
		Ceylan::Float64 Round( Ceylan::Float64 x ) throw() ;


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
		 * @note On some platforms (at least OpenBSD 3.8), round operation 
		 * is not available, floor will be used instead.
		 *
		 */
		Ceylan::Float64 Round( Ceylan::Float64 x, Ceylan::Uint8 precision )
			throw() ;
				
				
		
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
		 * @note On some platforms (at least OpenBSD 3.8), round operation 
		 * is not available, floor will be used instead.
		 *
		 */
		Ceylan::LongFloat Round( Ceylan::LongFloat x ) throw() ;


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
		 * @note On some platforms (at least OpenBSD 3.8), round operation 
		 * is not available, floor will be used instead.
		 *
		 */
		Ceylan::LongFloat Round( Ceylan::LongFloat x, Ceylan::Uint8 precision )
			throw() ;
	
			
				
		/// Absolute value, polluted by ambiguous calls :
		
		
		// Integer signed types :

		/// Computes the absolute value of specified argument.
		Ceylan::Sint8 Abs( Ceylan::Sint8 x ) throw() ;		
		
		/// Computes the absolute value of specified argument.
		Ceylan::Sint16 Abs( Ceylan::Sint16 x ) throw() ;
				
		/// Computes the absolute value of specified argument.
		Ceylan::Sint32 Abs( Ceylan::Sint32 x ) throw() ;
				
		/// Computes the absolute value of specified argument.
		Ceylan::SignedLongInteger Abs( Ceylan::SignedLongInteger x ) throw() ;
		
		
		/**
		 * Computes the absolute value of specified argument.
		 *
		 * @note Disabled since ISO C++ does not support `long long'.
		 *
		 */
		//long long int Abs( long long int x ) throw() ;
		
		
		// Floating point types :
		 
		/// Computes the absolute value of specified argument.
		Ceylan::Float32 Abs( Ceylan::Float32 x ) throw() ;
		
		/// Computes the absolute value of specified argument.
		Ceylan::Float64 Abs( Ceylan::Float64 x ) throw() ;
		
		/// Computes the absolute value of specified argument.
		Ceylan::LongFloat Abs( Ceylan::LongFloat x ) throw() ;
		
		
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
		Ceylan::Sint8 Min( Ceylan::Sint8 x, Ceylan::Sint8 y ) throw() ;		

		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Uint8 Min( Ceylan::Uint8 x, Ceylan::Uint8 y ) throw() ;		
		
		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Sint16 Min( Ceylan::Sint16 x, Ceylan::Sint16 y ) throw() ;
			
			
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Uint16 Min( Ceylan::Uint16 x, Ceylan::Uint16 y ) throw() ;
				
				
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Sint32 Min( Ceylan::Sint32 x, Ceylan::Sint32 y ) throw() ;
	
		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Uint32 Min( Ceylan::Uint32 x, Ceylan::Uint32 y ) throw() ;
		
				
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::SignedLongInteger Min( Ceylan::SignedLongInteger x,
			Ceylan::SignedLongInteger y ) throw() ;

				
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::UnsignedLongInteger Min( Ceylan::UnsignedLongInteger x,
			Ceylan::UnsignedLongInteger y ) throw() ;
	
		
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
		Ceylan::Float32 Min( Ceylan::Float32 x, Ceylan::Float32 y ) throw() ;

		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Float64 Min( Ceylan::Float64 x, Ceylan::Float64 y ) throw() ;

		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::LongFloat Min( Ceylan::LongFloat x, Ceylan::LongFloat y )
			throw() ;
		
		
		
	
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
		Ceylan::Sint8 Max( Ceylan::Sint8 x, Ceylan::Sint8 y ) throw() ;		

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Uint8 Max( Ceylan::Uint8 x, Ceylan::Uint8 y ) throw() ;		
		
		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Sint16 Max( Ceylan::Sint16 x, Ceylan::Sint16 y ) throw() ;
			
			
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Uint16 Max( Ceylan::Uint16 x, Ceylan::Uint16 y ) throw() ;
				
				
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Sint32 Max( Ceylan::Sint32 x, Ceylan::Sint32 y ) throw() ;
	
		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Uint32 Max( Ceylan::Uint32 x, Ceylan::Uint32 y ) throw() ;
		
				
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::SignedLongInteger Max( Ceylan::SignedLongInteger x,
			Ceylan::SignedLongInteger y ) throw() ;

				
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::UnsignedLongInteger Max( Ceylan::UnsignedLongInteger x,
			Ceylan::UnsignedLongInteger y ) throw() ;
	
		
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
		Ceylan::Float32 Max( Ceylan::Float32 x, Ceylan::Float32 y ) throw() ;

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::Float64 Max( Ceylan::Float64 x, Ceylan::Float64 y ) throw() ;

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		Ceylan::LongFloat Max( Ceylan::LongFloat x, Ceylan::LongFloat y )
			throw() ;
		
			

				
		// Exponentials :
		
		
		/**
		 * Computes the value of e (the base of natural logarithms) 
		 * raised to the power of x.
		 *
		 */
		Ceylan::Float32 Exp( Ceylan::Float32 x ) throw() ;
		
		
		/**
		 * Computes the value of e (the base of natural logarithms) 
		 * raised to the power of x.
		 *
		 */
		Ceylan::Float64 Exp( Ceylan::Float64 x ) throw() ;
		
		
		/**
		 * Computes the value of e (the base of natural logarithms)
		 * raised to the power of x.
		 *
		 */
		Ceylan::LongFloat Exp( Ceylan::LongFloat x ) throw() ;
		
		
		
		// Power :
		
		/// Computes the value of x raised to the power of y.
		Ceylan::Float32 Pow( Ceylan::Float32 x, Ceylan::Float32 y ) throw() ;
		
		/// Computes the value of x raised to the power of y.		
		Ceylan::Float64 Pow( Ceylan::Float64 x, Ceylan::Float64 y ) throw() ;
		
		/// Computes the value of x raised to the power of y.		
		Ceylan::LongFloat Pow( Ceylan::LongFloat x, Ceylan::LongFloat y )
			throw() ;
		
		
		
		// Power of two :
		
		/// Computes the value of x² (x*x).
		Ceylan::Float32 Pow2( Ceylan::Float32 x ) throw() ;
		
		/// Computes the value of x² (x*x).		
		Ceylan::Float64 Pow2( Ceylan::Float64 x ) throw() ;
		
		/// Computes the value of x² (x*x).		
		Ceylan::LongFloat Pow2( Ceylan::LongFloat x ) throw() ;
		
			
				
		// Square root :
		
		
		/**
		 * Computes the non-negative square root of x, throw an 
		 * exception if x is negative.
		 *
		 */
		Ceylan::Float32 Sqrt( Ceylan::Float32 x ) throw( MathsException ) ;
		
		
		/**
		 * Computes the non-negative square root of x, throw an 
		 * exception if x is negative.
		 *
		 */
		Ceylan::Float64 Sqrt( Ceylan::Float64 x ) throw( MathsException ) ;
		
		
		/**
		 * Computes the non-negative square root of x, throw an 
		 * exception if x is negative.
		 *
		 */
		Ceylan::LongFloat Sqrt( Ceylan::LongFloat x ) throw( MathsException ) ;
			
			
		
		// Some trigonometry.
			
				
				
		// Cosine :

		
		/**
		 * Computes the cosine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		Ceylan::Float32 Cos( Ceylan::Float32 x ) throw() ;
		
		
		/**
		 * Computes the cosine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		Ceylan::Float64 Cos( Ceylan::Float64 x ) throw() ;
		
		
		/**
		 * Computes the cosine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		Ceylan::LongFloat Cos( Ceylan::LongFloat x ) throw() ;
		

		
		// Sine :
		
		
		/**
		 * Computes the sine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		Ceylan::Float32 Sin( Ceylan::Float32 x ) throw() ;
		
		
		/**
		 * Computes the sine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		Ceylan::Float64 Sin( Ceylan::Float64 x ) throw() ;
		
		
		/**
		 * Computes the sine of x, where x is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		Ceylan::LongFloat Sin( Ceylan::LongFloat x ) throw() ;
		
		
		
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
				explicit IntToIntFunctor( 
					Ceylan::Sint32 creationParameter ) throw() ;
				
				/// Basic virtual destructor;
				virtual ~IntToIntFunctor() throw() ;
				
				/// The callable method.
				virtual Ceylan::Sint32 operator() ( 
					Ceylan::Sint32 callParameter ) throw() = 0 ;
	
		
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
				Ceylan::Sint32 _creationParameter ;
			
 		} ;
				
	}

}


#endif // CEYLAN_MATHS_BASIC_H_
