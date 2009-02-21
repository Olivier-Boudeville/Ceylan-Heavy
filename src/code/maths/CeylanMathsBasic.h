#ifndef CEYLAN_MATHS_BASIC_H_
#define CEYLAN_MATHS_BASIC_H_



#include "CeylanException.h"        // for Exception
#include "CeylanTypes.h"            // for Float32, etc.
#include "CeylanFunctor.h"          // for functor, etc.


#include <string>



namespace Ceylan
{


	namespace Maths
	{
	
	
		/*
		 * Numerical constants commonly used for mathematic computations.
		 *
		 * Instead of standard variables (ex: M_E), their GNU counterparts
		 * (ex: M_El) are used, since their accuracy (precision) is better:
		 * they are adequate for computation using `Ceylan::LongFloat'.
		 * 
		 * Since depending on a GNU set of constants would not be portable
		 * enough, their values are explicitly set here, one time for all.
		 *
		 * @note Curiously, the overall accuracy is not that great, for 
		 * example with E: 
		 * 	- original: 2.7182818284590452353602874713526625
		 *  - displayed: 2.7182818284590450908
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
		
		
		/**
		 * Angles, expressed in degrees.
		 *
		 * @note Degrees typically range in [0;360[.
		 *
		 */
		typedef Ceylan::Float32 AngleInDegrees ;
		

		/**
		 * Angles, expressed in radians.
		 *
		 * @note Radians typically range in [0;2*Pi[.
		 *
		 */
		typedef Ceylan::Float32 AngleInRadians ;
		
		
		/**
		 * Integer percentage values, which should range from 0 to 100, 
		 * both included.
		 *
		 */
		typedef Ceylan::Uint8 Percentage ;
	
		
		/**
		 * Unit for frequencies (Hz). 
		 *
		 * For example, a periodical event whose frequency is 100 Hz would
		 * happen once on each period P = 1/100 s = 10 ms.
		 *
		 * @note For our needs, it is an integer type.
		 *
		 */
		typedef Ceylan::Uint32 Hertz ;

		
		/**
		 * Ratio unit, should store floating point values between 0 and 1.
		 *
		 */
		typedef Ceylan::Float32 Ratio ;
		
		
		/// Exception for Maths-related issues.
		class CEYLAN_DLL MathsException: public Ceylan::Exception
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
		extern CEYLAN_DLL const Ceylan::LongFloat E ;


		/**
		 * The number log_2(e).
		 * (used to be the M_LOG2E maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat Log2E ;
	
	
		/**
		 * The number log_10(e). 
		 * (used to be the M_LOG10E maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat Log10E ;
	
	
		/**
		 * The number log_e(2) = ln(2). 
		 * (used to be the M_LN2 maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat LogE2 ;
	
	
		/**
		 * The number log_e(10) = ln(10) 
		 * (used to be the M_LN10 maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat LogE10 ;



		// Section related to the Pi constant.
		
	
		/** 
		 * The number pi.
		 * (used to be the M_PI maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat Pi ;
	
	
		/** 
		 * The number pi/2. 
		 * (used to be the M_PI_2 maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat Pi_div_2 ;
	
	
		/** 
		 * The number pi/4. 
		 * (used to be the M_PI_4 maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat Pi_div_4 ;
	
	
		/** 
		 * The number 1/pi. 
		 * (used to be the M_1_PI maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat One_div_Pi ;
	
	
		/** 
		 * The number 2/pi.
		 * (used to be the M_2_PI maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat Two_div_Pi ;
	
	
		/** 
		 * The number 2/sqrt(pi). 
		 * (used to be the M_2_SQRTPI maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat Two_div_sqrt_Pi ;
	
	
	
	
		// Section related to the sqrt(2) constant.
		
	
		/** 
		 * The number sqrt(2). 
		 * (used to be the M_SQRT2 maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat Sqrt_2 ;
	
	
		/** 
		 * The number 1/sqrt(2). 
		 * (used to be the M_SQRT1_2 maths.h constant).
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat One_div_sqrt_2 ;
	
	
		/**
		 * A very small number, used for floating point comparisons:
		 * two numbers x and y are deemed equal if | x - y | < EpsilonFloat32
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat EpsilonFloat32 ;


		/**
		 * A very small number, used for floating point comparisons: 
		 * two numbers x and y are deemed equal if | x - y | < EpsilonFloat64
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat EpsilonFloat64 ;


		/**
		 * A very small number, used for floating point comparisons: 
		 * two numbers x and y are deemed equal if | x - y | < EpsilonLongFloat
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat EpsilonLongFloat ;


		/**
		 * A very small number, used for floating point comparisons:
		 * two numbers x and y are deemed equal if | x - y | < Epsilon
		 *
		 */
		extern CEYLAN_DLL const Ceylan::LongFloat Epsilon ;

		
		
		
		// Templates could have been more widely used.



		// IsNull section.
		

		/** 
		 * Tells whether x is null as regard to floating point numbers:
		 * | x | < EpsilonFloat32.
		 *
		 */
		CEYLAN_DLL bool IsNull( Ceylan::Float32 x ) throw() ;

		
		/** 
		 * Tells whether x is null as regard to floating point numbers:
		 * | x | < epsilon.
		 *
		 */
		CEYLAN_DLL bool IsNull( Ceylan::Float32 x, Ceylan::Float32 epsilon )
			throw() ;
		
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers:
		 * | x | < EpsilonFloat64.
		 *
		 */
		CEYLAN_DLL bool IsNull( Ceylan::Float64 x ) throw() ;
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers:
		 * | x | < epsilon.
		 *
		 */
		CEYLAN_DLL bool IsNull( Ceylan::Float64 x, Ceylan::Float64 epsilon ) 
			throw() ;
		
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers:
		 * | x | < EpsilonLongFloat.
		 *
		 */
		CEYLAN_DLL bool IsNull( Ceylan::LongFloat x ) throw() ;
		
		
		/** 
		 * Tells whether x is null as regard to floating point numbers:
		 * | x | < epsilon.
		 *
		 */
		CEYLAN_DLL bool IsNull( Ceylan::LongFloat x, 
			Ceylan::LongFloat epsilon ) throw() ;
		
		
		
		
		// AreEqual section.
		
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers:
		 * | x - y | < EpsilonFloat32.
		 *
		 */
		CEYLAN_DLL bool AreEqual( Ceylan::Float32 x, Ceylan::Float32 y ) 
			throw() ;
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers:
		 * | x - y | < epsilon.
		 *
		 */
		CEYLAN_DLL bool AreEqual( Ceylan::Float32 x, Ceylan::Float32 y, 
			Ceylan::Float32 epsilon ) throw() ;
		
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers:
		 * | x - y | < EpsilonFloat64.
		 *
		 */
		CEYLAN_DLL bool AreEqual( Ceylan::Float64 x, Ceylan::Float64 y ) 
			throw() ;
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers:
		 * | x - y | < epsilon.
		 *
		 */
		CEYLAN_DLL bool AreEqual( Ceylan::Float64 x, Ceylan::Float64 y, 
			Ceylan::Float64 epsilon ) throw() ;
		

		/** 
		 * Tells whether x and y are exactly 'equal': x == y, with no compile
		 * warning about the fact that comparing floating point values 
		 * with == operator is most of the time absurd, which is true indeed.
		 *
		 * @note Only to be used on special cases where one wants bit-per-bit
		 * comparison, for example for marshalling tests. Hence usually using
		 * this function is a mistake, as comparing floating point with ==
		 * or != is unsafe.
		 *
		 */
		CEYLAN_DLL bool AreExactlyEqual( Ceylan::Float64 x, Ceylan::Float64 y ) 
			throw() ;
		
		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers: 
		 * | x - y | < EpsilonLongFloat.
		 *
		 */
		CEYLAN_DLL bool AreEqual( Ceylan::LongFloat x, Ceylan::LongFloat y )
			throw() ;

		
		/** 
		 * Tells whether x and y are 'equal' as floating point numbers: 
		 * | x - y | < epsilon.
		 *
		 */
		CEYLAN_DLL bool AreEqual( Ceylan::LongFloat x, Ceylan::LongFloat y, 
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
			 * One goal, avoid division by zero on final formula:
			 * x + y == 0 iff x == -y, in this case:  
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
			 * Relative comparison: compare their difference (|x-y|) 
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
			 * One goal, avoid division by zero on final formula:
			 * x + y == 0 iff x == -y, in this case:  
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
			 * Relative comparison: compare their difference (|x-y|) 
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
		CEYLAN_DLL Ceylan::Float32 Floor( Ceylan::Float32 x ) throw() ;

		
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
		CEYLAN_DLL Ceylan::Float64 Floor( Ceylan::Float64 x ) throw() ;
		
		
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
		CEYLAN_DLL Ceylan::LongFloat Floor( Ceylan::LongFloat x ) throw() ;
		
		
		
		
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
		CEYLAN_DLL Ceylan::Float32 Ceil( Ceylan::Float32 x ) throw() ;
		
		
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
		CEYLAN_DLL Ceylan::Float64 Ceil( Ceylan::Float64 x ) throw() ;

		
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
		CEYLAN_DLL Ceylan::LongFloat Ceil( Ceylan::LongFloat x ) throw() ;
		
		
		
		// Round section.
		
		
		/**
		 * Returns nearest integer value from argument. 
		 * When half-way (ex: 0.5 or -1.5), will choose, among the 
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
		CEYLAN_DLL Ceylan::Float32 Round( Ceylan::Float32 x ) throw() ;


		/**
		 * Returns nearest float value, with specified fractional 
		 * precision, from argument. 
		 *
		 * When half-way (ex: 0.5 or -1.5), will choose, among the 
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
		CEYLAN_DLL Ceylan::Float32 Round( Ceylan::Float32 x, 
			Ceylan::Uint8 precision ) throw() ;

		
		
		/**
		 * Returns nearest integer value from argument. 
		 * When half-way (ex: 0.5 or -1.5), will choose, among the 
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
		CEYLAN_DLL Ceylan::Float64 Round( Ceylan::Float64 x ) throw() ;


		/**
		 * Returns nearest double value, with specified fractional 
		 * precision, from argument. 
		 *
		 * When half-way (ex: 0.5 or -1.5), will choose, among the
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
		CEYLAN_DLL Ceylan::Float64 Round( Ceylan::Float64 x, 
			Ceylan::Uint8 precision ) throw() ;
				
				
		
		/**
		 * Returns nearest integer value from argument. 
		 * When half-way (ex: 0.5 or -1.5), will choose, among the 
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
		CEYLAN_DLL Ceylan::LongFloat Round( Ceylan::LongFloat x ) throw() ;


		/**
		 * Returns nearest float value, with specified fractional 
		 * precision, from argument. 
		 *
		 * When half-way (ex: 0.5 or -1.5), will choose, among the 
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
		CEYLAN_DLL Ceylan::LongFloat Round( Ceylan::LongFloat x, 
			Ceylan::Uint8 precision ) throw() ;
	
			
				
		/// Absolute value, polluted by ambiguous calls:
		
		
		// Integer signed types:

		/// Computes the absolute value of specified argument.
		CEYLAN_DLL Ceylan::Sint8 Abs( Ceylan::Sint8 x ) throw() ;		
		
		/// Computes the absolute value of specified argument.
		CEYLAN_DLL Ceylan::Sint16 Abs( Ceylan::Sint16 x ) throw() ;
				
		/// Computes the absolute value of specified argument.
		CEYLAN_DLL Ceylan::Sint32 Abs( Ceylan::Sint32 x ) throw() ;
				
		/// Computes the absolute value of specified argument.
		CEYLAN_DLL Ceylan::SignedLongInteger Abs( 
			Ceylan::SignedLongInteger x ) throw() ;
		
		
		/**
		 * Computes the absolute value of specified argument.
		 *
		 * @note Disabled since ISO C++ does not support `long long'.
		 *
		 */
		//long long int Abs( long long int x ) throw() ;
		
		
		// Floating point types:
		 
		/// Computes the absolute value of specified argument.
		CEYLAN_DLL Ceylan::Float32 Abs( Ceylan::Float32 x ) throw() ;
		
		/// Computes the absolute value of specified argument.
		CEYLAN_DLL Ceylan::Float64 Abs( Ceylan::Float64 x ) throw() ;
		
		/// Computes the absolute value of specified argument.
		CEYLAN_DLL Ceylan::LongFloat Abs( Ceylan::LongFloat x ) throw() ;
		
		
		/// Min:
		

		/**
		 * Min operator template available, since using only specialized
		 * Min functions lead to way too many ambiguities and implies 
		 * several uneasy static_cast.
		 *
		 */
		template<typename T>
		T Min( T x, T y ) throw()
		{
			return ( ( x < y ) ? x: y ) ;
				
		}


		/**
		 * Three-argument Min operator.
		 *
		 * Available as a template, since using only specialized
		 * Min functions lead to way too many ambiguities and implies 
		 * several uneasy static_cast.
		 *
		 */
		template<typename T>
		T Min( T x, T y, T z ) throw()
		{
		
			if ( x < y ) 
				return Min( x, z ) ;
			else
				return Min( y, z ) ;
								
		}

		
		// Integer signed and unsigned types:

		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Sint8 Min( Ceylan::Sint8 x, Ceylan::Sint8 y ) 
			throw() ;		

		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Uint8 Min( Ceylan::Uint8 x, Ceylan::Uint8 y ) 
			throw() ;		
		
		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Sint16 Min( Ceylan::Sint16 x, Ceylan::Sint16 y ) 
			throw() ;
			
			
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Uint16 Min( Ceylan::Uint16 x, Ceylan::Uint16 y ) 
			throw() ;
				
				
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Sint32 Min( Ceylan::Sint32 x, Ceylan::Sint32 y )
			throw() ;
	
		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Uint32 Min( Ceylan::Uint32 x, Ceylan::Uint32 y )
			throw() ;
		
				
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::SignedLongInteger Min( Ceylan::SignedLongInteger x,
			Ceylan::SignedLongInteger y ) throw() ;

				
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::UnsignedLongInteger Min( 
			Ceylan::UnsignedLongInteger x,
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
		
		
		
		// Floating point types:

		 
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Float32 Min( Ceylan::Float32 x, Ceylan::Float32 y ) 
			throw() ;

		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Float64 Min( Ceylan::Float64 x, Ceylan::Float64 y ) 
			throw() ;

		
		/**
		 * Returns the minimum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::LongFloat Min( Ceylan::LongFloat x, 
			Ceylan::LongFloat y ) throw() ;
		
		
		
	
		/// Max:

		
		/**
		 * Max operator template available, since using only 
		 * specialized Max functions lead to way too many ambiguities 
		 * and implies several uneasy static_cast.
		 *
		 */
		template<typename T>
		T Max( T x, T y ) throw()
		{
			return ( ( x > y ) ? x: y ) ;
				
		}

		
		// Integer signed and unsigned types:

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Sint8 Max( Ceylan::Sint8 x, Ceylan::Sint8 y ) 
			throw() ;		

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Uint8 Max( Ceylan::Uint8 x, Ceylan::Uint8 y ) 
			throw() ;		
		
		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Sint16 Max( Ceylan::Sint16 x, Ceylan::Sint16 y )
			throw() ;
			
			
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Uint16 Max( Ceylan::Uint16 x, Ceylan::Uint16 y )
			throw() ;
				
				
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Sint32 Max( Ceylan::Sint32 x, Ceylan::Sint32 y )
			throw() ;
	
		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Uint32 Max( Ceylan::Uint32 x, Ceylan::Uint32 y )
			throw() ;
		
				
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::SignedLongInteger Max( Ceylan::SignedLongInteger x,
			Ceylan::SignedLongInteger y ) throw() ;

				
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::UnsignedLongInteger Max( 
			Ceylan::UnsignedLongInteger x,
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
		
		
		
		// Floating point types:

		 
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Float32 Max( Ceylan::Float32 x, Ceylan::Float32 y ) 
			throw() ;

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::Float64 Max( Ceylan::Float64 x, Ceylan::Float64 y ) 
			throw() ;

		
		/**
		 * Returns the maximum value of specified parameters.
		 *
		 * @note No macro used to avoid nasty side effects.
		 *
		 */
		CEYLAN_DLL Ceylan::LongFloat Max( Ceylan::LongFloat x, 
			Ceylan::LongFloat y ) throw() ;
		
			

				
		// Exponentials:
		
		
		/**
		 * Computes the value of e (the base of natural logarithms) 
		 * raised to the power of x.
		 *
		 */
		CEYLAN_DLL Ceylan::Float32 Exp( Ceylan::Float32 x ) throw() ;
		
		
		/**
		 * Computes the value of e (the base of natural logarithms) 
		 * raised to the power of x.
		 *
		 */
		CEYLAN_DLL Ceylan::Float64 Exp( Ceylan::Float64 x ) throw() ;
		
		
		/**
		 * Computes the value of e (the base of natural logarithms)
		 * raised to the power of x.
		 *
		 */
		CEYLAN_DLL Ceylan::LongFloat Exp( Ceylan::LongFloat x ) throw() ;
		
		
		
		// Power:
		
		/// Computes the value of x raised to the power of y.
		CEYLAN_DLL Ceylan::Float32 Pow( Ceylan::Float32 x, Ceylan::Float32 y ) 
			throw() ;
		
		/// Computes the value of x raised to the power of y.		
		CEYLAN_DLL Ceylan::Float64 Pow( Ceylan::Float64 x, 
			Ceylan::Float64 y ) throw() ;
		
		/// Computes the value of x raised to the power of y.		
		CEYLAN_DLL Ceylan::LongFloat Pow( Ceylan::LongFloat x, 
			Ceylan::LongFloat y ) throw() ;
		
		
		
		// Power of two:
		
		/// Computes the value of x² (x*x).
		CEYLAN_DLL Ceylan::Float32 Pow2( Ceylan::Float32 x ) throw() ;
		
		/// Computes the value of x² (x*x).		
		CEYLAN_DLL Ceylan::Float64 Pow2( Ceylan::Float64 x ) throw() ;
		
		/// Computes the value of x² (x*x).		
		CEYLAN_DLL Ceylan::LongFloat Pow2( Ceylan::LongFloat x ) throw() ;
		
			
			
		// Logarithm:
		
		
		/**
		 * Computes the value of the natural logarithm of argument x.
		 *
		 * @note The argument must be strictly positive.
		 *
		 */
		CEYLAN_DLL Ceylan::Float32 Log( Ceylan::Float32 x ) throw() ;
		
		
		/**
		 * Computes the value of the natural logarithm of argument x.
		 *
		 * @note The argument must be strictly positive.
		 *
		 */
		CEYLAN_DLL Ceylan::Float64 Log( Ceylan::Float64 x ) throw() ;
		
		
		/**
		 * Computes the value of the natural logarithm of argument x.
		 *
		 * @note The argument must be strictly positive.
		 *
		 */
		CEYLAN_DLL Ceylan::LongFloat Log( Ceylan::LongFloat x ) throw() ;
		
		
				
		// Square root:
		
		
		/**
		 * Computes the non-negative square root of x, throw an 
		 * exception if x is negative.
		 *
		 */
		CEYLAN_DLL Ceylan::Float32 Sqrt( Ceylan::Float32 x ) 
			throw( MathsException ) ;
		
		
		/**
		 * Computes the non-negative square root of x, throw an 
		 * exception if x is negative.
		 *
		 */
		CEYLAN_DLL Ceylan::Float64 Sqrt( Ceylan::Float64 x ) 
			throw( MathsException ) ;
		
		
		/**
		 * Computes the non-negative square root of x, throw an 
		 * exception if x is negative.
		 *
		 */
		CEYLAN_DLL Ceylan::LongFloat Sqrt( Ceylan::LongFloat x ) 
			throw( MathsException ) ;
			
			
		
		// Some trigonometry.
			
				
				
		// Cosine:

		
		/**
		 * Computes the cosine of 'angle', where angle is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 * @note Uses a look-up table on the ARM9 Nintendo DS.
		 *
		 */
		CEYLAN_DLL Ceylan::Float32 Cos( Ceylan::Float32 angle ) throw() ;
		
		
		/**
		 * Computes the cosine of 'angle', where angle is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		CEYLAN_DLL Ceylan::Float64 Cos( Ceylan::Float64 angle ) throw() ;
		
		
		/**
		 * Computes the cosine of 'angle', where angle is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		CEYLAN_DLL Ceylan::LongFloat Cos( Ceylan::LongFloat angle ) throw() ;
		


		
		// Sine:
		
		
		/**
		 * Computes the sine of 'angle', where angle is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 * @note Uses a look-up table on the ARM9 Nintendo DS.
		 *
		 */
		CEYLAN_DLL Ceylan::Float32 Sin( Ceylan::Float32 angle ) throw() ;
		
		
		/**
		 * Computes the sine of 'angle', where angle is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		CEYLAN_DLL Ceylan::Float64 Sin( Ceylan::Float64 angle ) throw() ;
		
		
		/**
		 * Computes the sine of 'angle', where angle is given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 */
		CEYLAN_DLL Ceylan::LongFloat Sin( Ceylan::LongFloat angle ) throw() ;
		
		
	
	
		// Tangent:
		
		
		/**
		 * Computes the tangent of 'angle', where angle is given in radians.
		 *
		 * @return the tangent.
		 *
		 * @note Uses a look-up table on the ARM9 Nintendo DS.
		 *
		 */
		CEYLAN_DLL Ceylan::Float32 Tan( Ceylan::Float32 angle ) throw() ;
		
		
		/**
		 * Computes the tangent of 'angle', where angle is given in radians.
		 *
		 * @return the tangent.
		 *
		 */
		CEYLAN_DLL Ceylan::Float64 Tan( Ceylan::Float64 angle ) throw() ;
		
		
		/**
		 * Computes the tangent of 'angle', where angle is given in radians.
		 *
		 * @return the tangent.
		 *
		 */
		CEYLAN_DLL Ceylan::LongFloat Tan( Ceylan::LongFloat angle ) throw() ;
		
	
		
		
#if defined(CEYLAN_ARCH_NINTENDO_DS) && CEYLAN_ARCH_NINTENDO_DS == 1


		// Fixed-point counterparts.	

		
		/**
		 * Computes the fixed-point non-negative square root of x.
		 *
		 */
		CEYLAN_DLL Ceylan::Uint32 SqrtFixed( Ceylan::Uint32 x ) throw() ;
		

 		
#ifdef CEYLAN_RUNS_ON_ARM9

		/**
		 * Computes the fixed-point cosine of 'angle', where angle is
		 * itself fixed-point, and given in radians.
		 *
		 * @return a value between -1 and 1.
		 *
		 * @note Uses a look-up table on the ARM9 Nintendo DS.
		 *
		 */
		CEYLAN_DLL Ceylan::Sint32 CosFixed( Ceylan::Sint32 angle ) throw() ;


		/**
		 * Computes the fixed-point sine of 'angle', where angle is
		 * itself fixed-point, and given in radians.
		 * @return a value between -1 and 1.
		 *
		 * @note Uses a look-up table on the ARM9 Nintendo DS.
		 *
		 */
		CEYLAN_DLL Ceylan::Sint32 SinFixed( Ceylan::Sint32 angle ) throw() ;
	

		/**
		 * Computes the fixed-point tangent of 'angle', where angle is
		 * itself fixed-point, and given in radians.
		 *
		 * @return the tangent.
		 *
		 * @note Uses a look-up table on the ARM9 Nintendo DS.
		 *
		 */
		CEYLAN_DLL Ceylan::Sint32 TanFixed( Ceylan::Sint32 angle ) throw() ;
	
		
#endif // CEYLAN_RUNS_ON_ARM9


#endif // defined(CEYLAN_ARCH_NINTENDO_DS) && CEYLAN_ARCH_NINTENDO_DS == 1


		
		/**
		 * Converts an angle expressed in degrees into an angle 
		 * expressed in radians.
		 *
		 */		
		CEYLAN_DLL AngleInRadians DegreeToRadian( 
			AngleInDegrees angleInDegrees ) throw() ;
		
		
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
		CEYLAN_DLL Uint16 NextPowerOfTwo( Uint16 value ) throw() ;
	
		
		/**
		 * Tells whether the specified value is a power of two.
		 *
		 */
		CEYLAN_DLL bool IsAPowerOfTwo( Uint16 value ) throw() ;	


		
		/**
		 * Returns the first multiple of specified multiple greater or equal 
		 * to the specified value.
		 *
		 * @param multiple the multiple to align the value with
		 *
		 * @param value for which the smallestmulitple must be returned.
		 *
		 * @return the smallest multiple of the specified multiple greater or
		 * equal to the specified value.
		 *
		 * @note No overflow checking is done.
		 *
		 * @example NextMultipleOf( 8, 15 ) == 16, and 
		 * NextMultipleOf( 8, 16 ) == 16.
		 *
		 */
		CEYLAN_DLL Uint16 NextMultipleOf( Uint16 multiple, Uint16 value )
			throw() ;
		
			
		

		
		/**
		 * A functor dealing with integers (one integer as parameter, 
		 * returns integer).
		 *
		 */
		class CEYLAN_DLL IntToIntFunctor: public Ceylan::Functor
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
