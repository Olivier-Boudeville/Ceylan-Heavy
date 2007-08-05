#ifndef CEYLAN_TYPES_H_
#define CEYLAN_TYPES_H_


#include <list>     // for ListSize


// Could be used to define bound: #include <climits>


/**
 * Cross-platform definition of the main Ceylan common basic data types.
 *
 * These basic data types depend on the platform Ceylan will run on: an
 * unsigned short on a platform may be internally the same as an unsigned 
 * int on another platform, it has to be known at compile time.
 *
 * These informations may come from:
 *   - the configure step
 *	 - set of platform specific definitions, as provided by the operating
 * system (ex: stdint.h, inttypes.h, cstdint, etc.)
 *
 */
 

/*
 * inttypes.h is to be preferred to stdint.h, since the former is more
 * portable and includes the latter when appropriate.
 *
 * Even inttypes.h is not included here since it is not present on all
 * platforms. 
 * Otherwise we would need to use here a CEYLAN_USES_INTTYPES_H, which
 * would force us to include beforehand CeylanConfig.h (our "config.h").
 * It would not be satisfactory in installed headers, because of risks of
 * name clashes.
 *
 * For the moment, we dodge the issue and use fairly the same tricks as SDL
 * (http://www.libsdl.org, see SDL_types.h).
 *
 * cstdint, defined in Boost (http://www.boost.org), is sadly not existing 
 * yet in the C++ standard.
 *
 */
//#include <inttypes.h>   // for int8_t and others



namespace Ceylan
{
	
	
	/*
	 * Links can be made between the Ceylan basic data types (ex: Sint16) and
	 * the ones defined by the C language (ex: signed short) and by OpenGL
	 * (ex: GLshort).
	 *
	 * C synonyms to data types are specified, then OpenGL ones (they all 
	 * start with'GL'). 
	 *
	 * For each Ceylan basic numerical datatype, its lower and higher accepted
	 * values are specified. For example, if x is a Uint8, then:
	 * Uint8Min <= x <= Uint8Max (hence Min and Max bounds are included).
	 *
	 * @note Depending on the platform and the compiler, size of C types may
	 * differ. All sizes are therefore checked at compile time (see
	 * CEYLAN_COMPILE_TIME_ASSERT) and at run-time (see
	 * testCeylanBasicDatatypes.cc). 
	 *
	 * @note For example, int (signed or not) is 4 bytes (32 bits) on 32-bit
	 * processors, and 2 bytes (16 bits) on 16-bit processors. With 4-bit
	 * processors, it will be probably a mess again.
	 *                                         
	 */
	 
	 
	// First, integer types.
		
	

	/**
	 * Signed 8-bit int (signed char, GLbyte). 	
	 * 
	 * Ranges from -128 to 127 (both included).
	 *
	 * Could be as well, if inttypes.h was used: typedef int8_t Sint8 ;
	 *
	 */
	typedef signed char	Sint8 ;

	extern CEYLAN_DLL Ceylan::Sint8 Sint8Min /* = -128 */  ;
	extern CEYLAN_DLL Ceylan::Sint8 Sint8Max /* =  127 */  ;


	/**
	 * Unsigned 8-bit int (unsigned char, GLubyte, GLboolean).
	 * 
	 * Ranges from 0 to 255 (both included).
	 *
	 * Could be as well, if inttypes.h was used: typedef uint8_t Uint8 ;
	 *
	 */
	typedef unsigned char Uint8 ;
	
	extern CEYLAN_DLL Ceylan::Uint8 Uint8Min /* =   0 */ ;
	extern CEYLAN_DLL Ceylan::Uint8 Uint8Max /* = 255 */ ;


	/**
	 * char, signed char, and unsigned char are different types.
	 *
	 * Hence if:
	 *
	 * signed char * u ;
	 * char * v = 0 ;
	 *
	 * then
	 * u = v ;
	 *
	 * or 
	 *
	 * u = static_cast<signed char *>( v ) ; 
	 *
	 * will trigger a compiler error, invalid conversion or static_cast
	 * from type `char*' to type `signed char*'
	 * 
	 * Hence we defined Ceylan::Byte to trace the fact we are using internally
	 * char (returned for example by the c_str() method of std::string) 
	 * without knowing whether it is signed or not, hence without being able
	 * to specify Ceylan::Uint8 or Ceylan::Sint8.
	 *
	 * At least under 32-bit GNU/Linux, Ceylan::Byte is like a signed char.
	 *
	 */
	typedef char Byte ;
	 
	extern CEYLAN_DLL Ceylan::Byte ByteMin /* should be -128 */ ;
	extern CEYLAN_DLL Ceylan::Byte ByteMax /* should be 127 */ ;



	/**
	 * Signed 16-bit int (signed short, GLshort). 
	 *
	 * Ranges from -32 768 to 32 767 (both included).
	 *
	 * Could be as well, if inttypes.h was used: typedef int16_t Sint16 ;
	 *
	 */
	typedef signed short Sint16 ;

	extern CEYLAN_DLL Ceylan::Sint16 Sint16Min /* = -32768  */ ;
	extern CEYLAN_DLL Ceylan::Sint16 Sint16Max /* =  32767  */ ;


	/**
	 * Unsigned 16-bit int (unsigned short, GLushort). 
	 *
	 * Ranges from 0 to 65 535 (both included).
	 *
	 * Could be as well, if inttypes.h was used: typedef uint16_t Uint16 ;
	 *
	 */
	typedef unsigned short Uint16 ;
	
	extern CEYLAN_DLL Ceylan::Uint16 Uint16Min /* =     0 */ ;
	extern CEYLAN_DLL Ceylan::Uint16 Uint16Max /* = 65535 */ ;
	


	
	/**
	 * Signed 32-bit int (signed int, GLint, GLsizei).
	 *
	 * Ranges from -2 147 483 648 to 2 147 483 647 (both included).
	 *
	 * Could be as well, if inttypes.h was used: typedef int32_t Sint32 ;
	 *
	 */
	typedef signed int Sint32 ;


	/*
	 * Actually is -2147483648 but is incremented since 
	 * 'this decimal constant is unsigned only in ISO C90'.
	 *
	 */
	extern CEYLAN_DLL Ceylan::Sint32 Sint32Min /* = -2147483648  */ ;
	extern CEYLAN_DLL Ceylan::Sint32 Sint32Max /* =  2147483647  */ ;
	
	
	/**
	 * Unsigned 32-bit int (unsigned int, GLuint, GLenum, GLbitfield). 	
	 *
	 * Ranges from 0 to 4 294 967 295 (both included).
	 *
	 * Could be as well, if inttypes.h was used: typedef uint32_t Uint32 ;
	 *
	 */
	typedef unsigned int Uint32 ;

	extern CEYLAN_DLL Ceylan::Uint32 Uint32Min /* = 0  */ ;
	extern CEYLAN_DLL Ceylan::Uint32 Uint32Max /* = 4294967295 */ ;
	


		
	/**
	 * Variable able to store very large signed integer values.
	 *
	 * Actual Min/Max ranges depend on the platform.
	 *
	 * @see testCeylanBasicDatatypes.cc
	 *
	 * At least under 32-bit GNU/Linux, Ceylan::SignedLongInteger uses 4 bytes.
	 *
	 */
	typedef signed long SignedLongInteger ;
	
	extern CEYLAN_DLL Ceylan::SignedLongInteger SignedLongIntegerMin ;
	extern CEYLAN_DLL Ceylan::SignedLongInteger SignedLongIntegerMax ;
	
	
	/**
	 * Variable able to store very large positive integer values.
	 *
	 * Actual Min/Max ranges depend on the platform.
	 *
	 * @see testCeylanBasicDatatypes.cc
	 *
	 * At least under 32-bit GNU/Linux, Ceylan::UnsignedLongIntegerMin uses 
	 * 4 bytes.
	 *
	 */
	typedef unsigned long UnsignedLongInteger ;
	
	extern CEYLAN_DLL Ceylan::UnsignedLongInteger UnsignedLongIntegerMin ;
	extern CEYLAN_DLL Ceylan::UnsignedLongInteger UnsignedLongIntegerMax ;
	
	

	/** 
	 * 64-bit data types are not supported on all platforms.
	 *
	 * @see Low level APIs which manage them (ex: SDL, with SDL_types.h).
	 *
	 * @note Our definitions for 64-bit data types come directly from SDL
	 * (http://www.libsdl.org), many thanks to the SDL community !
	 *
	 * We did not want Ceylan to depend on any other non-system library, we
	 * therefore chose not to include SDL headers here. 
	 * Moreover this concern is very close to the platform, and does not
	 * depend on other parts of SDL. We therefore adapted the SDL code.
	 *
	 * Ceylan uses for this topic The Simple DirectMedia Layer library, that
	 * is currently available under the GNU Lesser General Public License 
	 * (LGPL) version 2 or newer, which can be found online at:
	 * http://www.gnu.org/copyleft/lgpl.html.
	 * This library is therefore included under the terms of the LGPL license.
	 * 
	 * @see COPYING.LIB under our src directory.
	 * The source code for this 1.2.9 version of SDL and the full source of
	 * Ceylan are available from the internet, in their respective official 
	 * web sites: htpp://libsdl.org and http://ceylan.sourceforge.net
	 *
	 */
   
/*
 * Identifies whether there exists a suitable 64-bit type: 
 * (64-bit datatype is not supported on all platforms)
 *
 */  
#if !defined(__STRICT_ANSI__)
	#if defined(__GNUC__) || defined(__MWERKS__) || defined(__SUNPRO_C) || defined(__DECC)
		#define CEYLAN_64_BIT_TYPE long long
	#elif defined(_MSC_VER) 
			// Visual C++:
			#define CEYLAN_64_BIT_TYPE __int64
	#endif // if defined(__GNUC__)...
#endif // ! __STRICT_ANSI__


// The 64-bit type is not available on EPOC/Symbian OS:
#ifdef __SYMBIAN32__
	#undef CEYLAN_64_BIT_TYPE
#endif // __SYMBIAN32__



// Now defines accordingly this 64-bit type:
#ifdef CEYLAN_64_BIT_TYPE


	/**
	 * Unsigned 64-bit int. 	
	 *
	 * Ranges not specified for the moment. 
	 *
	 * Could be as well, if inttypes.h was used: typedef uint64_t Uint64 ;
	 *
	 */
	typedef unsigned CEYLAN_64_BIT_TYPE Uint64 ;
		

	/**
	 * Signed 64-bit int.	
	 *
	 * Ranges not specified for the moment. 
	 *
	 * Could be as well, if inttypes.h was used: typedef int64_t Sint64 ;
	 *
	 */
	typedef signed CEYLAN_64_BIT_TYPE Sint64 ;
	
	
#else // CEYLAN_64_BIT_TYPE

#define CEYLAN_FAKES_64_BIT_TYPE

	/// This is really just a hack to prevent the compiler from complaining:
	typedef struct 
	{
	
		Ceylan::Uint32 hi ;
		Ceylan::Uint32 lo ;
		
	} Uint64, Sint64 ;
	
	
#endif // CEYLAN_64_BIT_TYPE


/*
 * From here normally Ceylan::Uint64 and Ceylan::Sint64 can be used in 
 * all cases.
 *
 *
 */
	 
	
	
	/**
	 * Now, floating point numbers.
	 *
	 * @note They should be named, checked and compared not only according to
	 * their size in memory, but also according to the precision of their
	 * mantissa, etc.
	 *
	 */
	
	
	/**
	 * 32-bit floats (GLfloat, GLclampf).	 
	 *
	 * Ranges from -3.4*10^-38 to 3.4*10^38 (both included).
	 *
	 * Mantissa is coded in 23 bits, exponent in 8 bits, sign in 1 bit.
	 *
	 * Precision is at least 6 digits after the decimal point. 
	 * 
	 */
	typedef float Float32 ;
	
	extern CEYLAN_DLL Ceylan::Float32 Float32Min /* = -3.4E-38 */ ;
	extern CEYLAN_DLL Ceylan::Float32 Float32Max /* =  3.4E38  */ ;
	
	
	/**
	 * 64-bit doubles a.k.a. long floats (GLdouble, GLclampd).	 
	 *
	 * Ranges from -1.7*10^-308 to 1.7*10^308 (both included).
	 *
	 * Mantissa is coded in 52 bits, exponent in 11 bits, sign in 1 bit.
	 *
	 * Precision is at least 15 digits after the decimal point. 
	 *
	 */
	typedef double Float64 ;
	
	extern CEYLAN_DLL Ceylan::Float64 Float64Min /* = -1.7E-308 */ ;
	extern CEYLAN_DLL Ceylan::Float64 Float64Max /* =  1.7E308  */ ;
	


	/*
	 * Very large floating point values are not encapsulated with regard to
	 * their bit widths since they depend too much on the underlying platform.
	 *
	 * For example, GNU/Linux IA32 thinks `long double' is 96-bits (while the
	 * real number of used bits is only 80, both in processor and in memory),
	 * whereas HP-UX thinks it is 128-bits, other 80, etc.
	 *
	 * So the largest fixed-size floating point value used by Ceylan is
	 * Ceylan::Float64 (64-bits).
	 *
	 * One can use nevertheless (Un)SignedLongFloat, provided she does not
	 * rely on any specific bit width.
	 *
	 */ 
	 
	 
	/**
	 * Variable able to store very large (signed) floating-point values.
	 *
	 * Actual Min/Max ranges depend on the platform.
	 *
	 * @see testCeylanBasicDatatypes.cc
	 *
	 * At least under 32-bit GNU/Linux, Ceylan::LongFloat uses 12 bytes.
	 *
	 */
	typedef long double LongFloat ;
	
	extern CEYLAN_DLL Ceylan::LongFloat LongFloatMin ;
	extern CEYLAN_DLL Ceylan::LongFloat LongFloatMax ;
		
	
	
	/**
	 * 80-bit long double are non standard.
	 *
	 * Ranges from -3.4*10^-4932 to 3.4*10^4932 (both included).
	 *
	 * Mantissa is coded 64 in bits, exponent in 15 bits, sign in 1 bit.
	 *
	 * Precision is at least 17 digits after the decimal point. 
	 *
	 */
	//typedef long double Float80 ;
	
	//extern CEYLAN_DLL Ceylan::Float80 Float80Min /* = -3.4E-4932 */ ;

	/*
	 * Actually is 3.4E4932 but is set to a lower value (the highest 
	 * possible one) since 'floating constant exceeds range of double'.
	 *
	 */
	//extern CEYLAN_DLL Ceylan::Float80 Float80Max 
	//	/*= 3.4E4932 in theory, 1.7E308 actually */ ;
	
	
	
	/**
	 * 96-bit long double are non standard.
	 *
	 */
	//typedef long double Float96 ;

	
	//extern CEYLAN_DLL Ceylan::Float96 Float96Min /* = -3.4E-4932 */ ;

	/*
	 * Actually is 3.4E4932 but is set to a lower value (the highest 
	 * possible one) since 'floating constant exceeds range of double'.
	 *
	 */
	//extern CEYLAN_DLL Ceylan::Float96 Float96Max 
	//	/*= 3.4E4932 in theory, 1.7E308 actually */ ;
	
	
	
	
	/**
	 * Variable able to store element counts, such as the number of CD-ROM
	 * drives attached to a system, for example.
	 *
	 */
	typedef Ceylan::Uint16 Count ;
	
	
	/**
	 * Variable able to store element for list.
	 *
	 * Using 'Ceylan::Uint16' just because a datatype has to be specified,
	 * hopefully the size does not depend on the listed type.
	 *
	 * @see CeylanStringUtils.h for StringSize
	 *
	 */
	typedef std::list<Ceylan::Uint16>::size_type ListSize ;	
	
	
	/// Flags, fields of 32 bits. 
	typedef Ceylan::Uint32 Flags ;
	
	
	/*
	 * Makes sure the data types really have the right sizes, thanks to a 
	 * trick coming from SDL:
	 * if the size of tested type does not match desired value, expression
	 * 'sizeof(t)  == n' is false (0) and the macro attempts to define an 
	 * array of '0*2 -1' = -1 element, which results in a compilation error,
	 * such as 'error: size of array
	 * `CEYLAN_stop_wrong_size_for_uint64' is negative'.
	 *
	 * If the sizes are the expected ones, all arrays have exactly one element
	 * and the compiler does not complain.
	 *
	 */
#define CEYLAN_COMPILE_TIME_ASSERT(name, x)               \
		typedef int CEYLAN_stop_wrong_size_for_ ## name[(x) * 2 - 1]

	CEYLAN_COMPILE_TIME_ASSERT( uint8,   sizeof(Uint8)   == 1  ) ;
	CEYLAN_COMPILE_TIME_ASSERT( sint8,   sizeof(Sint8)   == 1  ) ;
	
	CEYLAN_COMPILE_TIME_ASSERT( uint16,  sizeof(Uint16)  == 2  ) ;
	CEYLAN_COMPILE_TIME_ASSERT( sint16,  sizeof(Sint16)  == 2  ) ;
	
	CEYLAN_COMPILE_TIME_ASSERT( uint32,  sizeof(Uint32)  == 4  ) ;
	CEYLAN_COMPILE_TIME_ASSERT( sint32,  sizeof(Sint32)  == 4  ) ;
	CEYLAN_COMPILE_TIME_ASSERT( float32, sizeof(Float32) == 4  ) ;
	
	CEYLAN_COMPILE_TIME_ASSERT( uint64,  sizeof(Uint64)  == 8  ) ;
	CEYLAN_COMPILE_TIME_ASSERT( sint64,  sizeof(Sint64)  == 8  ) ;
	CEYLAN_COMPILE_TIME_ASSERT( float64, sizeof(Float64) == 8  ) ;

	//CEYLAN_COMPILE_TIME_ASSERT( Float96, sizeof(Float96) == 12 ) ;
	 

}


#endif // CEYLAN_TYPES_H_
