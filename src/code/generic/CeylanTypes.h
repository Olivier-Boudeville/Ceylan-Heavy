#ifndef CEYLAN_TYPES_H_
#define CEYLAN_TYPES_H_


#include <list>     // for ListSize


// Could be used to define bound : #include <climits>


/**
 * Cross-platform definition of the main Ceylan common basic data types.
 *
 * These basic data types depend on the platform Ceylan will run on : an
 * unsigned short on a platform may be internally the same as an unsigned 
 * int on another platform, it has to be known at compile time.
 *
 * These informations may come from :
 *   - the configure step
 *	 - set of platform specific definitions, as provided by the operating
 * system (ex : stdint.h, inttypes.h, cstdint, etc.)
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
	 * Links can be made between the Ceylan basic data types (ex : Sint16) and
	 * the ones defined by the C language (ex : signed short) and by OpenGL
	 * (ex : GLshort).
	 * Between brackets, C synonyms to data types are specified, then OpenGL
	 * ones (they all start by 'GL'). 
	 *
	 * @note Depending on the platform and the compiler, size of C types may
	 * differ. All sizes are therefore checked at compile time. 
	 *                                         
	 */
	 
	 
	// First, integer types.
	

	/**
	 * Unsigned 8-bit int (unsigned char, GLubyte, GLboolean). 
	 *
	 * Could be as well, if inttypes.h was used : typedef uint8_t Uint8 ;
	 *
	 */
	typedef unsigned char Uint8 ;
	
	

	/**
	 * Signed 8-bit int (signed char, GLbyte). 	
	 *
	 * Could be as well, if inttypes.h was used : typedef int8_t Sint8 ;
	 *
	 */
	typedef signed char	Sint8 ;

	
	/**
	 * Unsigned 16-bit int (unsigned short, GLushort). 
	 *
	 * Could be as well, if inttypes.h was used : typedef uint16_t Uint16 ;
	 *
	 */
	typedef unsigned short Uint16 ;
	
	
	/**
	 * Signed 16-bit int (signed short, GLshort). 
	 *
	 * Could be as well, if inttypes.h was used : typedef int16_t Sint16 ;
	 *
	 */
	typedef signed short Sint16 ;

	
	/**
	 * Unsigned 32-bit int (unsigned int, GLuint, GLenum, GLbitfield). 	
	 *
	 * Could be as well, if inttypes.h was used : typedef uint32_t Uint32 ;
	 *
	 */
	typedef unsigned int Uint32 ;


	/**
	 * Signed 32-bit int (signed int, GLint, GLsizei).
	 *
	 * Could be as well, if inttypes.h was used : typedef int32_t Sint32 ;
	 *
	 */
	typedef signed int Sint32 ;


	/** 
	 * 64-bit data types are not supported on all platforms.
	 *
	 * @see Low level APIs which manage them (ex : SDL, with SDL_types.h).
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
	 * web sites : htpp://libsdl.org and http://ceylan.sourceforge.net
	 *
	 */
   
/*
 * Identifies whether there exists a suitable 64-bit type : 
 * (64-bit datatype is not supported on all platforms)
 *
 */  
#if !defined(__STRICT_ANSI__)
	#if defined(__GNUC__) || defined(__MWERKS__) || defined(__SUNPRO_C) || defined(__DECC)
		#define CEYLAN_64_BIT_TYPE long long
	#elif defined(_MSC_VER) 
			// Visual C++ :
			#define CEYLAN_64_BIT_TYPE __int64
	#endif // if defined(__GNUC__)...
#endif // ! __STRICT_ANSI__


// The 64-bit type is not available on EPOC/Symbian OS :
#ifdef __SYMBIAN32__
	#undef CEYLAN_64_BIT_TYPE
#endif // __SYMBIAN32__



// Now defines accordingly this 64-bit type :
#ifdef CEYLAN_64_BIT_TYPE


	/**
	 * Unsigned 64-bit int. 	
	 *
	 * Could be as well, if inttypes.h was used : typedef uint64_t Uint64 ;
	 *
	 */
	typedef unsigned CEYLAN_64_BIT_TYPE Uint64 ;
		

	/**
	 * Signed 64-bit int.	
	 *
	 * Could be as well, if inttypes.h was used : typedef int64_t Sint64 ;
	 *
	 */
	typedef CEYLAN_64_BIT_TYPE Sint64 ;
	
	
#else // CEYLAN_64_BIT_TYPE


	/// This is really just a hack to prevent the compiler from complaining :
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
 */
	 
	
	
	/**
	 * Now, floating point numbers.
	 *
	 * @note They should be named, checked and compared not only according to
	 * their size in memory, but also according to the precision of their
	 * mantissa, etc.
	 *
	 */
	
	/// 32-bit floats (GLfloat, GLclampf).	 
	typedef float Float32 ;
	
	/// 64-bit doubles a.k.a. long floats (GLdouble, GLclampd).	 
	typedef double Float64 ;
	
	// long double are non standard.
	
	
	/**
	 * Variable able to store element counts, such as the number of CD-ROM
	 * drives attached to a system, for example.
	 *
	 */
	typedef Ceylan::Uint16 Count ;
	
	
	/**
	 * Variable able to store element for list.
	 *
	 * Using 'int' just because a datatype has to be specified, hopefully 
	 * the size does not depend on the listed type.
	 *
	 * @see CeylanStringUtils.h for StringSize
	 *
	 */
	typedef std::list<int>::size_type ListSize ;
	
	
	/// Flags, fields of 32 bits. 
	typedef Ceylan::Uint32 Flags ;
	
	
	/// Variable able to store very large signed integer values.
	typedef signed long SignedLongInteger ;
	
	/// Variable able to store very large positive integer values.
	typedef unsigned long UnsignedLongInteger ;
	
	
	
	/*
	 * Makes sure the data types really have the right sizes, thanks to a 
	 * trick coming from SDL :
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

	CEYLAN_COMPILE_TIME_ASSERT( uint8,   sizeof(Uint8)   == 1 ) ;
	CEYLAN_COMPILE_TIME_ASSERT( sint8,   sizeof(Sint8)   == 1 ) ;
	
	CEYLAN_COMPILE_TIME_ASSERT( uint16,  sizeof(Uint16)  == 2 ) ;
	CEYLAN_COMPILE_TIME_ASSERT( sint16,  sizeof(Sint16)  == 2 ) ;
	
	CEYLAN_COMPILE_TIME_ASSERT( uint32,  sizeof(Uint32)  == 4 ) ;
	CEYLAN_COMPILE_TIME_ASSERT( sint32,  sizeof(Sint32)  == 4 ) ;
	CEYLAN_COMPILE_TIME_ASSERT( float32, sizeof(Float32) == 4 ) ;
	
	CEYLAN_COMPILE_TIME_ASSERT( uint64,  sizeof(Uint64)  == 8 ) ;
	CEYLAN_COMPILE_TIME_ASSERT( sint64,  sizeof(Sint64)  == 8 ) ;
	CEYLAN_COMPILE_TIME_ASSERT( float64, sizeof(Float64) == 8 ) ;

}


#endif // CEYLAN_TYPES_H_
