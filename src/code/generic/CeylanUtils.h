#ifndef CEYLAN_UTILS_H_
#define CEYLAN_UTILS_H_


#include "CeylanLibtoolVersion.h"  // for LibtoolVersion
#include "CeylanException.h"       // for inheritance
#include "CeylanTypes.h"           // for Ceylan::Uint16, Ceylan::Sint16
#include "CeylanHeaderVersion.h"   // for actual header-embedded libtool version

#include <string>
#include <list>



/**
 * This part of the Ceylan namespace gathers some convenient conventions 
 * to be widely used.
 *
 */

namespace Ceylan
{
	
	
	// Some generic and useful definitions.
	
	
	/// Returns the version of the Ceylan library currently linked.
	const Ceylan::LibtoolVersion & GetVersion() throw() ;
	
	
	/** 
	 * Allows to ensure that the actual Ceylan library being linked with is
	 * compatible with the one expected by the code that will use it (be it a
	 * library itself or a program).
	 *
	 * The Ceylan version is directly encoded in the library, whereas the 
	 * version expected by a user program is found in this Ceylan header file,
	 * according to the version being used to compile it.
	 *
	 * Use this macro in your application that way : 
	 * 'CHECK_CEYLAN_VERSIONS() ;'
	 * for example in the first lines of your 'main' function. Of course the 
	 * main Ceylan header file ('Ceylan.h') should have been included
	 * previously.
	 *
	 * This is a macro since it has to be evaluated within the user code 
	 * environment, not when the Ceylan library is built.
	 *
	 * 
	 */
	#define CHECK_CEYLAN_VERSIONS()                                            \
        Ceylan::LibtoolVersion headerVersion(                                  \
			Ceylan::actualCeylanHeaderLibtoolVersion ) ;                       \
        if ( ! /* library version */                                           \
                Ceylan::GetVersion().isCompatibleWith( headerVersion ) )       \
            Ceylan::emergencyShutdown(                                         \
                "Ceylan library version currently linked ("                    \
                + Ceylan::GetVersion().toString()                              \
                + ") is not compatible with the one read from the Ceylan "     \
                "header files used to compile this application ("              \
                + headerVersion.toString() + "), aborting." ) ;
	
				
				
	/**
	 * Swaps the two bytes of the specified 16 bit argument.
	 *
	 * @see SDL (SDL_endian.h) for assembly-optimized versions.
	 *
	 */
	inline Ceylan::Uint16 swapBytes( Ceylan::Uint16 arg ) throw()
	{
		return ( (arg<<8) | (arg>>8) ) ;
	}
	
	
	/// Exception raised by common utils services. 
	class UtilsException : public Exception
	{
	
		public:
		
			UtilsException( const std::string & message ) throw() : 
				Exception( message )
			{		
			
			}
			
			virtual ~UtilsException() throw()
			{
			
			}	
		
	} ;	



	/**
	 * Exception raised wheneve the parsing of a command line failed, for
	 * exemple when a given option requires more arguments than available 
	 * in the command line.
	 *
	 * @see testCeylanCommandLineOptions.cc
	 *
	 */
	class CommandLineParseException : public UtilsException
	{
	
		public:
		
			CommandLineParseException( const std::string & message ) throw() : 
				UtilsException( message )
			{		
			
			}
			
			virtual ~CommandLineParseException() throw()
			{
			
			}	
		
	} ;	


	/**
	 * Parses arguments given to an executable (argc, argv) and translates it
	 * into an executable name and a set of option words, stored in specified
	 * list of strings.
	 *
	 * Arguments are easier to take into account this way.
	 *
	 * @param readExecutableName the string where this function will store 
	 * the executable name.
	 *
	 * @param readOptions the list of strings where this function will store 
	 * the options, one word by string, in the same order as they were in 
	 * the command line.
	 *
	 * @param argumentCount the number of arguments (argc).
	 *
	 * @param argumentVector the array of option strings (char ** argv).
	 *
	 * @see testCeylanCommandLineOptions.cc
	 *
	 */
	void parseCommandLineOptions( std::string & readExecutableName ,
		std::list<std::string> & readOptions, 
		Ceylan::Uint16 argumentCount, char ** argumentVector ) throw() ;
		
	

	/// Return value to be used on success.
	extern const Ceylan::Sint16 ExitSuccess ;


	/// Return value to be used on failure (any non zero value could be used).
	extern const Ceylan::Sint16 ExitFailure ;


	/**
	 * Return value to be used on debug assertion failure. 
	 * It is returned only by "#if CEYLAN_DEBUG"-enclosed sections.
	 * 
	 * This value should never been actually returned, since it would mean
	 * a real basic assumption was unexpectedly not met.
	 *
	 */
	extern const Ceylan::Sint16 ExitDebugFailure ;
	
	
	/**
	 * Stops immediatly the program, without performing any cleanup.
	 *
	 * @note Call me when run-time abnormal behaviours occurs, such
	 * as state incoherence, that shows that some code is faulty. 
	 *
	 * @note That kind of function is useful, since raising a special
	 * exception would oblige declaring it everywhere where runtime
	 * checks are performed, even if that kind of checking is often
	 * done only in debug mode.
	 *
	 */
	void emergencyShutdown( const std::string & message ) throw()
		__attribute__ ((noreturn)) ; 
	


	// Some keyboard events facilities.


	/**
	 * UNIX port of well-known kbhit.
	 *
	 * Taken from http://www.geocities.com/SiliconValley/Park/4572/tips.html.
	 *
	 * Thanks Petey Leinonen !
	 *
	 */
	bool keyboardHit() throw() ;


	/// Corresponds to a read character.
	typedef Ceylan::Sint32 KeyChar ;
	
	
	/**
	 * Portable old-fashioned getchar.
	 *
	 * UNIX port taken from
	 * http://www.geocities.com/SiliconValley/Park/4572/tips.html.
	 *
	 * Thanks Petey Leinonen !
	 *
	 */
	KeyChar getChar() throw() ;


	/**
	 * Default string to display when waiting for a key to be hit.
	 *
	 * Ex : "Press any key to continue".
	 *
	 */
	extern const std::string DefaultWaitForKeyMessage ;


	/**
	 * Waits for a key to be pressed.
	 *
	 * @return the hit key as getChar read it.
	 *
	 * @note One should not use for example : 
	 * <code>"Hit key is : " + waitForKey()</code> since waitForKey returns
	 * a numerical value. Instead, use :
	 * <code>"Hit key is : " + toString( waitForKey() )</code>
	 *
	 */
	KeyChar waitForKey( const std::string & message = DefaultWaitForKeyMessage )
		throw() ;

	
	/**
	 * Template function splitting a container according to a delimiter.
	 *
	 * This function takes a container (e.g. a string), a delimiter (e.g. a
	 * char), and appends to the list of containers the ones that result from
	 * splitting the first container.
	 *
	 * Its obvious use is for splitting strings, but the function has been made
	 * a template in order to be able to :
	 * 1. split strings using any kind of char
	 * 2. split anything else (list of events, for example)
	 *
	 * The function may be extended by allowing a container to be used as the
	 * delimiter (e.g. using a word as a delimiter).
	 *
	 * @param toSplit container to be split.
	 *
	 * @param delimiter delimiter between the slices.
	 *
	 * @param result list of containers to which will be appended the results 
	 * of the split.
	 *
	 * @author Marc Petit.
	 *
	 */
	template<class T, class Element>
	void split(	const T & toSplit, const Element & delimiter, 
		std::list<T> & result ) throw() 
	{
	
		typename T::const_iterator beginOfSlice = toSplit.begin() ;
		typename T::const_iterator endOfSlice   = toSplit.begin() ;

		typename T::const_iterator endOfToSplit = toSplit.end() ;

		while ( endOfSlice != endOfToSplit )
		{
			endOfSlice = std::find( beginOfSlice, endOfToSplit, delimiter ) ;

			T temp ;

			std::copy( beginOfSlice, endOfSlice, std::back_inserter( temp ) ) ;

			result.push_back( temp ) ;

			beginOfSlice = endOfSlice + 1 ;
		}
		
	}


	/**
	 * Template function splitting a container according to a predicate.
	 * 
	 * This function takes a container (e.g. a string), a predicate (see
	 * std::find_if), and appends to the list of containers the ones that 
	 * result from splitting the first container.
	 *
	 * Being based on the same algorithm as Split, this function does not
	 * include the instances found to be "delimiters".
	 *
	 * @param toSplit container to be split.
	 *
	 * @param predicate predicate used to differentiate slices.
	 *
	 * @param result list of containers to which will be appended the results 
	 * of the split.
	 *
	 * @example :
	 * <pre>
	 *	const std::string test( "This is a test." ) ;
	 *	const char d = ' ' ;
	 *
	 *	typedef std::list<std::string> List ;
	 *	List result ;
	 *
	 *	Split( test, d, result ) ;
	 *	
	 *	int n = 0 ;
	 *	for( List::iterator i = result.begin(); i != result.end(); ++i, ++n )
	 *	{
	 *		std::cout << "item " << n << " : >" << *i << "<" << std::endl ;
	 *  }
	 *
	 * </pre>
	 *
	 * @see Split
	 *
	 * @see std::find_if
	 *
 	 * @author Marc Petit.
	 *
	 */
	template<class T, class Predicate>
	void split_if( const T & toSplit, Predicate & predicate, 
		std::list<T> & result ) throw()
	{
	
		typename T::const_iterator beginOfSlice = toSplit.begin() ;
		typename T::const_iterator endOfSlice   = toSplit.begin() ;

		typename T::const_iterator endOfToSplit = toSplit.end() ;

		while( endOfSlice != endOfToSplit )
		{
			endOfSlice = std::find_if( beginOfSlice, endOfToSplit, predicate ) ;

			T temp ;

			std::copy( beginOfSlice, endOfSlice, std::back_inserter( temp ) ) ;

			result.push_back( temp ) ;

			beginOfSlice = endOfSlice + 1 ;
		}
		
	}
	
	
	/**
	 * Prints in standard output a checkpoint message, with a checkpoint count
	 * incremented at each call, starting from 1.
	 *
	 * @note Might be useful for light debugging.
	 *
	 */
	void checkpoint( const std::string & message = "" ) throw() ;
	
	
	/**
	 * Prints in standard output a breakpoint message, with a breakpoint count
	 * incremented at each call, starting from 1, and then waits for the user 
	 * to press a key.
	 *
	 * @note Might be useful for light debugging.
	 *
	 */
	void breakpoint( const std::string & message = "" ) throw() ;
	
			 
}



#endif // CEYLAN_UTILS_H_
