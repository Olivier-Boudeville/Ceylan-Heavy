/*
 * Copyright (C) 2003-2013 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option)
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef CEYLAN_UTILS_H_
#define CEYLAN_UTILS_H_


#include "CeylanLibtoolVersion.h"  // for LibtoolVersion
#include "CeylanException.h"       // for inheritance
#include "CeylanTypes.h"           // for Ceylan::Uint16, Ceylan::Sint16
#include "CeylanHeaderVersion.h"   // for actualCeylanHeaderLibtoolVersion


#include <string>
#include <list>
#include <algorithm> // for std::find



/*
 * Sometimes, the most convenient way of specifying configuration options is to
 * pass them in the build command-line, rather than for example generating an
 * ad-hoc file. We thus can use -DFOO=1.2 to have FOO in the code be replaced by
 * 1.2.
 *
 * However, when a path is to be set that way, a problem arises:
 * -DFOO_PATH=/some/path/foo-0.1.4 will not be accepted (ex: "error: too many
 * decimal points in number"), as the constant will then be read as a
 * floating-point value.
 *
 * The following preprocessor hack allows to overcome that issue. It *needs* the
 * two macros.
 *
 * Example of use: const string MyPath = CEYLAN_STRINGIFY(FOO_PATH) +
 *  std::string( "/share/foo/" ) ;
 *
 */
#define CEYLAN_STRINGIFY_HELPER(s) #s
#define CEYLAN_STRINGIFY(s) CEYLAN_STRINGIFY_HELPER(s)



/**
 * This part of the Ceylan namespace gathers some convenient conventions to be
 * widely used.
 *
 */

namespace Ceylan
{



	// Some generic and useful definitions.



	/**
	 * Returns the version of the Ceylan library currently linked.
	 *
	 * @throw Ceylan::Exception if the operation failed.
	 *
	 */
	CEYLAN_DLL const Ceylan::LibtoolVersion & GetVersion() ;



	/**
	 * Allows to ensure that the actual Ceylan library being linked with is
	 * compatible with the one expected by the code that will use it (be it a
	 * library itself or a program).
	 *
	 * The Ceylan version is directly encoded in the library, whereas the
	 * version expected by a user program is found in this Ceylan header file,
	 * according to the version being used to compile it.
	 *
	 * Use this macro in your application that way:
	 * 'CHECK_CEYLAN_VERSIONS() ;'
	 * for example in the first lines of your 'main' function. Of course the
	 * main Ceylan header file ('Ceylan.h') should have been included
	 * previously.
	 *
	 * This is a macro since it has to be evaluated within the user code
	 * environment, not when the Ceylan library is built.
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
	inline Ceylan::Uint16 swapBytes( Ceylan::Uint16 arg )
	{

		return static_cast<Ceylan::Uint16>( (arg<<8) | (arg>>8) ) ;

	}



	/// Exception raised by common utils services.
	class CEYLAN_DLL UtilsException : public Exception
	{

		public:

			UtilsException( const std::string & message ):
				Exception( message )
			{

			}


			virtual ~UtilsException() throw()
			{

			}

	} ;



	/**
	 * Exception raised wheneve the parsing of a command line failed, for
	 * example when a given option requires more arguments than available in the
	 * command line.
	 *
	 * @see testCeylanCommandLineOptions.cc
	 *
	 */
	class CEYLAN_DLL CommandLineParseException : public UtilsException
	{

		public:

			CommandLineParseException( const std::string & message ):
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
	 * @param readExecutableName the string where this function will store the
	 * executable name.
	 *
	 * @param readOptions the list of strings where this function will store the
	 * options, one word by string, in the same order as they were in the
	 * command line.
	 *
	 * @param argumentCount the number of arguments (argc).
	 *
	 * @param argumentVector the array of option strings (char ** argv).
	 *
	 * @see testCeylanCommandLineOptions.cc
	 *
	 */
	CEYLAN_DLL void parseCommandLineOptions( std::string & readExecutableName ,
		std::list<std::string> & readOptions,
		Ceylan::Uint16 argumentCount, char ** argumentVector ) ;



	// Codes to be returned by executables on exit.
	typedef Ceylan::Sint16 ExitCode ;


	/// Return value to be used on success.
	extern CEYLAN_DLL const ExitCode ExitSuccess ;



	/// Return value to be used on failure (any non zero value could be used).
	extern CEYLAN_DLL const ExitCode ExitFailure ;



	/**
	 * Return value to be used on debug assertion failure.
	 * It is returned only by "#if CEYLAN_DEBUG"-enclosed sections.
	 *
	 * This value should never been actually returned, since it would mean a
	 * real basic assumption was unexpectedly not met.
	 *
	 */
	extern CEYLAN_DLL const ExitCode ExitDebugFailure ;



	/**
	 * Performs a full normal clean-up and shutdown of the ceylan library. It
	 * should be called as one of the last instructions of the user program (as
	 * no Ceylan service should be used after), for instance just before
	 * returning the exit code.
	 *
	 * @note This call is fully optional (most, if not all Ceylan-using programs
	 * could work as well without it), however it allows to end the program
	 * perfectly cleanly, i.e. with no memory leak nor blocks that are still
	 * reachable due to Ceylan. That is for example useful to run Valgrind
	 * against a Ceylan-using program with no specific suppression file while
	 * still seeing neither memory leaks nor still-reachable blocks.
	 *
	 * @see also emergencyShutdown
	 *
	 */
	CEYLAN_DLL void shutdown() ;



	/**
	 * Stops immediatly the program, without performing any cleanup. Never
	 * returns.
	 *
	 * @note Call me when run-time abnormal behaviours occurs, such as state
	 * incoherence, that shows that some code is faulty.
	 *
	 * @note That kind of function is useful in the cases (that should be
	 * avoided) where exception specifications are used.
	 *
	 * @see also shutdown
	 *
	 */
	CEYLAN_DLL void emergencyShutdown( const std::string & message )

#ifndef CEYLAN_RUNS_ON_WINDOWS
				/*
				 * g++ (gcc) needs this __attribute__ (otherwise a blocking
				 * warning is issued), but Visual C++ does not understand it.
				 *
				 * As we are here in a public header file, only the
				 * CEYLAN_RUNS_ON_WINDOWS configuration-specific preprocessor
				 * symbol is available here.
				 *
				 */
		__attribute__ ((noreturn))

#endif // CEYLAN_RUNS_ON_WINDOWS
	;




	// Some keyboard events facilities.



	/**
	 * Tells whether a key has been hit, and consequently is waiting to be
	 * read. All key presses are taken into account, no only the new transitions
	 * from released to pressed.
	 *
	 * @throw UtilsException if the operation is not available or could not be
	 * performed correctly.
	 *
	 * @note For the Nintendo DS, the libdns scanKeys function is supposed to be
	 * called regularly outside of this function (ex: once per main loop).
	 *
	 */
	CEYLAN_DLL bool keyboardHit() ;



	/// Corresponds to a read character.
	typedef Ceylan::Sint32 KeyChar ;



#if defined(CEYLAN_ARCH_NINTENDO_DS) && CEYLAN_ARCH_NINTENDO_DS == 1


	// Mapping for Nintendo DS buttons.


	/// Corresponds to a Nintendo DS binary input device.
	typedef Ceylan::Uint32 DSBinaryInput ;



	/// The keypad 'X' of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput ButtonX ;


	/// The keypad 'Y' of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput ButtonY ;



	/// The keypad 'A' of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput ButtonA ;


	/// The keypad 'B' of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput ButtonB ;



	/// The keypad 'START' of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput ButtonStart ;


	/// The keypad 'SELECT' of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput ButtonSelect ;



	/// The keypad 'left' of the Nintendo DS cursor.
	extern CEYLAN_DLL const DSBinaryInput ButtonLeft ;


	/// The keypad 'right' of the Nintendo DS cursor.
	extern CEYLAN_DLL const DSBinaryInput ButtonRight ;



	/// The keypad 'up' of the Nintendo DS cursor.
	extern CEYLAN_DLL const DSBinaryInput ButtonUp ;


	/// The keypad 'down' of the Nintendo DS cursor.
	extern CEYLAN_DLL const DSBinaryInput ButtonDown ;



	/// The left shoulder button of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput ShoulderButtonLeft ;


	/// The right shoulder button of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput ShoulderButtonRight ;



	/// The pen down (stylus touches touchscreen) of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput StylusContact ;


	/// The lid status (open/closed) of the Nintendo DS.
	extern CEYLAN_DLL const DSBinaryInput LidOpen ;



	/// Gathers all buttons and stylus contact (everything but the lid).
	extern CEYLAN_DLL const DSBinaryInput AllUserInputs ;


#endif // CEYLAN_ARCH_NINTENDO_DS




	/**
	 * Returns a new key being hit.
	 * Waits, if necessary, until this occurs.
	 *
	 * On the Nintendo DS ARM9, returns the key state.
	 *
	 * @throw UtilsException if the operation is not available or could not be
	 * performed correctly.
	 *
	 */
	CEYLAN_DLL KeyChar getChar() ;



	/**
	 * Default string to display when waiting for a key to be hit.
	 *
	 * Ex: "Press any key to continue".
	 *
	 */
	extern CEYLAN_DLL const std::string DefaultWaitForKeyMessage ;



	/**
	 * Waits for a key to be pressed.
	 *
	 * @param the sentence to display once before waiting. Just specify "" for
	 * no message.
	 *
	 * @return the hit key as getChar read it.
	 *
	 * @note One should not use for example:
	 * <code>"Hit key is: " + waitForKey()</code> since waitForKey returns
	 * a numerical value. Instead, use:
	 * <code>"Hit key is: " + toString( waitForKey() )</code>
	 *
	 * @throw UtilsException if the operation failed or in not supported on this
	 * platform.
	 *
	 */
	CEYLAN_DLL KeyChar waitForKey( const std::string & message
		= DefaultWaitForKeyMessage ) ;



	/**
	 * Template function splitting a container according to a delimiter.
	 *
	 * This function takes a container (e.g. a string), a delimiter (e.g. a
	 * char), and appends to the list of containers the ones that result from
	 * splitting the first container.
	 *
	 * Its obvious use is for splitting strings, but the function has been made
	 * a template in order to be able to:
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
	 * @param result list of containers to which will be appended the results of
	 * the split.
	 *
	 * @author Marc Petit.
	 *
	 */
	template<class T, class Element>
	void split( const T & toSplit, const Element & delimiter,
		std::list<T> & result )
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
	 * std::find_if), and appends to the list of containers the ones that result
	 * from splitting the first container.
	 *
	 * Being based on the same algorithm as Split, this function does not
	 * include the instances found to be "delimiters".
	 *
	 * @param toSplit container to be split.
	 *
	 * @param predicate predicate used to differentiate slices.
	 *
	 * @param result list of containers to which will be appended the results of
	 * the split.
	 *
	 * @example:
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
	 *		std::cout << "item " << n << ": >" << *i << "<" << std::endl ;
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
		std::list<T> & result )
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
	 * @note Might be useful for light debugging. Do not use std::cout for that,
	 * as it does not flush the output, putting you at risk of having a program
	 * go past a std::cout while never being able to see it.
	 *
	 */
	CEYLAN_DLL void checkpoint( const std::string & message = "" ) ;



	/**
	 * Prints in standard output a breakpoint message, with a breakpoint count
	 * incremented at each call, starting from 1, and then waits for the user to
	 * press a key.
	 *
	 * @note Might be useful for light debugging.
	 *
	 */
	CEYLAN_DLL void breakpoint( const std::string & message = "" ) ;


}



#endif // CEYLAN_UTILS_H_
