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


#ifndef CEYLAN_STRING_UTILS_H_
#define CEYLAN_STRING_UTILS_H_


#include "CeylanTypes.h"          // for Ceylan::Uint16
#include "CeylanException.h"      // for Ceylan::Exception


#include <sstream>                // for istringstream
#include <string>                 // for string, string::size_type
#include <list>                   // for list
#include <map>                    // for map


// Deserialization can fail easily, better check it systematically:
#define CEYLAN_DEBUG_STRING_TO_OBJECT 1



/**
 * This part of the Ceylan namespace gathers some convenient string manipulation
 * facilities to be widely used.
 *
 * The system default locale is the one being used.
 *
 * @see CeylanOperators.h for operators appending numerical values to strings.
 *
 */

namespace Ceylan
{


	/// Exception raised by string utils services.
	class CEYLAN_DLL StringUtilsException : public Exception
	{

		public:

			StringUtilsException( const std::string & message ) :
				Exception( message )
			{

			}

			virtual ~StringUtilsException() throw()
			{

			}

	} ;



	/**
	 * Describes a character encoded in Latin-1 (ISO 8859-1 character set).
	 *
	 */
	typedef Ceylan::Uint8 Latin1Char ;



	/**
	 * Datatype able to store all std::string sizes, could be as well
	 * Ceylan::Uint32 should size_type depend on the platform.
	 *
	 */
	typedef std::string::size_type StringSize ;



	/**
	 * This is the name of the option which is to be passed to tests run from
	 * the command line, so that they run in batch mode.
	 *
	 * They should therefore be non-interactive.
	 *
	 * @note Stress tests might be very long for a user to wait for them.
	 *
	 * @see playTests.sh
	 *
	 */
	extern CEYLAN_DLL const std::string BatchTestOption ;




	// Container formatting section.


	/**
	 * Formats the specified list of strings, according to the specified format.
	 *
	 * Text output format is determined from overall settings: the list will be
	 * output with HTML tags, or use raw formatting, accordingly.
	 *
	 * @param stringList the list of strings to format.
	 *
	 * @param surroundByTicks tells whether each entry of the list should be
	 * surrounded by ticks on output.
	 *
	 * @param indentationLevel the desired level of indentation, starting at 1
	 * (used for raw text output).
	 *
	 * @note if the STL was smarter, it would have allowed arguments whose type
	 * is std::list<std::string> in std::list<const std::string>.
	 *
	 * However no parameter of type 'const std::list<const std::string> &' can
	 * be used, since std::list elements have to be able to be assigned, hence
	 * 'std::list<const X>' cannot be used.
	 *
	 */
	CEYLAN_DLL std::string formatStringList(
		const std::list<std::string> & stringList,
		bool surroundByTicks = false, Ceylan::Uint8 indentationLevel = 1 ) ;



	/**
	 * Formats the specified list of strings, according to the specified format.
	 *
	 * Text output format is determined from overall settings: the list will be
	 * output with HTML tags, or use raw formatting, accordingly.
	 *
	 * @param stringList the list of strings to format.
	 *
	 * @param targetFormat the format (ex: raw text, HTML, etc.) that should be
	 * used to encode the returned string
	 *
	 * @param surroundByTicks tells whether each entry of the list should be
	 * surrounded by ticks on output.
	 *
	 * @param indentationLevel the desired level of indentation, starting at 1
	 * (used for raw text output).
	 *
	 * @note if the STL was smarter, it would have allowed arguments whose type
	 * is std::list<std::string> in std::list<const std::string>. However no
	 * parameter of type 'const std::list<const std::string> &' can be used,
	 * since std::list elements have to be able to be assigned, hence
	 * 'std::list<const X>' cannot be used.
	 *
	 */
	CEYLAN_DLL std::string formatStringList(
		const std::list<std::string> & stringList,
		TextDisplayable::TextOutputFormat targetFormat,
		bool surroundByTicks = false, Ceylan::Uint8 indentationLevel = 1 ) ;



	/**
	 * Formats the specified map whose keys and values are strings, according to
	 * the specified format.
	 *
	 * Text output format is determined from overall settings: the map will be
	 * output with HTML tags, or use raw formatting, accordingly.
	 *
	 * @param stringMap the map of strings to format.
	 *
	 * @param surroundByTicks tells whether each entry of the list should be
	 * surrounded by ticks on output.
	 *
	 * @note if the STL was smarter, it would have allowed both const and
	 * non-const arguments.
	 *
	 */
	CEYLAN_DLL std::string formatStringMap(
		const std::map<std::string, std::string> & stringMap,
		bool surroundByTicks = false ) ;



	// Miscellaneous.


	/**
	 * Displays the specified message to the user, on his console, if any.
	 *
	 * @note On some platforms (ex: the Nintendo DS), the console must have been
	 * initialized beforehand.
	 *
	 * @see System::Console::Initialize
	 *
	 * @throw StringUtilsException if the operation is not available on this
	 * platform.
	 *
	 */
	CEYLAN_DLL void display( const std::string & message ) ;



	/**
	 * Displays the specified error message to the user, on his console, if any.
	 *
	 * @throw StringUtilsException if the operation is not available on this
	 * platform.
	 *
	 */
	CEYLAN_DLL void displayError( const std::string & errorMessage ) ;



	/**
	 * Counts how many characters <b>targetChar</b> can be found in
	 * <b>aString</b>.
	 *
	 */
	CEYLAN_DLL StringSize countChars( const std::string & aString,
		char targetChar ) ;



	/**
	 * Returns the reversed source string, as read from right to left.
	 *
	 * Example: "Ceylan" becomes "nalyeC".
	 *
	 */
	CEYLAN_DLL std::string reverse( const std::string & source ) ;



	/**
	 * Converts a constant string into a 'char *', not a 'const char *'.
	 *
	 * This method still works but has been deprecated in favor of a better
	 * solution, the use the <code>const_cast</code> C++ keyword, see example
	 * below.
	 *
	 * This is especially useful for broken old C libraries which have 'char *'
	 * arguments where they should ask only for 'const char *': STL string
	 * cannot be used directly since the std::string c_str() method returns a
	 * '<b>const<b> char *'.
	 *
	 * @note Memory gets allocated by this function, one should therefore free,
	 * with 'delete []', the returned char * when finished with it: the caller
	 * is responsible for the returned char * deallocation, otherwise memory
	 * is leaked.
	 *
	 * @return the requested 'char *'. If ever there was enough memory, the
	 * application is stopped in emergency.
	 *
	 * @example:
	 * <pre>
	 * void aStupidFunction( char * name ) ;
	 *
	 * const string aString = "Ceylan rocks!" ;
	 * char * convertedString = Ceylan::getNonConstCharFrom( aString ) ;
	 * aStupidFunction( convertedString ) ;
	 * delete [] convertedString ;
	 * ...
	 * or:
	 * ...
	 * aStupidFunction( const_cast<char *>( aString.c_str() ) ) ;
	 * ...
	 *
	 * </pre>
	 *
	 */
	 CEYLAN_DLL char * getNonConstCharFrom( const std::string & source ) ;



	/**
	 * Substitutes, in string <b>targetString</b>, the string
	 * <b>toBeReplaced</b> with the string <b>replacement</b>
	 *
	 * @return the number of substitutions performed.
	 *
	 */
	CEYLAN_DLL StringSize substituteInString(
		std::string & targetString,
		const std::string & toBeReplaced,
		const std::string & replacement ) ;



	/**
	 * Returns a new string, corresponding to the source string in which the
	 * substring <b>toBeReplaced</b> is replaced by the substring
	 * <b>replacement</b>
	 *
	 * @return the new string.
	 *
	 */
	CEYLAN_DLL std::string substituteIn(
		const std::string & sourceString,
		const std::string & toBeReplaced,
		const std::string & replacement ) ;



	/**
	 * Returns whether specified character is a letter, i.e. is in [a-z] or
	 * [A-Z].
	 *
	 * @param targetChar the character to test.
	 *
	 * @return true iff targetChar is a letter.
	 *
	 */
	CEYLAN_DLL bool isLetter( char targetChar ) ;



	/**
	 * Returns whether specified character is a figure, i.e. is in [0-9].
	 *
	 * @param targetChar the character to test.
	 *
	 * @return true iff targetChar is a figure.
	 *
	 */
	CEYLAN_DLL bool isFigure( char targetChar ) ;



	/**
	 * Returns whether specified character is a letter or a figure, i.e.  is in
	 * [a-z], [A-Z] or [0-9].
	 *
	 * @param targetChar the character to test.
	 *
	 * @return true iff targetChar is a letter or a figure.
	 *
	 */
	CEYLAN_DLL bool isAlphanumeric( char targetChar ) ;



	/**
	 * Returns whether specified character is a punctuation character, i.e. is
	 * not a letter, not a figure, not a whitespace.
	 *
	 * @param targetChar the character to test.
	 *
	 * @return true iff targetChar is a punctuation character.
	 *
	 */
	CEYLAN_DLL bool isPunctuation( char targetChar ) ;



	/**
	 * Returns whether specified character is a whitespace character, i.e. is in
	 * [0x09;0x0D;0x20].
	 *
	 * @param targetChar the character to test.
	 *
	 * @return true iff targetChar is a whitespace character.
	 *
	 */
	CEYLAN_DLL bool isWhitespace( char targetChar ) ;



	/**
	 * Returns specified text with all the letters translated to uppercase,
	 * other characters being left untouched.
	 *
	 */
	CEYLAN_DLL std::string toUppercase( const std::string & text ) ;



	/**
	 * Encodes the specified string by converting it into a valid HTML text,
	 * replacing special characters such as '<', '>' or '&' by their
	 * corresponding HTML codes.
	 *
	 */
	CEYLAN_DLL std::string encodeToHTML( const std::string & message ) ;



	/**
	 * Encodes the specified string in 'phonetic alphabet', also known as
	 * 'police letter alphabet' or 'analogy international code', which converts
	 * letters such as 'a', 'b', 'c' to respectively 'alpha', 'bravo', 'charlie'
	 * etc.
	 *
	 * Non alphabetic characters (not in a-z, A-Z) are left untouched.
	 * Uppercase and lowercase letters have respectively their translated word
	 * in uppercase and lowercase.
	 *
	 */
	CEYLAN_DLL std::string encodeToPhonetic( const std::string & message ) ;



	/**
	 * Encodes the specified string in 'rot13', a very simple substitution
	 * cipher.
	 *
	 * @see http://en.wikipedia.org/wiki/ROT13
	 *
	 * Non alphabetic characters (not in a-z, A-Z) are left untouched.
	 *
	 * @note ROT13 is its own inverse, that some function can be used for
	 * decoding as well.
	 *
	 */
	CEYLAN_DLL std::string encodeToROT13( const std::string & message ) ;


	/**
	 * Encodes each of the strings in the specified list in 'rot13', a very
	 * simple substitution cipher, and returns a new list with their encoded
	 * version (whereas the specified list is not changed).
	 *
	 * @see http://en.wikipedia.org/wiki/ROT13
	 *
	 * Non alphabetic characters (not in a-z, A-Z) are left untouched.
	 *
	 * @note ROT13 is its own inverse, that some function can be used for
	 * decoding as well.
	 *
	 */
	CEYLAN_DLL std::list<std::string> encodeToROT13(
	  const std::list<std::string> & messages ) ;



	/**
	 * Demangles a C++ symbol so that it becomes human-readable.
	 *
	 * Example: 'N3One3Two11ExampleFourE' should be converted in
	 * 'One::Two::ExampleFour'.
	 *
	 * @note This function operates only on symbols mangled by g++, version 3.x
	 * or higher. The mangling can be changed is newer g++ versions.
	 *
	 */
	CEYLAN_DLL std::string demangleSymbol( const std::string & symbol ) ;



	/**
	 * Splits <b>stringToSplit</b> according to character <b>splittingChar</b>.
	 *
	 * @example split( "/home/user/Projects/OSDL-loanized", '/' ) should return
	 * [ "", "home", "user", "Projects", "OSDL-loanized" ]
	 *
	 * @see join
	 *
	 */
	CEYLAN_DLL std::list<std::string> split( const std::string & stringToSplit,
		char splittingChar ) ;



	/**
	 * Splits <b>stringToSplit</b> according to string <b>splittingString</b>.
	 *
	 * @example split( "little, wicked, naughty, stinky", " ," ) returns
	 * [ "little", "wicked", "naughty", "stinky" ]
	 *
	 * @see join
	 *
	 */
	/*
	std::list<std::string> split( const std::string & stringToSplit,
		 std::string splittingString ) ;
	*/



	/**
	 * Joins the strings in <b>toJoin</b> with specified joining string.
	 *
	 * @example join( [ "little", "wicked", "naughty", "stinky" ], ", " )
	 * returns: "little, wicked, naughty, stinky".
	 *
	 * @see split
	 *
	 */
	CEYLAN_DLL std::string join( const std::list<std::string> & toJoin,
		const std::string & joiningString ) ;



	/**
	 * Splits the specified sentence into a list of words.
	 *
	 * Words are found based on the space (' ') character, which acts as a
	 * separator. However n>1 spaces in a row (ex: 'abc   def', n=3) should
	 * result in a word made of
	 *  - if n=2, 0 space (empty word, '')
	 *  - if n>2, (n-2) spaces
	 * (n=3: ['abc', ' ', 'def' ] instead of ['abc', '', '', 'def']).
	 *
	 * @param sentenceToSplit the sentence to split into words.
	 *
	 * @return a list of paragraphs. Paragraphs may be empty (two separators in
	 * a row).
	 *
	 */
	CEYLAN_DLL std::list<std::string> splitIntoWords(
		const std::string & sentenceToSplit ) ;



	/**
	 * Splits the specified text into a list of paragraphs.
	 *
	 * Paragraphs are found based on the '\n' character which acts as a
	 * paragraph separator.
	 *
	 * @param textToSplit the text to split into paragraphs.
	 *
	 * @return a list of paragraphs. Paragraphs may be empty (two separators in
	 * a row).
	 *
	 */
	CEYLAN_DLL std::list<std::string> splitIntoParagraphs(
		const std::string & textToSplit )  ;



	/**
	 * Returns a string describing the specified list.
	 *
	 */
	template <typename T>
	std::string toString( const std::list<T> & targetList )
	{

		std::list<std::string> res ;

		for ( typename std::list<T>::const_iterator it = targetList.begin() ;
				it != targetList.end(); it++ )
			res.push_back( Ceylan::toString( *it ) ) ;

		return "[ " + join( res, ", " ) + " ]" ;

	}



	/**
	 * String to object (deserialization).
	 *
	 * @note No CEYLAN_DLL for templates.
	 *
	 */
	template <typename T>
	void stringToObject( const std::string & source, T & object )
	{

		std::istringstream iss( source ) ;
		iss >> object ;

#if CEYLAN_DEBUG_STRING_TO_OBJECT

		if ( iss.fail() )
		{
			throw Ceylan::Exception(
				"Conversion error in Ceylan string utilities "
				"while using template stringToObject." ) ;
		}

#endif // CEYLAN_DEBUG_STRING_TO_OBJECT

	}


}


#endif // CEYLAN_STRING_UTILS_H_
