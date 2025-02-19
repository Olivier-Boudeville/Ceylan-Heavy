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


#include "CeylanStringUtils.h"


#include "CeylanUtils.h"               // for generic templated split

#include "CeylanSystem.h"
#include "CeylanOperators.h"
#include "CeylanLogPlug.h"             // for the LogPLug


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for CEYLAN_DEBUG_DEMANGLE, etc.
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for iprintf, CEYLAN_DS_LOG
#endif // CEYLAN_ARCH_NINTENDO_DS


#include "CeylanLogLight.h"            // for CEYLAN_LOG


#if CEYLAN_ARCH_WINDOWS

extern "C"
{

#include "string.h"                    // for strcpy_s

}

#endif // CEYLAN_ARCH_WINDOWS


#include <cctype>                      // for isdigit, isupper, etc.
#include <iostream>                    // for cout, endl, flush.
#include <cstring>                     // for strcpy


using std::string ;
using std::list ;
using std::map ;

using namespace Ceylan ;
using namespace Ceylan::Log ;


#if CEYLAN_DEBUG_DEMANGLE

#define DISPLAY_DEBUG_DEMANGLE(message) LogPlug::debug( string( "[Demangle] " ) + message ) ;

#else // CEYLAN_DEBUG_DEMANGLE

#define DISPLAY_DEBUG_DEMANGLE(message)

#endif // CEYLAN_DEBUG_DEMANGLE


const std::string Ceylan::BatchTestOption = "--batch" ;



StringSize Ceylan::countChars( const string & aString, char targetChar )
{

	StringSize charCount = 0 ;

	StringSize size = aString.size() ;

	for ( StringSize i = 0; i < size; i++ )
		if ( aString[i] == targetChar )
			charCount++ ;

	return charCount ;

}



std::string Ceylan::reverse( const std::string & source )
{

	StringSize count = source.size() ;

	if ( count == 0 )
		return "" ;

	string result ;

	do
	{
		count-- ;
		result += source[count] ;

	}
	while ( count > 0 ) ;

	return result ;

}



char * Ceylan::getNonConstCharFrom( const std::string & source )
{

	// One more character for the trailing '0':
	char * res = new char[ source.size() + 1 ] ;

	if ( res == 0 )
		Ceylan::emergencyShutdown(
			"Ceylan::getNonConstCharFrom: not enough memory." ) ;

#if CEYLAN_ARCH_WINDOWS

	strcpy_s( res, source.size() + 1, source.c_str() ) ;

#else // CEYLAN_ARCH_WINDOWS

	::strcpy( res, source.c_str() ) ;

#endif // CEYLAN_ARCH_WINDOWS

	return res ;

	/*
	 * Many variations could be used as well: using::strdup instead
	 * (with::free), raising an exception if out of memory, etc.
	 *
	 * Too complicated to use (try/catch, even a null pointer checking would be
	 * uselessly cumbersome ):

	char * result = ::strdup( source.c_str() ) ;

	if ( result )
	{
		return result ;
	}
	else
	{
		// If memory is so low, maybe the exception will fail too:
		throw UtilsException(
			"Ceylan::getNonConstCharFrom: not enough memory." ) ;
	}

	*/

}



StringSize Ceylan::substituteInString( string & targetString,
	const string & toBeReplaced, const string & replacement )
{

	StringSize substitutionCount = 0 ;

	StringSize charCount ;

	Ceylan::Sint32 lenDiff = static_cast<Ceylan::Sint32>(
		toBeReplaced.size() - replacement.size() ) ;

	do
	{

		charCount = targetString.find( toBeReplaced ) ;

		if ( charCount == string::npos )
			break ;
		substitutionCount++ ;

		/*
		 * If replacement is longer than replaced string, we have to add dummy
		 * characters at the end of each replaced string, so that there is
		 * enough room for replacement.
		 *
		 */
		if ( lenDiff < 0 )
			targetString.insert( charCount + toBeReplaced.size(),
				- lenDiff, 'x' ) ;

		targetString.replace( charCount, replacement.size(), replacement ) ;

		/*
		 * If replaced string is longer than replacement, we have to delete
		 * exceeding characters at the end of each replacement, so that the end
		 * of replaced string is eaten.
		 *
		 */
		if ( lenDiff > 0 )
			 targetString.erase( charCount + replacement.size(), lenDiff ) ;

	}
	while ( true ) ;

	return substitutionCount ;

}



string Ceylan::substituteIn( const string & sourceString,
	const string & toBeReplaced, const string & replacement )
{

	string res = sourceString ;

	substituteInString( res, toBeReplaced, replacement ) ;

	return res ;

}



bool Ceylan::isLetter( char targetChar )
{

	if ( ::isalpha( targetChar ) )
		return true ;

	return false ;

}



bool Ceylan::isFigure( char targetChar )
{

	if ( ::isalpha( targetChar ) )
		return true ;

	return false ;

}



bool Ceylan::isAlphanumeric( char targetChar )
{

	if ( ::isalnum( targetChar ) )
		return true ;

	return false ;

}



bool Ceylan::isPunctuation( char targetChar )
{

	if ( ::ispunct( targetChar ) )
		return true ;

	return false ;

}



bool Ceylan::isWhitespace( char targetChar )
{

	if ( ::iswspace( targetChar ) )
		return true ;

	return false ;

}



string Ceylan::toUppercase( const std::string & text )
{

	string result ;

	for ( string::const_iterator it = text.begin();
			it != text.end(); it++ )
		if ( ::islower( *it ) )
			result += static_cast<char>( ::toupper( *it ) ) ;
		else
			result += *it ;

	return result ;

}



string Ceylan::encodeToHTML( const std::string & message )
{

	string result ;

	for ( string::const_iterator it = message.begin();
		it != message.end(); it++ )
	{

		switch( (*it) )
		{

			/*
			 * '"' and many others left untouched, assumed wanted, i.e.
			 * belonging to an implied HTML expression.
			 *
			 * Only the following might be annoying:
			 *
			 */

			case '&':
				result += "&amp;" ;
				break ;

			case '<':
				result += "&lt;" ;
				break ;

			case '>':
				result += "&gt;" ;
				break ;

			default:
				result += (*it) ;
				break ;

		}
	}

	return result ;

}



string Ceylan::encodeToPhonetic( const std::string & message )
{

	const std::string phonetics[26] = {

		"alpha",
		"bravo",
		"charlie",
		"delta",
		"echo",
		"fox-trot",
		"golf",
		"hotel",
		"india",
		"juliet",
		"kilo",
		"lima",
		"mike",
		"november",
		"oscar",
		"papa",
		"quebec",
		"romeo",
		"sierra",
		"tango",
		"uniform",
		"victor",
		"whisky",
		"x-ray",
		"yankee",
		"zulu"

	} ;

	bool firstLetter = true ;
	string result ;

	for ( string::const_iterator it = message.begin();
		it != message.end(); it++ )
	{

		// No leading nor trailing space:
		if ( firstLetter )
			firstLetter = false ;
		else
			result += " " ;

		if ( ::isupper( *it ) )
			result += toUppercase( phonetics[ (*it) - 'A' ] ) ;
		else
			if ( ::islower( *it ) )
				result += phonetics[ (*it) - 'a' ] ;
			else
				result += *it ;

	}

	return result ;

}



string Ceylan::encodeToROT13( const std::string & message )
{

	string result ;

	for ( string::const_iterator it = message.begin();
		it != message.end(); it++ )
	{

		if ( ( (*it) >= 'A' ) && ( (*it) <= 'Z' ) )
		{

			result += ( (*it) - 'A' + 13) % 26 + 'A' ;

		}
		else if ( (*it) >= 'a' && (*it) <= 'z' )
		{

			result += ( (*it) - 'a' + 13) % 26 + 'a' ;

		}
		else
		{

			result += (*it) ;

		}

	}

	return result ;

}


list<string> Ceylan::encodeToROT13( const list<string> & messages )
{

  list<string> res ;

  for ( list<string>::const_iterator it = messages.begin();
		it != messages.end(); it++ )
	res.push_back( encodeToROT13( *it ) ) ;

  return res ;

}


string Ceylan::demangleSymbol( const std::string & symbol )
{


	/*
	 * Objective: convert something like 'N3One3Two11ExampleFourE' into
	 * 'One::Two::ExampleFour'
	 *
	 */
	DISPLAY_DEBUG_DEMANGLE( "Initial symbol is " + symbol ) ;


	/*
	 * If it does not start by 'N' or does not end by 'E', not a mangled
	 * message, return as is:
	 *
	 */
	//if ( symbol[0] != 'N' || symbol[symbol.size()-1] != 'E' )
	//	return symbol ;

	/*
	 * Code above disabled, as we can have:
	 * typeid( * this ).name() = "10ExampleTwo" (thus to become "ExampleTwo").
	 *
	 */

	string shorten ;

	/*
	 * Removes 'N' and 'E' if necessary (assuming either none or both are
	 * there):
	 *
	 */
	if ( symbol[0] == 'N' || symbol[symbol.size()-1] == 'E' ) 
	{

	  shorten = symbol.substr( 1, symbol.size() - 2 ) ;

	}
	else
	{

	  shorten = symbol ;

	}

	StringSize shortenSize = shorten.size() ;


	DISPLAY_DEBUG_DEMANGLE( "Shorten symbol is " + shorten ) ;

	// We would have now: 3One3Two11ExampleFour

	/*
	 * Algorithm:
	 *
	 * - while is a digit, add character to numString (here, read 3, could have
	 * been 38 or so)
	 *
	 * - convert numString into num
	 *
	 * - jumps over the word (it might end with digits...) and add it into
	 * result with "::" added
	 *
	 * - loop until end of string reached
	 *
	 */

	/*
	 * On g++, a name is prefixed with the length in characters of the name (Log
	 * would be 3Log), so we remove this numerical prefix.
	 *
	 */


	/*
	 * With Visual C++ 2005 compiler, class names start with 'class ', we remove
	 * this mostly useless prefix.
	 *
	 * (to be checked back on Windows)
	 *
	 */
	//const string toRemove = "class " ;
	//string::size_type pos = className.find( toRemove,
	//	/* starting position */ 0) ;

	//if ( pos != string::npos && pos == 0 )
	//	className = className.substr( toRemove.size() ) ;

	StringSize count = 0 ;
	StringSize num ;
	string numString, extracted ;

	while ( count < shortenSize )
	{

		DISPLAY_DEBUG_DEMANGLE( "Count is "
			+ Ceylan::toString(
				static_cast<Ceylan::Uint32>( count ) )
			+ ", extracted is "
			+ extracted
			+ ", shorten is "
			+ shorten ) ;

		while ( ::isdigit( shorten[count] ) )
		{
			numString += shorten[count] ;
			count++ ;
		}

		if ( numString.empty() )
		{
			// Abnormal, a misleading symbol such as NxyzE?
			return symbol ;
		}

		// We are at the end of the numerical string:
		num = Ceylan::stringToUnsignedLong( numString ) ;

		if ( num + count > shortenSize )
		{
			// Abnormal, a misleading symbol such as N99xyzE?
			return symbol ;
		}

		DISPLAY_DEBUG_DEMANGLE( "Count is "
			+ Ceylan::toString(
				static_cast<Ceylan::Uint32>( count ) )
			+ ", extracted is " + extracted
			+ ", shorten is " + shorten ) ;

		DISPLAY_DEBUG_DEMANGLE( "Jumping " + numString + " ("
			+ Ceylan::toString( static_cast<long>( num ) )
			+ ") characters, taking word ["
			+ shorten.substr( count, num ) + "]" ) ;

		extracted += shorten.substr( count, num ) + "::" ;

		DISPLAY_DEBUG_DEMANGLE( "Count is "
			+ Ceylan::toString( static_cast<Ceylan::Uint32>( count ) )
			+ ", extracted is " + extracted
			+ ", shorten is " + shorten ) ;

		count += num ;

		DISPLAY_DEBUG_DEMANGLE( "Remaining string is "
			+ shorten.substr( count, shortenSize - count ) ) ;

		numString = "" ;

	}

	// Remove last '::':
	extracted.erase( extracted.size()-2, 2 ) ;

	DISPLAY_DEBUG_DEMANGLE( "Ceylan::demangleSymbol returning "
		+ extracted ) ;

	return extracted ;

}



list<string> Ceylan::split( const string & stringToSplit, char splittingChar )
{

	list<string> result ;

#define CEYLAN_TRUSTS_STL_WITH_TEMPLATES 0

#if CEYLAN_TRUSTS_STL_WITH_TEMPLATES

	split<string, char>( stringToSplit, splittingChar, result ) ;

#else // CEYLAN_TRUSTS_STL_WITH_TEMPLATES

	string splitWord ;


	for ( std::string::const_iterator it = stringToSplit.begin() ;
		it != stringToSplit.end(); it++ )
	{


		if ( *it == splittingChar )
		{
			result.push_back( splitWord ) ;
			splitWord.clear() ;
		}
		else
		{

		  /*
		   * Valgrind says there is a memory leak with
		   * std::string::operator+=(char)
		   * (in /usr/lib/x86_64-linux-gnu/libstdc++.so.6.0.14),
		   * but we do not believed our code is guilty here:
		   *
		   */
		  splitWord += *it ;

		}

	}

	if ( ! splitWord.empty() )
		result.push_back( splitWord ) ;

#endif // CEYLAN_TRUSTS_STL_WITH_TEMPLATES

	return result ;

}


/* STL + templates = stupid nightmare
list<string> Ceylan::split( const string & stringToSplit,
	const string & splittingString )
{

	list<string> result ;

	split<string, const string>( stringToSplit, splittingString, result ) ;

	return result ;

}
*/



string Ceylan::join( const list<string> & toJoin, const string & joiningString )
{

	// Copying the whole list (and poping it) would not be efficient.

	if ( toJoin.empty() )
		return "" ;

	string res ;

	list<string>::const_iterator it = toJoin.begin() ;

	res = (*it) ;

	it++ ;

	while ( it != toJoin.end() )
	{
		res += joiningString + (*it) ;
		it++ ;
	}

	return res ;

}



std::list<string> Ceylan::splitIntoWords( const string & sentenceToSplit )
{

	/*
	 * Maybe here all non-space whitespaces should be managed separately.
	 *
	 * Usually this function is called on each item of a list build by
	 * splitIntoParagraphs, hence no '\n' should be left (we expect a sentence).
	 *
	 * However '\t' might still exist, each tabulation is replaced by four
	 * spaces in a row.
	 *
	 */

	string copiedSendtence = sentenceToSplit ;

	substituteInString( copiedSendtence, "\t", "    " ) ;

	list<string> splitted = Ceylan::split( copiedSendtence, ' ' ) ;

	/*
	 * Some changes are needed in the splitted list in the case where there are
	 * multiple spaces in a row.
	 *
	 * More precisely, if there are in the sentence n spaces in a row, then we
	 * have A instead of the desired B:
	 *
	 * (example pattern: 'a' + n * ' ' + 'b' ):
	 * n=0: A = [ 'ab' ],                B = [ 'ab' ]			  -> OK
	 * n=1: A = [ 'a', 'b' ],            B = [ 'a', 'b' ]		  -> OK
	 * n=2: A = [ 'a', '', 'b' ]    ,    B = [ 'a', '', 'b' ]     -> OK
	 * n=3: A = [ 'a', '', '', 'b' ],    B = [ 'a', ' ', 'b' ]    -> KO,
	 * must be corrected
	 * n=4: A = [ 'a', '', '', '', 'b' ],B = [ 'a', '  ', 'b' ]   -> KO,
	 * must be corrected
	 *
	 * Solution is: count all list items equal to '' on a row.
	 *
	 * Replace them by an item made of (count-1) spaces, if count is equal to 3
	 * or greater.
	 *
	 * @see testStringUtils.cc
	 *
	 */

	list<string> corrected ;

	StringSize voidItemCount = 0 ;

	for ( list<string>::const_iterator it = splitted.begin();
		it != splitted.end(); it++ )
	{

		// React only at the end of a series of '' with more than two items:
		if ( (*it).empty() )
		{
			voidItemCount++ ;
		}
		else
		{

			// Here we have a non-empty word. End of space sequence?

			if ( voidItemCount > 0 )
			{

				string spaces ;

				// Start at 1 so that having (n-1) spaces:
				for ( Ceylan::Uint32 i = 1; i < voidItemCount; i++ )
					spaces += " " ;

				corrected.push_back( spaces ) ;
				voidItemCount = 0 ;

			}
			// else: no space, nothing special to do.

			corrected.push_back( *it ) ;

		}

	}

	return corrected ;

}



std::list<string> Ceylan::splitIntoParagraphs( const string & textToSplit )
{

	return Ceylan::split( textToSplit, '\n' ) ;

}



std::string Ceylan::formatStringList( const list<string> & stringList,
	bool surroundByTicks, Ceylan::Uint8 indentationLevel )
{

	return formatStringList( stringList, TextDisplayable::GetOutputFormat(),
		surroundByTicks, indentationLevel ) ;

}



std::string Ceylan::formatStringList( const list<string> & stringList,
	TextDisplayable::TextOutputFormat targetFormat,
	bool surroundByTicks, Ceylan::Uint8 indentationLevel )
{

	if ( stringList.empty() )
		return "(empty list)" ;

	string res ;

	if ( targetFormat == TextDisplayable::html )
	{

		res = "<ul>" ;

		for ( list<string>::const_iterator it = stringList.begin();
				it != stringList.end();	it++ )
			if ( surroundByTicks )
				res += "<li>'" + ( *it ) + "'</li>" ;
			else
				res += "<li>" + ( *it ) + "</li>" ;

		res += "</ul>" ;

	}
	else
	{

		// Raw text:

		res = '\n' ;

		string prefix ;

		// Two space offset for each indentation level:
		for ( Ceylan::Uint8 i = 0; i < indentationLevel; i++ )
			prefix += "  " ;

		switch ( indentationLevel )
		{

			case 1:
				prefix += '-' ;
				break ;

			case 2:
				prefix += '*' ;
				break ;

			case 3:
				prefix += '.' ;
				break ;

			case 4:
				prefix += '+' ;
				break ;

			default:
				prefix += '-' ;
				break ;

		}

		prefix += " " ;

		for ( list<string>::const_iterator it = stringList.begin();
				it != stringList.end();	it++ )
			if ( surroundByTicks )
				res += prefix + "'" + ( *it ) + "'\n" ;
			else
				res += prefix + ( *it ) + '\n' ;

	}

	return res ;

}



std::string Ceylan::formatStringMap(
	const std::map<std::string, std::string> & stringMap, bool surroundByTicks )
{

	if ( stringMap.empty() )
		return "(empty map)" ;

	string res ;

	if ( TextDisplayable::GetOutputFormat() == TextDisplayable::html )
	{

		res = "<ul>" ;

		for ( map<string,string>::const_iterator it = stringMap.begin();
				it != stringMap.end();	it++ )
			if ( surroundByTicks )
				res += "<li>'" + (*it).first + "' = '"
					+ (*it).second + "'</li>" ;
			else
				res += "<li>" + (*it).first + " = "
					+ (*it).second  + "</li>" ;

		res += "</ul>" ;

	}
	else
	{

		// Raw text:

		res = '\n' ;
		for ( map<string,string>::const_iterator it = stringMap.begin();
				it != stringMap.end();	it++ )
			if ( surroundByTicks )
				res += "\t+ '" + (*it).first + "' = '"
					+ (*it).second + "'\n" ;
			else
				res += "\t+ " + (*it).first + " = "
					+ (*it).second + '\n' ;

	}

	return res ;

}



void Ceylan::display( const string & message )
{

#if CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_RUNS_ON_ARM7

	throw StringUtilsException( "Ceylan::display: not available for ARM7." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	::iprintf( ( message + '\n' ).c_str()  ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	std::cout << message << std::endl << std::flush ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void Ceylan::displayError( const string & errorMessage )
{

#if CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_RUNS_ON_ARM7

	throw StringUtilsException(
		"Ceylan::displayError: not available for ARM7." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	BG_PALETTE_SUB[255] = RGB15(31,0,0) ;
	display( errorMessage ) ;

	// Supposed default is white:
	BG_PALETTE_SUB[255] = RGB15(31,31,31) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS

	std::cerr << errorMessage << std::endl << std::flush ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}
