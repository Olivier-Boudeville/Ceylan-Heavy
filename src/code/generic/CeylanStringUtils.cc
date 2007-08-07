#include "CeylanStringUtils.h"


#include "CeylanUtils.h"               // for generic templated split

#include "CeylanSystem.h"
#include "CeylanOperators.h"
#include "CeylanLogPlug.h"             // for the LogPLug


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for CEYLAN_DEBUG_DEMANGLE, etc.
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for iprintf
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


using std::string ;
using std::list ;
using std::map ;
using std::pair ;

using namespace Ceylan ;
using namespace Ceylan::Log ;
		

#if CEYLAN_DEBUG_DEMANGLE

#define DISPLAY_DEBUG_DEMANGLE(message) LogPlug::debug( string( "[Demangle] " ) + message ) ;

#else // CEYLAN_DEBUG_DEMANGLE

#define DISPLAY_DEBUG_DEMANGLE(message)
		
#endif // CEYLAN_DEBUG_DEMANGLE


#include "CeylanLogLight.h"
#define CEYLAN_OTHER_TEXTBUFFER_LOG(message) CEYLAN_LOG((string(message)))


#if CEYLAN_DEBUG_TEXTBUFFER

#include "CeylanLogLight.h"
#define CEYLAN_TEXTBUFFER_LOG(message) CEYLAN_LOG((string(message)))

#else // CEYLAN_DEBUG_TEXTBUFFER

#define CEYLAN_TEXTBUFFER_LOG(message)

#endif // CEYLAN_DEBUG_TEXTBUFFER


const std::string Ceylan::BatchTestOption = "--batch" ;




// TextBuffer class section.


TextBuffer::TextBuffer( CharAbscissa screenWidth, CharOrdinate screenHeight ) 
		throw( StringUtilsException ):
	_width( screenWidth ),
	_height( screenHeight )
{
		
	// Starts with no text.	
	_currentText = _textEntries.end() ;

}


TextBuffer::~TextBuffer() throw()
{
	
	// Deallocations all text grids:
	blank() ;
		
}
	
	

TextBuffer::CharAbscissa TextBuffer::getWidth() const throw()		
{

	return _width ;
	
}


TextBuffer::CharOrdinate TextBuffer::getHeight() const throw()		
{

	return _height ;
	
}



void TextBuffer::add( const std::string & text ) throw( StringUtilsException )
{

	CEYLAN_TEXTBUFFER_LOG( "TextBuffer::add begin" ) ;
	CEYLAN_OTHER_TEXTBUFFER_LOG("rr") ;
	TextGrid * newGrid = & createTextGridFrom( text ) ;
		
	_textEntries.push_back( TextEntry( text, newGrid ) ) ;
	
	
	// Update text iterator if it is the first text added:
	if ( _textEntries.size() == 1 )
	{
		_currentText = _textEntries.begin() ;
		_currentLine = newGrid->begin() ;		
	}	

	// Needed in all cases (ex: a second text showing up after the first):
	updateScreenLines() ;
	
	CEYLAN_TEXTBUFFER_LOG( "TextBuffer::add end" ) ;
	
}


void TextBuffer::blank() throw()
{
	
	// Removes all text:
	
	for ( std::list<TextEntry>::iterator it = _textEntries.begin(); 
			it != _textEntries.end(); it++ )
		deleteTextGrid( (*it).second ) ;

	_textEntries.clear() ;
	
}



// Screen positioning section.



bool TextBuffer::jumpNextText() throw()
{

	CEYLAN_TEXTBUFFER_LOG( "TextBuffer::jumpNextText" ) ;
	
	ListOfTexts::const_iterator nextText = _currentText ;
	nextText++ ;

	if ( nextText != _textEntries.end() )
	{
	
		_currentText = nextText ;
		
		TextGrid * currentTextGrid = (*_currentText).second ;
		_currentLine = currentTextGrid->begin() ;
		
		updateScreenLines() ;
		
		return true ;	
		
	}	

	// Not moved:
	return false ;	
		
}



bool TextBuffer::jumpPreviousText() throw()
{
	
	CEYLAN_TEXTBUFFER_LOG( "TextBuffer::jumpPreviousText" ) ;
	
	if ( _currentText != _textEntries.begin() )
	{
	
		_currentText-- ;
		
		TextGrid * currentTextGrid = (*_currentText).second ;
		_currentLine = currentTextGrid->begin() ;
		
		updateScreenLines() ;
		
		return true ;
		
	}	

	// Not moved:
	return false ;	

}



bool TextBuffer::jumpNextLine() throw()
{

	CEYLAN_TEXTBUFFER_LOG( "TextBuffer::jumpNextLine" ) ;
	
	bool moved ;
	
	TextGrid * currentTextGrid = (*_currentText).second ;
	
	if ( _currentLine != currentTextGrid->end() )
	{

		_currentLine++ ;
		moved = true ;
		
	}	
	else
	{

		
		ListOfTexts::const_iterator nextText = _currentText ;
		nextText++ ;
		
		if ( nextText != _textEntries.end() 
			&& getHeightFromCurrentPosition() > _height )
		{
			_currentText = nextText ;
			currentTextGrid = (*_currentText).second ;
			_currentLine = currentTextGrid->begin() ;
			moved = true ;
		}
		else
		{
			moved = false ;
		}
		
	}
	
	// FIXME: optimize:
	updateScreenLines() ;
	
	return moved ;	
	
}



bool TextBuffer::jumpPreviousLine() throw()
{

	CEYLAN_TEXTBUFFER_LOG( "TextBuffer::jumpPreviousLine" ) ;
	
	bool moved ;
	
	TextGrid * currentTextGrid = (*_currentText).second ;
	
	if ( _currentLine != currentTextGrid->begin() )
	{

		_currentLine-- ;
		moved = true ;
		
	}	
	else
	{
	
		// Was at the beginning of current text, let's take previous if any:
		if ( _currentText != _textEntries.begin() )
		{
			_currentText-- ;
			currentTextGrid = (*_currentText).second ;
			_currentLine = currentTextGrid->end() ;
			moved = true ;
			
		}
		else
		{
			moved = false ;
		}

	}	

		
	// FIXME: optimize:
	updateScreenLines() ;
	
	return moved ;	

}




const TextBuffer::TextGrid & TextBuffer::getScreenLines() const throw()
{

	return _screenLines ;
	
}


const std::string TextBuffer::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "Text buffer of width " + Ceylan::toNumericalString( _width )
		+ " and of height " + Ceylan::toNumericalString( _height ) 
		+ ", containing " + Ceylan::toString( _textEntries.size() ) 
		+ " text(s)" ;
		
	if ( level != Ceylan::high )
		return res ;
	
	if ( _screenLines.empty() )
		return res + ". Abstract screen is empty" ;
		
	res += ". Abstract screen contains " 
		+ Ceylan::toString( _screenLines.size() ) + " lines:" ;
	
	list<string> linesList ;
	
	// Beware, lines are not null-terminated:
	char * tempLine = new char[_width+1] ;
	tempLine[_width] = 0 ;
	
	CharOrdinate lineCount = 1 ;
	 
	for ( TextGrid::const_iterator it = _screenLines.begin() ;
			it != _screenLines.end(); it++ )
		{	
		
			for ( CharAbscissa i = 0; i < _width; i++ )
				tempLine[i] = (*it)[i] ;
			
			linesList.push_back( "Line #" 
				+ Ceylan::toNumericalString( lineCount ) + ": '" 
				+ string( tempLine ) + "'." ) ;
			
			lineCount++ ;
						
		}
			
	delete [] tempLine ;
	
	return res + formatStringList( linesList ) ;
		
}




// Protected section.


void TextBuffer::updateScreenLines() throw()
{

	CEYLAN_TEXTBUFFER_LOG( "TextBuffer::updateScreenLines begin" ) ;

	_screenLines.clear() ;
	

	CharOrdinate lineCount = 0 ;
	
	
	// At least a text, use _currentText:	
	
	ListOfTexts::const_iterator textIterator = _currentText ;
	
	TextGrid::const_iterator textLineIterator = _currentLine ;
	
	TextGrid * textListOfLines  ;
	
	// First loop step should begin from current line, others from first line:
	bool resetLineInText = false ;
	
	while ( textIterator != _textEntries.end() )
	{
		
		
		textListOfLines = (*textIterator).second ;
	
		if ( resetLineInText )
			textLineIterator = textListOfLines->begin() ;
		
		while ( textLineIterator != textListOfLines->end() 
			&& lineCount < _height )
		{
			
			_screenLines.push_back( (*textLineIterator) ) ;
			lineCount++ ;
			textLineIterator++ ;
		}
	
		if ( lineCount == _height )
			return ;
	
		/*
		 * Still needing lines beside current text.
		 * Here we arrived to the end of the lines for current text, let's
		 * take next, if any:
		 *
		 */
		textIterator++ ;
		resetLineInText = true ;
		
	}
			
	CEYLAN_TEXTBUFFER_LOG( "TextBuffer::updateScreenLines text end" ) ;
	
}



TextBuffer::TextGrid & TextBuffer::createTextGridFrom( 
	const std::string & text ) throw()
{

	TextGrid * res = new std::list<char *> ;
	
	char * currentLine = getNewLine() ; 
		
	// Char index in current line:
	CharAbscissa currentAbscissa = 0 ;
	
	
	for ( string::const_iterator it = text.begin(); it != text.end() ; it++ )
	{
	
		switch( *it )
		{
		
		
			case '\n':
				res->push_back( currentLine ) ;
				currentLine = getNewLine() ;
				currentAbscissa = 0 ;
				break ;
			
			
			case '\t':
				for ( Ceylan::Uint8 i = 0; i < TabSpacing; i++ )
				{
					currentLine[currentAbscissa] = ' ' ;
					currentAbscissa++ ;
					if ( currentAbscissa == _width )
					{
						res->push_back( currentLine ) ;
						currentLine = getNewLine() ;
						currentAbscissa = 0 ;	
					}
			
				}
				break ;
				
				
			default:
				currentLine[currentAbscissa] = (*it) ;
				currentAbscissa++ ;
				if ( currentAbscissa == _width )
				{
					res->push_back( currentLine ) ;
					currentLine = getNewLine() ;
					currentAbscissa = 0 ;	
				}
				break ;
				
		}		
				
	}

	// Flush remaining characters when finished:
	if ( currentAbscissa!= 0 )
		res->push_back( currentLine ) ;
		
	return * res ;
	
}


TextBuffer::LineIndex TextBuffer::getHeightFromCurrentPosition() 
	const throw()
{

	LineIndex count = 0 ;
	
	TextGrid::const_iterator startLine = _currentLine ;
	

	// First, lines remaining to the end of current text entry:

	while ( startLine != (*_currentText).second->end() )
	{
		startLine++ ;
		count++ ;
	}
	
	ListOfTexts::const_iterator textIterator = _currentText ;
	textIterator++ ;
	
	// textIterator passed by value:
	while ( textIterator != _textEntries.end() )
	{
	
		count += (*textIterator).second->size() ;
		textIterator++ ;
	
	}
	
	CEYLAN_OTHER_TEXTBUFFER_LOG( 
		( "TextBuffer::getHeightFromCurrentPosition " 
		+ Ceylan::toString ( count ) ).c_str() ) ;
		
	return count ;
	
	
}

	
TextBuffer::LineIndex TextBuffer::getHeightFromEntry( 
	ListOfTexts::const_iterator textIterator ) const throw()
{

	LineIndex count = 0 ;
	
	// textIterator passed by value:
	while ( textIterator != _textEntries.end() )
	{
	
		count += (*textIterator).second->size() ;
		textIterator++ ;
	
	}
	
	return count ;
	
}
	

char * TextBuffer::getNewLine() throw()
{

	char * res = new char[ _width ] ;
	for ( CharAbscissa i = 0; i < _width; i++ )
		res[i] = ' ' ;
	
	return res ;	
		
}



void TextBuffer::deleteTextGrid( TextGrid * grid ) throw()
{

	for ( std::list<char *>::iterator it = grid->begin(); it != grid->end();
			it++ )
		delete [] (*it)	;
		
	delete grid ;
		
}







// Other text operations.



StringSize Ceylan::countChars( const string & aString, char targetChar )
	throw()
{

	StringSize charCount = 0 ;
	
	StringSize size = aString.size() ;
	
	for ( StringSize i = 0; i < size; i++ )
		if ( aString[i] == targetChar )
			charCount++ ;
	
	return charCount ;
	
}


std::string Ceylan::reverse( const std::string & source ) throw() 
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


char * Ceylan::getNonConstCharFrom( const std::string & source ) throw()
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
	 * Too complicated to use (try/catch, even a null pointer checking 
	 * would be uselessly cumbersome ):
	
	char * result =::strdup( source.c_str() ) ;
	
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
	const string & toBeReplaced, const string & replacement ) throw()
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
		 * If replacement is longer than replaced string, we have 
		 * to add dummy characters at the end of each replaced string,
		 * so that there is enough room for replacement.
		 *
		 */ 
		if ( lenDiff < 0 )
			targetString.insert( charCount + toBeReplaced.size(), 
				- lenDiff, 'x' ) ;
			
		targetString.replace( charCount, replacement.size(), replacement ) ;
		
		/*
		 * If replaced string is longer than replacement, we have to delete
		 * exceeding characters at the end of each replacement, so that the 
		 * end of replaced string is eaten.
		 *
		 */
		if ( lenDiff > 0 )
			 targetString.erase( charCount + replacement.size(), lenDiff ) ;
				 	 
	} 
	while ( true ) ;
	
	return substitutionCount ;
	
}

bool Ceylan::isLetter( char targetChar ) throw()
{

	if (::isalpha( targetChar ) ) 
		return true ;

	return false ;

}


bool Ceylan::isFigure( char targetChar ) throw()
{

	if (::isalpha( targetChar ) ) 
		return true ;

	return false ;

}


bool Ceylan::isAlphanumeric( char targetChar ) throw()
{

	if (::isalnum( targetChar ) ) 
		return true ;

	return false ;

}


bool Ceylan::isPunctuation( char targetChar ) throw()
{

	if (::ispunct( targetChar ) )
		return true ;

	return false ;

}


bool Ceylan::isWhitespace( char targetChar ) throw()
{

	if (::iswspace( targetChar ) ) 
		return true ;

	return false ;

}


string Ceylan::toUppercase( const std::string & text ) throw()
{

	string result ;
	
	for ( string::const_iterator it = text.begin(); 
			it != text.end(); it++ )
		if (::islower( *it ) )
			result +=::toupper( *it ) ;
		else
			result += *it ;
	
	return result ;			
	
}


string Ceylan::encodeToHTML( const std::string & message ) throw()
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


string Ceylan::encodeToPhonetic( const std::string & message ) throw()
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
		"zulu" } ;
	
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
	
		if (::isupper( *it ) )
			result += toUppercase( phonetics[ (*it) - 'A' ] ) ;			
		else
			if (::islower( *it ) )
				result += phonetics[ (*it) - 'a' ] ;
			else
				result += *it ;
					
	}			
	
	return result ;
	
}		


string Ceylan::demangleSymbol( const std::string & symbol ) throw()
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
	if ( symbol[0] != 'N' || symbol[symbol.size()-1] != 'E' )
		return symbol ;
	
	// Removes 'N' and 'E':
	const string shorten = symbol.substr( 1, symbol.size() - 2 ) ;
	StringSize shortenSize = shorten.size() ;
	
	DISPLAY_DEBUG_DEMANGLE( "Shorten symbol is " + shorten ) ;	
	
	// We would have now: 3One3Two11ExampleFour
	
	/*
	 * Algorithm: 
	 * - while is a digit, add character to numString (here, read 3, could 
	 * have been 38 or so)
	 * - convert numString into num
	 * - jumps over the word (it might end with digits...) and add it into
	 * result with "::" added
	 * - loop until end of string reached
	 *
	 */ 	
	
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
	
		while (::isdigit( shorten[count] ) )
		{
			numString += shorten[count] ;
			count++ ;
		}
		
		if ( numString.empty() )
		{
			// Abnormal, a misleading symbol such as NxyzE ?
			return symbol ;
		}
		
		// We are at the end of the numerical string:
		num = Ceylan::stringToUnsignedLong( numString ) ;

		if ( num + count > shortenSize )
		{
			// Abnormal, a misleading symbol such as N99xyzE ?
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
	throw()
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
	const string & splittingString ) throw()
{

	list<string> result ;
	
	split<string, const string>( stringToSplit, splittingString, result ) ;
	
	return result ;
	
}
*/


string Ceylan::join( const list<string> & toJoin, 
	const string & joiningString ) throw()
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


list<string> Ceylan::splitIntoWords( const string & sentenceToSplit ) throw()
{
	
	// Maybe here all non-space whitespaces should be managed separatly.
	
	list<string> splitted = Ceylan::split( sentenceToSplit, ' ' ) ;
	
	/*
	 * Some changes are needed in the splitted list in the case where there
	 * are multiple spaces in a row. 
	 * More precisely, if there are in the sentence n spaces in a row, then 
	 * we have A instead of the desired B:
	 * (example pattern: 'a' + n * ' ' + 'b' ):
	 * n=0: A = [ 'ab' ],                B = [ 'ab' ]  		   -> OK
	 * n=1: A = [ 'a', 'b' ],            B = [ 'a', 'b' ]  	   -> OK
	 * n=2: A = [ 'a', '', 'b' ]    ,    B = [ 'a', '', 'b' ]     -> OK
	 * n=3: A = [ 'a', '', '', 'b' ],    B = [ 'a', ' ', 'b' ]    -> KO, 
	 * must be corrected
	 * n=4: A = [ 'a', '', '', '', 'b' ],B = [ 'a', '  ', 'b' ]   -> KO, 
	 * must be corrected
	 *
	 * Solution is: count all list items equal to '' on a row. 
	 * Replace them by an item made of (count-1) spaces, if count is equal 
	 * to 3 or greater.
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
		
			// Here we have a non-empty word. End of space sequence ?
			
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


list<string> Ceylan::splitIntoParagraphs( const string & textToSplit ) throw()
{

	return Ceylan::split( textToSplit, '\n' ) ;
	
}


string Ceylan::formatStringList( const list<string> & stringList, 
	bool surroundByTicks ) throw()
{

	if ( stringList.empty() )
		return "(empty list)" ;
		
	string res ;
	
	if ( TextDisplayable::GetOutputFormat() == TextDisplayable::html )
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
		for ( list<string>::const_iterator it = stringList.begin(); 
				it != stringList.end();	it++ )
			if ( surroundByTicks )	
				res += "\t+ '" + ( *it ) + "'\n" ; 	
			else		
				res += "\t+ " + ( *it ) + '\n' ; 		
	
	}

	return res ;
	
}


string Ceylan::formatStringMap( 
	const std::map<std::string, std::string> & stringMap, 
	bool surroundByTicks ) throw()
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


void Ceylan::display( const string & message ) throw( StringUtilsException )
{

#if CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw StringUtilsException( "Ceylan::display: not available for ARM7." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	iprintf( ( message + '\n' ).c_str()  ) ;

#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS
	
	std::cout << message << std::endl << std::flush ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}


void Ceylan::displayError( const string & errorMessage ) 
	throw( StringUtilsException )
{

#if CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_RUNS_ON_ARM7
	
	throw StringUtilsException( 
		"Ceylan::displayError: not available for ARM7." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	// FIXME: BG_PALETTE_SUB[255] = RGB15(31,0,0);
	display( errorMessage ) ;
	
#endif // CEYLAN_RUNS_ON_ARM7


#else // CEYLAN_ARCH_NINTENDO_DS
	
	std::cerr << errorMessage << std::endl << std::flush ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}
