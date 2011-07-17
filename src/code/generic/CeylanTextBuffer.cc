/*
 * Copyright (C) 2003-2011 Olivier Boudeville
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
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "CeylanTextBuffer.h"


#include "CeylanOperators.h"
#include "CeylanLogPlug.h"             // for the LogPLug
#include "CeylanSystem.h"              // for System::Size
#include "CeylanMathsBasic.h"          // for Maths::Round


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for CEYLAN_DEBUG_DEMANGLE, etc.
#endif // CEYLAN_USES_CONFIG_H



using std::string ;
using std::list ;
using std::pair ;

using namespace Ceylan ;
using namespace Ceylan::Log ;


#if CEYLAN_DEBUG_TEXTBUFFER

#include "CeylanLogLight.h"
#define CEYLAN_TEXTBUFFER_LOG(message) LogPlug::debug(message)

#else // CEYLAN_DEBUG_TEXTBUFFER

#define CEYLAN_TEXTBUFFER_LOG(message)

#endif // CEYLAN_DEBUG_TEXTBUFFER




TextBuffer::TextBuffer( CharAbscissa screenWidth, CharOrdinate screenHeight,
		TextLayout layout ) :
	_width( screenWidth ),
	_height( screenHeight ),
	_layout( layout ),
	_alineaWidth( 2 )
{

	// Starts with no text.
	_currentText = _textEntries.end() ;

}



TextBuffer::~TextBuffer() throw()
{

	// Deallocates all text grids:
	blank() ;

}



TextBuffer::CharAbscissa TextBuffer::getWidth() const
{

	return _width ;

}



TextBuffer::CharOrdinate TextBuffer::getHeight() const
{

	return _height ;

}



TextBuffer::TextLayout TextBuffer::getTextLayout() const
{

	return _layout ;

}



void TextBuffer::setTextLayout( TextLayout newLayout )

{


	if ( newLayout == _layout )
		return ;

	_layout = newLayout ;

	recomputeGrids() ;

}



TextBuffer::CharAbscissa TextBuffer::getAlineaWidth() const
{

	return _alineaWidth ;

}



void TextBuffer::setAlineaWidth( CharAbscissa newAlineaWidth )
{

	_alineaWidth = newAlineaWidth ;

}



void TextBuffer::add( const std::string & text )
{

	TextGrid * newGrid = & createTextGridFrom( text ) ;


	_textEntries.push_back( TextEntry( text, newGrid ) ) ;


	// Update text iterator if it is the first text added:
	if ( _textEntries.size() == 1 )
	{
		_currentText = _textEntries.begin() ;
		_currentLine = newGrid->begin() ;
	}

	// Scrolls:

	Ceylan::Uint32 addedLinesCount = static_cast<Ceylan::Uint32>(
			newGrid->size() ) ;

	for ( Ceylan::Uint32 i = 0; i < addedLinesCount; i++ )
		jumpNextLineNoRefresh() ;

	// Needed in all cases (ex: a second text showing up after the first):
	updateScreenLines() ;

}



void TextBuffer::blank()
{

	// Removes all text:

	for ( std::list<TextEntry>::iterator it = _textEntries.begin();
			it != _textEntries.end(); it++ )
		deleteTextGrid( (*it).second ) ;

	_textEntries.clear() ;

}




// Screen positioning section.



bool TextBuffer::jumpNextText()
{

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



bool TextBuffer::jumpPreviousText()
{

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



bool TextBuffer::jumpNextLine()
{

	if ( getHeightFromCurrentPosition() <= _height )
		return false ;

	bool moved ;

	TextGrid * currentTextGrid = (*_currentText).second ;

	_currentLine++ ;;

	if ( _currentLine != currentTextGrid->end() )
	{

		moved = true ;

	}
	else
	{

		ListOfTexts::const_iterator nextText = _currentText ;
		nextText++ ;

		if ( nextText != _textEntries.end() )
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

	/*
	 * There is no point in optimizing, as, if removing top line is easy
	 * ('_screenLines.pop_front() ;'), adding latest one involves doing the
	 * same as:
	 *
	 */
	if ( moved )
		updateScreenLines() ;

	return moved ;

}



void TextBuffer::jumpNextLineNoRefresh()
{

	if ( getHeightFromCurrentPosition() <= _height )
		return ;

	TextGrid * currentTextGrid = (*_currentText).second ;

	_currentLine++ ;;

	if ( _currentLine == currentTextGrid->end() )
	{

		ListOfTexts::const_iterator nextText = _currentText ;
		nextText++ ;

		if ( nextText != _textEntries.end() )
		{

			_currentText = nextText ;
			currentTextGrid = (*_currentText).second ;
			_currentLine = currentTextGrid->begin() ;
		}

	}

}



bool TextBuffer::jumpPreviousLine()
{

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

			// Not using rbegin() to avoid mixing regular and reverse iterators:
			_currentLine = currentTextGrid->end() ;

			if ( _currentLine != currentTextGrid->begin() )
				_currentLine-- ;

			moved = true ;

		}
		else
		{
			moved = false ;
		}

	}


	// More optimized than to call 'updateScreenLines() ;':

	if ( moved && _currentLine != currentTextGrid->end() )
	{

		/*
		 * Remove bottom line (beware when going up from last incomplete
		 * screen):
		 *
		 */
		if ( _screenLines.size() == _height )
			_screenLines.pop_back() ;

		// Add top line:
		_screenLines.push_front( *_currentLine ) ;

	}

	return moved ;

}




const TextBuffer::TextGrid & TextBuffer::getScreenLines() const
{

	return _screenLines ;

}



const std::string TextBuffer::toString( Ceylan::VerbosityLevels level ) const
{

	string res = "Text buffer of width " + Ceylan::toNumericalString( _width )
		+ " and of height " + Ceylan::toNumericalString( _height )
		+ ", containing " + Ceylan::toString(
			static_cast<Ceylan::Uint32>( _textEntries.size() ) )
		+ " text(s). Selected text layout is " ;

	switch( _layout )
	{

		case Raw:
			res += "raw" ;
			break ;

		case WordWrapped:
			res += "word-wrapped" ;
			break ;

		case Justified:
			res += "justified" ;
			break ;

		default:
			res += "unexpected (abnormal)" ;
			break ;

	}


	if ( level != Ceylan::high )
		return res ;

	if ( _screenLines.empty() )
		return res + ". Abstract screen is empty" ;

	res += ". Abstract screen contains "
		+ Ceylan::toString( static_cast<Ceylan::Uint32>( _screenLines.size() ) )
		+ " line(s):" ;

	list<string> linesList ;

	// Beware, stored lines are not null-terminated:
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


void TextBuffer::recomputeGrids()
{


	for ( ListOfTexts::iterator it = _textEntries.begin();
		it != _textEntries.end(); it++ )
	{

		if ( (*it).second != 0 )
			deleteTextGrid( (*it).second ) ;

		(*it).second = & createTextGridFrom( (*it).first ) ;

		// Resets line in current text to its first one:
		if ( it == _currentText )
			_currentLine = (*it).second->begin() ;

	}

	updateScreenLines() ;

}



void TextBuffer::updateScreenLines()
{

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
		 *
		 * Here we arrived to the end of the lines for current text, let's
		 * take next, if any:
		 *
		 */
		textIterator++ ;
		resetLineInText = true ;

	}

}



TextBuffer::TextGrid & TextBuffer::createTextGridFrom(
	const std::string & text )
{

	TextBuffer::TextGrid * res ;

	switch( _layout )
	{

		case TextBuffer::Raw:
			res = & createRawGridFrom( text ) ;
			break ;

		// Both handled by the same method that uses _layout to discriminate:
		case TextBuffer::WordWrapped:
		case TextBuffer::Justified:
			res = & createAdvancedGridFrom( text ) ;
			break ;

		default:
			LogPlug::error( "TextBuffer::createTextGridFrom: "
				"unexpected layout, defaulting to raw layout." ) ;
			res = & createRawGridFrom( text ) ;
			break ;

	}

	return *res ;

}



TextBuffer::TextGrid & TextBuffer::createRawGridFrom( const std::string & text )
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
	else
	  delete [] currentLine ;

	return * res ;

}



TextBuffer::TextGrid & TextBuffer::createAdvancedGridFrom(
	const std::string & text )
{

	TextGrid * res = new std::list<char *> ;


	/**
	 * Adapted from OSDL: in
	 * OSDL/trunk/src/code/video/twoDimensional/OSDLFont.cc, see
	 * OSDL::Video::Surface & Font::renderLatin1MultiLineText
	 *
	 */

	list<string> paragraphs = Ceylan::splitIntoParagraphs( text ) ;

	list<string> words = Ceylan::splitIntoWords( paragraphs.front() ) ;
	paragraphs.pop_front() ;

	CharAbscissa currentWidth = _alineaWidth ;
	CharAbscissa storedWidth  = currentWidth ;

	CharAbscissa wordWidth ;


	/*
	 * Start from the left edge, and select as many words as possible within
	 * this line:
	 *
	 */
	CharAbscissa totalWordWidth = 0 ;

	bool lineFull ;
	char * currentLine ;
	list<string> wordsOnTheLine ;
	string currentWord ;


	while ( ! words.empty() )
	{

		// currentWidth already set.

		// Eat words line by line:

		lineFull = false ;
		wordsOnTheLine.clear() ;

		currentLine = getNewLine() ;

		storedWidth = currentWidth ;
		totalWordWidth = 0 ;

		// Selects words for that line, be it justified or not:
		while ( ! lineFull && ! words.empty() )
		{

			// Filling a new line now.

			currentWord = words.front() ;

			/*
			 * Manage words longer than a line by splitting them in a full line
			 * and then the remainder (potentially many lines or an incomplete
			 * line).
			 *
			 * Ensures the first word chunk will be always of the size of a line
			 * and, hence, will begin on the leftmost part of the text, to tell
			 * the reader that it is indeed a full word, even if broken into
			 * pieces.
			 *
			 */
			if ( currentWord.size() > _width )
			{

				string toSplit = currentWord ;

				// Have to split and order correctly:
				words.pop_front() ;

				/*
				 * Keep currentWord for the rest of this while loop:
				 * (_width considered higher than 1)
				 *
				 */
				currentWord = toSplit.substr( /* start index */ 0,
					/* length */ _width - 1 ) ;

				/*
				 * Add, between current word and next real ones, the remaining
				 * part of this word (watch out the order):
				 *
				 */
				words.push_front( toSplit.substr( /* start index */ _width - 1
					/* rest of length */ ) ) ;

				words.push_front( currentWord + "-" ) ;


			}


			// Multiple whitespaces in a row can lead to empty words.

			wordWidth = static_cast<CharAbscissa>( currentWord.size() ) ;

			if ( currentWidth + wordWidth <= _width )
			{

				totalWordWidth += wordWidth ;

				// Supposed either Justified or WordWrapped:
				wordsOnTheLine.push_back( currentWord ) ;

				currentWidth += wordWidth + /* space width */ 1 ;
				words.pop_front() ;

			}
			else
			{
				// With this last word, the line would be too long:
				lineFull = true ;
			}

		}


		/*
		 * Words are selected for the current line, now time to write them in
		 * grid.
		 *
		 */

		System::Size wordCount = wordsOnTheLine.size() ;
		currentWidth = storedWidth ;


		/*
		 * Last part of a paragraph should not be justified: it would result in
		 * huge inter-word spaces, instead the text can stop anywhere before the
		 * line's end.
		 *
		 * Hence we check that 'words' is not empty.
		 *
		 * Zero word or only one word? Do nothing special to justify text.
		 *
		 */
		if ( ( _layout == Justified ) && ! words.empty() && wordCount > 1 )
		{

			for ( list<string>::const_iterator it = wordsOnTheLine.begin();
				it != wordsOnTheLine.end(); it++ )
			{

				wordWidth = static_cast<CharAbscissa>( (*it).size() ) ;

				// Copy word in grid:
				for ( string::const_iterator charIt = (*it).begin() ;
					 charIt != (*it).end(); charIt++ )
				{
					currentLine[currentWidth] = (*charIt) ;
					currentWidth++ ;
				}


				/*
				 * For justified text, space width is computed with
				 * pixel-perfect accuracy.
				 *
				 * Knowing exactly what words fit with normal space width, a new
				 * space width is computed so that these words are dispatched
				 * regularly on the line, and begin and end with it, provided it
				 * is not a paragraph end.
				 *
				 * As this space width has to be an integer, round off errors
				 * would accumulate if a constant corrected space width was
				 * used, and the last word of the line would not end perfectly
				 * at the end of it, which would lead to a rather unpleasant
				 * visual effect: the right edge of the text would not be
				 * vertically aligned.
				 *
				 * To correct that, after each word, the perfect space width for
				 * this step is computed, considering only what remains to be
				 * written.
				 *
				 * Hence the space width is adapted and the text fit perfectly
				 * on the line.
				 *
				 * Better round to lowest integer (ceil or static_cast) than to
				 * nearest, since if space width is rounded up (floor) the text
				 * might be clipped by the line edge.
				 *
				 * Number of spaces is equal to number of remaining words minus
				 * one, the width of the current justified space is the one that
				 * would be chosen if it was divided equally for all remaining
				 * spaces.
				 *
				 */
				wordCount-- ;

				if ( wordCount == 0 )
					break ;

				totalWordWidth -= wordWidth ;


				// Could be computed incrementally to avoid the division:
				currentWidth += /* justified space */ static_cast<CharAbscissa>(
					( _width - currentWidth - totalWordWidth )
						/ wordCount ) ;


			}

		}
		else
		{

			// We do not justify text here:

			for ( list<string>::const_iterator it = wordsOnTheLine.begin();
				it != wordsOnTheLine.end(); it++ )
			{

				// Copy word in grid:
				for ( string::const_iterator charIt = (*it).begin() ;
					 charIt != (*it).end() ; charIt++ )
				{
					currentLine[currentWidth] = (*charIt) ;
					currentWidth++ ;
				}

				// For inter-word space:
				currentWidth++ ;

			}

		}

		res->push_back( currentLine ) ;

		if ( words.empty() )
		{

			if ( paragraphs.empty() )
				break ;

			words = Ceylan::splitIntoWords( paragraphs.front() ) ;
			paragraphs.pop_front() ;


#define CEYLAN_SEPARATE_PARAGRAPHS 0

#if CEYLAN_SEPARATE_PARAGRAPHS

			// One empty line between paragraphs:
			currentLine = getNewLine() ;
			res->push_back( currentLine ) ;

#endif // CEYLAN_SEPARATE_PARAGRAPHS

			currentWidth = _alineaWidth ;

		}
		else
		{
			currentWidth = 0 ;
		}


	}

	return * res ;

}



TextBuffer::LineIndex TextBuffer::getHeightFromCurrentPosition() const
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

		count += static_cast<LineIndex>( (*textIterator).second->size() ) ;
		textIterator++ ;

	}


	return count ;

}



TextBuffer::LineIndex TextBuffer::getHeightFromEntry(
	ListOfTexts::const_iterator textIterator ) const
{

	LineIndex count = 0 ;

	// textIterator passed by value:
	while ( textIterator != _textEntries.end() )
	{

		count += static_cast<LineIndex>( (*textIterator).second->size() ) ;
		textIterator++ ;

	}

	return count ;

}



char * TextBuffer::getNewLine()
{

	char * res = new char[ _width ] ;
	for ( CharAbscissa i = 0; i < _width; i++ )
		res[i] = ' ' ;

	return res ;

}



void TextBuffer::deleteTextGrid( TextGrid * grid )
{

	// Deletes the list elements then the list itself:

	for ( std::list<char *>::iterator it = grid->begin(); it != grid->end();
			it++ )
		delete [] (*it) ;

	delete grid ;

}
