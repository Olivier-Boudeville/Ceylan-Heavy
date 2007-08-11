#include "CeylanTextBuffer.h"


#include "CeylanOperators.h"
#include "CeylanLogPlug.h"             // for the LogPLug


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for CEYLAN_DEBUG_DEMANGLE, etc.
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for iprintf, CEYLAN_DS_LOG
#endif // CEYLAN_ARCH_NINTENDO_DS


using std::string ;
using std::list ;
using std::pair ;

using namespace Ceylan ;
using namespace Ceylan::Log ;
		

//#define CEYLAN_DEBUG_TEXTBUFFER 1

#if CEYLAN_DEBUG_TEXTBUFFER

#include "CeylanLogLight.h"
#define CEYLAN_TEXTBUFFER_LOG(message) LogPlug::debug(message)

#else // CEYLAN_DEBUG_TEXTBUFFER

#define CEYLAN_TEXTBUFFER_LOG(message)

#endif // CEYLAN_DEBUG_TEXTBUFFER




TextBuffer::TextBuffer( CharAbscissa screenWidth, CharOrdinate screenHeight,
		TextLayout layout ) throw( TextBufferException ):
	_width( screenWidth ),
	_height( screenHeight ),
	_layout( layout )
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


TextBuffer::TextLayout TextBuffer::getTextLayout() const throw()
{
	return _layout ;
}


void TextBuffer::setTextLayout( TextLayout newLayout ) 
	throw( TextBufferException )
{

	_layout = newLayout ;
	
	//FIXME
	
}
	


void TextBuffer::add( const std::string & text ) throw( TextBufferException )
{


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



bool TextBuffer::jumpPreviousLine() throw()
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
		+ Ceylan::toString( _screenLines.size() ) + " line(s):" ;
	
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

