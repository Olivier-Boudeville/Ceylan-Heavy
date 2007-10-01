#ifndef CEYLAN_TEXT_BUFFER_H_
#define CEYLAN_TEXT_BUFFER_H_


#include "CeylanStringUtils.h"    // for StringUtilsException

#include <string>                 // for string, string::size_type
#include <list>                   // for list
#include <utility>                // for std::pair




namespace Ceylan
{
	
	

	/**
	 * Stores a sequence of texts, and allows to access the overall text
	 * according to various ways, either text-by-text or line-by-line.
	 * 
	 * Computes the corresponding character layout for onscreen rendering,
	 * depending on the specified abstract screen dimensions (expressed in
	 * characters, not in pixels, as character look-up and rendering are
	 * uncoupled here).
	 *
	 * The abstract screen is a kind of grid of characters, of 
	 * user-specified size, that can slide over parts of the stored text.
	 *
	 * This helps for example fixed-font text rendering.
	 *
	 */
	class CEYLAN_DLL TextBuffer: public Ceylan::TextDisplayable 	
	{


		public:
					
					
	
			/// Exception raised by text buffers. 
			class CEYLAN_DLL TextBufferException: public StringUtilsException
			{
	
	
				public:
		
					TextBufferException( const std::string & message ) throw(): 
						StringUtilsException( message )
					{		
			
					}
			
					virtual ~TextBufferException() throw()
					{
			
					}	
		
			} ;	

				
					
			/**
			 * Describes the various modes supported for text layout.
			 *
			 */
			enum TextLayout
			{
			
				/**
				 * Letters displayed on after the other with no regards for 
				 * words.
				 *
				 */
				Raw,
				
				/**
				 * Words are wrapped (not cut), but text is not justified.
				 *
				 */
				WordWrapped,
				
				/**
				 * Words are wrapped (not cut), and text is justified.
				 *
				 */
				Justified
			
			} ;
			
			
			
			/// Abscissa index of a character in a buffer.	
			typedef Ceylan::Uint8 CharAbscissa ;
				
			/// Ordinate index of a character in a buffer.	
			typedef Ceylan::Uint8 CharOrdinate ;
			
			
			/// Index of a text in buffer list.
			typedef Ceylan::Uint32 TextIndex ;
			
			/// Index of a preformatted line in a text.
			typedef Ceylan::Uint32 LineIndex ;
			
			
			
			/** 
			 * Creates a new text buffer, whose sliding window (abstracted
			 * screen) is of specified size.
			 *
			 * @param width the width of the character grid.
			 *
			 * @param height the width of the character grid.
			 *
			 * @param layout the desired text layout for this buffer.
			 *
			 * @see TextLayout
			 *
			 * By default, an alinea is 2-character wide.
			 *
			 * @see setAlineaWidth
			 *
			 * @throw StringUtilsException if the operation failed.
			 *
			 */		
			TextBuffer( CharAbscissa screenWidth, CharOrdinate screenHeight,
				TextLayout layout = Raw ) throw( TextBufferException ) ;
			
			
			/// Virtual destructor.
			virtual ~TextBuffer() throw() ;
						
							
	
				
			/**
			 * Returns the width of the abstract screen associated to this
			 * buffer.
			 *
			 */
			virtual CharAbscissa getWidth() const throw() ;
		
		
			/**
			 * Returns the height of the abstract screen associated to this
			 * buffer.
			 *
			 */
			virtual CharOrdinate getHeight() const throw() ;			
			


			/// Returns the text layout being currently used.
			virtual TextLayout getTextLayout() const throw() ;
		
			
			/**
			 * Sets a new text layout.
			 * Triggers an update of the preformatted text.
			 *
			 * @param newLayout the new layout to be used from now on.
			 *
			 * @throw TextBufferException if the operation failed.
			 *
			 */
			virtual void setTextLayout( TextLayout newLayout ) 
				throw( TextBufferException ) ;


			/**
			 * Returns the current alinea width, in characters.
			 *
			 */
			virtual CharAbscissa getAlineaWidth() const throw() ;
			
			
			/**
			 * Sets a new alinea width, in character.
			 *
			 * @param newAlineaWidth the new alinea width, in character.
			 * It can be null for no alinea at all.
			 *
			 */
			virtual void setAlineaWidth( CharAbscissa newAlineaWidth )
				throw() ;
				
				
			/**
			 * Adds specified text in the buffer.
			 *
			 * Updates the internal virtual screen accordingly.
			 *
			 * @param text the text to add in buffer.
			 *
			 * @throw TextBufferException if the operation failed.
			 *
			 */
			virtual void add( const std::string & text ) 
				throw( TextBufferException ) ;
			
			
			/**
			 * Blanks this whole buffer.
			 *
			 * Removes all text content.
			 *
			 */
			virtual void blank() throw() ;
			
			
			
			
			// Screen positioning section.
			
			
			/**
			 * Centers the abstract screen on next text entry, if any.
			 *
			 * Does nothing if there is no text left.
			 *
			 * @return true iff there was a text entry left indeed.
			 *
			 */
			virtual bool jumpNextText() throw() ;
			
			
			/**
			 * Centers the abstract screen on previous text, if any.
			 *
			 * Does nothing if there is no text left.
			 *
			 * @return true iff there was a prior text entry indeed.
			 *
			 */
			virtual bool jumpPreviousText() throw() ;
			
			
			
			/**
			 * Makes the abstract screen go one line down.
			 *
			 * Does nothing if there is no text line left (already at bottom).
			 *
			 * @return true iff there was a line left indeed.
			 *
			 */
			virtual bool jumpNextLine() throw() ;
			
			
			/**
			 * Makes the abstract screen go one line up.
			 *
			 * Does nothing if there is no text line left (already on top).
			 *
			 * @return true iff there was a line left indeed.
			 *
			 */
			virtual bool jumpPreviousLine() throw() ;
			
			
			
			/**
			 * Records a preformatted text entry, whose escape sequences 
			 * (\t,\n) have been translated into a series of basic characters
			 * (alphanumerical and spaces) stored in a list of lines, 
			 * each line being an array of width characters (char *), to fit 
			 * in character grid.
			 *
			 */
			typedef std::list<char *> TextGrid ;

			 
			/**
			 * Returns the list of lines that should be displayed, should
			 * this buffer be rendered.
			 *
			 * Depends on the stored texts and on the location of the abstract
			 * screen.
			 *
			 * The width of each line is the one of this buffer (char[_width]).
			 * The screen list has between [0.._height] lines, missing ones mean
			 * they are blank.
			 *
			 */
			virtual const TextGrid & getScreenLines() const throw() ;
						
					
						 
			/**
			 * Returns an user-friendly description of the state of
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
						
			
			/// Number of spaces corresponding to one tabulation.
			static const CharAbscissa TabSpacing = 4 ;
			
			
			
			
		protected:
	

			/**
			 * Recomputes cached grid lines with current settings.
			 * Any previous ones are removed first.
			 *
			 * @note The current line of text will be reset to the first line
			 * of the current text, as any change in dimension or layout will
			 * invalidate line content.
			 *
			 */
			virtual void recomputeGrids() throw() ;
			 
			 
			/**
			 * Updates screen lines according to current buffer text and line 
			 * index.
			 *
			 * Recreates from scratch the list of line references.
			 *
			 */
			void updateScreenLines() throw() ;
			
			
			/**
			 * Translates specified text entry into a list of lines of buffer
			 * width, according to current screen dimensions and layout.
			 *
			 * @note Ownership of the text grid is transferred to the caller.
			 *
			 * @see deleteTextGrid
			 *
			 */
			TextGrid & createTextGridFrom( const std::string & text ) throw() ;
			
			
			/// Deletes a TextGrid.
			void deleteTextGrid( TextGrid * grid ) throw()	;



			/**
			 * Translates specified text entry into a list of lines of buffer
			 * width, with no special concern for layout: text written letter
			 * by letter.
			 *
			 * @note Ownership of the text grid is transferred to the caller.
			 *
			 */
			TextGrid & createRawGridFrom( const std::string & text ) 
				throw() ;


			/**
			 * Translates specified text entry into a list of lines of buffer
			 * width, with word-wrapped or justified (i.e. word-wrapped with
			 * adapted spaces) lines, depending on current layout.
			 *
			 * @note Ownership of the text grid is transferred to the caller.
			 *
			 */
			TextGrid & createAdvancedGridFrom( const std::string & text ) 
				throw() ;

			

			/**
			 * Tells how many lines spread from the current position to 
			 * the end of text.
			 *
			 */
			LineIndex getHeightFromCurrentPosition() const throw() ;
				

			
			/**
			 * A text entry is made of a chunk of text and its precomputed
			 * lines (if any).
			 *
			 */
			typedef std::pair<std::string, TextGrid*> TextEntry ;


/*
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks.
 *
 */
#pragma warning( push )
#pragma warning( disable : 4251 )

			/// A list of text entries.
			typedef std::list<TextEntry> ListOfTexts ;

#pragma warning( pop ) 
				
			/**
			 * Tells how many lines spread from the beggining of specified 
			 * text entry to the end of text.
			 *
			 */
			LineIndex getHeightFromEntry( 
				ListOfTexts::const_iterator textIterator ) const throw() ;
				
				
			/**
			 * Creates a new blank line of the appropriate width (length).
			 *
			 * @note Ownership of the line is transferred to the caller.
			 *
			 */
			char * getNewLine() throw() ;
				

/*
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks.
 *
 */
#pragma warning( push )
#pragma warning( disable : 4251 )
			

			/**
			 * Records the text stored by this buffer and its precomputed 
			 * lines.
			 *
			 */
			ListOfTexts _textEntries ;

#pragma warning( pop ) 
	
	
			/// The width of the character grid.
 			CharAbscissa _width ;
			
			/// The ordinate of the character grid.
			CharOrdinate _height ;
			
			
			/// Records the current text layout being used.
			TextLayout _layout ;
			
			/// The width, in characters, of a paragraph alinea.
 			CharAbscissa _alineaWidth ;
					
/*
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks.
 *
 */
#pragma warning( push )
#pragma warning( disable : 4251 )

			/// The index of the text entry being rendered.
			ListOfTexts::const_iterator _currentText ;
			
			/// Index of a preformatted line in current rendered text grid.
			TextGrid::const_iterator _currentLine ;			
			
			/// The current screen, seen as a list of (at most _height) lines.	
			TextGrid _screenLines ;
			
#pragma warning( pop ) 						
			
			
			
		private:
			
				
			/**
			 * Copy constructor made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			TextBuffer( const TextBuffer & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it 
			 * will be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			TextBuffer & operator = ( const TextBuffer & source ) throw() ;
	
	
	} ;
		

}


#endif // CEYLAN_TEXT_BUFFER_H_

