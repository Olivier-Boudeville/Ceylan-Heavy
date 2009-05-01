/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#ifndef CEYLAN_CONSOLE_H_
#define CEYLAN_CONSOLE_H_



#include "CeylanTextDisplayable.h"   // for inheritance
#include "CeylanTypes.h"             // for Ceylan::Uint8 and al
#include "CeylanSystem.h"            // for SystemException
#include "CeylanTextBuffer.h"        // for CharAbscissa and al




namespace Ceylan
{



	namespace System
	{
	
	
	
	
		/**
		 * Provides a console abstraction for basic output of text in a
		 * terminal.
		 *
		 * Uses a character buffer.
		 *
		 */
		class CEYLAN_DLL Console : public Ceylan::TextDisplayable
		{


			public:
					
				
				
				/**
				 * Use this constant when terminal supports unlimited width
				 * (i.e. performs line-wrapping).
				 *
				 */
				static const TextBuffer::CharAbscissa UnlimitedWidth = 0 ;
				
				
				
				/**
				 * Use this constant when terminal supports unlimited height
				 * (i.e. performs line-scrolling).
				 *
				 */
				static const TextBuffer::CharOrdinate UnlimitedHeight = 0 ;
				
				
				
				
				/**
				 * Exception thrown when an operation on a Console failed.
				 *
				 */
				class CEYLAN_DLL ConsoleException : public SystemException
				{
				
					public: 
					
						explicit ConsoleException( 
								const std::string & reason ) : 
							SystemException( reason )
						{
						
						}
						
				} ;
						
					
						
						
				/**
				 * Constructor for a basic console, mostly suitable for
				 * debugging.
				 *
				 * The console will use all available terminal space.
				 *
				 * On classical terminals (ex: xterm-like), it implies that
				 * terminal-provided line-wrapping and scrolling will be used.
				 * 
				 * This console will start with a raw layout.
				 *
				 * On Nintendo DS, it implies it will take the full extent
				 * of the bottom LCD screen and will provide a text area of 
				 * 32x24 characters. As a side-effect, the sub 2D core will be
				 * powered on, the console will use the second (sub) core, here
				 * located at the bottom actual screen, and using the VRAM bank
				 * C, the background #0 and the default font.
				 * Only available for the ARM9.
				 *
				 * @param startInForeground tells whether the console is to
				 * start in foreground (thus directly taking control of the
				 * its display and input devices, i.e. keys) or if it should
				 * be on the background, storing text non-interactively until
				 * it is set to foreground.
				 *
				 * @throw ConsoleException if the operation failed or is not
				 * supported.
				 *
				 */
				explicit Console( bool startInForeground = true ) ;
			
			
			
				/**
				 * Constructor for a basic console, mostly suitable for
				 * debugging. It 
				 *
				 * @param startingX the abscissa of the top-left corner of
				 * the rectangle used for text output.
				 *
				 * @param startingY the ordinate of the top-left corner of
				 * the rectangle used for text output.
				 *
				 * @param width the width allowed for text output, starting 
				 * from startingX.
				 *
				 * @param height the height allowed for text output, starting 
				 * from startingY.
				 *
				 * @param layout the desired text layout for this buffer.
				 *
				 * @param useBottomScreen tells whether the bottom or the top
				 * physical screen should be used (default: bottom screen).
				 *
				 * @param useSubCore tells whether the main or the sub 2D
				 * engine should be used (default: sub core).
				 *
				 * @see TextBuffer::TextLayout
				 *
				 * @throw ConsoleException if the operation failed or is not
				 * supported.
				 *
				 */
				Console( TextBuffer::CharAbscissa startingX,
						 TextBuffer::CharOrdinate startingY,
						 TextBuffer::CharAbscissa width,
						 TextBuffer::CharOrdinate height,
						 TextBuffer::TextLayout   layout,
						 bool                     useBottomScreen = true,
						 bool                     useSubCore = true,
						 bool                     startInForeground = true ) ;
	
	
	
				/// Virtual destructor.
				virtual ~Console() throw() ;



				/**
				 * Sets the console to foreground if needed, render its text
				 * and enter its event loop so that the user can use the 
				 * keys to browse its content.
				 *
				 * Key controls are:
				 *  - button X: go to previous paragraph
				 *  - button B: go to next paragraph
				 *  - button up: go to previous line
				 *  - button down: go to next line
				 *  - button Y: toggle text layout (raw/justified/word-wrapped)
				 *  - button A: quit
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				virtual void goInteractive() ; 

				

				/// Returns the text layout being currently used.
				virtual TextBuffer::TextLayout getTextLayout() const ;
			
			
			
				/**
				 * Sets a new text layout.
				 * Triggers an update of the preformatted text.
				 *
				 * @param newLayout the new layout to be used from now on.
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				virtual void setTextLayout( TextBuffer::TextLayout newLayout ) ;




				// Buffer manipulation section.
				

				/**
				 * Makes the console display next text on top
				 *
				 * Does nothing if there is no text left.
				 *
				 * @return true iff there was a text left indeed.
				 *
				 */
				virtual bool jumpNextText() ;



				/**
				 * Makes the console display previous text on top
				 *
				 * Does nothing if there is no text left.
				 *
				 * @return true iff there was a prior text indeed.
				 *
				 */
				virtual bool jumpPreviousText() ;



				/**
				 * Offsets the console display text of one line to the bottom.
				 *
				 * Does nothing if there is no line left.
				 *
				 * @return true iff there was a line left indeed.
				 *
				 */
				virtual bool jumpNextLine() ;
				
				

				/**
				 * Offsets the console display text of one line to the bottom.
				 *
				 * Does nothing if there is no line left.
				 *
				 * @return true iff there was a line left indeed.
				 *
				 */
				virtual bool jumpPreviousLine() ;
				


				/** 
				 * Adds specified text in the character buffer of this console.
				 *
				 * @note Does not render anything, just stores the text.
				 *
				 * @param text the text to add.
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				virtual void addInBuffer( const std::string & text ) ;



				/** 
				 * Blanks the character buffer of this console.
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				virtual void blankBuffer() ;





				// Render section.



				/**
				 * Sets the console either in the foreground (hence using the
				 * screen and the keys), or in the background (not using them).
				 *
				 * @param toForeground sets the console to foreground iff true.
				 *
				 * @note On transition from foreground to background, do not
				 * change the video or input settings, the caller is expected
				 * to set them as wished after the call.
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				virtual void setToForeground( bool toForeground = true ) ; 



				/**
				 * Renders in console output the current text of its 
				 * character buffer.
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				virtual void render() ; 
				
				
				
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
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;


				
				/**
				 * Sets for the console the key repeat settings.
				 *
				 * @param durationBeforeFirstRepeat duration in milliseconds
				 * before, once a key is help, it starts repeating.
				 *
				 * @param durationBetweenRepeats duration in milliseconds 
				 * before two key repeats are triggered, once they are 
				 * repeating.
				 *
				 * @throw ConsoleException if the key repeat could not be set.
				 *
				 */
				static void SetKeyRepeat( 
					Millisecond durationBeforeFirstRepeat = 300,
					Millisecond durationBetweenRepeats    = 100 ) ;
				
				
				
				/**
				 * Initializes the console for text output, with as little
				 * side-effects as possible.
				 *
				 * After that call, Ceylan::display can be used.
				 *
				 * This method performs action only on the ARM9 of the 
				 * Nintendo DS.
				 *
				 * @param useBottomScreen tells whether the bottom or the top
				 * physical screen should be used (default: bottom screen).
				 *
				 * @param useSubCore tells whether the main or the sub 2D
				 * engine should be used (default: sub core).
				 *
				 * @param force tells whether the initialization is to be
				 * performed unconditionally (if true) or if we should rely on
				 * the memory of this method (if false; rendering settings may
				 * have been modified outside of this method, thus without it
				 * knowing about them). Default is to rely on the method memory.
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				static void Initialize( bool useBottomScreen = true,
						bool useSubCore = true, bool force = false ) ;
				
				
				
				/**
				 * Escape sequences for color backgrounds in terminal.
				 *
				 * Just use for example:
				 *  cout << Console::BackgroundColor::Yellow << "Hello!" ;
				 *
				 * @note Some color names may relate to the same actual colors
				 * (ex: Grey and Black).
				 *
				 */
				struct BackgroundColor
				{
		
					static const char * const Red ;
					static const char * const Green ;
					static const char * const Blue ;
					static const char * const Cyan ;
					static const char * const White ;
					static const char * const Yellow ;
					static const char * const Magenta ;
					static const char * const Grey ;
					static const char * const Black ;
					static const char * const Default ;
					
				} ;



				/**
				 * Escape sequences for color foregrounds in terminal.
				 *
				 * Just use for example:
				 *  cout << Console::ForegroundColor::Red << "Hello!" ;
				 *
				 * @note Some color names may relate to the same actual colors
				 * (ex: Grey and Black).
				 *
				 */
				struct ForegroundColor
				{
		
					static const char * const Red ;
					static const char * const Green ;
					static const char * const Blue ;
					static const char * const Cyan ;
					static const char * const White ;
					static const char * const Yellow ;
					static const char * const Magenta ;
					static const char * const Grey ;
					static const char * const Black ;
					static const char * const Default ;
					
				} ;

	
	
				// Terminal attributes.
				
				static const char * const DefaultColors ;
				
				static const char * const Bold ;
				static const char * const Faint ;
				static const char * const BoldAndFaintOff ;
				
				static const char * const Underline ;
				static const char * const UnderlineOff;
				
				static const char * const Blinking ;
				static const char * const BlinkingOff ;
				
				static const char * const NegativeImage ;
				static const char * const NegativeImageOff ;

				static const char * const InvisibleImage ;
				static const char * const InvisibleImageOff ;
				
							
			
			protected:
			
			
				/**
				 * Initializes a console, used by constructors.
				 *
				 * @throw ConsoleException if the operation failed or is not
				 * supported.
				 *
				 * @param useBottomScreen tells whether the bottom or the top
				 * physical screen should be used (default: bottom screen).
				 *
				 * @param useSubCore tells whether the main or the sub 2D
				 * engine should be used (default: sub core).
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				virtual void initConsole( 
						TextBuffer::CharAbscissa startingX,
						TextBuffer::CharOrdinate startingY,
						TextBuffer::CharAbscissa width, 
						TextBuffer::CharOrdinate height,
						TextBuffer::TextLayout   layout,
						bool                     useBottomScreen = true,
						bool                     useSubCore = true ) ; 

				
				
				TextBuffer::CharAbscissa _xstart ;
				
				TextBuffer::CharOrdinate _ystart ;
				
				
				
				/// Buffer storing all the texts.
				TextBuffer * _buffer ;
				
				
				
				/**
				 * Tells whether the console is in interactive mode or hidden
				 * from the user.
				 *
				 */
				bool _inForeground ;
				
				
				/// Tells whether any bottom screen should be used.
				bool _useBottomScreen ;
				
				
				/// Tells whether any sub rendering core should be used.
				bool _useSubCore ;
		
		
				
				
			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				Console( const Console & source ) ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				Console & operator = ( const Console & source ) ;
	
	
		} ;
		
				
	}	
		
}



#endif // CEYLAN_CONSOLE_H_

