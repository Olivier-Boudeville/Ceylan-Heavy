#ifndef CEYLAN_CONSOLE_H_
#define CEYLAN_CONSOLE_H_



#include "CeylanTextDisplayable.h"   // for inheritance
#include "CeylanTypes.h"             // for Ceylan::Uint8 and al
#include "CeylanSystem.h"            // for SystemException
#include "CeylanStringUtils.h"       // for CharAbscissa and al


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
								const std::string & reason ) throw() : 
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
				 * On Nintendo DS, it implies it will take the full extent
				 * of the bottom LCD screen and will provide a text area of 
				 * 32x24 characters. As a side-effect, the 2D cores will be
				 * powered on, the console will use the second (sub) core, here
				 * located at the bottom actual screen, and using the VRAM bank
				 * C, the background #0 and the default font.
				 * Only available for the ARM9.
				 *
				 * @throw ConsoleException if the operation failed or is not
				 * supported.
				 *
				 */
				explicit Console() throw( ConsoleException ) ;
			
			
			
				/**
				 * Constructor for a basic console, mostly suitable for
				 * debugging.
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
				 * @throw ConsoleException if the operation failed or is not
				 * supported.
				 *
				 */
				Console( TextBuffer::CharAbscissa startingX,
						 TextBuffer::CharOrdinate startingY,
						 TextBuffer::CharAbscissa width,
						 TextBuffer::CharOrdinate height )
					throw( ConsoleException ) ;
	
	
				/// Destructor.
				virtual ~Console() throw() ;



				// Buffer manipulation section.



				/**
				 * Makes the console display next text on top
				 *
				 * Does nothing if there is no text left.
				 *
				 * @return true iff there was a text left indeed.
				 *
				 */
				virtual bool jumpNextText() throw() ;


				/**
				 * Makes the console display previous text on top
				 *
				 * Does nothing if there is no text left.
				 *
				 * @return true iff there was a prior text indeed.
				 *
				 */
				virtual bool jumpPreviousText() throw() ;



				/**
				 * Offsets the console display text of one line to the bottom.
				 *
				 * Does nothing if there is no line left.
				 *
				 * @return true iff there was a line left indeed.
				 *
				 */
				virtual bool jumpNextLine() throw() ;
				

				/**
				 * Offsets the console display text of one line to the bottom.
				 *
				 * Does nothing if there is no line left.
				 *
				 * @return true iff there was a line left indeed.
				 *
				 */
				virtual bool jumpPreviousLine() throw() ;
				


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
				virtual void addInBuffer( const std::string & text ) 
					throw( ConsoleException ) ;


				/** 
				 * Blanks the character buffer of this console.
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				virtual void blankBuffer() throw( ConsoleException ) ;



				// Render section.


				/**
				 * Renders in console output the current text of its 
				 * character buffer.
				 *
				 * @throw ConsoleException if the operation failed.
				 *
				 */
				virtual void render() throw( ConsoleException ) ; 
				
				
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
						Millisecond durationBeforeFirstRepeat = 800,
						Millisecond durationBetweenRepeats    = 500 ) 
					throw( ConsoleException ) ;
				
				
				
				/**
				 * Escape sequences for color backgrounds in terminal.
				 *
				 * Just use for example:
				 *  cout << Console::BackgroundColor::Yellow << "Hello !" ;
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
				 *  cout << Console::ForegroundColor::Red << "Hello !" ;
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
				 */
				void initConsole( 
						TextBuffer::CharAbscissa startingX,
						TextBuffer::CharOrdinate startingY,
						TextBuffer::CharAbscissa width, 
						TextBuffer::CharOrdinate height)
					throw( ConsoleException ) ; 


				TextBuffer::CharAbscissa _xstart ;
				TextBuffer::CharOrdinate _ystart ;
				
				
				/// Buffer storing all the texts.
				TextBuffer * _buffer ;
				
				
				
			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				Console( const Console & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				Console & operator = ( const Console & source )
					throw() ;
	
	
		} ;
				
	}	
		
}


#endif // CEYLAN_CONSOLE_H_
