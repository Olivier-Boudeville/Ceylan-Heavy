#ifndef CEYLAN_TEXT_DISPLAYABLE_H_
#define CEYLAN_TEXT_DISPLAYABLE_H_


#include "CeylanDisplayable.h"  // for inheritance


#include <string>
#include <list>
#include <iosfwd>               // for ostream


namespace Ceylan
{


    /**
     * Interface that every Displayable able to show a textual representation
	 * of itself should implement. 
     *
	 * @note Copy constructor and assignment operator cannot be private, 
	 * because some TextDisplayableinstances need to rely on their copy
	 * constructor, for example for operators returning them by value.
	 *
	 * @note The output format (raw text or HTML format) is retrieved from
	 * overall settings, so that it can be easily chosen at runtime, for 
	 * example thanks to the LogPlug.
	 *
     * @see Displayable.
     *
     */
    class TextDisplayable : public Displayable
    {


        public:



			/// Do-nothing constructor.
			TextDisplayable() throw()
			{
			
			}
			
			
			/// Do-nothing virtual destructor.
			virtual ~TextDisplayable() throw()
			{
			
			}


            /**
             * Returns an user-friendly description of the state of this object.
             *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see TextDisplayable
             *
             */
            virtual const std::string toString( Ceylan::VerbosityLevels level 
				= Ceylan::high ) const throw() = 0 ;



			// Static section.
			
			
            /**
             * Returns a user-friendly description of this list of pointers 
			 * to text displayable instances.
             *
			 * @param displayables a list of pointers to TextDisplayable
			 * instances/
			 *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
             * @see toString, Ceylan::VerbosityLevels
             *
             */			
			static const std::string ToString( 
				std::list<TextDisplayable*> displayables,
				Ceylan::VerbosityLevels level = Ceylan::high ) throw() ;
				
			
			/**
			 * Defines what text output formats for TextDisplayable instances
			 * are available.
			 *
			 */		
			enum TextOutputFormat { rawText, html } ;


			/**
			 * Returns the current overall text format to be used by
			 * TextDisplayable instances.
			 *
			 */		
			static TextOutputFormat GetOutputFormat() throw() ;
			
			
			/**
			 * Sets the current overall text format to be used by
			 * TextDisplayable instances.
			 *
			 * @param newOutputFormat the new output format.
			 *
			 */
			static void SetOutputFormat( TextOutputFormat newOutputFormat )
				throw() ; 
			
			
			
		protected:
		
		
			/**
			 * The text format to be used currently by TextDisplayable
			 * instances.
			 *
			 * @note Defaults to raw text.
			 *
			 */
			static TextOutputFormat _OutputFormat ;
			
			
			
		private:
		
		
			/**
			 * Copy constructor cannot be private since needed in some cases
			 * (ex : Vector instances).
			 *
			 * TextDisplayable( const TextDisplayable & source ) throw() ;
			 *
			 */			 
			
			
			/**
			 * Assignment operator should not be private since it is useful
			 * in some cases (ex : Vector instances).
			 *
			 * TextDisplayable & operator = ( const TextDisplayable & source )
			 * throw() ;
	    	 *
			 */
			 
					
    } ;


}

	
/**
 * Operator used to display easily a TextDisplayable into an output stream.
 * The description is the one returned by toString with high level of detail
 * selected.
 *
 * @see TextDisplayable::toString.
 * 
 */
std::ostream & operator << ( std::ostream & os, 
	const Ceylan::TextDisplayable & textDisplayable ) throw() ;
	



#endif // CEYLAN_TEXT_DISPLAYABLE_H_
