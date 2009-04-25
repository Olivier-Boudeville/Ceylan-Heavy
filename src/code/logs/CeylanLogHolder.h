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


#ifndef CEYLAN_LOG_HOLDER_H_
#define CEYLAN_LOG_HOLDER_H_


#include "CeylanTextDisplayable.h" // for inheritance
#include "CeylanLog.h"             // for LogException
#include "CeylanTypes.h"           // for Ceylan::Uint16

#include <string>



namespace Ceylan
{

	namespace Log
	{
	
	
		/**
		 * List of the log plugs available to the LogHolder:
		 *
		 * - consolePlug: all logs are output in standard console streams,
		 * the default one being the standard log stream (clog), the others 
		 * being the standard output stream (cout) and the standard error
		 * stream (cerr). Associated command-line option: '--consolePlug'.
		 *
		 * - classicalPlug: all logs are output in a *.log file, whose
		 * filename by default matches the executable name, with a raw 
		 * encoding, all logs in chronological order. Associated 
		 * command-line option: '--classicalPlug'
		 *
		 * - HTMLPlug: all logs are output in a set of HTML files 
		 * gathered by a generated frameset, where each log channel has a 
		 * dedicated HTML page. Associated command-line option:
		 * '--HTMLPlug'
		 *
		 * - nullPlug: no logs are stored. Associated command-line option:
		 * '--nullPlug'
		 *   
		 */
		enum KnownPlugs 
		{
		
			consolePlug,
			classicalPlug,
			HTMLPlug,
			nullPlug
										
		} ;
	
		
		
		/**
		 * The LogHolder object is made to be instanciated as an automatic
		 * variable in main() functions, so that it is automatically
		 * in charge of the Log system's life cycle. 
		 *
		 * Besides, it allows to select the log plug to use at run-time,
		 * thanks the command-line plug options that may be passed to the
		 * executable and, therefore, to the LogHolder instance.
		 *
		 * If this program has been called with '--HTMLPlug' as parameter
		 * in the command line, then the HTML log plug with be selected
		 * instead of the classical raw text plug ('--classicalPlug') or the
		 * console plug ('--consolePlug'), and log messages will be output
		 * accordingly.
		 * Chosing '--nullPlug' will result in no logs being output.
		 *
		 * The classical plug is the default one.
		 *
		 * The LogHolder instance should preferably by created outside the
		 * try/catch pair to ensure it is destroyed after all automatic
		 * variables (which must be within the try/catch pair), since their
		 * destruction might result in log messages. Thus no operation on 
		 * a LogHolder instance should result in an exception, instead an
		 * emergency shutdown is triggered.
		 *
		 * @example: 
		 * <pre>
		 * int main( int argc, char * argv[] )
		 * {
		 * 
		 *   LogHolder log( argc, argv ) ;
		 *  
		 *   try 
		 *   { 
		 *
		 *     // ensures LogHolder will be the last to be deallocated.
		 * 
		 *     ... put your program here, automatic variables included ...
		 *
		 *   } 
		 *   catch( etc. )
		 *   {
		 *	   ...
		 *   }	
		 *
		 * }
		 * </pre>
	 	 *
		 *
	 	 */
		class CEYLAN_DLL LogHolder: public TextDisplayable
		{
	
				
			public:
			
			
				/**
				 * Creates a LogHolder.
				 *
				 * Reads the command-line arguments, and uses them to 
				 * choose a specific LogPlug, if specified, otherwise uses
				 * default log plug. 
				 *
				 * Command-line parameters are left untouched (read-only). 
				 *
				 * Only the first log plug specification found in
				 * command-line is taken into account.
				 *
				 * @param argCount the number of arguments (argc).
				 *
				 * @param arguments the arguments themselves (argv).
				 *
				 * @param immediateWrite if true, selects immediate write
				 * for the selected plug, if supported (this is the case of 
				 * the console and the classical plug, but not for the HTML
				 * one); otherwise (if false), deferred writes will be
				 * performed (if the plug supports it).
				 *
				 * @note There is no point in using more than one instance 
				 * of LogHolder in a process.
				 *
				 * @throw LogException if the log system could not be
				 * correctly started.
				 *
		 		 */
				LogHolder( Ceylan::Uint16 argCount, 
					const char * const arguments[],
					bool immediateWrite = true ) ;
			
	
				/// Basic virtual destructor.
				virtual ~LogHolder() throw() ;


								
				/**
				 * Returns a user-friendly description of the state of 
				 * this object.
				 *
				 * @see TextDisplayable, Displayable
				 * @see Ceylan::VerbosityLevels
				 *
				 */
				virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;
								
				
				
				/**
				 * Returns whether the specified option is a known plug 
				 * option.
				 *
				 * Useful for command-line argument parsing.
				 *
				 * @example "--consolePlug" returns true.
				 *
				 */ 
				static bool IsAKnownPlugOption( const std::string & option ) ;
					
					
									
				/**
				 * Command-line option which is to be used to select the
				 * console plug.	
				 *
				 */		
				static const std::string ConsolePlugOption ;
				
								
				/**
				 * Command-line option which is to be used to select the
				 * classical plug, which is the default plug.	
				 *
				 */		
				static const std::string ClassicalPlugOption ;
				
				
				/**
				 * Command-line option which is to be used to select the HTML
				 * plug.		
				 */		
				static const std::string HTMLPlugOption ;

										
				/**
				 * Command-line option which is to be used to select the null
				 * plug.		
				 */		
				static const std::string NullPlugOption ;
										
				
				/**
				 * The default plug, chosen if no specific plug is 
				 * requested thanks to command-line parameters.
				 *
				 */
				static KnownPlugs DefaultPlug ;
				
				
				
			protected:	 		
			
				/// Records the chosen plug, in order to be able to stop it.
				KnownPlugs _chosenPlug ;
				
				
				
			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogHolder( const LogHolder & source ) ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogHolder & operator = ( const LogHolder & source ) ;
				
								
		} ;

	}

}


#endif // CEYLAN_LOG_HOLDER_H_

