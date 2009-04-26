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


#ifndef CEYLAN_LOG_PLUG_HTML_H_
#define CEYLAN_LOG_PLUG_HTML_H_


#include "CeylanLog.h"     // for LogException
#include "CeylanLogPlug.h" // for LogPlug

#include <string>



namespace Ceylan
{


	namespace Log
	{


		/// Used as plug endpoints, the Log messages come through them.
		class LogSource ;
		
		
		/**
		 * Where messages from a LogSource will be output: the first
		 * part of the LogTransport-LogListener pair.
		 *
		 */
		class LogTransport ;
		
		
		/**
		 * Where messages from a LogSource will be received: the 
		 * second part of the LogTransport-LogListener pair, before
		 * the LogAggregator.
		 *
		 */
		class LogListener ;
		
		
		/**
		 * Where messages are finally user-friendly output. 
		 *
		 */
		class LogAggregator ;
		
		
		
		/**
		 * This class implements the LogPlug interface, registers the 
		 * various instances of the log framework in their HTML version, 
		 * and creates the relevant standard channels. 
		 *
		 * With this Log implementation, log informations will be 
		 * output in HTML format.
		 *
		 * @note As the messages will be stored in memory, and only aggregated
		 * once the program stopped, on one hand if the program crashes the logs
		 * are lost, but on the other hand logging will induce very low
		 * runtime overhead (processing-wise; memory-wise this can be another
		 * story), so this kind of logs can be useful to debug time-critical
		 * operations (ex: a scheduler).
		 *
		 * A web page will correspond to each log channel and list its 
		 * messages.
		 *
		 * All web pages will be gathered by an HTML frameset allowing to 
		 * browse channels by picking their names.
		 *
		 * @example, a channel named 'Ceylan rocks' will be stored as
		 * 'Ceylan-rocks.html'.
		 *
		 * An object channel's name could be something similar to
		 * 'sonata/PID-21097/Example/0x80510a0'. 
		 *
		 * @see LogPlug
		 *
		 */
		class CEYLAN_DLL LogPlugHTML : public LogPlug
		{
		
			public:



		        /**
		         * Starts the Log HTML service.
		         *
		         * @param plugInitiator the name of the speaker, for 
				 * instance argv[0].
				 *
				 * @throw LogException if the service could not be started.
				 * @note plugInitiator will be used to create the
				 * directory which will be containing all HTML log output.
				 *
				 * @param smart tells whether the HTML aggregator should
				 * be smart.
				 *
		         */
		        static void StartService( const std::string & plugInitiator, 
					bool smart = true ) ;


		        /// Stops the Log HTML service.
		        static void StopService() ;
				
				
				/// Returns some informations about the HTML LogPlug's state.
	           	static const std::string ToString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) ;
				
				
					
			protected:	
				 
				 
				 /**
				  * The suffix that will be added to <b>sourceName</b> to
				  * define the log directory.
				  *
				  */
				 static const std::string & LogDirectorySuffix ;
	
	
	
			private:
			
			
				/* 
				
				Not even declared to avoid: 
				'warning: will never be executed'
				/// LogPlug HTML should not be instanciated.
				LogPlugHTML() __attribute__ ((noreturn)) ;

				
				/// This destructor will be never called.
				virtual ~LogPlugHTML() throw() ;
			
				*/
				
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogPlugHTML( const LogPlugHTML & source ) ;
			
			
				/**
				 * Assignment operator made private to ensure that it will<
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogPlugHTML & operator = ( const LogPlugHTML & source ) ;
				
	
		} ;	
		
	}

} 



#endif // CEYLAN_LOG_PLUG_HTML_H_

