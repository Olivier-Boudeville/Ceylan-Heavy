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


#ifndef CEYLAN_LOG_PLUG_CONSOLE_H_
#define CEYLAN_LOG_PLUG_CONSOLE_H_


#include "CeylanLog.h"       // for LogException
#include "CeylanLogPlug.h"   // for LogPlug

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
		 * Where messages from a LogSource will be received: 
		 * the second part of the LogTransport-LogListener pair, before
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
		 * various instances of the log framework in their raw version,
		 * and creates the relevant standard channels.
		 *
		 * @see LogPlug
		 *
		 */
		class CEYLAN_DLL LogPlugConsole : public LogPlug
		{
		
			public:



		        /**
		         * Starts the Log console service.
		         *
		         * @param plugInitiator the name of the speaker, for 
				 * instance argv[0].
				 *
				 * @param immediateWrite tells whether the console 
				 * aggregator should store messages as soon as they arrive,
				 * or wait till asked before aggregating them.
				 *
				 * @param smart tells whether the console aggregator 
				 * should be smart.
				 *
		         * @note Since log messages will be output to a file 
				 * named 'sourceName'.log, sourceName must be a valid 
				 * prefix file name.
				 *
				 * @throw LogException if the service could not be started.
				 *
		         */
		        static void StartService( const std::string & plugInitiator, 
					bool immediateWrite = true, bool smart = true ) ;


		        /// Stops the Log console service.
		        static void StopService() ;
				
				
				/**
				 * Returns some informations about the console LogPlug's
				 * state.
				 *
				 */
	           	static const std::string ToString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) ;
				
				
					
	
			private:


				/* 
				
				Not even declared to avoid: 
				'warning: will never be executed'
				
				/// LogPlug console should not be instanciated.
				LogPlugConsole() __attribute__ ((noreturn)) ;
				
				/// This destructor will be never called.
				virtual ~LogPlugConsole() throw() ;
			
				*/


				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */			 
				LogPlugConsole( const LogPlugConsole & source ) ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogPlugConsole & operator = ( const LogPlugConsole & source ) ;
				
	
		} ;	
		
	}

} 



#endif // CEYLAN_LOG_PLUG_CONSOLE_H_

