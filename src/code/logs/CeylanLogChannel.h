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


#ifndef CEYLAN_LOG_CHANNEL_H_
#define CEYLAN_LOG_CHANNEL_H_


#include "CeylanTypes.h"           // for Ceylan::Uint32
#include "CeylanLog.h"             // for LogException
#include "CeylanTextDisplayable.h" // for inheritance


#include <string>
#include <list>



namespace Ceylan
{


	namespace Log
	{
	
	
	
	
		// Log messages are gathered into these Log channels.
		class LogMessage ;
		
		
		
		/**
	 	 * Log channels are thematic containers for log messages, 
		 * meant to be used by LogAggregators.
		 *
	 	 * @see LogAggregator
	 	 *
	 	 */
		class CEYLAN_DLL LogChannel : public TextDisplayable
		{
	
	
	
			/**
			 * Aggregators must able to access to messages of log channels.
			 *
			 */
			friend class LogAggregator ;
			
			
			/**
			 * Necessary, since friend relationships are not inherited
			 * apparently.
			 *
			 */
			friend class LogAggregatorHTML ;
			
			
			/// Designates a number of log messages.
			typedef Ceylan::Uint32 MessageCount ;
			
			
			
			public:
			
			
			
				/**
		 	 	 * Creates a named Log channel.
		 	 	 * 
		 		 */
				explicit LogChannel( const std::string & name ) ;
			
			
	
				/// Basic virtual destructor.
				virtual ~LogChannel() throw() ;



				/**
				 * Add a new Log message to this channel.
				 *
				 * @note this method takes ownership of the specified 
				 * log message.
				 *
				 * @param message the log message to record
				 *
				 * @param check if true, raises a LogException if this
				 * added message's channel does not match this channel.
				 *
				 * @throw LogException if the operation failed.
				 *
				 */
				virtual void addMessage( LogMessage & message, 
					bool check = true ) ;
				
				
				
				/**
				 * Returns this channel's name.
				 *
				 */
				virtual const std::string getName() const ;
				
					
					
				/**
				 * Returns the number of messages this channel 
				 * currently gathered.
				 *
				 */
				virtual MessageCount getMessageCount() const ;
					
					
											
	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from 
				 * overall settings.
				 *
				 * @see TextDisplayable
				 *
	             */
				virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;
				
					
					
								
				
			protected:	



				/// This channel's name.
				std::string _name ;
			

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
				 * Chronologically-ordered set of messages for this
				 * Log channel.
				 *
				 */
				std::list<LogMessage *> _messages ;

#pragma warning( pop ) 								



			private:
			
			
				/**
				 * Copy constructor made private to ensure that it 
				 * will be never called.
				 * The compiler should complain whenever this 
				 * undefined constructor is called, implicitly or not.
				 *
				 */			 
				LogChannel( const LogChannel & source ) ;
			
			
			
				/**
				 * Assignment operator made private to ensure that
				 * it will be never called.
				 *
				 * The compiler should complain whenever this 
				 * undefined operator is called, implicitly or not.
				 *
				 */			 
				LogChannel & operator = ( const LogChannel & source ) ;
				
				
				
		} ;
		

	}

}



#endif // CEYLAN_LOG_CHANNEL_H_

