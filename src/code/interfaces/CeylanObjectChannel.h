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


#ifndef CEYLAN_OBJECT_CHANNEL_H_
#define CEYLAN_OBJECT_CHANNEL_H_


#include "CeylanLog.h"        // for LogException
#include "CeylanLogChannel.h" // for inheritance


#include <string>
#include <list>


namespace Ceylan
{


	namespace Log
	{
			
			
		// An object channed has an object identifier.	
		class ObjectIdentifier ;
		
		
		/**
	 	 * ObjectChannels correspond to log channels dedicated to a 
		 * Ceylan object.
		 *
		 * Each ObjectChannel owns an ObjectIdentifier that matches 
		 * the one of its corresponding Ceylan object.
		 *
	 	 * @see Object
	 	 *
	 	 */
		class CEYLAN_DLL ObjectChannel : public LogChannel
		{
	
			public:
			
			
				/**
		 	 	 * Creates a named Log channel.
		 	 	 * 
				 * @param channelName is the name of the channel carried
				 * by the log message from which this ObjectChannel 
				 * is spawned.
				 *
		 		 */
				explicit ObjectChannel( const std::string & channelName ) 
					throw( LogException ) ;
			
	
				/// Basic virtual destructor.
				virtual ~ObjectChannel() throw() ;


				/**
				 * Add a new Log message to this channel.
				 *
				 * @param message the log message to record.
				 *
				 * @param check if true, raises a LogException if this 
				 * added message's channel does not match this channel.
				 *
				 * @note This method takes ownership of the specified 
				 * log message.
				 *
				 * @note this method overrides the LogChannel one, so that 
				 * the protocol prefix is taken into account when 
				 * matching the message's channel name and this channel name.
				 *
				 */
				virtual void addMessage( LogMessage & message, 
					bool check = true ) throw( LogException ) ;


				/// Returns this ObjectChannel's Object identifier.
				virtual ObjectIdentifier & getObjectIdentifier() 
					const throw( LogException ) ;
				
				
	            /**
	             * Returns a user-friendly description of the state 
				 * of this object.
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
				
				
								
			protected:	


				/**
				 * This is the identifier of the Ceylan object this
				 * ObjectChannel corresponds to.
				 *
				 */
				ObjectIdentifier * _linkedObjectID ;
			
			
					
			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				ObjectChannel( const ObjectChannel & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				ObjectChannel & operator = ( const ObjectChannel & source )
					throw() ;

		} ;

	}

}

#endif // CEYLAN_OBJECT_CHANNEL_H_
