/*
 * Copyright (C) 2003-2013 Olivier Boudeville
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


#ifndef CEYLAN_LOG_LISTENER_H_
#define CEYLAN_LOG_LISTENER_H_


#include "CeylanLog.h"              // for LogException
#include "CeylanLogMessage.h"       // for LogMessage
#include "CeylanTextDisplayable.h"  // for inheritance

#include <string>


namespace Ceylan
{


	namespace Log
	{


		// They will finally get the messages of listeners.
		class LogAggregator ;


		/**
		 * LogListeners receive log messages propagated by LogTransports, and
		 * deliver them to an aggregator.
		 *
		 * This class acts an intermediary between a LogTransport, a channel of
		 * communication, and the log aggregator, so that transportation and log
		 * logic are uncoupled.
		 *
		 * That way, the communication channel could be void (direct transfer of
		 * references in process memory for local log implementation) or use
		 * whichever means to send log messages, such as socket I/O in the case
		 * of distributed logs.
		 *
		 * @note the inheritance from TextDisplayable is virtual since, for
		 * local log messaging, a LogListener could be a LogTransport too,
		 * whereas only one instance of the TextDisplayable mother class would
		 * be required.
		 *
		 */
		class CEYLAN_DLL LogListener : public virtual TextDisplayable
		{


			public:


				/**
				 * Links this new LogListener to the specified log aggregator so
				 * that incoming log messages are sent to it.
				 *
				 */
				explicit LogListener( LogAggregator & aggregator ) ;


				/// Basic virtual destructor.
				virtual ~LogListener() throw() ;


				/**
				 * Returns a user-friendly description of the state
				 * of this object.
				 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				virtual const std::string toString(
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;



			protected:


				/**
				 * Sends the received message to the internal aggregator.
				 *
				 * @throw LogException if the operation fails.
				 *
				 */
				void sendToAggregator( LogMessage & message ) const ;


				/**
				 * The aggregator this listener will send the incoming log
				 * messages to.
				 *
				 */
				LogAggregator * _aggregator ;



			private:


				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				LogListener( const LogListener & source ) ;


				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				LogListener & operator = ( const LogListener & source ) ;


		} ;

	}

}


#endif // CEYLAN_LOG_LISTENER_H_
