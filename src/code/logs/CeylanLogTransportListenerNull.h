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


#ifndef CEYLAN_LOG_TRANSPORT_LISTENER_NULL_H_
#define CEYLAN_LOG_TRANSPORT_LISTENER_NULL_H_


#include "CeylanLog.h"             // for LogException
#include "CeylanLogTransport.h"    // for inheritance
#include "CeylanLogListener.h"     // for inheritance

#include <string>



namespace Ceylan
{


	namespace Log
	{


		// Where the transports output their log messages.
		class LogAggregator ;


		/**
		 * This class gathers the two endpoints of Log propagation: the
		 * LogTransport (the beginning) and the LogListener (the end). However,
		 * as it is to be used in the context of the null plug, all logs are
		 * just ignored immediately and not forwarded further in the log chain.
		 *
		 */
		class CEYLAN_DLL LogTransportListenerNull :	public LogTransport
		{


			public:


				/**
				 * Creates a void null transport.
				 *
				 */
				explicit LogTransportListenerNull() ;


				/// Basic virtual destructor.
				virtual ~LogTransportListenerNull() throw() ;



				/**
				 * Propagates <b>message</b> to the relevant aggregator.
				 *
				 * @param message the message which is to be propagated
				 *
				 * @note Just ignores the message.
				 *
				 * @throw LogException if the operation fails.
				 *
				 */
				virtual void propagate( LogMessage & message ) ;



				/**
				 * Returns a user-friendly description of the state of this
				 * object.
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



			private:


				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				LogTransportListenerNull(
					const LogTransportListenerNull & source ) ;


				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				LogTransportListenerNull & operator = (
					const LogTransportListenerNull & source ) ;


		} ;

	}

}


#endif // CEYLAN_LOG_TRANSPORT_LISTENER_NULL_H_
