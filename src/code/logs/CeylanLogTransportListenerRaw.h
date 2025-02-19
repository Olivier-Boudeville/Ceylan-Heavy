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
 * Author: Olivier Boudeville (olivier (dot) boudeville (at) esperide (dot) com)
 *
 */


#ifndef CEYLAN_LOG_TRANSPORT_LISTENER_RAW_H_
#define CEYLAN_LOG_TRANSPORT_LISTENER_RAW_H_


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
		 * LogTransport (the beginning) and the LogListener (the end).
		 *
		 * This particular implementation of the log system corresponds to the
		 * special case where log sources and their target (the aggregator) are
		 * both local, i.e. hosted by the same process.
		 *
		 * Obviously, their is no need to do anything for log messages to be
		 * propagated, so the only thing the instances of this class do is
		 * comply with the framework and simply takes a log message reference
		 * from a log source and gives it to the aggregator: this is the most
		 * basic communication bus.
		 *
		 */
		class CEYLAN_DLL LogTransportListenerRaw :
			public LogTransport, public LogListener
		{


			public:


				/**
				 * Links this new LogListener to the specified log aggregator so
				 * that incoming log messages are sent to it.
				 *
				 */
				explicit LogTransportListenerRaw( LogAggregator & aggregator ) ;


				/// Basic virtual destructor.
				virtual ~LogTransportListenerRaw() throw() ;



				/**
				 * Propagates <b>message</b> to the relevant aggregator.
				 *
				 * @param message the message which is to be propagated
				 *
				 * @note Here lies the trivial bridge between the transport side
				 * to the listener's one.
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
				LogTransportListenerRaw(
					const LogTransportListenerRaw & source ) ;


				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				LogTransportListenerRaw & operator = (
					const LogTransportListenerRaw & source ) ;


		} ;

	}

}


#endif // CEYLAN_LOG_TRANSPORT_LISTENER_RAW_H_
