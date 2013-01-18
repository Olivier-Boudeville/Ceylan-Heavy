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


#ifndef CEYLAN_LOG_AGGREGATOR_RAW_H_
#define CEYLAN_LOG_AGGREGATOR_RAW_H_


#include "CeylanLogAggregator.h"  // for LogAggregator
#include "CeylanDisplayable.h"    // for VerbosityLevels

#include <string>



namespace Ceylan
{


	namespace System
	{

		// LogAggregatorRaw instances aggregate their log into files.
		class File ;
	}


	namespace Log
	{


		// They are aggregated by aggregators.
		class LogMessage ;


		/**
		 * This class implements the LogAggregator interface thanks to text
		 * files encoded with raw formatting.
		 *
		 * @note This is the simpliest file-based log aggregator, for example
		 * for users that cannot access to an HTML browser, or for
		 * non-terminating programs (example : when being debugged), which
		 * therefore need on-the-fly log writing.
		 *
		 * Log messages are stored in chronological order (based on the time
		 * they were received, not on their respective timestamp), all in a row,
		 * each prefixed by its channel name.
		 *
		 * @see LogAggregator
		 *
		 */
		class CEYLAN_DLL LogAggregatorRaw : public LogAggregator
		{


			public:


				/**
				 * Constructs a LogAggregatorRaw which will output incoming log
				 * messages in file specified by <b>logFilename</b>.
				 *
				 * The logs are stored with a raw encoding, as opposed to HTML
				 * encoding for example.
				 *
				 * @param logFilename the file where logs should be stored.
				 *
				 * @param immediateWrite tells whether the aggregator should
				 * write log messages as soon as they are received (the safe and
				 * default behaviour).
				 *
				 * @param useGlobalLevelOfDetail tells whether log channels
				 * levels of detail are to be overriden by the aggregator-wide
				 * one.
				 *
				 * @param beSmart tells whether this aggregator should be smart
				 * and auto-correct messages with faulty classnames.
				 *
				 * @throw LogAggregatorException if the operation failed.
				 *
				 */
				explicit LogAggregatorRaw(
					const std::string & logFilename,
					bool immediateWrite = true,
					bool useGlobalLevelOfDetail = true,
					bool beSmart = true ) ;



				/**
				 * Virtual destructor.
				 *
				 * @note If this aggregator has been set to be smart, it will
				 * automatically trigger log aggregation upon deletion.
				 *
				 */
				virtual ~LogAggregatorRaw() throw() ;



				/**
				 * Aggregates all channel and log messages informations in the
				 * aggregator file.
				 *
				 * @throw LogAggregatorException if the operation failed.
				 *
				 * @note this method does nothing if the immediate write flag is
				 * turned on : work should be already done.
				 *
				 */
				virtual void aggregate() ;



				/**
				 * Classifies and stores <b>message</b> internally, according to
				 * the corresponding channels they contain.
				 *
				 * @note This implementation, if immediateWrite mode was
				 * selected, will immediatly write this message to log file.
				 *
				 * @param message the log message to be stored, the aggregator
				 * takes ownership of it.
				 *
				 * @throw LogException if the operation failed.
				 *
				 */
				virtual void store( LogMessage & message ) ;



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




			protected:



				/**
				 * Internal method used to perform the effective writing of log
				 * channels into file.
				 *
				 * @param channel the log channel to save into file.
				 *
				 * @throw LogException if the write operation failed.
				 *
				 */
				virtual void write( const LogChannel & channel ) const ;



				/**
				 * Internal method used to perform the effective writing of log
				 * messages into file.
				 *
				 * @param message the log message to write
				 *
				 * @note this method is mainly used when immediate writing mode
				 * is set.
				 *
				 * @throw LogException if the write operation failed.
				 *
				 */
				virtual void write( const LogMessage & message ) const ;



				/**
				 * The file name where log message aggregation should take
				 * place.
				 *
				 */
				std::string _logFilename ;



				/// The file where aggregations will take place.
				System::File * _outputFile ;



				/**
				 * Tells whether log messages should be written to log file as
				 * soon as they are received.
				 *
				 */
				bool _immediateWrite ;




		private:


				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				LogAggregatorRaw( const LogAggregatorRaw & source ) ;



				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				LogAggregatorRaw & operator = (
					const LogAggregatorRaw & source ) ;


		} ;


	}


}


#endif // CEYLAN_LOG_AGGREGATOR_RAW_H_
