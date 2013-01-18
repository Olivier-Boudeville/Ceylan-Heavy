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


#ifndef CEYLAN_LOG_AGGREGATOR_CONSOLE_H_
#define CEYLAN_LOG_AGGREGATOR_CONSOLE_H_


#include "CeylanLogAggregator.h"  // for LogAggregator
#include "CeylanDisplayable.h"    // for VerbosityLevels

#include <iosfwd>                 // for ostream
#include <string>



namespace Ceylan
{


	namespace System
	{


		// LogAggregatorConsole instances aggregate their log into files.
		class File ;

		// Used in some cases (ex: the Nintendo DS) instead of an output stream.
		class Console ;


	}



	namespace Log
	{


		// They are aggregated by aggregators.
		class LogMessage ;


		/**
		 * This class implements the LogAggregator interface thanks to
		 * console-based direct output, hence with no file being written.
		 *
		 * @note This is the simpliest log aggregator, useful whenever the
		 * Ceylan library is built with no file descriptor support.
		 *
		 * Log messages are stored in chronological order (based on the time
		 * they were received, not on their respective timestamp), all in a row,
		 * each prefixed by its channel name.
		 *
		 * @see LogAggregator
		 *
		 */
		class CEYLAN_DLL LogAggregatorConsole : public LogAggregator
		{


			public:


				/**
				 * Describes the three standard streams:
				 *
				 *  - Output is the standard output stream, which is the
				 * default standard stream being used
				 *
				 *  - Error is the standard error stream
				 *
				 *  - Log is the standard log stream
				 *
				 * The difference between Log and Error is that Log is buffered
				 * whereas Error is not.
				 *
				 */
				enum StandardStream { Output, Error, Log } ;


				/**
				 * Constructs a LogAggregatorConsole which will output incoming
				 * log messages in console-based output.
				 *
				 * The logs are stored with a raw encoding, as opposed to HTML
				 * encoding for example.
				 *
				 * @param consoleStream tells to which standard stream logs
				 * should be written. The default is the standard output stream,
				 * Output. On the Nintendo DS, a Ceylan text console will be
				 * used instead of a C++ output stream.
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
				 * @throw LogAggregatorException if the operation fails.
				 *
				 */
				explicit LogAggregatorConsole(
					StandardStream consoleStream
						= LogAggregatorConsole::Output,
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
				virtual ~LogAggregatorConsole() throw() ;


				/**
				 * Aggregates all channel and log messages informations in the
				 * console.
				 *
				 * @throw LogAggregatorException if the operation fails.
				 *
				 * @note This method does nothing if the immediate write flag is
				 * turned on: work should be already done.
				 *
				 */
				virtual void aggregate() ;


				/**
				 * Classify and stores <b>message</b> internally, according to
				 * the corresponding channels they contain.
				 *
				 * @note This implementation, if immediateWrite mode was
				 * selected, will immediatly output this message on the console.
				 *
				 * @param message the log message to be stored, the aggregator
				 * takes ownership of it.
				 *
				 * @throw LogException if the operation fails.
				 *
				 */
				virtual void store( LogMessage & message ) ;


				/**
				 * Returns a user-friendly description of the state of this
				 * object.
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
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;




			protected:


				/**
				 * Internal method used to perform the effective output of log
				 * channels on the console.
				 *
				 * @param channel the log channel to output on the console.
				 *
				 * @throw LogException if the operation fails.
				 *
				 */
				virtual void write( const LogChannel & channel ) const ;


				/**
				 * Internal method used to perform the effective writing of log
				 * messages on the console.
				 *
				 * @param message the log message to write
				 *
				 * @note This method is mainly used when immediate writing mode
				 * is set.
				 *
				 */
				virtual void write( const LogMessage & message ) const ;


				/// The identifier of the standard steam being used.
				StandardStream _streamNumber ;


				/**
				 * The output stream where log messages should be written.
				 *
				 */
				std::ostream * _outputStream ;


				/**
				 * The text console, used on embedded devices (ex: the Nintendo
				 * DS) instead of an output stream.
				 *
				 */
				System::Console * _console ;


				/**
				 * Tells whether log messages should be written to log file as
				 * soon as they are received.
				 *
				 */
				bool _immediateWrite ;


				/**
				 * Defines the aggregator-wide level of detail, used if
				 * _useGlobalLevelOfDetail is set.
				 *
				 */
				LevelOfDetail _globalLevelOfDetail ;



		private:


				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				LogAggregatorConsole( const LogAggregatorConsole & source ) ;


				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				LogAggregatorConsole & operator = (
					const LogAggregatorConsole & source ) ;


		} ;


	}


}


#endif // CEYLAN_LOG_AGGREGATOR_CONSOLE_H_
