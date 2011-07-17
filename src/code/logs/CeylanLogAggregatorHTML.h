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


#ifndef CEYLAN_LOG_AGGREGATOR_HTML_H_
#define CEYLAN_LOG_AGGREGATOR_HTML_H_


#include "CeylanLog.h"           // for LevelOfDetail
#include "CeylanLogAggregator.h" // for LogAggregator


#include <string>


namespace Ceylan
{


	namespace System
	{


		// LogAggregatorHTML aggregate their log under directories.
		class Directory ;


		// LogAggregatorHTML aggregate their log in weg pages, which are files.
		class File ;


	}



	namespace Log
	{


		/**
		 * This class implements thanks to HTML files the LogAggregator
		 * interface.
		 *
		 * The Log messages are stored in the web page corresponding to their
		 * channel.
		 *
		 * @note Log messages used to be filtered by Ceylan::encodeToHTML, so
		 * that '>' and '<' for example did not mess with the log output.  This
		 * functionality has been rolled back, since it prevented us from using
		 * HTML code on purpose, for example in order to send HTML list in the
		 * logs.
		 *
		 * @todo Maybe add the log session name in the HTML page name so that it
		 * remains visible when using multiple browser tabs/windows, instead of
		 * 'Ceylan - Log browser'.
		 *
		 * @see LogAggregator
		 *
		 */
		class CEYLAN_DLL LogAggregatorHTML : public LogAggregator
		{


			public:


				/**
				 * Constructs a LogAggregatorHTML which will output incoming log
				 * messages in directory specified by <b>logDirectoryname</b>.
				 *
				 * @param callerDescription A text describing the initiator of
				 * this log session, typically the executable name.
				 *
				 * @param logDirectoryname The directory where log pages should
				 * be stored.
				 *
				 * @param useGlobalLevelOfDetail Tells whether log channels
				 * levels of detail are to be overridden by the aggregator-wide
				 * one.
				 *
				 * @param beSmart tells whether this aggregator should be smart
				 * and auto-correct messages with faulty classnames due to C++
				 * name mangling.
				 *
				 * @throw LogAggregatorException if the construction failed.
				 *
				 */
				explicit LogAggregatorHTML(
					const std::string & callerDescription,
					const std::string & logDirectoryName,
					bool useGlobalLevelOfDetail = true, bool beSmart = true ) ;



				/// Basic virtual destructor.
				virtual ~LogAggregatorHTML() throw() ;



				/**
				 * Aggregates all channel and log messages informations in the
				 * aggregator web pages.
				 *
				 * @note This method does nothing if the immediate write flag is
				 * turned on: work should be already done in this case.
				 *
				 * @throw LogAggregatorException if the operation failed.
				 *
				 */
				virtual void aggregate() ;



				/**
				 * Classify and stores <b>message</b> internally, according to
				 * the corresponding channels it contains.
				 *
				 * @note This implementation, if immediateWrite mode is
				 * selected, will immediatly write this message to corresponding
				 * channel web file.
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




				// Static section.


				/// Default value for the aggregator-wide level of detail.
				static const LevelOfDetail DefaultGlobalLevelOfDetail ;



				/**
				 * The suffix to add to log output HTML page filenames,
				 * typically ".html".
				 *
				 */
				static const std::string HTMLPageSuffix ;




			protected:



				// Write section.


				/**
				 * Internal method used to perform the effective writing of log
				 * channels into files.
				 *
				 * @param channel the log channel to write.
				 *
				 * @throw LogException if the write operation failed.
				 *
				 */
				virtual void write( const LogChannel & channel ) const ;



				/**
				 * Internal method used to perform the effective writing of log
				 * messages into files.
				 *
				 * @param channel the log message to write
				 *
				 * @note This method is mainly used when immediate writing mode
				 * is set.
				 *
				 * @throw LogException if the write operation failed.
				 *
				 */
				virtual void write( const LogMessage & message,
					System::File & targetFile ) const ;



				/**
				 * Writes a standard HTML header for a channel page.
				 *
				 * @param targetFile the log channel page to write in.
				 *
				 * @see ChannelHeader
				 *
				 * @throw LogException if the write operation failed.
				 *
				 */
				static void WriteChannelHeader( const LogChannel & channel,
					System::File & targetFile ) ;



				/**
				 * Writes a standard HTML footer for a channel page.
				 *
				 * @param channel
				 *
				 * @param targetFile the log channel page to write in.
				 *
				 * @see ChannelFooter
				 *
				 * @throw LogException if the write operation failed.
				 *
				 */
				static void WriteChannelFooter( const LogChannel & channel,
					System::File & targetFile ) ;



				/**
				 * A text describing the initiator of this log session,
				 * typically the executable name.
				 *
				 */
				std::string _callerDescription ;


				/**
				 * The directory where log message aggregation should take
				 * place.
				 *
				 */
				std::string _logDirectoryName ;


				/// The directory where aggregations will take place.
				System::Directory * _outputDirectory ;


				/**
				 * Defines the aggregator-wide level of detail, used if
				 * _useGlobalLevelOfDetail is set.
				 *
				 */
				LevelOfDetail _globalLevelOfDetail ;


				/// The HTML static content for frameset.
				static const std::string FrameSet ;


				/// The HTML header for the default web page.
				static const std::string DefaultPageHeader ;

				/// The HTML footer for the default web page.
				static const std::string DefaultPageFooter ;


				/// The HTML header for browser menu web pages.
				static const std::string MenuHeader ;

				/// The HTML footer for browser menu web pages.
				static const std::string MenuFooter ;


				/// The HTML header for channel web pages.
				static const std::string ChannelHeader ;

				/// The HTML footer for channel web pages.
				static const std::string ChannelFooter ;



			private:


				/**
				 * Copy constructor made private to ensure that it will
				 *  be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				LogAggregatorHTML( const LogAggregatorHTML & source ) ;


				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				LogAggregatorHTML & operator = (
					const LogAggregatorHTML & source ) ;


		} ;


	}


}


#endif // CEYLAN_LOG_AGGREGATOR_HTML_H_
