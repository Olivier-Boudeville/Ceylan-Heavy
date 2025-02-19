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


#ifndef CEYLAN_LOG_PLUG_H_
#define CEYLAN_LOG_PLUG_H_


#include "CeylanLog.h"

#include <string>



namespace Ceylan
{


	namespace Log
	{



		/// Used as plug endpoints, the Log messages come through them.
		class LogSource ;


		/**
		 * Where messages from a LogSource will be output: the first part of the
		 * LogTransport-LogListener pair.
		 *
		 */
		class LogTransport ;


		/**
		 * Where messages from a LogSource will be received: the second part of
		 * the LogTransport-LogListener pair, before the LogAggregator.
		 *
		 */
		class LogListener ;


		/**
		 * Where messages are finally user-friendly output.
		 *
		 */
		class LogAggregator ;



		/**
		 * This class is in charge of managing the core of the Log system: it
		 * maintains a shared knowledge about all the implementation classes for
		 * LogSource, LogTransport, LogListener and LogAggregator, and links
		 * them together.
		 *
		 * Default standard log channels are registered through the LogPlug too.
		 *
		 * LogPlug is the bridge between Log service users and the Log
		 * implementation that is in use.
		 *
		 * @note This class is mainly a clean container dedicated to the
		 * gathering of static methods.
		 *
		 * @note The log system is not designed specifically to be used in a
		 * multithread (or IRQ-based) context: it was not designed to be
		 * reentrant.
		 *
		 */
		class CEYLAN_DLL LogPlug
		{

			public:


				/// Assigns a LogSource for the default log channel info.
				static void SetInfoLogSource( LogSource & newInfoLogSource ) ;


				/// Returns the LogSource for the default log channel info.
				static LogSource & GetInfoLogSource() ;



				/// Assigns a LogSource for the default log channel trace.
				static void SetTraceLogSource( LogSource & newTraceLogSource ) ;


				/// Returns the LogSource for default log channel trace.
				static LogSource & GetTraceLogSource() ;



				/// Assigns a LogSource for the default log channel debug.
				static void SetDebugLogSource( LogSource & newDebugLogSource ) ;


				/// Returns the LogSource for default log channel debug.
				static LogSource & GetDebugLogSource() ;



				/// Assigns a LogSource for the default log channel warning.
				static void SetWarningLogSource(
					LogSource & newWarningLogSource ) ;


				/// Returns the LogSource for default log channel warning.
				static LogSource & GetWarningLogSource() ;



				/// Assigns a LogSource for the default log channel error.
				static void SetErrorLogSource(
					LogSource & newErrorLogSource ) ;


				/// Returns the LogSource for default log channel error.
				static LogSource & GetErrorLogSource() ;




				/// Assigns a LogSource for the default log channel fatal.
				static void SetFatalLogSource(
					LogSource & newFatalLogSource ) ;


				/// Returns the LogSource for default log channel fatal.
				static LogSource & GetFatalLogSource() ;


				/**
				 * Tells whether the fatal channel of the Log Plug is
				 * initialized.
				 *
				 */
				static bool IsFatalLogSourceAvailable() ;




				/// Assigns a LogSource for the default log channel logroot.
				static void SetLogRootLogSource(
					LogSource & newLogRootLogSource ) ;


				/// Returns the LogSource for default log channel logroot.
				static LogSource & GetLogRootLogSource() ;



				/// Assigns a LogSource for the default log channel logroot.
				static void SetTransport( LogTransport & newTransport ) ;


				/// Returns the log transport currently being used by the plug.
				static LogTransport & GetTransport() ;



				/// Assigns a LogSource for the default log channel logroot.
				static void SetListener( LogListener & newListener ) ;


				/// Returns the log Listener currently being used by the plug.
				static LogListener & GetListener() ;



				/// Assigns a LogSource for the default log channel logroot.
				static void SetAggregator( LogAggregator & newAggregator ) ;


				/// Returns the log Aggregator currently being used by the plug.
				static LogAggregator & GetAggregator() ;


				/**
				 * Verifies that the Log Plug is totally empty, no log source,
				 * transport, listener, aggregator being registered.
				 *
				 */
				static void CheckBlank() ;


				/**
				 * Records for later use the full path of current executable.
				 *
				 * @param plugInitiatorFullName the full path, corresponding to
				 * argv[0] generally.
				 *
				 * @throw LogException on error, including if the path was
				 * already set.
				 *
				 */
				static void SetFullExecutablePath(
						const std::string & plugInitiatorFullName ) ;



				/**
				 * Returns the full path of current executable, as stored by
				 * SetFullExecutablePath.
				 *
				 * @throw LogException on error, including if no path was set
				 * yet.
				 *
				 */
				static std::string GetFullExecutablePath() ;



				/**
				 * Determines a suitable speaker name from specified plug
				 * initiator full name.
				 *
				 * @param plugInitiatorFullName the full name of the plug
				 * initiator (usually argv[0]).
				 *
				 * @throw LogException on error.
				 *
				 */
				static std::string GetSpeakerNameFrom(
						const std::string & plugInitiatorFullName ) ;


				/**
				 * Creates a basic plug, formed of the standard channels,
				 * provided a transport is already available.
				 *
				 */
				static void CreateBasicPlug() ;



				/**
				 * Creates a null plug, formed of muted channels.
				 *
				 */
				static void CreateNullPlug() ;



				/**
				 * Starts the Log service and declares the name of the source of
				 * the Log messages.
				 *
				 * @param plugCreator the name of the plug initiator, for
				 * instance argv[0].
				 *
				 * @note To ensure all implementations can cope with the
				 * supplied name, prefer simple words for source name.  One
				 * constraint would be to take a sequence of characters which
				 * could stand as a filename, so that a file-based
				 * implementation of the log system could be used with no
				 * further encoding.
				 *
				 */
				static void StartService( const std::string & plugCreator ) ;



				/**
				 * Stops the Log service.
				 *
				 * @param warnIfAlreadyStopped if true, if the log system is not
				 * available when this method is called (either not launched or
				 * already stopped), then an error message is output. Otherwise
				 * this is a silent shutdown of the log system.
				 *
				 */
				static void StopService( bool warnIfAlreadyStopped = true ) ;



				/// Returns the source name with initiated the log plug.
				static const std::string GetSourceName() ;



				/// Returns some informations about the LogPlug's state.
				static const std::string ToString(
					Ceylan::VerbosityLevels level = Ceylan::high ) ;


				/**
				 * Error message to be displayed on attempt of using the Log
				 * system, whereas the service has not been started yet (empty
				 * plug).
				 *
				 */
				static const std::string LogSystemNotInitialized ;



				/**
				 * Sends a message to the default log channel Info.
				 *
				 * This channel is to be used to send messages which should be
				 * read by the user, so its level of detail should be high.
				 *
				 * @note Despite the method is static, its name does not start
				 * with an uppercased letter since it is deemed more readable
				 * that way.
				 *
				 */
				static void info( const std::string & message,
					LevelOfDetail levelOfDetail
							= Log::MaximumLevelOfDetailForMessage ) ;


				/**
				 * Sends a message to the default log channel trace.
				 *
				 * This channel is to be used to send messages describing the
				 * function calls. This may, or may not, interest the user.
				 *
				 * @note Despite the method is static, its name does not start
				 * with an uppercased letter since it is deemed more readable
				 * that way.
				 *
				 */
				static void trace( const std::string & message,
					LevelOfDetail levelOfDetail =
						Log::DefaultLevelOfDetailForMessage ) ;



				/**
				 * Sends a message to the default log channel debug.
				 *
				 * This channel is to be used to send messages helping the
				 * developer figuring out what happens in the system.
				 *
				 * @note Despite the method is static, its name does not start
				 * with an uppercased letter since it is deemed more readable
				 * that way.
				 *
				 */
				static void debug( const std::string & message,
					LevelOfDetail levelOfDetail =
						Log::DefaultLevelOfDetailForMessage ) ;



				/**
				 * Sends a message to the default log channel warning.
				 *
				 * This channel is to be used to issue non critical errors on
				 * abnormal events.
				 *
				 * Its level of detail should be high.
				 *
				 * @note Despite the method is static, its name does not start
				 * with an uppercased letter since it is deemed more readable
				 * that way.
				 *
				 */
				static void warning( const std::string & message,
					LevelOfDetail levelOfDetail =
						Log::MaximumLevelOfDetailForMessage ) ;



				/**
				 * Sends a message to the default log channel error.
				 *
				 * This channel is to be used to issue critical errors on
				 * abnormal events.
				 *
				 * Its level of detail should be the highest, so that it will
				 * never be missed.
				 *
				 * @note Despite the method is static, its name does not start
				 * with an uppercased letter since it is deemed more readable
				 * that way.
				 *
				 */
				static void error( const std::string & message,
					LevelOfDetail levelOfDetail =
						Log::MaximumLevelOfDetailForMessage ) ;


				/**
				 * Sends a message to the default log channel fatal.
				 *
				 * This channel is to be used to trace fatal situtations, just
				 * before the immediate failure of the program.
				 *
				 * Its level of detail should be the highest, so that it will
				 * never be missed.
				 *
				 * @note Despite the method is static, its name does not start
				 * with an uppercased letter since it is deemed more readable
				 * that way.
				 *
				 */
				static void fatal( const std::string & message,
					LevelOfDetail levelOfDetail =
						Log::MaximumLevelOfDetailForMessage ) ;


				/**
				 * Sends a message to the special log channel logroot.
				 *
				 * This channel is to be used solely for internal log system
				 * purpose.
				 *
				 * Its level of detail should be the highest, so that it will
				 * never be missed.
				 *
				 * @note Despite the method is static, its name does not start
				 * with an uppercased letter since it is deemed more readable
				 * that way.
				 *
				 */
				static void logroot( const std::string & message,
					LevelOfDetail levelOfDetail =
						Log::MaximumLevelOfDetailForMessage ) ;




			protected:


				/// Plug for the info channel.
				static LogSource * InfoLogSource ;


				/// Plug for the trace channel.
				static LogSource * TraceLogSource ;


				/// Plug for the debug channel.
				static LogSource * DebugLogSource ;


				/// Plug for the warning channel.
				static LogSource * WarningLogSource ;


				/// Plug for the error channel.
				static LogSource * ErrorLogSource ;


				/// Plug for the fatal channel.
				static LogSource * FatalLogSource ;


				/// Plug for the log root channel.
				static LogSource * LogrootLogSource ;


				/// Plug for the Log transport.
				static LogTransport * Transport ;



				/**
				 * Plug for the possible Log Listener, if framework is purely
				 * local.
				 *
				 */
				static LogListener * Listener ;


				/**
				 * Plug for the possible Log Aggregator, if framework is purely
				 * local.
				 *
				 */
				static LogAggregator * Aggregator ;


				/**
				 * Records the full path of current executable, for later use.
				 *
				 */
				static std::string FullExecutablePath ;


				/// Records the source name, which initiated the log session.
				static std::string SourceName ;



				/// LogPlug should not instanciated, even as a mother class.
				LogPlug()

#ifndef CEYLAN_RUNS_ON_WINDOWS
				/*
				 * g++ (gcc) needs this __attribute__ (otherwise a blocking
				 * warning is issued), but Visual C++ does not understand it.
				 *
				 * As we are here in a public header file, only the
				 * CEYLAN_RUNS_ON_WINDOWS configuration-specific preprocessor
				 * symbol is available here.
				 *
				 */
					__attribute__ ((noreturn))
#endif // CEYLAN_RUNS_ON_WINDOWS
				;


				/// Virtual destructor.
				virtual ~LogPlug() throw() ;



			private:


				/**
				 * Copy constructor made private to ensure that it will be never
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				LogPlug( const LogPlug & source ) ;


				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				LogPlug & operator = ( const LogPlug & source ) ;


		} ;

	}

}



#endif // CEYLAN_LOG_PLUG_H_
