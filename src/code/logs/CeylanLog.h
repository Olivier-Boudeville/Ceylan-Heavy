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


#ifndef CEYLAN_LOG_H_
#define CEYLAN_LOG_H_


#include "CeylanException.h"    // for Exception
#include "CeylanTypes.h"        // for Ceylan::Uint8

#include <string>



namespace Ceylan
{


	namespace Log
	{



		/**
		 * This part of the Log system is meant to centralize common features
		 * needed by the actors of the Log framework.
		 *
		 * @see LogPlug
		 *
		 */


		/**
		 * Logging system.
		 *
		 * Ceylan's Log system uses interfaces defined in order to abstract,
		 * through an uniform paradigm, various log implementations: as being
		 * able to swap log implementations seamlessly (at runtime) is useful
		 * for programs and objects, they all make use of this Log interface to
		 * send their log messages.
		 *
		 * The interface in turn delegates log messages according to the
		 * registered log implementations, thanks to the LogPlug facility.
		 *
		 *
		 * To store thematically log messages, log channels are available. They
		 * are designed to be subclassed by various implementations.
		 *
		 * Each message has also its own level of detail.
		 *
		 * The level of detail of a log message defines how important it is.
		 *
		 * Levels start from 1 (the most important level), and the higher a
		 * level is, the less important the corresponding message is.
		 *
		 * Example: level 2 messages are considered as more important than level
		 * 3 ones.
		 *
		 * Log sources have also a level of detail, and they filter out their
		 * outgoing log messages if their own level of detail is smaller than
		 * the message's one, which would mean that this message is not
		 * important enough for that source.
		 *
		 * Each Log listener has its own level of detail too (default: level
		 * 10).
		 *
		 * A Log listener will output any message whose level is equal or
		 * smaller than its current level of detail.
		 *
		 * Example: a listener with a registered level of detail of 4 will
		 * output a message with a level of 3 or 4, but will not propagate a
		 * message whose level is 5 or higher.
		 *
		 * Incoming messages can be time-stamped if the corresponding Log
		 * channel option is set.
		 *
		 * The six default log channels are:
		 *
		 *   1. info   : to gather informative messages for the user
		 *   2. trace  : to know where the execution went
		 *   3. debug  : to help troubleshooting
		 *   4. warning: to notify that a non-critical execution property
		 * was not verified
		 *   5. error  : to track down non-fatal abnormal behaviours
		 *   6. fatal  : to trace fatal abnormalities, just before the
		 * immediate failure of the program.
		 *
		 * A shadow log channel exists, logroot, it is used by the log system
		 * itself.
		 *
		 */


		/// Level of detail (LOD) for log messages and channels.
		typedef Ceylan::Uint8 LevelOfDetail ;


		/// The default level of detail for a Log message (5).
		extern CEYLAN_DLL const LevelOfDetail DefaultLevelOfDetailForMessage ;

		// The maximum level of detail for a Log message (0).
		extern CEYLAN_DLL const LevelOfDetail MaximumLevelOfDetailForMessage ;


		/// The default level of detail of a Log listener (10).
		extern CEYLAN_DLL const LevelOfDetail DefaultLevelOfDetailForListener ;

		/**
		 * The maximum level of detail for a Log Listener (0).
		 *
		 * Listeners with this level are interested only in top-priority
		 * messages.
		 *
		 */
		extern CEYLAN_DLL const LevelOfDetail MaximumLevelOfDetailForListener ;



		/// Exception by all actors of the log service.
		class CEYLAN_DLL LogException : public Ceylan::Exception
		{

			public:

				explicit LogException( const std::string & reason ) ;

				virtual ~LogException() throw() ;

		} ;

	}

}



#endif // CEYLAN_LOG_H_
