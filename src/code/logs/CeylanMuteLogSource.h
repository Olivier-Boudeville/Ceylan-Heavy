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


#ifndef CEYLAN_MUTE_LOG_SOURCE_H_
#define CEYLAN_MUTE_LOG_SOURCE_H_


#include "CeylanLogSource.h"               // for inheritance and al

#include <string>



namespace Ceylan
{


	namespace Log
	{




		/**
		 * This is a specific child class of LogSource, whose role is to ignore
		 * log messages, in the context of the use of the null log plug.
		 *
		 * @see LogPlugNull, LogTransport
		 *
		 */
		class CEYLAN_DLL MuteLogSource : public LogSource
		{

			public:


				/**
				 * Constructs a MuteLogSource.
				 *
				 */
				explicit MuteLogSource() ;



				/// Basic virtual destructor.
				virtual ~MuteLogSource() throw() ;



				/**
				 * Sends <b>message</b> to this MuteLogSource's internal
				 * channel, knowing it will be ignored.
				 *
				 * @param message the log message to send, which will be ignored
				 * here.
				 *
				 * @param levelOfDetail the level of detail of this message,
				 * which will be ignored here.
				 *
				 */
				virtual void send(
					const std::string & message,
					LevelOfDetail levelOfDetail
						= DefaultLevelOfDetailForMessage ) ;


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
				MuteLogSource( const MuteLogSource & source ) ;


				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				MuteLogSource & operator = ( const MuteLogSource & source ) ;


		} ;


	}


}


#endif // CEYLAN_MUTE_LOG_SOURCE_H_
