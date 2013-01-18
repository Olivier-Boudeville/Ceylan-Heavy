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


#ifndef CEYLAN_LOGGABLE_H_
#define CEYLAN_LOGGABLE_H_


#include "CeylanLog.h"        // for LevelOfDetail
#include "CeylanLogSource.h"  // for constructor

#include <string>



namespace Ceylan
{


	namespace Log
	{



		/**
		 * All objects which should be able to log their behaviour should
		 * implement this interface.
		 *
		 * A Loggable objet is a LogSource dedicated to an object.
		 *
		 * @see Log, LogSource
		 *
		 */
		class CEYLAN_DLL Loggable : public LogSource
		{


			public:


				/**
				 * Constructs a Loggable object whose Log identifier is
				 * <b>name</b>.
				 *
				 *
				 * @param name the name of the Loggable which will be used for
				 * its private channel.
				 *
				 * @param trackInstance tells whether the created instances's
				 * lifecycle is to be watched through the log system
				 * (ex: deallocation notice).
				 *
				 */
				explicit Loggable( const std::string & name ) ;



				/// Basic virtual destructor, closes the underlying log channel.
				virtual ~Loggable() throw() ;



				/// Sets this Logable channel name.
				void setChannelName( const std::string & channelName ) ;




				// Static section.


				/**
				 * Returns whether the specified channel name is an object
				 * channel name, based on the possible presence of the protocol
				 * prefix and separators (typically, loggable://).
				 *
				 */
				static bool IsALoggableChannelName(
					const std::string & channelName ) ;



				/**
				 * Returns the real channel name used by a Loggable by removing
				 * the protocol prefix and separators (typically,
				 * 'loggable://').
				 *
				 */
				static const std::string GetEmbeddedChannelName(
					const std::string & fullChannelName ) ;



				/**
				 * The protocol prefix for Loggable, in the way URL are built.
				 *
				 * @example: 'http://', 'ftp://', etc.
				 *
				 */
				static const std::string ProtocolName ;



			private:


				/**
				 * Copy constructor made private to ensure that it will never be
				 * called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				Loggable( const Loggable & source ) ;


				/**
				 * Assignment operator made private to ensure that it will never
				 * be called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				Loggable & operator = ( const Loggable & source ) ;


		} ;


	}


}



#endif // CEYLAN_LOGGABLE_H_
