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


#ifndef CEYLAN_OBJECT_IDENTIFIER_H_
#define CEYLAN_OBJECT_IDENTIFIER_H_


#include "CeylanTextIdentifier.h" // for inheritance
#include "CeylanProcess.h"        // for Pid

#include <string>



namespace Ceylan
{



	// Object instances have to rely on identifiers.
	class Object ;



	namespace Log
	{



		/**
		 * This class encapsulates identifiers of Ceylan objects.
		 *
		 * It is convenient to easily retrieve the subparts of this kind of
		 * identifier, and allows smart aggregation, when messages sent from a
		 * object constructor, therefore with a wrong class name, are conveyed
		 * in their right log channel, thanks to the address matching.
		 *
		 * @see Object, Loggable, LogAggregator
		 *
		 */
		class CEYLAN_DLL ObjectIdentifier : public TextIdentifier
		{


			public:



				/**
				 * Generates, from a class name, the corresponding Object
				 * identifier.
				 *
				 * @param object the object that is to be identified.
				 *
				 * @note Use that constructor when object is available.
				 *
				 */
				explicit ObjectIdentifier( const Object & object ) ;



				/**
				 * Constructs an Object identifier by specifying its content.
				 *
				 * @param hostname the hostname embedded in this identifier.
				 *
				 * @param pid the PID embedded in this identifier.
				 *
				 * @param className the class name embedded in this identifier.
				 *
				 * @param address the address in memory of the referenced
				 * object, embedded in this identifier.
				 *
				 * @note Use that constructor when object is not available, such
				 * as when being on log aggregator's side.
				 *
				 */
				ObjectIdentifier( const std::string & hostname,
					Ceylan::System::Pid pid, const std::string & className,
					const void * address ) ;



				/// Basic virtual destructor.
				virtual ~ObjectIdentifier() throw() ;



				/**
				 * Tells whether this Object identifier only differs from the
				 * specified one because of the class name.
				 *
				 * If so, one could assume they refer actually to the same
				 * Loggable instance, since messages sent from their constructor
				 * have a faulty class name (mangled).
				 *
				 * A risk of mistake arises with automatic variables, since, as
				 * they are allocated on the stack, they can easily be at the
				 * same address.
				 *
				 * This method is notably used by smart aggregators so that the
				 * corresponding messages are classified in the right channels.
				 *
				 */
				virtual bool differentButMatches(
					const ObjectIdentifier & otherID ) const ;



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


				/**
				 * Generates the object identifier from specified channel name:
				 * converts a channel name, which can be found for instance
				 * carried by a log message, to an object identifier.
				 *
				 * @throw IdentifierException if unable to demangle the channel
				 * name, which would probably mean that this channel name does
				 * not correspond to the private channel of an object.
				 *
				 * @note Ownership of the identifier is transferred to the
				 * caller.
				 *
				 * If an exception is raised, no prior ObjectIdentifier has been
				 * instanciated, so there will not be memory leaks.
				 *
				 */
				static ObjectIdentifier & generateFromChannelName(
					const std::string & channelName ) ;


				/// Separator inside an Object identifier.
				static const char Separator ;


				/// Prefix marker in Objet identifier for hosting PID.
				static const std::string PIDTag ;


				/// The pattern matching all Object identifiers.
				static const std::string Pattern ;



			protected:


				/// The hostname of this identifier.
				std::string _hostname ;


				/// The PID of the hosting process of the referenced object.
				System::Pid _pid ;


				/// The class name of the referenced object.
				std::string _className ;


				/**
				 * The address of the referenced object in process memory space.
				 *
				 */
				const void * _address ;


		} ;


	}


}



#endif // CEYLAN_OBJECT_IDENTIFIER_H_
