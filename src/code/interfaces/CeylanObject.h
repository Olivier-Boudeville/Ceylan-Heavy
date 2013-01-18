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


#ifndef CEYLAN_OBJECT_H_
#define CEYLAN_OBJECT_H_


#include "CeylanIdentifierOwner.h" // for inheritance
#include "CeylanLoggable.h"        // for inheritance
#include "CeylanLog.h"             // for LevelOfDetail


#include <string>



namespace Ceylan
{



	/**
	 * This abstract mother class, root of the whole Ceylan object's hierarchy,
	 * plays a very similar role to java.lang.Object, namely describing the
	 * basic services every Ceylan object should be able to provide.
	 *
	 * One of the services provided is that each Ceylan::Object can have its own
	 * private log channel.
	 *
	 * The resource overhead for that behaviour (identifier building, message
	 * propagating, etc.) will be non-null if and only if this service is
	 * actually used, apart some very minor costs.
	 *
	 */
	class CEYLAN_DLL Object : public IdentifierOwner, public Log::Loggable
	{


		public:



			/**
			 * Constructs a brand new Ceylan::Object.
			 *
			 * @param trackInstance tells whether the created instances's
			 * lifecycle is to be watched through the log system
			 * (ex: deallocation notice).
			 *
			 * @param dropIdentifierOnExit if trackInstance is true, then a
			 * mangled message has been emitted (since the instance tracking has
			 * to be made in the constructor), the identifier will therefore
			 * have to be forged again.
			 *
			 * Setting dropIdentifierOnExit to true will cause the identifier to
			 * be rebuilt on the next sending after the constructor call, which
			 * is what should be done if no more messages are to be sent from
			 * Object's child constructors.
			 *
			 * If, on the contrary, the instance inherits from Object and has to
			 * send more messages from child constructors, then
			 * dropIdentifierOnExit should better be false, and after the last
			 * statement of the deepest constructor, a call to
			 * Object::dropIdentifier would have to be done to rely on an
			 * unmangled relevant identifier.
			 *
			 * @throwLog::LogException in case of failure.
			 *
			 * @note This object's identifier will not be computed as long as it
			 * is not needed.
			 *
			 * @note Beware of Ceylan::Objects instanciated as automatic
			 * variables: they share their mother type (Ceylan::Object), they
			 * are on the same host and PID, and they have the same address:
			 * they might be mixed up by the log system.
			 *
			 * One way to spot that is when an object channel as more than one
			 * "Being allocated now." sentence.
			 *
			 */
			explicit Object( bool trackInstance = true,
				bool dropIdentifierOnExit = true ) ;



			/**
			 * Basic do-nothing destructor used to force virtual destructors in
			 * the hierarchy.
			 *
			 */
			virtual ~Object() throw() ;



			/**
			 * Returns the real class name of any class inheriting from Object.
			 *
			 * The name returned is dependant on the ISO C++ compiler's
			 * implementation, but two different classes should always, with the
			 * same compiler, have different names.
			 *
			 * @note the class name given by the compiler depends on the
			 * compiler and its version.
			 *
			 * @example With g++ 3.x, the classname is prefixed with the
			 * character length: Lockable classname would be actually 8Lockable,
			 * Log would be 3Log.
			 *
			 * @see isOfSameType
			 *
			 */
			virtual const std::string getClassName() const ;



			/**
			 * Returns whether <b>other</b> is an instance of the same type as
			 * this object.
			 *
			 * @param other the object whose type is to be compared with this
			 * object's type.
			 *
			 * @see getClassName
			 *
			 */
			virtual bool isOfSameType( const Object & other ) const ;



			/**
			 * Uses its dedicated log channel to display its state.
			 *
			 * @note This is the very convenient combination of a Loggable and a
			 * TextDisplayable: it requests this Object to log its textual
			 * representation in its own channel.
			 *
			 * @param level chooses the level of detail
			 *
			 * @note This method cannot have the const qualifier since the send
			 * method might have to forge a new identifier.
			 *
			 */
			virtual void logState(
				Ceylan::VerbosityLevels level = Ceylan::high ) ;



			/**
			 * Sends <b>message</b> to the internal channel.
			 *
			 * @param message the log message to send. Please avoid characters
			 * '<' and '>' since they have a special meaning for HTML log
			 * output. These characters used to be filtered in HTML aggregators
			 * but it prevented messages to contain HTML tags on purpose, which
			 * proved to be convenient in the case only HTML aggregators are to
			 * be used.
			 *
			 * @param levelOfDetail the level of detail of this message (level 5
			 * by default).
			 *
			 * @throw Log::LogException if the operation failed.
			 *
			 * @note This method had to be overriden because when forging the
			 * identifier from the object constructor, the class name is
			 * mangled. So we delay the construction of the identifier until the
			 * first log message in internal channel is sent. This is an elegant
			 * solution too, since objects which will not send messages on their
			 * private channel will not have to construct their identifier.
			 *
			 * @note There may be a small lag when the first send is called, due
			 * to the identifier construction.
			 *
			 * @note This method cannot have the const qualifier since it might
			 * have to forge a new identifier.
			 *
			 */
			virtual void send( const std::string & message,
				Log::LevelOfDetail levelOfDetail
					= Log::DefaultLevelOfDetailForMessage ) ;



			/**
			 * Forges this object's identifier.
			 *
			 * @see TextIdentifier
			 *
			 * @throw Log::LogException if the identifier could not be forged.
			 *
			 */
			virtual void forgeIdentifier() ;



			/**
			 * Returns a user-friendly description of the state of this object.
			 *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see TextDisplayable
			 *
			 */
			virtual const std::string toString(
				Ceylan::VerbosityLevels level = Ceylan::high ) const  ;



		protected:


			/**
			 * Removes this Object's identifier, in order to avoid class name
			 * mangling.
			 *
			 * @note This method should be used at least after the last message
			 * sent from an Object's deepest constructor (the final child).
			 *
			 */
			void dropIdentifier() ;


			/**
			 * Tells whether this instance's lifecycle should be advertised in
			 * log system.
			 *
			 */
			bool _trackInstance ;



		private:


			/**
			 * Copy constructor made private to ensure that it will be never
			 * called.
			 *
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 *
			 */
			Object( const Object & source ) ;


			/**
			 * Assignment operator made private to ensure that it will be never
			 * called.
			 *
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 *
			 */
			Object & operator = ( const Object & source ) ;


	} ;


}


#endif // CEYLAN_OBJECT_H_
