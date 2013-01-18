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


#ifndef CEYLAN_ANONYMOUS_INPUT_OUTPUT_STREAM_H_
#define CEYLAN_ANONYMOUS_INPUT_OUTPUT_STREAM_H_


#include "CeylanInputOutputStream.h"     // for inheritance
#include "CeylanStream.h"                // for StreamException




namespace Ceylan
{



	namespace System
	{



		/**
		 * Abstract anonymous input/output stream class used in IPC.
		 *
		 * This class is mostly used for servers offering multiplexed accesses
		 * to clients : the server is an I/O stream that controls the socket
		 * bound to its published port, and for each connected client a
		 * dedicated socket is spawned. This socket is an anonymous input/
		 * output stream.
		 *
		 * @see ServerStreamSocket, Socket, InputStream.
		 *
		 */
		class CEYLAN_DLL AnonymousInputOutputStream : public InputOutputStream
		{


			public:



				/**
				 * Basic constructor for AnonymousInputOutputStream.
				 *
				 * @throw Stream::StreamException if the construction failed.
				 *
				 */
				AnonymousInputOutputStream() ;



				/// Basic virtual destructor, closes the file descriptor.
				virtual ~AnonymousInputOutputStream() throw() ;



				/**
				 * Returns an user-friendly description of the state of this
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
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				AnonymousInputOutputStream(
					const AnonymousInputOutputStream & source ) ;


				/**
				 * Assignment operator made private to ensure that it will be
				 * never called.
				 *
				 * The compiler should complain whenever this undefined operator
				 * is called, implicitly or not.
				 *
				 */
				AnonymousInputOutputStream & operator = (
					const AnonymousInputOutputStream & source ) ;



		} ;


	}


}



#endif // CEYLAN_ANONYMOUS_INPUT_OUTPUT_STREAM_H_
