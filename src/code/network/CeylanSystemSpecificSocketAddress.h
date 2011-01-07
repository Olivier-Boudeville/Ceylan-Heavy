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


#ifndef CEYLAN_SYSTEM_SPECIFIC_SOCKET_ADDRESS_H_
#define CEYLAN_SYSTEM_SPECIFIC_SOCKET_ADDRESS_H_



// It is a private header, hence configuration settings can be used here:

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_USES_NETWORK


// Not available in their C++ form:
extern "C"
{

#ifdef CEYLAN_USES_STRING_H
#include <string.h>            // for memset and bzero, for select, on OpenBSD
#endif // CEYLAN_USES_STRING_H


#ifdef CEYLAN_USES_RESOLV_H
#include <resolv.h>            // for sockaddr_in
#endif // CEYLAN_USES_RESOLV_H

#ifdef CEYLAN_USES_ARPA_INET_H
#include <arpa/inet.h>         // for sockaddr_in
#endif // CEYLAN_USES_ARPA_INET_H

#ifdef CEYLAN_USES_NETINET_IN_H
#include <netinet/in.h>        // for sockaddr_in
#endif // CEYLAN_USES_NETINET_IN_H

#ifdef CEYLAN_USES_WINSOCK2_H
#include <WinSock2.h>
#endif // CEYLAN_USES_WINSOCK2_H

}




namespace Ceylan
{



	namespace Network
	{



		/**
		 * Avoid exposing system-dependent sockaddr_in in the headers, hence it
		 * is defined here one for all, in this private system-specific
		 * non-installed header.
		 *
		 * No virtual cost (virtual table) should apply to this object.
		 *
		 */
		class SystemSpecificSocketAddress
		{


			public:


				/// Basic constructor, starts with a blank address.
				SystemSpecificSocketAddress()
				{
					blank() ;
				}


				// Non-virtual destructor.
				~SystemSpecificSocketAddress() throw()
				{

				}


				/// Blanks this address.
				void blank()
				{

					// Clean up this structure:

#ifdef CEYLAN_USES_MEMSET

					// System V-style:
					::memset( & _socketAddress, 0 ,
						sizeof( _socketAddress ) ) ;


#else // CEYLAN_USES_MEMSET

#ifdef CEYLAN_USES_BZERO

					// BSD-style:
					::bzero( & _socketAddress, sizeof( _socketAddress ) ) ;

#else // CEYLAN_USES_BZERO

#error No memset nor bzero function available for your architecture.

#endif // CEYLAN_USES_BZERO

#endif // CEYLAN_USES_MEMSET

				}


				/// The publicly accessible system-specific internal address.
				sockaddr_in _socketAddress ;



			private:



				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				SystemSpecificSocketAddress(
			 		const SystemSpecificSocketAddress & source ) ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 *
				 */
				SystemSpecificSocketAddress & operator = (
					const SystemSpecificSocketAddress & source ) ;


		} ;
		

	}

}


#endif // CEYLAN_USES_NETWORK



#endif // CEYLAN_SYSTEM_SPECIFIC_SOCKET_ADDRESS_H_

