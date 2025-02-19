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


#ifndef CEYLAN_IP_ADDRESS_V_FOUR_H_
#define CEYLAN_IP_ADDRESS_V_FOUR_H_


#include "CeylanIPAddress.h" // for inheritance, IPAddressException
#include "CeylanTypes.h"     // for Ceylan::Uint8

#include <string>



namespace Ceylan
{
	

	namespace Network
	{



		/// Handles IPv4 addresses.
		class CEYLAN_DLL IPAddressvFour: public IPAddress
		{


			public:
			

			
				// An IPv4 component is a byte.
				typedef Ceylan::Uint8 IPvFourComponent ;
				

				
				/**
				 * Constructs an IPv4 address from its four sub-components.
				 *
				 * Example: 'IPAddressvFour( 120, 200, 65, 3)' constructs
				 * the IPv4 address '120.200.65.3'
				 *
				 * @throw NetworkException is the specified arguments are 
				 * not valid.
				 *
				 */
				IPAddressvFour(
					IPvFourComponent first,
					IPvFourComponent second,
					IPvFourComponent third,
					IPvFourComponent fourth ) ;
				
				
				
				/**
				 * Constructs an IPv4 from specified string, whose form 
				 * must be that of "120.200.65.3". 
				 * 
				 * @throw NetworkException if the string does not contain a
				 * representation of a valid IPv4 address.
				 *
				 */				
				explicit IPAddressvFour( const std::string & IPString ) ;		
												
												
								
				/// Basic virtual destructor.
				virtual ~IPAddressvFour() throw() ;


			
				/**
				 * Returns the actual type of the IP address, IPv4.
				 *
				 */
				virtual NetworkAddressType getType() const ;



	            /**
	             * Returns a user-friendly description of the state of
				 * this object.
	             *
				 * @param level the requested verbosity level. For level 
				 * 'Ceylan::low', the exact usual stringified IP will be
				 * returned (ex: "82.225.152.215").
				 *
				 * @note Text output format is determined from overall 
				 * settings.
				 *
				 * @see TextDisplayable
				 *
	             */			
				virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;


				
				/**
				 * Returns whether <b>IPString</b> is a valid IP.
				 *
				 * @note If the regular expression support is not available,
				 * less checks are performed.
				 *
				 */
				static bool IsValidIPv4( const std::string & ipString ) ;
					
					
			
		
			protected:
			

				 
				/**
				 * Checks that internal IP is valid.
				 *
				 * @throw NetworkException if the address is not valid.
				 *
				 */
				virtual void validate() const ;
				
				
				/// First IPv4 component.
				IPvFourComponent _first ;

				
				/// Second IPv4 component.
				IPvFourComponent _second ;

				
				/// Third IPv4 component.
				IPvFourComponent _third ;

				
				/// Fourth IPv4 component.
				IPvFourComponent _fourth ;
				
				

				
			private:
				

			
				/**
				 * Copy constructor made private to ensure that it will 
				 * never be called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */			 
				IPAddressvFour( const IPAddressvFour & source ) ;
			

			
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				IPAddressvFour & operator = ( const IPAddressvFour & source ) ;
				
				
		} ;
				

	}
	
	
}


#endif // CEYLAN_IP_ADDRESS_V_FOUR_H_

