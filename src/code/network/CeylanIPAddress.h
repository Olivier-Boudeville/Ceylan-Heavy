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


#ifndef CEYLAN_IP_ADDRESS_H_
#define CEYLAN_IP_ADDRESS_H_


#include "CeylanNetwork.h"          // for inheritance, NetworkAddressType
#include "CeylanTextDisplayable.h"  // for inheritance


#include <string>



namespace Ceylan
{
	

	namespace Network
	{



		/// Exception dedicated to the IP addressing.
		class CEYLAN_DLL IPAddressException: public NetworkException
		{
		
			public:
			
			
				explicit IPAddressException( const std::string message ):
					NetworkException( message )
				{
				
				}	
			
		} ;
		



		/**
		 * Handles abstract IP (Internet Protocol) addresses.
		 *
		 * @todo Add informations from netinet/in.h (ex: IN_CLASSA).
		 *
		 */		
		class CEYLAN_DLL IPAddress: public TextDisplayable
		{


			public:
			

		
				/// Basic constructor.
				IPAddress() ;
			
			
				/// Basic virtual destructor.
				virtual ~IPAddress() throw() ;
			

			
				/**
				 * Returns the actual type of the IP address, for example:
				 * IPv4.
				 *
				 */
				virtual NetworkAddressType getType() const = 0 ;
				
				
				
	            /**
	             * Returns a user-friendly description of the state of 
				 * this object.
	             *
				 * @param level the requested verbosity level. For level 
				 * 'Ceylan::low', the exact standard notation for IP address
				 * will be returned, depending on the actual address type.
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
				 * Tells whether the specified string is a valid IP address.	
				 *
				 * @note For abstract IPAddress, always returns false.
				 *
				 */
				static bool IsValid( const std::string IPString ) ;
			
			
			
			
			protected:
			
			
				/**
				 * Checks that internal IP is valid, raises a 
				 * NetworkException otherwise.
				 *
				 */
				virtual void validate() const = 0 ;




			private:
				
			
			
				/**
				 * Copy constructor made private to ensure that it will 
				 * never be called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */			 
				IPAddress( const IPAddress & source ) ;
			
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will never be called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				IPAddress & operator = ( const IPAddress & source ) ;
			
				
		} ;
		

	}
	
	
}



#endif // CEYLAN_IP_ADDRESS_H_

