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


#ifndef CEYLAN_PROTOCOL_ENDPOINT_H_
#define CEYLAN_PROTOCOL_ENDPOINT_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanMiddleware.h"       // for MiddlewareException


#include <string>



namespace Ceylan
{


	
	namespace Middleware
	{



		/**
		 * Exception to be raised whenever a protocol-related issue arises.
		 *
		 */
		class CEYLAN_DLL ProtocolException : public MiddlewareException 
		{
		
			public:
			
			
				ProtocolException( const std::string & message ) ;
				
				virtual ~ProtocolException() throw() ;
				
		
		} ;
		
		
		
		
		/**
		 * An endpoint needs a marshaller to interact with the stream on 
		 * its behalf.
		 *
		 */
		class Marshaller ;
		
		 	
			
			
		/**
		 * Designates a protocol endpoint, which is an abstraction for both
		 * the client and the server side.
		 *
		 * A protocol endpoint is built on top of an InputOutputStream, 
		 * this stream being most often a network one, or a pipe one.
		 *
		 * A protocol needs a Marshaller object to read and write informations
		 * from and to the stream.
		 *
		 */
		class CEYLAN_DLL ProtocolEndpoint: public TextDisplayable
		{
		
		
			public:
			
			
			
				/**
				 * Constructs a new protocol endpoint.
				 *
				 * @param marshaller a marshaller that will encode and/or
				 * decode data to/from the stream it encapsulates, on the 
				 * behalf of this protocol endpoint so that the actual 
				 * protocol-driven informations are properly serialized. 
				 * 
				 * As marshallers are per-connection objects, a protocol
				 * endpoint takes ownership of its marshaller and will delete
				 * it when itself deleted.
				 *
				 */
				ProtocolEndpoint( Marshaller & marshaller ) ;
				
				
				/// Virtual destructor.
				virtual ~ProtocolEndpoint() throw() ;
				
				
				
            	/**
            	 * Returns a user-friendly description of the state of 
				 * this object.
            	 *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall 
				 * settings.
				 *
				 * @see TextDisplayable
				 *
				 */
				virtual const std::string toString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) const ;
	
	
	
	
			protected:



				/**
				 * The marshaller that will be used to (de)serialize
				 * informations of this endpoint.
				 *
				 */
				Marshaller * _marshaller ;




			private:
	
	
			
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				ProtocolEndpoint( const ProtocolEndpoint & source ) ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				ProtocolEndpoint & operator = ( 
					const ProtocolEndpoint & source ) ;

			
		} ;
		
	}
	
}		



#endif // CEYLAN_PROTOCOL_ENDPOINT_H_

