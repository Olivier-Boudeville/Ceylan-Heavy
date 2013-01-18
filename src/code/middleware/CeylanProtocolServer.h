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


#ifndef CEYLAN_PROTOCOL_SERVER_H_
#define CEYLAN_PROTOCOL_SERVER_H_


#include "CeylanProtocolEndpoint.h"  // for inheritance

#include <string>



namespace Ceylan
{



	namespace Middleware
	{



		/**
		 * Designates an applicative server which implements the server side
		 * of a protocol specification.
		 *
		 * Such server handles requests sent by clients, and manages them in
		 * a remote-invocation-method maneer. 
		 * 
		 * There marshaller hides to the protocol server the details of the 
		 * underlying stream that will be used to transport protcolol 
		 * informations : the marshaller will take care of the appropriate
		 * encoding/decoding on the behalf of this protocol server. 
		 *
		 * @note Here the "server" word means an object whose role is to answer
		 * to requests, it is not especially linked with a networked server 
		 * for example : a protocol server respects the server-side behaviour
		 * of a protocol specification, the protcol itself is conveyed by any 
		 * technical solution, which may be a network-based one (ex : TCP/IP 
		 * server socket), or a UNIX pipe, or anything else, the protocol server
		 * does not need to know that. 
		 * 
		 * Following methods have to be subclassed so that the protocol server
		 * can perform its specific task :
		 *   - notifyDataAvailability, which may drive the internal marshaller
		 * so that enough data is retrieved to form a self-standing request
		 * (PDU).
		 *
		 * (see Marshaller::retrieveData)
		 *
		 */
		class CEYLAN_DLL ProtocolServer : public ProtocolEndpoint
		{
		
		
		
			public:
			
			
			
				/**
				 * Constructs a new protocol server.
				 *
				 * @param marshaller the marshaller that will encode and/or
				 * decode data to/from the stream for the protocol to be 
				 * serialized. As marshallers are per-connection object, the
				 * protocol endpoint takes ownership of it and will delete the
				 * marshaller when itself deleted.
				 *
				 */
				ProtocolServer( Marshaller & marshaller ) ;
				
				
				
				/// Virtual destructor.
				virtual ~ProtocolServer() throw() ;
				
				
				
				/**
				 * Reacts on data arrival, performs any work needed, and sends
				 * back an answer to the client if needed.
				 *
				 * This pure virtual method must be overriden by the user.
				 *
				 * The marshaller can help writing a protocol exchange by 
				 * taking care of lower-level decoding/encoding, and if the
				 * marshaller is buffered, one may use :
				 * _marshaller->retrieveData( expectedPDUSize) here, where
				 * expectedPDUSize is the size needed by this protocol server
				 * so that it can interpret the request. The retrieveData
				 * method returns the number of bytes ready to be decoded in 
				 * the buffer of the marshaller. Therefore if this returned
				 * size is less than the minimum size requested by the server,
				 * then the protocol server can just return true and wait 
				 * next notifications, until the read bytes accumulate in the
				 * buffer and the size it needs is reached.
				 *
				 * @return whether, from the protocol server point of view,
				 * the connection should be kept at the return of this method
				 * (if true). If false, incoming client data will not be 
				 * waited for anymore, and the connection will be closed.
				 *
				 * @throw ProtocolException on failure.
				 *
				 * @see testCeylanMultiLwProtocolServer.cc
				 *
				 */
				virtual bool notifyDataAvailability() = 0 ;
					
					

				/**
				 * Tells whether the underlying communication system (for 
				 * example a network server) is expected to shutdown, once
				 * this instance of protocol exchange is over.
				 *
				 * @note This request is a special one that must be managed
				 * specifically, as it goes beyond the field of a particular
				 * protocol-based connection : the underlying media can be
				 * stopped only by specific means that violate the separation
				 * between protocol objects and communication objects.
				 * 
				 */
				virtual bool isShutdownRequested() const ;
				
				
					
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
				 * Records the fact that the protocol determined that the
				 * underlying medium should be stopped once the protocol
				 * exchange is terminated.
				 *
				 */
				virtual void askForShutdown() ;



				/**
				 * Tells whether, when the protocol exchange will be
				 * terminated, the underlying medium should be stopped
				 *
				 * @note This is the only means for protocol objects to i
				 * interfere with communication objects : without this
				 * mechanism, they are truly independent. 
				 *
				 * The communication object (ex : network server) may or 
				 * may not take this information into account.
				 *
				 */
				bool _shutdownRequested ;



			private:
	
	
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				ProtocolServer( const ProtocolServer & source ) ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				ProtocolServer & operator = ( const ProtocolServer & source ) ;

		
		
		} ;
		
		
	}
	
}		



#endif // CEYLAN_PROTOCOL_SERVER_H_

