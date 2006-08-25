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
		 *
		 */
		class ProtocolServer: public ProtocolEndpoint
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
				ProtocolServer( Marshaller & marshaller ) throw() ;
				
				
				/// Virtual destructor.
				virtual ~ProtocolServer() throw() ;
				
				
				
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
					Ceylan::VerbosityLevels level = Ceylan::high ) 
						const throw() ;
	
	
	
			protected:




			private:
	
	
			
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				ProtocolServer( const ProtocolServer & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				ProtocolServer & operator = ( const ProtocolServer & source )
					throw() ;

			
		
		
		} ;
		
	}
	
}		



#endif // CEYLAN_PROTOCOL_SERVER_H_
