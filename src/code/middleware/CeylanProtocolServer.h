#ifndef CEYLAN_PROTOCOL_SERVER_H_
#define CEYLAN_PROTOCOL_SERVER_H_


#include "CeylanProtocolEndpoint.h"  // for inheritance

#include <string>



namespace Ceylan
{


	namespace Middleware
	{



		/**
		 * Designates an applicative server, built on top of an 
		 * InputOutputStream, this stream being most often a network one.
		 *
		 * Such server handles requests sent by clients, and manages them in
		 * a remote-invocation-method maneer. 
		 * 
		 * Between the protocol server and the stream, a marshaller takes
		 * care of the appropriate encoding/decoding. 
		 *
		 */
		class ProtocolServer: public ProtocolEndpoint
		{
		
		
			public:
			
			
				/**
				 * Constructs a new protocol server.
				 *
				 * @param stream the stream that will be used
				 * to exchange informations with peers according to the
				 * underlying protocol.
				 *
				 * @note The endpoint has a reference on the stream but it
				 * does not own it, hence it will not deallocate it when
				 * the endpoint itself will be deallocated.
				 *
				 * @param marshaller the marshaller that will encode and/or
				 * decode data to/from the stream for the protocol to be 
				 * serialized. As marshallers are per-connection object, the
				 * protocol endpoint takes ownership of it and will delete the
				 * marshaller when itself deleted.
				 *
				 */
				ProtocolServer( System::InputOutputStream & stream,
					Marshaller & marshaller ) throw() ;
				
				
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
