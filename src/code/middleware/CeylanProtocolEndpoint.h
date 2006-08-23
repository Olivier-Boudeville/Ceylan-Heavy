#ifndef CEYLAN_PROTOCOL_ENDPOINT_H_
#define CEYLAN_PROTOCOL_ENDPOINT_H_


#include "CeylanTextDisplayable.h"  // for inheritance
#include "CeylanMiddleware.h"       // for MiddlewareException


#include <string>



namespace Ceylan
{



	namespace System
	{
	
		// Endpoints rely on an InputOutputStream to communicate.
		class InputOutputStream ;
		
	}
	
	
	
	namespace Middleware
	{



		/**
		 * Exception to be raised whenever a protocol-related issue arises.
		 *
		 */
		class ProtocolException : public MiddlewareException 
		{
		
			public:
			
			
				ProtocolException( const std::string & message ) throw() ;
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
		class ProtocolEndpoint: public TextDisplayable
		{
		
		
			public:
			
			
			
				/**
				 * Constructs a new protocol endpoint.
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
				ProtocolEndpoint( System::InputOutputStream & stream,
					Marshaller & marshaller ) throw() ;
				
				
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
					Ceylan::VerbosityLevels level = Ceylan::high ) 
						const throw() ;
	
	
	
			protected:



				/**
				 * The stream that will be used to exchange informations 
				 * with peers.
				 *
				 */
				System::InputOutputStream * _stream ;


				/**
				 * The marshaller that will be used to (de)serialize
				 * informations from/to the stream.
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
				ProtocolEndpoint( const ProtocolEndpoint & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				ProtocolEndpoint & operator = ( 
					const ProtocolEndpoint & source ) throw() ;

			
		
		
		} ;
		
	}
	
}		



#endif // CEYLAN_PROTOCOL_ENDPOINT_H_
