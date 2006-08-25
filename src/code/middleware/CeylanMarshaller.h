#ifndef CEYLAN_MARSHALLER_H_
#define CEYLAN_MARSHALLER_H_


#include "CeylanMiddleware.h"       // for MiddlewareException


namespace Ceylan
{


	namespace Middleware
	{



		/**
		 * Exception to be raised whenever a protocol-related issue arises.
		 *
		 */
		class MarshallException : public MiddlewareException 
		{
		
			public:
			
			
				MarshallException( const std::string & message ) throw() ;
				virtual ~MarshallException() throw() ;
				
		
		} ;



		/**
		 * Encodes and decodes basic data types, for example so that they
		 * can be directly written and read from and to a given stream.
		 *
		 * It is notably the building block of a protocol endpoint, as for
		 * example a protocol server must demarshall what the client sent,
		 * perform any corresponding action, and in turn it may marshall back
		 * an answer to the client.
		 *
		 * Hence it is the place where the actual marshalling/demarshalling 
		 * is provided.
		 *
		 * Various encodings can be used, from basic home-made ones,
		 * which just take care of endianness, to more powerful ones, 
		 * including PER ASN.1 encodings.
		 *
		 * @note The Marshaller abstract class does not enforce a specific
		 * interface, for example to ensure that its child classes all 
		 * implement readUint32, since depending an the actual marshalling 
		 * this may or may not be make sense. 
		 * For example, ASN marshalling is PDU-based (it handles only full
		 * structures), hence fine-grain (ex : Uint32) encoding cannot be used
		 * with it.
		 *
		 */
		class Marshaller: public TextDisplayable
		{
		
		
			public:
			
			
				/**
				 * Constructs a new marshaller/demarshaller object.
				 *
				 *
				 */
				Marshaller() throw() ;
				
				
				/// Virtual destructor.
				virtual ~Marshaller() throw() ;
				
				
				
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
				Marshaller( const Marshaller & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				Marshaller & operator = ( const Marshaller & source )
					throw() ;

			
		
		
		} ;
		
	}
	
}		


#endif // CEYLAN_MARSHALLER_H_
