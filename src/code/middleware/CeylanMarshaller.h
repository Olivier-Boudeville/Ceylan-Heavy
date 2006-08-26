#ifndef CEYLAN_MARSHALLER_H_
#define CEYLAN_MARSHALLER_H_


#include "CeylanMiddleware.h"       // for MiddlewareException
#include "CeylanSystem.h"           // for Size



namespace Ceylan
{


	namespace System
	{
	
	
		// Marshallers reference an InputOutputStream they encapsulate.
		class InputOutputStream ;
		
		
		/*
		 * Marshallers can have a buffered stream in memory to accumulate read
		 * data.
		 *
		 */
		class MemoryStream ;
		
		
	}


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
		 * Exception to be raised whenever a bitstream could not be decoded
		 * into a higher-level construct.
		 *
		 */
		class DecodeException : public MarshallException 
		{
		
			public:
			
			
				DecodeException( const std::string & message ) throw() ;
				virtual ~DecodeException() throw() ;
				
		
		} ;


		/**
		 * Exception to be raised whenever a higher-level construct could not
		 * be encoded into a bitstream.
		 *
		 */
		class EncodeException : public MarshallException 
		{
		
			public:
			
			
				EncodeException( const std::string & message ) throw() ;
				virtual ~EncodeException() throw() ;
				
		
		} ;



		/**
		 * Encodes and decodes informations so that they can be directly 
		 * written and read from and to the stream that the marshaller
		 * encapsulates. 
		 *
		 * These informations may be basic data types, for example a set of
		 * Ceylan::Uint32 numbers, or more complex data structures, such as 
		 * full-fledged applicative PDU. 
		 *
		 * A marshaller is notably a building block of a protocol endpoint, 
		 * as, for example, a protocol server must demarshall what the client
		 * sent, perform any corresponding action, and in turn it may marshall
		 * back an answer to the client. It is the task of the Marshaller to
		 * hide to the protocol endpoint the various operations necessary to
		 * convert higher-level informations, manipulated by the endpoint, 
		 * into a bitstream, as needed by the underlying transport stream, and
		 * the other way round, from transport to endpoint.
		 *
		 * Hence it is the place where the actual marshalling/demarshalling 
		 * is provided.
		 *
		 * Various encodings can be used, from basic home-made ones,
		 * which just take care of endianness, to more powerful ones, 
		 * including PER ASN.1 encodings.
		 *
		 * @note The Marshaller abstract class does not enforce a specific
		 * interface (which for example would ensure that its child classes all 
		 * implement readUint32), since, depending an the actual marshalling, 
		 * this may or may not be make sense.
		 * 
		 * For example, ASN marshalling is PDU-based (it handles only full
		 * structures), hence fine-grain (ex : Uint32) encoding is 
		 * meaningless with it.
		 *
		 */
		class Marshaller: public TextDisplayable
		{
		
		
			public:
			
			
				/**
				 * Constructs a new marshaller/demarshaller object.
				 *
				 * @param lowerLevelStream the stream that will be used by 
				 * the marshaller to read/write the bistream to be 
				 * transformed into higher level constructs.
				 *
				 * @param bufferedSize the size in bytes of an internal
				 * buffered stream used so that only full PDU can be made
				 * available by the marshaller. A null size means no buffer
				 * wanted.
				 *
				 * @note The marshaller does not take ownership of the stream,
				 * hence will not deallocate it.
				 *
				 */
				explicit Marshaller( 
					System::InputOutputStream & lowerLevelStream,
					System::Size bufferedSize = 0 )
					throw() ;
				
				
				/// Virtual destructor.
				virtual ~Marshaller() throw() ;
				
				
				/**
				 * Requests this marshaller to read as much data as possible,
				 * so that there may be enough data to read at least a full PDU.
				 *
				 * In practise, this marshaller will try to read from its
				 * encapsulated lower level stream the full remaining free size
				 * between the current index of its internal buffer and the
				 * end of that buffer.
				 *
				 * @return the number of bytes available in the buffer for
				 * reading. This is not the number of bytes read, insofar as 
				 * there may already be some data before this method was 
				 * called. The caller can use the returned information to 
				 * figure out if it waits for more data or if there is already
				 * enough to be decoded in place.
				 *
				 * @note Using this method only makes sense if the marshaller
				 * has an internal buffer.
				 *
				 * @throw MarshallException if the operation failed, including
				 * if there is no buffer available, or DecodeException if
				 * there is not enough space in the buffer to read more data.
				 *
				 */
				virtual System::Size retrieveData() throw( DecodeException ) ;
				 
				
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
				 * The stream that will be used to exchange
				 * marshall/demarshall informations.
				 *
				 */
				System::InputOutputStream * _lowerLevelStream ;


				/**
				 * An in-memory buffered stream that may be used to cache
				 * read bytes until they form a full structure that has to
				 * be read as a whole (like a PDU) or not at all, for example
				 * by a protocol endpoint.
				 * 
				 */
				System::MemoryStream * _bufferStream ;
				


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
