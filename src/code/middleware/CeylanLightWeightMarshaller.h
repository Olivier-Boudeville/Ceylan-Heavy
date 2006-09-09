#ifndef CEYLAN_LIGHT_WEIGHT_MARSHALLER_H_
#define CEYLAN_LIGHT_WEIGHT_MARSHALLER_H_


#include "CeylanMarshaller.h"       // for inheritance
#include "CeylanInputStream.h"      // for IOException
#include "CeylanMemoryStream.h"     // for MemoryStream


namespace Ceylan
{


	namespace Middleware
	{



		/**
		 * Encodes et decodes basic data types from a given stream with most
		 * basic encoding routines, which deal only with endianness for 
		 * fixed-size types.
		 *
		 * More precisely, this marshaller allows to encode and decode basic
		 * data types such as Ceylan::Uint32 so that they can for example go
		 * through a network channel while being correctly understood by both
		 * sides of the channel, no matter their respective endiannesses are.
		 *
		 * To do so, a pivot format is chosen, and both sides ensure that it is
		 * respected : the sender converts its own format to the pivot one, the
		 * result goes through the network layer, and the receiver converts it
		 * from the pivot format to its own format.
		 *
		 * Many formats can be chosen for the pivot, we chose here to rely on
		 * the usual sequence of bits transformed into litlle endian format.
		 * We preferred little endian (the native encoding for x86) to the big
		 * one (the one of PowerPC, Sparc, but also the network order), so 
		 * that on most cases no conversion has to be performed (x86 actually
		 * just writes and reads bytes as they are).
		 *
		 * @see CeylanTypes.h
		 *
		 * @note Actually the conversion is directly performed by the 
		 * underlying input/output stream, hence this marshaller is 
		 * light-weight insofar as it is just an empty "letter-box" interface.
		 * 
		 */
		class LightWeightMarshaller: public Marshaller
		{
		
		
			public:
			
			
				/**
				 * Constructs a new marshaller/demarshaller object.
				 *
				 * @param lowerLevelStream the stream that will be used by 
				 * the marshaller to read/write the bitstream to be 
				 * transformed into higher level constructs, here basic Ceylan
				 * datatypes such as Ceylan::Sint16, or strings.
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
				LightWeightMarshaller(
					System::InputOutputStream & lowerLevelStream,
					System::Size bufferedSize = 0 ) throw() ;
				
				
				/// Virtual destructor.
				virtual ~LightWeightMarshaller() throw() ;
				
					
				
				/*
				 * Decoding (read) basic datatypes section.
				 *
				 * @see Ceylan::System::InputStream
				 *
				 */
				
				
				// Decode integer types subsection.
				
				
				/**
				 * Returns a Ceylan::Sint8 decoded from internal stream.
				 *
				 * @throw DecodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured, 
				 * including if there are fewer bytes available than expected.
				 *
				 */
				virtual Ceylan::Sint8 decodeSint8() 
					throw( DecodeException, System::IOException ) ;

		
				/**
				 * Returns a Ceylan::Uint8 decoded from internal stream.
				 *
				 * @throw DecodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured, 
				 * including if there are fewer bytes available than expected.
				 *
				 */
				virtual Ceylan::Uint8 decodeUint8() 
					throw( DecodeException, System::IOException ) ;

		
		
				/**
				 * Returns a Ceylan::Sint16 decoded from internal stream.
				 *
				 * @throw DecodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured, 
				 * including if there are fewer bytes available than expected.
				 *
				 */
				virtual Ceylan::Sint16 decodeSint16() 
					throw( DecodeException, System::IOException ) ;

		
				/**
				 * Returns a Ceylan::Uint16 decoded from internal stream.
				 *
				 * @throw DecodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured, 
				 * including if there are fewer bytes available than expected.
				 *
				 */
				virtual Ceylan::Uint16 decodeUint16() 
					throw( DecodeException, System::IOException ) ;



				/**
				 * Returns a Ceylan::Sint32 decoded from internal stream.
				 *
				 * @throw DecodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured, 
				 * including if there are fewer bytes available than expected.
				 *
				 */
				virtual Ceylan::Sint32 decodeSint32() 
					throw( DecodeException, System::IOException ) ;


				/**
				 * Returns a Ceylan::Uint32 decoded from internal stream.
				 *
				 * @throw DecodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured, 
				 * including if there are fewer bytes available than expected.
				 *
				 */
				virtual Ceylan::Uint32 decodeUint32() 
					throw( DecodeException, System::IOException ) ;




				// Decode floating-point types subsection.
				
				
				/**
				 * Returns a Ceylan::Float32 decoded from internal stream.
				 *
				 * @throw DecodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured, 
				 * including if there are fewer bytes available than expected.
				 *
				 */
				virtual Ceylan::Float32 decodeFloat32() 
					throw( DecodeException, System::IOException ) ;


				/**
				 * Returns a Ceylan::Float64 decoded from internal stream.
				 *
				 * @throw DecodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured, 
				 * including if there are fewer bytes available than expected.
				 *
				 */
				virtual Ceylan::Float64 decodeFloat64() 
					throw( DecodeException, System::IOException ) ;



				// Decode text types subsection.


				/**
				 * Reads a string from this internal stream, and stores it in 
				 * the specified string.
				 *
				 * @note Read strings can have no more than 65535 characters.
				 *
				 * @param result the string to fill from this stream.
				 *
				 * @throw DecodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured, 
				 * including if there are fewer bytes available than expected.
				 *
				 */
				virtual void decodeString( std::string & result ) 
					throw( DecodeException, System::IOException ) ;
				
				
				
				
				
				
				/*
				 * Encoding (write) basic datatypes section.
				 *
				 * @see Ceylan::System::OutputStream
				 *
				 */
				
				

				// Encode integer types subsection.


				/**
				 * Encodes a Ceylan::Sint8 to internal stream.
				 *
				 * @throw EncodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured.
				 *
				 */
				virtual void encodeSint8( Ceylan::Sint8 toEncode ) 
					throw( EncodeException, System::IOException ) ;


				/**
				 * Encodes a Ceylan::Uint8 to internal stream.
				 *
				 * @throw EncodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured.
				 *
				 */
				virtual void encodeUint8( Ceylan::Uint8 toEncode ) 
					throw( EncodeException, System::IOException ) ;



				/**
				 * Encodes a Ceylan::Sint16 to internal stream.
				 *
				 * @throw EncodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured.
				 *
				 */
				virtual void encodeSint16( Ceylan::Sint16 toEncode ) 
					throw( EncodeException, System::IOException ) ;


				/**
				 * Encodes a Ceylan::Uint16 to internal stream.
				 *
				 * @throw EncodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured.
				 *
				 */
				virtual void encodeUint16( Ceylan::Uint16 toEncode ) 
					throw( EncodeException, System::IOException ) ;



				/**
				 * Encodes a Ceylan::Sint32 to internal stream.
				 *
				 * @throw EncodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured.
				 *
				 */
				virtual void encodeSint32( Ceylan::Sint32 toEncode ) 
					throw( EncodeException, System::IOException ) ;


				/**
				 * Encodes a Ceylan::Uint32 to internal stream.
				 *
				 * @throw EncodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured.
				 *
				 */
				virtual void encodeUint32( Ceylan::Uint32 toEncode ) 
					throw( EncodeException, System::IOException ) ;




				// Encode floating-point types subsection.
				
				
				/**
				 * Encodes a Ceylan::Uint32 to internal stream.
				 *
				 * @throw EncodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured.
				 *
				 */
				virtual void encodeFloat32( Ceylan::Float32 toEncode ) 
					throw( EncodeException, System::IOException ) ;


				/**
				 * Encodes a Ceylan::Uint32 to internal stream.
				 *
				 * @throw EncodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured.
				 *
				 */
				virtual void encodeFloat64( Ceylan::Float64 toEncode ) 
					throw( EncodeException, System::IOException ) ;



				/**
				 * Encodes a string to internal stream.
				 *
				 * @note Written strings can have no more than 65535 
				 * characters.
				 *
				 * @param result the string to fill from this stream.
				 *
				 * @throw EncodeException in case a conversion error occured,
				 * or IOException if a transport protocol error occured.
				 *
				 */
				virtual void encodeString( std::string & toEncode ) 
					throw( EncodeException, System::IOException ) ;



				
				
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
				 * Returns the input stream that should be used for direct
				 * encoding/decoding.
				 *
				 * For buffered stream, it is the buffer, for the rest it 
				 * is directly the lower-level stream.
				 *
				 *
				 */
				inline System::InputOutputStream & getEffectiveStream()
					throw()
				{
				
					if ( isBuffered() ) 
						return * _bufferStream ;
					else
						return * _lowerLevelStream ;
							
				}
				
					

			private:
	
			
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				LightWeightMarshaller( const LightWeightMarshaller & source )
					throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				LightWeightMarshaller & operator = ( 
					const LightWeightMarshaller & source ) throw() ;

			
		
		
		} ;
		
	}
	
}		


#endif // CEYLAN_LIGHT_WEIGHT_MARSHALLER_H_
