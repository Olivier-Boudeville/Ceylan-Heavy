#ifndef CEYLAN_SERIALIZABLE_H_
#define CEYLAN_SERIALIZABLE_H_


#include "CeylanException.h" 

#include <string>


namespace Ceylan
{


	namespace System
	{
	
		
		/// A Serializable instance can be loaded from an InputStream.
		class InputStream ;
		
		/// A Serializable instance can be saved to an OutputStream.
		class OutputStream ;
		
	
	}
	


	/// This exception is raised when a serialization-related operation failed.
	class CEYLAN_DLL SerializationException : public Ceylan::Exception
	{
			
		public :
				
			explicit SerializationException( 
				const std::string & reason ) throw() ;
			virtual ~SerializationException() throw() ;
				
	} ;
	


    /**
     * Interface that every object which can be serialized should implement. 
     *
	 * Serializing objects is converting them to a stream of bytes. 
	 * It is useful to send them accross the network, or to store them in files
	 * for example.
	 *
     */
    class CEYLAN_DLL Serializable
    {

        public:


			/// Do-nothing constructor.
			Serializable() throw()
			{
			
			}
			
			
			/// Do-nothing virtual destructor.
			virtual ~Serializable() throw()
			{
			
			}
			
			
			
			/**
			 * Saves the instance state to specified stream.
			 *
			 * @param output the output stream to which the state will be
			 * written.
			 *
			 * @throw SerializationException if the operation failed.
			 *
			 */
			virtual void saveTo( System::OutputStream & output ) 
				const throw( SerializationException ) = 0 ;
				
			
			/**
			 * Loads a new instance state from specified stream.
			 *
			 * @param input the input stream from which the state will be read.
			 *
			 * @throw SerializationException if the operation failed.
			 *
			 */
			virtual void loadFrom( System::InputStream & input ) 
				throw( SerializationException ) = 0 ;
				
			
			/*
			 * A recommended factory :
			 *
			 
			static Serializable & CreateFrom( System::InputStream & input ) 
				throw( SerializationException ) ;
			 *
			 */
			 
			  

		private:
		
		
		
			/**
			 * Copy constructor made private to ensure that it will be 
			 * never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 *
			 */			 
			Serializable( const Serializable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be
			 * never called.
			 *
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 *
			 */			 
			Serializable & operator = ( const Serializable & source ) throw() ;
		
			 
    } ;

}


#endif // CEYLAN_SERIALIZABLE_H_
