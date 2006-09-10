#ifndef CEYLAN_STREAM_H_
#define CEYLAN_STREAM_H_


#include "CeylanTextDisplayable.h"     // for inheritance
#include "CeylanSystem.h"              // for IOException


#include <string>



namespace Ceylan 
{


	namespace System 
	{
	
	
	
		/**
		 * A stream's unique ID.
		 *
		 * @note A value of -1 denotes an invalid stream ID.
		 *
		 */
		typedef int StreamID ;
		
		
		/**
 		 * Abstract stream class used in IPC.
 		 *
 		 * @see InputStream, Stream.
 		 *
 		 */
		class Stream : public Ceylan::TextDisplayable
		{


			public:
			
			
			
				/// Exception thrown when a stream operation failed.
				class StreamException : public Ceylan::System::IOException
				{
					public: 
					
						explicit StreamException( 
								const std::string & reason ) throw() : 
							IOException( reason )
						{
						
						}
						
				} ;



				/// Exception thrown when a stream operation failed.
				class CloseException : public StreamException
				{
					public: 
					
						explicit CloseException( 
								const std::string & reason ) throw() : 
							StreamException( reason )
						{
						
						}
						
				} ;
				
				
				/**
				 * Exception thrown when a stream is set to non-blocking
				 * whereas it failed or is not supported at all.
				 *
				 */
				class NonBlockingNotSupportedException: 
					public StreamException
				{ 
					public: 
					
						explicit NonBlockingNotSupportedException( 
								const std::string & reason ) throw() : 
							StreamException( reason )
						{
						
						}
								
				} ;
				
				
				
				/**
				 * Basic constructor for stream instances.
				 *
				 * @param blocking tells whether this stream should be
				 * created in blocking mode (the default) or in non-blocking
				 * mode (if supported).
				 *
				 */
				explicit Stream( bool blocking = true ) throw() ;
	
	
				/// Basic virtual destructor.
				virtual ~Stream() throw() ;
	
	

				/**
				 * Tells whether this stream is in blocking mode (if true),
				 * or in non-blocking mode (if false).
				 *
				 */
				bool isBlocking() const throw() ;
	
	
				/**
				 * Closes the stream.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 */
				virtual bool close() throw( CloseException ) = 0 ;
				
				
            	/**
            	 * Returns an user-friendly description of the state of
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
						const throw() = 0 ;
			
			
				/**
				 * Closes and zeroes the specified file descriptor.
				 * It is passed by address so that this function can set it
				 * to zero on successful close.
				 *
				 * @return true iff an operation had to be performed.
				 *
				 * @throw CloseException if the close operation failed.
				 *
				 */
				static bool Close( FileDescriptor & fd ) 
					throw( CloseException ) ;



		protected:
		
		
		
				/**
				 * Sets the blocking mode of this stream.
				 *
				 * @param newStatus if true, sets the stream in blocking mode,
				 * if false set to non-blocking mode. If the stream is 
				 * already in the target state, nothing is done.
				 *
				 * @throw NonBlockingNotSupportedException if the operation
				 * failed or is not supported.
				 *
				 * @note This default implementation always raises its 
				 * exception, streams that supports non-blocking access have to
				 * override it.
				 *
				 */
				virtual void setBlocking( bool newStatus )
					throw( NonBlockingNotSupportedException ) ;


				/// Stores whether the stream is in blocking mode.
				bool _isBlocking ;
				
				

			private:
			
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				Stream( const Stream & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				Stream & operator = ( const Stream & source )
					throw() ;
					
					
		} ;	
	
	}
	
}


#endif // CEYLAN_STREAM_H_
