#ifndef CEYLAN_ANONYMOUS_INPUT_STREAM_H_
#define CEYLAN_ANONYMOUS_INPUT_STREAM_H_


#include "CeylanInputStream.h"     // for inheritance
#include "CeylanSystem.h"          // for FileDescriptor



namespace Ceylan 
{


	namespace System 
	{
	
	
		/**
 		 * Abstract anonymous input stream class used in IPC.
 		 *
		 * This class is mostly used for servers offering multiplexed accesses
		 * to clients : the server is an I/O stream that controls the socket
		 * bound to its published port, and for each connected client a 
		 * dedicated socket is spawned. This socket is an anonymous stream.
		 *
 		 * @see ServerStreamSocket, Socket, InputStream.
 		 *
 		 */
		class AnonymousInputStream : public InputStream
		{


			public:
	
	
	
				/**
				 * Basic constructor for AnonymousInputStream, whose instances
				 * are created not selected.
				 *
				 * @throw StreamException if the operation failed, included
				 * if the file descriptor feature is not available.
				 *
				 */
				explicit AnonymousInputStream( FileDescriptor fd )
					 throw( StreamException ) ;
		
		
				/// Basic virtual destructor.
				virtual ~AnonymousInputStream() throw() ;
		
		
				/// Returns the stream's unique ID.
				virtual StreamID getInputStreamID() const ;


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
						const throw() ;



	
			private:


				/// Stores the file descriptor of this stream.
				FileDescriptor _fdes ;


				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				AnonymousInputStream( const AnonymousInputStream & source )
					throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				AnonymousInputStream & operator = ( 
					const AnonymousInputStream & source ) throw() ;
	
	
	
		} ;
					
	}
	
}


	

#endif // CEYLAN_ANONYMOUS_INPUT_STREAM_H_
