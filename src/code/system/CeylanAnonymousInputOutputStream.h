#ifndef CEYLAN_ANONYMOUS_INPUT_OUTPUT_STREAM_H_
#define CEYLAN_ANONYMOUS_INPUT_OUTPUT_STREAM_H_


#include "CeylanInputOutputStream.h"     // for inheritance



namespace Ceylan 
{


	namespace System 
	{
	
	
	
		/**
 		 * Abstract anonymous input/output stream class used in IPC.
 		 *
		 * This class is mostly used for servers offering multiplexed accesses
		 * to clients : the server is an I/O stream that controls the socket
		 * bound to its published port, and for each connected client a 
		 * dedicated socket is spawned. This socket is an anonymous input/
		 * output stream.
		 *
 		 * @see ServerStreamSocket, Socket, InputStream.
 		 *
 		 */
		class AnonymousInputOutputStream : public InputOutputStream
		{


			public:
	
	
	
				/**
				 * Basic constructor for AnonymousInputOutputStream.
				 *
				 */
				explicit AnonymousInputOutputStream()
					throw( StreamException ) ;
		
		
				/// Basic virtual destructor, closes the file descriptor.
				virtual ~AnonymousInputOutputStream() throw() ;
		
	

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


				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				AnonymousInputOutputStream( 
					const AnonymousInputOutputStream & source )	throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				AnonymousInputOutputStream & operator = ( 
					const AnonymousInputOutputStream & source ) throw() ;
	
	
	
		} ;
					
	}
	
}


	

#endif // CEYLAN_ANONYMOUS_INPUT_OUTPUT_STREAM_H_
