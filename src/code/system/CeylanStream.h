#ifndef CEYLAN_STREAM_H_
#define CEYLAN_STREAM_H_


#include "CeylanTextDisplayable.h"     // for inheritance


#include <list>
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
			
				
				
				/// Basic constructor.
				Stream() throw() ;
	
	
				/// Basic virtual destructor.
				virtual ~Stream() throw() ;
	
	
				/// Returns the stream's unique ID.
				virtual StreamID getStreamID() const throw() = 0 ;
				
				
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
