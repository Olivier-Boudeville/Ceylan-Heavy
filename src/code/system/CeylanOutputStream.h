#ifndef CEYLAN_OUTPUT_STREAM_H_
#define CEYLAN_OUTPUT_STREAM_H_


#include "CeylanStream.h"     // for inheritance


namespace Ceylan
{


	namespace System
	{
	
	
		/**
		 * Abstract output stream class used in IPC.
		 *
		 * @see Socket, Pipe, File, AnonymousInputStream, InputStream
		 *
		 * @note The virtual inheritance has been set for classes such as 
		 * System::File which are both input and output streams : had the 
		 * Stream class a data member, it would not be duplicated in a 
		 * System::File instance (it would be allocated only once).
		 *
		 */
		class OutputStream : public virtual Stream
		{


			public:
						
				/// Basic constructor.
				OutputStream() throw() ;
	
				/// Basic destructor.
				virtual ~OutputStream() throw() ;
	
	
				/// Returns the output stream's unique ID.
				virtual StreamID getOutputStreamID() const throw() = 0 ;


			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				OutputStream( const OutputStream & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				OutputStream & operator = ( const OutputStream & source )
					throw() ;
	
	
		} ;
				
	}	
		
}


#endif // CEYLAN_OUTPUT_STREAM_H_
