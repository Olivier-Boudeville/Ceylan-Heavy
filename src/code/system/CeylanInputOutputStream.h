#ifndef CEYLAN_INPUT_OUTPUT_STREAM_H_
#define CEYLAN_INPUT_OUTPUT_STREAM_H_


#include "CeylanInputStream.h"      // for inheritance
#include "CeylanOutputStream.h"     // for inheritance



namespace Ceylan 
{


	namespace System 
	{
	
	
	
		/**
 		 * Abstract input/output stream class used in IPC.
 		 *
 		 * @see Socket, Pipe, File, AnonymousInputOutputStream.
 		 *
		 *
		 * @note The virtual inheritance has been set for classes such as 
		 * System::File which are both input and output streams : had the 
		 * Stream class a data member, it would not be duplicated in a 
		 * System::File instance (it would be allocated only once).
		 *
 		 */
		class InputOutputStream : public InputStream, public OutputStream
		{


			public:
	
		
				/// Basic constructor for InputStream, created not selected.
				explicit InputOutputStream( bool blocking = true ) throw() ;
		
		
				/// Basic virtual destructor.
				virtual ~InputOutputStream() throw() ;
		
			
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
				InputOutputStream( const InputOutputStream & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				InputOutputStream & operator = ( 
					const InputOutputStream & source ) throw() ;
	
	
	
		} ;
					
	}
	
}


	

#endif // CEYLAN_INPUT_OUTPUT_STREAM_H_
