#ifndef CEYLAN_PROTOCOL_SERVER_H_
#define CEYLAN_PROTOCOL_SERVER_H_


#include "CeylanTextDisplayable.h"  // for inheritance

#include <string>
#include <list>



namespace Ceylan
{


	namespace Middleware
	{



		/**
		 * Designates an applicative server, built on top of an 
		 * InputOutputStream, this stream being most often a network one.
		 *
		 * Such server handles requests sent by clients, and manages them in
		 * a remote-invocation-method maneer. 
		 *
		 */
		class ProtocolServer: public TextDisplayable
		{
		
		
			public:
			
			
				/**
				 * Constructs a new protocol server.
				 *
				 *
				 */
				ProtocolServer() throw() ;
				
				
				/// Virtual destructor.
				virtual ~ProtocolServer() throw() ;
				
				
				
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
				ProtocolServer( const ProtocolServer & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				ProtocolServer & operator = ( const ProtocolServer & source )
					throw() ;

			
		
		
		} ;
		
	}
	
}		



#endif // CEYLAN_PROTOCOL_SERVER_H_
