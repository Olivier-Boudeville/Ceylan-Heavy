#ifndef CEYLAN_PROTOCOL_SERVER_H_
#define CEYLAN_PROTOCOL_SERVER_H_


//#include "CeylanStreamSocket.h"      // for inheritance

#include <string>



namespace Ceylan
{


	namespace Middleware
	{



		/**
		 * 
		 *
		 */
		class ProtocolServer: public 
		{
		
		
			public:
			
			
				/**
				 *
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
