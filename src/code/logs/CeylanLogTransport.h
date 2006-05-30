#ifndef CEYLAN_LOG_TRANSPORT_H_
#define CEYLAN_LOG_TRANSPORT_H_


#include "CeylanLog.h"                // for LevelOfDetail, etc.
#include "CeylanTextDisplayable.h"    // for inheritance


namespace Ceylan
{

	namespace Log
	{
	
	
		// They are transported by LogTransport.
		class LogMessage ;
	
	
		/**
		 * This abstract class has for mission to take a Log message from
		 * a LogSource and to bring it to the relevant LogListener.
		 *
		 * @note the inheritance from TextDisplayable is virtual since, 
		 * for local log messaging, a LogTransport could be a LogListener
		 * too, whereas only one instance of the TextDisplayable mother
		 * class would be required.
		 *
		 * @note the transport takes ownership of log message objects,
		 * it has therefore to make sure that they are eventually deallocated.
		 *
		 * @see LogSource, LogListener
		 *
		 */
		class LogTransport : public virtual TextDisplayable
		{
		
			public:
			
			
				/**
				 * Constructs a blank LogTransport.
				 *
				 * @see setTransport
				 * 
				 */
				LogTransport() throw() ;
			
				
				/// Basic virtual destructor.
				virtual ~LogTransport() throw() ;
						
						
		        /**
		         * Propagates <b>message</b> to the relevant LogListener.
		         *
				 * @param message the message which is to be propagated
				 *
				 * @note Most implementations, more elaborate that local
				 * raw log transfers, should end up deallocating the 
				 * specified message.
		         *
		         */		
				virtual void propagate( LogMessage & message ) 
					throw( LogException ) = 0 ; 
				
										
	            /**
	             * Returns a user-friendly description of the state of
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall settings.
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
				LogTransport( const LogTransport & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogTransport & operator = ( const LogTransport & source )
					throw() ;
	
		} ;
	
	
	}


}


#endif // CEYLAN_LOG_TRANSPORT_H_
