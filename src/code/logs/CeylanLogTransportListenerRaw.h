#ifndef CEYLAN_LOG_TRANSPORT_LISTENER_RAW_H_
#define CEYLAN_LOG_TRANSPORT_LISTENER_RAW_H_


#include "CeylanLog.h"             // for LogException
#include "CeylanLogTransport.h"    // for inheritance
#include "CeylanLogListener.h"     // for inheritance

#include <string>



namespace Ceylan
{


	namespace Log
	{

	
		// Where the transports output their log messages.
		class LogAggregator ;
		
		
		/**
		 * This class gathers the two endpoints of Log propagation : the
		 * LogTransport (the beginning) and the LogListener (the end). 
		 *
		 * This particular implementation of the log system corresponds 
		 * to the special case where log sources and their target (the
		 * aggregator) are both local, i.e. hosted by the same process.
		 * Obviously, their is no need to do anything for log messages 
		 * to be propagated, so the only thing the instances of this 
		 * class do is comply with the framework and simply takes a 
		 * log message reference from a log source and gives it to the
		 * aggregator : this is the most basic communication bus.
		 *
		 */
		class CEYLAN_DLL LogTransportListenerRaw : 
			public LogTransport, public LogListener
		{

			public:
			
			
				/**
				 * Links this new LogListener to the specified log 
				 * aggregator so that incoming log messages are sent to it.
				 *
				 */
				explicit LogTransportListenerRaw( LogAggregator & aggregator )
					throw() ;
				
				
				/// Basic virtual destructor.
				virtual ~LogTransportListenerRaw() throw() ;


		        /**
		         * Propagates <b>message</b> to the relevant aggregator.
		         *
				 * @param message the message which is to be propagated
		         *
				 * @note Here lies the trivial bridge between the 
				 * transport side to the listener's one.
				 *
		         */		
				virtual void propagate( LogMessage & message ) 
					throw( LogException ) ; 
					
				
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
				 *
				 */			 
				LogTransportListenerRaw( 
					const LogTransportListenerRaw & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 *
				 */			 
				LogTransportListenerRaw & operator = ( 
					const LogTransportListenerRaw & source ) throw() ;
				
								
		} ;			
	
	}

} 


#endif // CEYLAN_LOG_TRANSPORT_LISTENER_RAW_H_ 
