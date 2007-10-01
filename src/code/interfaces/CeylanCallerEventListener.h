#ifndef CEYLAN_CALLER_EVENT_LISTENER_H_
#define CEYLAN_CALLER_EVENT_LISTENER_H_


#include "CeylanEventListener.h"   // for inheritance


#include <string>


namespace Ceylan
{


	// Events are propagated from sources to listeners.
	class Event ;


	// Sources emit events.	
	class EventSource ;


    /**
     * This specialized event listener does not provide any more method than
	 * basic event listeners do. 
	 * Instead, it allows to define a specialized type which can be the only
	 * one used in calls to the CallableEventSource <code>getEventFor</code>
	 * method. This ensures stronger typing.
	 *
	 * @note Sources will check that the caller is an already registered 
	 * caller event listener indeed when asked for events.
	 *
     */
    class CEYLAN_DLL CallerEventListener : public EventListener
    {


        public:
					
					
			/// Basic constructor, not registered to any event source.
			CallerEventListener() throw() ;
			
			
			/**
			 * Listener constructor automatically performing subscription 
			 * to the event source.
			 *
			 */
			explicit CallerEventListener( EventSource & source ) throw() ;
			
						
			/**
			 * Virtual destructor, performs automatic unsubscribing from all
			 * event sources this listener is subscribed to.
			 *
			 */
			virtual ~CallerEventListener() throw() ;
			
					 	
            /**
             * Returns a user-friendly description of the state of this object.
             *
			 * @param level the requested verbosity level.
			 *
			 * @note Text output format is determined from overall settings.
			 *
			 * @see TextDisplayable
             *
             */
 			virtual const std::string toString( 
				Ceylan::VerbosityLevels level = Ceylan::high ) const throw() ;
					
				

		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			CallerEventListener( const CallerEventListener & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be 
			 * never called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 *
			 */			 
			CallerEventListener & operator = ( 
				const CallerEventListener & source ) throw() ;
				
				 			
    } ;	

}



#endif // CEYLAN_CALLER_EVENT_LISTENER_H_
