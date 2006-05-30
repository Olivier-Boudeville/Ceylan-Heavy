#ifndef CEYLAN_EVENT_SOURCE_H_
#define CEYLAN_EVENT_SOURCE_H_


#include "CeylanEvent.h"              // for EventException, Event
#include "CeylanTextDisplayable.h"    // for inheritance


#include <string>
#include <list>



namespace Ceylan
{


	// Events are sent by sources.
	class Event ;

	// Sources send events to listeners.
	class EventListener ;


    /**
     * Interface which should be implemented for all objects that 
	 * should be able to send events to listeners.
	 *
	 * Event sources are supposed to always be able to answer to any 
	 * listener asking for their latest event. 
	 *
	 * To do so, they may keep their last message, forge a new 
	 * summary event on purpose, etc.
	 *
	 * Any listener can be registered at most one time to an event source :
	 * attempt of multiple registrations result in an exception.
	 *
	 * @note The event source / listener framework is also known as 
	 * the Observer pattern.
	 *
	 * @see http://www.research.ibm.com/designpatterns/example.htm
	 *
     */
    class EventSource : public TextDisplayable
    {


        public:
					
					
			/// Basic constructor with no listener registered.
			EventSource() throw() ;
				
				
			/**
			 * Constructs an event source with already one listener
			 * registered.
			 *
			 */
			explicit EventSource( EventListener & listener ) throw() ;
				
									
			/// Basic virtual destructor.
			virtual ~EventSource() throw() ;
			
			
			/**
			 * Registers the specified listener to this source, so that
			 * forthcoming events will be sent to it as well.
			 *
			 * @throw EventException if listener is already registered 
			 * to the event source.
			 *
			 */
			virtual void add( EventListener & listener ) 
				throw( EventException ) ;
			
			
			/**
			 * Unregisters the specified listener from this source, no
			 * forthcoming event will be sent to the listener.
			 *
			 * @throw EventException if listener was not already 
			 * registered to the event source.
			 *
			 * @note This method does nothing special with regard to the 
			 * listener instance, it acts only on the source. The reason for
			 * that is that the unsubscriptions should be initiated on the
			 * listener side, not on the source one.
			 *
			 * @see Ceylan::EventListener::unsubscribeFrom
			 
			 */
			virtual void remove( const EventListener & listener ) 
				throw( EventException ) ;


			/**
			 * Unsubscribes all registered listeners from this source,
			 * no forthcoming event will be sent. 
			 *
			 * Should not be used under normal circumstances.
			 *
			 * @note This is not how things should be done, since 
			 * registered listeners will not be informed the source unlisted
			 * them. The process should be done the other way round :
			 * each listener should have unsubscribed itself instead from
			 * this source.
			 *
			 * @see Ceylan::EventListener::unsubscribeFromAllSources
			 *
			 */
			virtual void removeAllListeners() throw() ;
							
				
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
				Ceylan::VerbosityLevels level = Ceylan::high ) const throw() ;
			
			
					
		
		protected:


			/**
			 * Notifies all currently registered listeners of a new event.
			 *
			 * @note This source takes ownership of this event.
			 *
			 */
			 virtual void notifyAllListeners( const Event & newEvent ) 
			 	throw() ;


			/**
			 * Tells whether specified listener is registered to this 
			 * source.
			 *
			 */
			virtual bool isRegistered( const EventListener & listener ) 
				throw() ;
			
		
			/**
			 * The registered event listeners.
			 *
			 */
			std::list<EventListener *> _listeners ;
									 	


		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will 
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, 
			 * implicitly or not.
			 * 
			 */			 
			EventSource( const EventSource & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			EventSource & operator = ( const EventSource & source ) throw() ;
					
						
    } ;	

}



#endif // CEYLAN_EVENT_SOURCE_H_
