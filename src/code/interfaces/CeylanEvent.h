#ifndef CEYLAN_EVENT_H_
#define CEYLAN_EVENT_H_


#include "CeylanException.h"        // for Ceylan::Exception
#include "CeylanTextDisplayable.h"  // for inheritance

#include <string>



namespace Ceylan
{


	/**
	 * Exception to be raised when the event system encounters an abnormal
	 * situation.
	 *
	 */
	class CEYLAN_DLL EventException: public Ceylan::Exception
	{

		public:
			explicit EventException( const std::string & reason ) throw() ;
			virtual ~EventException() throw() ;
			
	} ;

	
	
	// They send events.
	class EventSource ;
	
	
    /**
     * Encapsulates the concept of high-level events.
     *
	 * @note In an event-driven system such as a graphical user interface (GUI),
	 * basic (raw) events might be too numerous to model them with this class,
	 * which is dedicated to higher-level, often user-centric, events.
	 * For example, instead of many 'mouse moved' basic events, an Event could
	 * be 'user pressed the quit button' or even 'user wants to quit'.
	 *
     */
    class CEYLAN_DLL Event : public TextDisplayable
    {


        public:
					
								
			/**
			 * All events should know where they come from.
			 *
			 * @note Events have only <b>breferences</b> towards their source,
			 * of course they do not own it.
			 *
			 */
			explicit Event( EventSource & source ) throw() ;
			
						
			/// Basic virtual destructor.
			virtual ~Event() throw() ;

							
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
			
					
		
		protected:
		
		
			/**
			 * The source of this event.
			 *
			 */
			EventSource * _source ;
			
			
			
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined
			 * constructor is called, implicitly or not.
			 * 
			 *
			 */			 
			Event( const Event & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will
			 * never be called.
			 *
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Event & operator = ( const Event & source ) throw() ;
			
			 			
    } ;	

}



#endif // CEYLAN_EVENT_H_
