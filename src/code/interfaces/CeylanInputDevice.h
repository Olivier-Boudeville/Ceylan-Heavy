#ifndef CEYLAN_INPUT_DEVICE_H_
#define CEYLAN_INPUT_DEVICE_H_


#include "CeylanTextDisplayable.h"     // for inheritance
#include "CeylanEvent.h"               // for EventExceptions

#include <string>




namespace Ceylan
{

	
	// Forward declaration.	
	class Controller ;


    /**
     * Input devices are sending informations to controllers so that they can animate the MVC
	 * framework.
	 *
	 * Basically, all input devices should be linked at a controller, and a controller may be
	 * linked to one or more input devices.
	 *
	 * Based on the notifications from input devices, the controller can generate higher level
	 * events, abstracted from the actual way of interacting. These events are the MVC events.
	 *
	 * Input device encapsulates all ways of driving a controller, the main ones deals with
	 * effective input devices such as keyboard, mouse or joystick.
	 *
	 * Input device and controller have separate life cycle, no one is owned by the other.
	 *
	 * @see Controller
	 *
     */
    class InputDevice : public TextDisplayable
    {


        public:
			
					
			/**
			 * Constructs an abstract input device not linked to any controller.
			 *
			 */
			InputDevice() throw() ;
						
													
			/**
			 * Constructs an abstract input device that will send events to the specified
			 * controller.
			 *
			 */
			explicit InputDevice( Controller & controller ) throw() ;
						
									
			/// Basic virtual destructor.
			virtual ~InputDevice() throw() ;
			
			
			/// Tells whether this input device is linked to a controller.
			virtual bool isLinkedToController() const throw() ;
			
			
			/**
			 * Returns the controller this input device should be linked to.
			 *
			 * @throw EventException if this input device is not linked to any controller.
			 * 
			 * @see isLinkedToController
			 *
			 */
			virtual Controller & getController() const throw( EventException ) ;
			
			 
			/**
			 * Set the controller this input device should send notifications to.
			 * 
			 * @param controller the controller to be linked with.
			 *
			 * @throw EventException if a controller was already linked to this input device.
			 *
			 * @see dropController
			 *
			 */
			virtual void setController( Controller & controller ) throw( EventException ) ;
			 
			 
			/**
			 * Drops the controller this input device might be linked with.
			 *
			 * @return true iff a controller was already linked to this input device.
			 *
			 */
			virtual bool dropController() throw() ;			 
			 
						
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
			virtual const std::string toString( Ceylan::VerbosityLevels level = Ceylan::high ) 
				const throw() ;
					
		
		
		protected:
		
			
			/// The controller this input device can be sending notifications to.
			Controller * _controller ;			
			
		
			
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will be never called.
			 * The compiler should complain whenever this undefined constructor is called, 
			 * implicitly or not.
			 * 
			 *
			 */			 
			InputDevice( const InputDevice & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be never called.
			 * The compiler should complain whenever this undefined operator is called, 
			 * implicitly or not.
			 * 
			 *
			 */			 
			InputDevice & operator = ( const InputDevice & source ) throw() ;
		
			
					 			
    } ;	

}



#endif // CEYLAN_INPUT_DEVICE_H_
