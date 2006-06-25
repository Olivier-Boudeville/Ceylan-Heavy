#ifndef CEYLAN_LOCKABLE_H_
#define CEYLAN_LOCKABLE_H_


#include "CeylanException.h"          // for inheritance
#include "CeylanTextDisplayable.h"    // for LockException



#include <string>


namespace Ceylan
{


    /**
     * Interface which should be implemented for all objects that 
	 * are able to be locked.
     *
     */
    class Lockable : public Ceylan::TextDisplayable
    {



        public:
		
		
		
			/// This exception is raised when lock gets misused.
			class LockException : public Ceylan::Exception
			{
			
				public :
				
					explicit LockException( 
						const std::string & reason ) throw() ;
					virtual ~LockException() throw() ;
				
			} ;
			
			
					
			/// Basic constructor, Lockable comes unlocked.
			Lockable() throw() ;
			
			
			/// Basic virtual destructor, unlocks Lockable if locked.
			virtual ~Lockable() throw() ;
			
			
			/**
			 * Tells whether locking is required for that Lockable.
			 *
			 * Some Lockable may be locked or not, depending on the
			 * actual context. 
			 *
			 * @example, video surfaces can behave according to two ways :
			 * software ones must be locked, whereas hardware ones do
			 * not need that.
			 *
			 * This default method returns always true, override it 
			 * when locking is conditional.
			 *
			 */
			virtual bool mustBeLocked() const throw() ;
			
			
			/**
			 * Returns the lock state, locked or unlocked.
			 *
			 * @note Only one of the few inlines for performance 
			 * reasons.
			 *
			 */
			inline bool isLocked() const throw() ;
					
			
			/**
			 * Locks the Lockable. 
			 *
			 * Locking an already locked Lockable raises an exception. 
			 * If mustBeLocked returns false, does nothing.
			 *
			 * This method is to be called by the user of the class 
			 * instances. 
			 *
			 * It will handle the locking process and will call the 
			 * 'postLock' method afterwards. 
			 * This 'postLock' method should be overriden so that the 
			 * locking does actually something.
			 *
			 * This method, 'lock', may be overriden for special locking 
			 * processes, for example for reentrant mutex instances.
			 *
			 * @see isLocked, postLock, unlock
			 *
			 */
			virtual void lock() throw( LockException ) ;
							
				
			
			/**
			 * Unlocks the Lockable. 
			 *
			 * Unlocking a non locked Lockable raises an exception. 
			 * If mustBeLocked returns false, does nothing.
			 *
			 * This method is to be called by the user of the class
			 * instances. 
			 *
			 * It will handle the unlocking process and will call the
			 * 'preUnlock' method afterwards. 
			 * This 'preUnlock' method should be overriden so that the 
			 * unlocking does actually something.
			 *
			 * This method, 'unlock', may be overriden for special 
			 * unlocking processes, for example for reentrant mutex 
			 * instances.
			 *
			 * @see isLocked, preUnlock, lock
			 *
			 */
			virtual void unlock() throw( LockException ) ;
	    
		
            /**
             * Returns an user-friendly description of the state
			 * of this object.
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


			/**
			 * Callback method called just after being locked.
			 *
			 * Meant to be overriden if necessary, since this default 
			 * implementation does nothing.
			 *
			 * @see lock, preUnlock
			 *
			 * @throw LockException if the effective locking failed.
			 *
			 */
			 virtual void postLock() throw( LockException ) ;
				
			
			/**
			 * Callback method called just before being unlocked.
			 *
			 * Meant to be overriden if necessary, since this default 
			 * implementation does nothing.
			 *
			 * @see postLock, unlock
			 *
			 * @throw LockException if the effective unlocking failed.
			 *
			 */
			 virtual void preUnlock() throw( LockException ) ;
		



		private:
		


			/**
			 * This is the actual piece of informations which is used 
			 * to know the lock state.
			 *
			 */
			bool _locked ;

		
			/**
			 * Copy constructor made private to ensure that it will
			 * be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Lockable( const Lockable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it 
			 * will be never called.
			 *
			 * The compiler should complain whenever this undefined 
			 * operator is called, implicitly or not.
			 * 
			 */			 
			Lockable & operator = ( const Lockable & source ) throw() ;
			
   		
			
    } ;


	bool Lockable::isLocked() const throw() 
	{
		return  _locked ;
	}
	

}



#endif // CEYLAN_LOCKABLE_H_
