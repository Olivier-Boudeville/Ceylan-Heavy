#include "CeylanMutex.h"

#include <cerrno>              // for EINVAL, EDEADLK, EPERM, etc.


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#ifdef CEYLAN_USES_PTHREAD_H
#include <pthread.h>           // for pthread_mutex_t, etc.
#endif //CEYLAN_USES_PTHREAD_H

}


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::System ;


#ifdef CEYLAN_USES_PTHREAD_H

// Duplicate definition, see : CeylanThread.cc

// Avoid exposing system-dependent pthread_mutex_t in the headers :
struct Mutex::SystemSpecificMutexType
{
	pthread_mutex_t _mutex ;
	
} ;

#endif // CEYLAN_USES_PTHREAD_H



/*
 * If the multithreading feature is available, then Mutex instances will be
 * able to exist, otherwise the construtor will fail and all other methods 
 * will be made empty.
 *
 */


Mutex::Mutex() throw( Features::FeatureNotAvailableException ) :
	Lockable(),
	_internalMutex( 0 )
{

#ifdef CEYLAN_USES_PTHREAD_H

	_internalMutex = new SystemSpecificMutexType() ;
	
	::pthread_mutex_init( & _internalMutex->_mutex, /* attributes */ 0 ) ;
	
#else // CEYLAN_USES_PTHREAD_H

	throw Features::FeatureNotAvailableException( 
		"Mutex constructor : multithreading feature not available" ) ;
		
#endif //CEYLAN_USES_PTHREAD_H
 	
}


Mutex::~Mutex() throw()
{

#ifdef CEYLAN_USES_PTHREAD_H

	if ( _internalMutex != 0 )
		::pthread_mutex_destroy( & _internalMutex->_mutex ) ;
		
	delete _internalMutex ;
	
	// Useless but may ease multithreading debugging :
	_internalMutex = 0 ;
	
#endif // CEYLAN_USES_PTHREAD_H

}


void Mutex::lock() throw( LockException )
{

	// Must be redefined as (trying to) lock an already locked mutex is legal.
	_locked = true ;
	postLock() ;
}


void Mutex::unlock() throw( LockException )
{
	
	// The mutex handles itself the lock logic and error management.
	preUnlock() ;
	_locked = false ;
	
}


void Mutex::postLock() throw( LockException )
{
	
#ifdef CEYLAN_USES_PTHREAD_H

	ErrorCode res ;
	
	if ( ( res = ::pthread_mutex_lock( & _internalMutex->_mutex ) ) != 0 )
	{
	
		string errorMessage ;
		
		switch( res )
		{
		
			case EINVAL:
				errorMessage = 
				 	"the mutex has not been properly initialized" ;
				break ;	
				
			case EDEADLK:
				errorMessage = 
				 	"the mutex is already locked by the calling thread" ;
				break ;	
				
			default:
				errorMessage = "unknown error" ;
				break ;	
		
		}
		
		throw LockException( 
			"Mutex::postLock : effective locking failed : "
			+ errorMessage ) ;
		
	
	}
	
#endif // CEYLAN_USES_PTHREAD_H
		
}


void Mutex::preUnlock() throw( LockException )
{

#ifdef CEYLAN_USES_PTHREAD_H

	ErrorCode res ;
	

	if ( ( res = ::pthread_mutex_unlock( & _internalMutex->_mutex ) ) != 0 )
	{
	
		string errorMessage ;
		
		switch( res )
		{
		
			case EINVAL:
				errorMessage = 
				 	"the mutex has not been properly initialized" ;
				break ;	
				
			case EPERM:
				errorMessage = 
				 	"the calling thread does not own the mutex" ;
				break ;	
				
			default:
				errorMessage = "unknown error" ;
				break ;	
		
		}
		
		throw LockException( 
			"Mutex::preUnlock : effective unlocking failed : "
			+ errorMessage ) ;
		
	
	}

#endif // CEYLAN_USES_PTHREAD_H
	
}


const std::string Mutex::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Mutex, which is " + Lockable::toString( level ) ;
	
}	
					
					
Mutex::SystemSpecificMutexType & Mutex::getMutexReference() throw()
{
	return *_internalMutex ;
}

