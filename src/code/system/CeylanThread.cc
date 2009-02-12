#include "CeylanThread.h"


#include "CeylanOperators.h"    // for string operators 
#include "CeylanLogPlug.h"      // for logs
#include "CeylanStringUtils.h"  // for formatStringList


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#ifdef CEYLAN_USES_SYS_TYPES_H
//#include <sys/types.h>
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_UNISTD_H
//#include <unistd.h>
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_SYS_TIME_H
#include <sys/time.h>           // for gettimeofday
#endif // CEYLAN_USES_SYS_TIME_H

}

#include <cerrno>
#include <list>


/*
 * CEYLAN_USES_PTHREAD_H can only be defined if the multithreading
 * feature is available, hence if CEYLAN_USES_THREADS is defined.
 *
 * Therefore CEYLAN_USES_PTHREAD_H is a specialized form of 
 * CEYLAN_USES_THREADS.
 *
 */


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::System ;
using namespace Ceylan::Log ;
using namespace Ceylan::Features ;



#ifdef CEYLAN_USES_PTHREAD_H

extern "C"
{

#include "pthread.h"

}


// Avoid exposing system-dependent pthread_t in the headers:
struct Thread::SystemSpecificThreadIdentifier
{
	pthread_t _thread ;
} ;



// Avoid exposing system-dependent pthread_attr_t in the headers:
struct Thread::SystemSpecificThreadAttribute
{
	pthread_attr_t _threadAttribute ;
} ;



// Avoid exposing system-dependent pthread_cond_t in the headers:
struct Thread::SystemSpecificThreadCondition
{
	pthread_cond_t _threadCondition ;
} ;



/*
 * Initially, there is no thread created.
 *
 * @note This must be a pointer to Synchronized<ThreadCount> instead of
 * simply a Synchronized<ThreadCount> instance, since its declaration in
 * Thread class cannot be conditional (no config.h dependency wanted) and
 * in the case the multithreading feature is not enabled, creating this
 * static Synchronized would require having a Mutex constructor available,
 * whereas it would raise a FeatureNotAvailable exception.
 *
 * This is a sustainable memory leak.
 *
 */
Synchronized<ThreadCount> * Thread::_Number 
	= new Synchronized<ThreadCount>( 0 ) ;
	

#else // 	CEYLAN_USES_PTHREAD_H

// Not used, no Mutex available thus avoid creating Synchronized instances:
Synchronized<ThreadCount> * Thread::_Number = 0 ;


#endif // CEYLAN_USES_PTHREAD_H



namespace
{


	/// Helper wrapper function for the Thread::Run static method.
	extern "C" void * Run( void * threadObject )
	{

		/*
		 * A dynamic_cast could not be used, since void * is not a pointer to
		 * class.
		 *
		 */
		#if CEYLAN_DEBUG_THREADS
		LogPlug::debug( "::Run wrapper for Thread::Run called." ) ;
		#endif // CEYLAN_DEBUG_THREADS

		Thread::Run( * reinterpret_cast<Thread * >( threadObject ) ) ;
		return 0 ;
		
	}

}


#ifdef CEYLAN_USES_PTHREAD_H

/*
 * Duplicate definition, see: CeylanMutex.cc
 *
 * Amazingly, it works without a double definition clashing when the library 
 * is created.
 *
 */

// Avoid exposing system-dependent pthread_mutex_t in the headers:
struct Mutex::SystemSpecificMutexType
{
	pthread_mutex_t _mutex ;
} ;

#endif // CEYLAN_USES_PTHREAD_H



Thread::Thread() throw( FeatureNotAvailableException ):
	Runnable( "UnnamedThread" ),
	_id( 0 ),
	_attr( 0 ),
	_terminated( false ),
	_clean     ( false ),
	_running   ( false ),
	_mustStop  ( false )
{

#ifdef CEYLAN_USES_PTHREAD_H

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread anonymous constructor: "
		"creation of an unnamed thread." ) ;
#endif // CEYLAN_DEBUG_THREADS

	_id = new SystemSpecificThreadIdentifier ;
	_attr = new SystemSpecificThreadAttribute ;
	
#else // CEYLAN_USES_PTHREAD_H
	
	throw FeatureNotAvailableException( "Thread anonymous constructor: "
		"multithreading feature is not available" ) ;
		
#endif // CEYLAN_USES_PTHREAD_H

}



Thread::Thread( const string & name ) 
		throw( Features::FeatureNotAvailableException ):
	Runnable( name ),
	_id( 0 ),
	_attr( 0 ),
	_terminated( false ),
	_clean     ( false ),
	_running   ( false ),
	_mustStop  ( false )
{

#ifdef CEYLAN_USES_PTHREAD_H

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread constructor: "
		"creation of a thread named '" + name + "'." ) ;
#endif // CEYLAN_DEBUG_THREADS
	
	_id = new SystemSpecificThreadIdentifier ;
	_attr = new SystemSpecificThreadAttribute ;
	
#else // CEYLAN_USES_PTHREAD_H
	
	throw FeatureNotAvailableException( "Thread constructor for '"
		+ name + "': multithreading feature is not available" ) ;
		
#endif // CEYLAN_USES_PTHREAD_H

}



Thread::~Thread() throw()
{

#if CEYLAN_USES_THREADS

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread destructor: "
		"destruction of a thread named '" + getName() + "'." ) ;
#endif // CEYLAN_DEBUG_THREADS

	if ( isRunning() )
		cancel() ;

	if ( *_Number > 0 )
		(*_Number)-- ;

	/*
	 * Zeroing dynamic members in useless but would ease multithread 
	 * debugging.
	 *
	 */
	
	if ( _id != 0 )
	{
		delete _id ;
		_id = 0 ;
	}

	if ( _attr != 0 )
	{
		delete _attr ;
		_attr = 0 ;
	}
	
		
#endif // CEYLAN_USES_THREADS
	
}



void Thread::run() throw( RunnableException )
{

#ifdef CEYLAN_USES_PTHREAD_H

	Sint32 error ;

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::run: the '" + getName() 
		+ "' thread will run now." ) ;
#endif // CEYLAN_DEBUG_THREADS

	if ( ( error =::pthread_create( 
		& _id->_thread, 
		/* pthread_attr_t */ 0, 
		/* wrapper function for Thread::Run */::Run, 
		dynamic_cast<void*>( this ) ) ) )
	{
		threadCreationFailed( error ) ;
	}
	else
	{
		(*_Number)++ ;
	}

#else // CEYLAN_USES_PTHREAD_H

	throw RunnableException( "Thread::run: "
		"multithreading feature is not available" ) ;
		
#endif // CEYLAN_USES_PTHREAD_H
	
}



// Thread::start is pure virtual.



void Thread::askToStop() throw()
{

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::askToStop: the '" + getName() 
		+ "' thread was asked to stop." ) ;
#endif // CEYLAN_DEBUG_THREADS

	_mustStop = true ;
	
}



Thread::SystemSpecificThreadIdentifier & Thread::id() const throw()
{ 
	return *_id ; 
}



bool Thread::isClean() const
{ 
	return _clean.getValue() ;  
}



bool Thread::isRunning() const throw()
{ 
	return _running.getValue() ; 
}



bool Thread::hasTerminated() const throw()
{ 
	return _terminated ; 
}



bool Thread::stopDemanded() const
{ 
	return _mustStop ; 
}



void Thread::waitUntilOver() throw()
{

#ifdef CEYLAN_USES_PTHREAD_H

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::waitUntilOver: "
		"the current thread is suspended until the '" + getName() 
		+ "' thread is actually stopped." ) ;
#endif // CEYLAN_DEBUG_THREADS

	// Ignored:
	void * threadReturn = 0 ;
	::pthread_join( _id->_thread, & threadReturn ) ;
	
#endif // CEYLAN_USES_PTHREAD_H

}



const string Thread::toString( Ceylan::VerbosityLevels level )
	const throw()
{

	string res = "Thread whose Thread ID is " ; 

#ifdef CEYLAN_USES_PTHREAD_H
	
	res += Ceylan::toString( _id->_thread ) ;
	
#endif // CEYLAN_USES_PTHREAD_H
	
	
	if ( level == Ceylan::low )
		return res ;

		
	std::list<string> stateList ;
	
	if ( _terminated )
		stateList.push_back( "It is terminated" ) ;
	else	
		stateList.push_back( "It is not terminated" ) ;
		
	if ( _clean )
		stateList.push_back( "It has been cleaned up" ) ;
	else	
		stateList.push_back( "It has not been cleaned up" ) ;
		
	if ( _running )
		stateList.push_back( "It is running" ) ;
	else	
		stateList.push_back( "It is not running" ) ;
		
	if ( _mustStop )
		stateList.push_back( "It has been requested to stop" ) ;
	else	
		stateList.push_back( "It has not been requested to stop" ) ;
	
		
	res += Ceylan::formatStringList( stateList ) ;

	if ( level == Ceylan::medium )
		return res ;
	
	if ( _Number != 0 )
		res += "There are currently a total of " 
			+ Ceylan::toString( *_Number ) + " thread objects" ;
	else
		res += "No thread object currently existing" ;
			
	return res ;
	
}



// Static section.


Ceylan::System::ThreadCount Thread::GetNumberOfThreads() throw()
{

	if ( _Number != 0 )
		return *_Number ;
	else
		return 0 ;	
		
}



void Thread::Sleep( System::Second seconds,	System::Microsecond microseconds )
	throw( ThreadException )
{

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::Sleep: current thread will be sleeping for "
		+ Ceylan::toString( seconds  ) + " second(s) and "
		+ Ceylan::toString( microseconds ) + " microsecond(s)." ) ;
#endif // CEYLAN_DEBUG_THREADS

	try
	{
		System::basicSleep( seconds, 
			/* nanoseconds */ 1000 * microseconds ) ;
	}
	catch( const SystemException & e )
	{
		throw ThreadException( "Thread::Sleep failed: "
			+ e.toString() ) ;
	}
	
}



void Thread::Run( Thread & thread ) throw()
{

#ifdef CEYLAN_USES_PTHREAD_H

	thread.setRunning( true ) ;
	::pthread_setcanceltype( PTHREAD_CANCEL_ASYNCHRONOUS, 0 ) ;

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::Run: the '" + thread.getName() 
		+ "' thread is about to start." ) ;
#endif // CEYLAN_DEBUG_THREADS
		
		
	thread.start() ;

	thread.setRunning( false ) ;

	thread.cleanup() ;
	
	
#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::Run: the '" + thread.getName() 
		+ "' thread has finished cleanup." ) ;
#endif // CEYLAN_DEBUG_THREADS


#endif // CEYLAN_USES_PTHREAD_H

}




// Waiter inner class section.


Thread::Waiter::Waiter() throw( FeatureNotAvailableException ):
	Mutex(),
	_condition()
{

#ifdef CEYLAN_USES_PTHREAD_H

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::Waiter constructor: "
		"creation of a Waiter mutex." ) ;
#endif // CEYLAN_DEBUG_THREADS
		
	::pthread_cond_init( & _condition->_threadCondition, 
		/* pthread_condattr_t */ 0 ) ;

#else

	throw FeatureNotAvailableException( "Thread::Waiter constructor: "
		"multithreading feature not available" ) ;
		
#endif // CEYLAN_USES_PTHREAD_H

}



Thread::Waiter::~Waiter() throw()
{

#ifdef CEYLAN_USES_PTHREAD_H


#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::Waiter constructor: "
		"destruction of a Waiter mutex." ) ;
#endif // CEYLAN_DEBUG_THREADS


	if ( ::pthread_cond_destroy( & _condition->_threadCondition ) )
	{
		LogPlug::warning( "Thread::Waiter destructor: "
			"some threads are currently waiting on condition." ) ;
	}
	
#endif // CEYLAN_USES_PTHREAD_H
	
}



bool Thread::Waiter::wait( System::Second seconds ) throw()
{


	bool ret = true ;

#ifdef CEYLAN_USES_PTHREAD_H


#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::Waiter::wait: waiter mutex will wait for " 
		+ Ceylan::toString( seconds ) + " second(s)." ) ;
#endif // CEYLAN_DEBUG_THREADS


	if ( seconds == 0 )
	{
	
		lock() ;
		
		::pthread_cond_wait( & _condition->_threadCondition, 
			& getMutexReference()._mutex ) ;
			
		unlock() ;
		
	}
	else
	{

#if CEYLAN_DEBUG_THREADS
		LogPlug::debug( "Thread::Waiter::wait: locking waiting mutex." ) ;
#endif // CEYLAN_DEBUG_THREADS

		lock() ;
		
		timeval now ;
		timespec timeout ;
		::gettimeofday( & now, 0 ) ;
		timeout.tv_sec  = now.tv_sec + seconds ;
		timeout.tv_nsec = now.tv_usec * 1000 ;


#if CEYLAN_DEBUG_THREADS
		LogPlug::debug( "Thread::Waiter::wait: "
			"just before waiting condition variable..." ) ;
#endif // CEYLAN_DEBUG_THREADS


		ret =::pthread_cond_timedwait(
			& _condition->_threadCondition, 
			& getMutexReference()._mutex, & timeout ) == ETIMEDOUT ;


#if CEYLAN_DEBUG_THREADS
		LogPlug::debug( "Thread::Waiter::wait: "
			"... condition variable signaled !" ) ;
#endif // CEYLAN_DEBUG_THREADS

		unlock() ;


#if CEYLAN_DEBUG_THREADS
		LogPlug::debug( "Thread::Waiter::wait: waiting mutex unlocked." ) ;
#endif // CEYLAN_DEBUG_THREADS


	}
	
#endif // CEYLAN_USES_PTHREAD_H

	return ret ;	

}



bool Thread::Waiter::signal() throw()
{

#ifdef CEYLAN_USES_PTHREAD_H

	lock() ;

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::Waiter::signal: waiter mutex is signaled." ) ;
#endif // CEYLAN_DEBUG_THREADS


	bool ret = ( ::pthread_cond_signal( 
		& _condition->_threadCondition ) == 0 ) ;
		
	unlock() ;
	
	return ret ;

#else // CEYLAN_USES_PTHREAD_H

	return true ;
		
#endif // CEYLAN_USES_PTHREAD_H
	
}



bool Thread::Waiter::broadcast() throw()
{

#ifdef CEYLAN_USES_PTHREAD_H

	lock() ;

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::Waiter::signal: waiter mutex is broadcasted" ) ;
#endif // CEYLAN_DEBUG_THREADS

	bool ret = ( ::pthread_cond_broadcast( 
		& _condition->_threadCondition ) == 0 ) ;
		
	unlock() ;
	
	return ret ;
	
#else // CEYLAN_USES_PTHREAD_H

	return true ;
		
#endif // CEYLAN_USES_PTHREAD_H
	
}




// Protected section.



void Thread::cancel() throw()
{

#ifdef CEYLAN_USES_PTHREAD_H

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::cancel: cancelling thread '" + getName()
		+ "'." ) ;
#endif // CEYLAN_DEBUG_THREADS

	_terminated = true ;
	_running = false ;
	::pthread_cancel( _id->_thread ) ;
	
#endif // CEYLAN_USES_PTHREAD_H
	
}



void Thread::cleanup() throw()
{

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::cleanup called." ) ;
#endif // CEYLAN_DEBUG_THREADS

	_clean = true ;
	
}



void Thread::setRunning( bool newRunningStatus ) throw() 
{ 

#if CEYLAN_DEBUG_THREADS
	LogPlug::debug( "Thread::setRunning: set to " 
		+ Ceylan::toString( newRunningStatus ) + "." ) ;
#endif // CEYLAN_DEBUG_THREADS

	_running = newRunningStatus ; 
	
}



void Thread::threadCreationFailed( ErrorCode error ) throw( ThreadException )
{

	throw ThreadException( "Thread::threadCreationFailed: "
		"Thread::run call failed: " + System::explainError() ) ;
		
}

