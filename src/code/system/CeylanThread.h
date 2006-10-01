#ifndef CEYLAN_THREAD_H_
#define CEYLAN_THREAD_H_


#include "CeylanSynchronized.h"   // for Synchronized template
#include "CeylanRunnable.h"       // for inheritance
#include "CeylanMutex.h"          // for Waiter inheritance
#include "CeylanSystem.h"         // for time units (ex : Second) 
#include "CeylanFeatures.h"       // for FeatureNotAvailableException


#include <string>



namespace Ceylan
{


	namespace System
	{

		
		/// Data type to store numbers of threads.
		typedef Ceylan::Uint32 ThreadCount ;
		
		
		
		/**
		 * Basic abstract class for threaded objects.
		 *
		 * @note No thread object will be created unless the multithreading
		 * feature is supported : Thread constructors will otherwise throw
		 * FeatureNotAvailableException instances.
		 *
		 * @see Feature::isMultithreadingSupported
		 *
		 * @see Runnable, Synchronized, POSIX threads.
		 *
		 */
		class CEYLAN_DLL Thread: public Runnable
		{


			/**
			 * Opaque handle for forward-declared but undefined struct
			 * pointer to system thread identifier, used to avoid including 
			 * system-specific headers such as pthread.
			 *
			 * Otherwise the API exposed by Ceylan would depend on these
			 * headers, then on a config.h that should then be installed 
			 * but may clash with others, and so on.
			 *
			 */
			struct SystemSpecificThreadIdentifier ;
			
			
			/**
			 * Opaque handle for forward-declared but undefined struct
			 * pointer to system thread attribute, used to avoid including 
			 * system-specific headers such as pthread.
			 *
			 * Otherwise the API exposed by Ceylan would depend on these
			 * headers, then on a config.h that should then be installed 
			 * but may clash with others, and so on.
			 *
			 */
			struct SystemSpecificThreadAttribute ;
			
			
			/**
			 * Opaque handle for forward-declared but undefined struct
			 * pointer to system condition variable, used to avoid including 
			 * system-specific headers such as pthread.
			 *
			 * Otherwise the API exposed by Ceylan would depend on these
			 * headers, then on a config.h that should then be installed 
			 * but may clash with others, and so on.
			 *
			 */
			struct SystemSpecificThreadCondition ;
			
			
			
			public:



				/**
				 * Raised when POSIX returns thread errors, mainly in 
				 * creation.
				 *
				 */
				class ThreadException : public SystemException
				{
				
					public:

						explicit ThreadException( 
							const std::string & message )
								throw() :
							SystemException( message )
						{
						
						}


						virtual ~ThreadException() throw()
						{
						
						}

				} ;



				/**
				 * Constructs an anonymous thread.
				 *
				 * @throw FeatureNotAvailableException if the multithreading
				 * feature is not available.
				 *
				 */
				Thread() throw( Features::FeatureNotAvailableException ) ;


				/**
				 * Constructs a named Thread object.
				 *
				 * @throw FeatureNotAvailableException if the multithreading
				 * feature is not available.
				 *
				 */
				explicit Thread( const std::string & name ) 
					throw( Features::FeatureNotAvailableException ) ;


				/// Basic destructor.
				virtual ~Thread() throw() ;


				/**
				 * This method must be invoked to run a thread.
				 *
				 * It creates a new POSIX thread which calls the
				 * method start().
				 *
				 * @example :
				 * <pre>
				 * Thread * myThread = new ThreadSubClass() ;
				 * myThread->run() ;
				 * </pre>
				 *
				 * @throw RunnableException if the thread could not
				 * be run properly, including if the multheading feature 
				 * is not supported.
				 *
				 */
				virtual void run() throw( RunnableException ) ;


				/// The start point for the newly created thread.
				virtual void start() throw() = 0 ;


				/**
				 * Soft thread cancelling method.
				 *
				 * Non blocking method to ask the thread to stop instead of
				 * cancelling it.
				 *
				 * This is the proper way to let the thread know that it should
				 * stop running.
				 * Thus it can terminate its critical work, before exiting.
				 * Hence this method does not make the thread stop, it does 
				 * ask it to stop in the hope it will be heard : if the thread
				 * does not invoke the stopDemanded() method systematically,
				 * it will never know about this request.
				 *
				 *
				 * @example Thread start() method :
				 * <pre>
				 * ...
				 *
				 * while ( ! stopDemanded() )
				 * {
				 *     // do my job
				 *     ...
				 * }
				 * ...
				 * </pre>
				 *
				 * @see cancel()
				 *
				 */
				virtual void askToStop() throw() ;


				/// Returns this Thread's unique id.
				SystemSpecificThreadIdentifier & id() const throw() ;


				/// Returns this thread's clean state.
				bool isClean() const ;


				/// Tells whether the thread is running.
				bool isRunning() const throw() ;


				/// Tells whether the thread has terminated its run.
				bool hasTerminated() const throw() ;


				/// Tells whether the thread has been asked to stop.
				bool stopDemanded() const ;


				/**
				 * Suspends the calling thread until the instance's thread 
				 * has stopped.
				 *
				 * @example :
				 * <pre>
				 *
				 * Thread * myThread = new ThreadSubClass() ;
				 * myThread->run() ;
				 * ...
				 * myThread->askToStop() ;
				 * myThread->waitUntilOver() ;
				 * ...
				 * </pre>
				 *
				 * @note Does nothing if the multithreading feature is not
				 * available.
				 *
				 */
				void waitUntilOver() throw() ;


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



				// Static section.
				
				
				/**
				 * Returns the number of Thread objects currently existing.
				 *
				 */
				static ThreadCount GetNumberOfThreads() throw() ;


				/**
				 * Makes the calling thread sleep during specified time.
				 *
				 * @param seconds the number of seconds to wait.
				 *
				 * @param microseconds the number of microseconds to wait,
				 * must be less than one second, it is only the remainder 
				 * of the total wanted duration.
				 *
				 * @throw ThreadException if the sleep failed.
				 *
				 */
				static void Sleep( System::Second seconds,
						System::Microsecond microseconds = 0 )
					throw( ThreadException ) ;


				/**
				 * Runs specified thread.
				 *
				 * @note Does nothing if the multithreading feature is not
				 * available.
				 *
				 */
				static void Run( Thread & thread ) throw()  ;



				/**
				 * Non-consuming CPU thread waiting mechanism.
				 *
				 * This inner class operates like a thread condition.
				 *
				 * @example :
				 * <pre>
				 *
				 * static Thread::Waiter waiter ;
				 * static bool ok = false ;
				 *
				 * thread #1 :
				 *
				 *   // Blocks the thread until ok is true.
				 *   while( ! ok ) 
				 *		waiter.wait() ;
				 *
				 * thread #2 :
				 *
				 *   // Do what you need to do :
				 *   ...
				 *   // Modify the shared value ok :
				 *   ok = true ;
				 *
				 *   // Restart the waiting thread 1 :
				 *   waiter.signal() ;
				 *
				 * </pre>
				 *
				 */
				class Waiter: protected Mutex
				{


					public:


						/**
						 * Basic constructor.
						 *
		 				 * @throw FeatureNotAvailableException if the
						 * multithreading feature is not available, so that 
						 * no Waiter instance can be created in this case.
		 				 *
						 * @see Feature::isMultithreadingSupported
						 *
						 */
						Waiter() 
							throw( Features::FeatureNotAvailableException ) ;


						/// Basic non-virtual destructor.
						~Waiter() throw() ;


						/**
						 * Blocks for <b>sec</b> seconds the calling thread,
						 * until time is elapsed, or signal() or broadcast() 
						 * is called by another thread.
						 *
						 * @param seconds the requested sleeping time, if null
						 * no timeout will occur.
						 *
						 * @return true if the thread has not been signaled
						 * during the requesting time. Always returns true
						 * if the multithreading feature is not available.
						 *
						 */
						bool wait( System::Second seconds = 0 ) throw() ;


						/**
						 * Signals only one waiting thread.
						 *
						 * @return true if the operation succeeded. Always
						 * returns true if the multithreading feature is 
						 * not available. 
						 *
						 */
						bool signal() throw() ;


						/**
						 * Signals all waiting threads.
						 *
						 * @return true if the operation succeeded. Always
						 * returns true if the multithreading feature is 
						 * not available. 
						 *
						 */
						bool broadcast() throw() ;



					private:


						/**
						 * Copy constructor made private to ensure that 
						 * it will never be called.
						 *
						 * The compiler should complain whenever this 
						 * undefined constructor is called, implicitly
						 * or not.
						 *
						 */
						Waiter( const Waiter & source ) throw() ;


						/**
						 * Assignment operator made private to ensure 
						 * that it will be never called.
						 *
						 * The compiler should complain whenever this
						 * undefined operator is called, implicitly or not.
						 *
						 */
						Waiter & operator = ( const Waiter & source ) throw() ;


						/**
						 * This Waiter's system-specific condition 
						 * variable, since multiple threads can wait on a
						 * single Waiter.
						 *
						 */
						SystemSpecificThreadCondition * _condition ;


				} ;



			protected:


				/**
				 * Cancels the thread's execution.
				 *
				 * This method is used to cancel the thread at any point,
				 * without letting it terminate properly its work.
				 *
				 * This method must be wrapped by a subclass method to treat 
				 * the critical behaviour.
				 *
				 * @note one should preferably use askToStop / stopDemanded /
				 * waitUntilOver.
				 *
				 * @example :
				 * <pre>
				 *
				 * class MyThread: public Thread
				 * {
				 *    public:
				 *
				 *        void start() ;
				 *
				 *        void kill()
				 *        {
				 *             // do what I need
				 *             ...
				 *             cancel() ;
				 *        }
				 *
				 * } ;
				 *
				 * ...
				 * myThread->run() ;
				 * ...
				 * myThread->kill() ;
				 * ...
				 * </pre>
				 *
				 */
				void cancel() throw() ;


				/**
				 * This method is called after the run.
				 * It is executed by the launching thread.
				 *
				 */
				virtual void cleanup() throw() ;

				
				/// Sets the running status.
				void setRunning( bool newRunningStatus ) throw() ;


				/// Called whenever the thread creation fails.
				virtual void threadCreationFailed( int error ) 
					throw( ThreadException ) ;



			private:


				/**
				 * Copy constructor made private to ensure that it will  
				 * never be called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */
				Thread( const Thread & source ) throw() ;


				/**
				 * Assignment operator made private to ensure that it will 
				 * never be called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 *
				 */
				Thread & operator = ( const Thread & source ) throw() ;


				/// This thread's unique system-specific identifier.
				SystemSpecificThreadIdentifier * _id ;


				/// This thread's system-specific attribute.
				SystemSpecificThreadAttribute * _attr ;


				
				/*
				 * Thread life cycle indicators.
				 *
				 * These informations can be read or written from different
				 * threads, hence have to be protected against concurrent
				 * accesses thanks to the Synchronized template.
				 * 
				 */
				
				
				/**
				 * Tells whether this thread has terminated, i.e. if its
				 * <code>cancel</code> method has already been called.
				 *
				 */
				Ceylan::System::Synchronized<bool> _terminated ;
				
				
				/**
				 * Tells whether this thread is clean, i.e. if its
				 * <code>cleanup</code> method has already been called.
				 *
				 */
				Ceylan::System::Synchronized<bool> _clean ;
				
				
				/**
				 * Tells whether this thread is currently running.
				 *
				 */
				Ceylan::System::Synchronized<bool> _running ;
				
				
				/**
				 * Tells whether this thread must stop, i.e. wheter it has
				 * been requested to do so.
				 *
				 */
				Ceylan::System::Synchronized<bool> _mustStop ;



				/**
				 * Number of the currently existing thread instances.
				 *
				 */
				static Synchronized<ThreadCount> * _Number ;


		} ;

	}

}



#endif // CEYLAN_THREAD_H_
