#ifndef CEYLAN_SINGLETON_H_
#define CEYLAN_SINGLETON_H_


#include <string>

#include "CeylanException.h"


namespace Ceylan
{



	/// Exception raised by the Singleton class.
	class SingletonException: public Ceylan::Exception
	{

		public:
	
			explicit SingletonException( const std::string & reason ) throw() ;
			virtual ~SingletonException() throw() ;
			
	} ;
	
	

	/**
	 * This helper class ensures that one and only one instance of a particular
	 * class exists, so that this instance is necessarily shared by all its
	 * users.
	 *
	 * This class should be templated so that it can be used with any class 
	 * that has to have only one instance at any time.
	 *
	 * Another possible implementation would be :
	 *
	 * static AClass::singleton()
	 * {
 	 *   static AClass singleton ;
	 *   return singleton ;
	 * }
	 *
	 * @note This implementation is mainly for explanation purpose.
	 *
	 */
	class Singleton
	{


	    public:


	        /**
	         * Returns the one and only one Singleton instance available.
	         *
	         * The returned value is a reference and not a pointer, to avoid 
			 * any abnormal deallocation by its users, that should never
			 * deallocate the Singleton.
	         *
	         */
	        static Singleton & GetSingleton() throw() ;


			/// Removes the shared Singleton.
	        static void DeleteSingleton() throw() ;



	    protected:


	        /// Basic constructor.
	        Singleton() throw() ;


	        /// Basic virtual destructor.
	        virtual ~Singleton() throw() ;



	    private:


			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 * @note Made to avoid unwanted hidden clone of the Singleton.
			 *
			 */			 
			Singleton( const Singleton & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 */			 
			Singleton & operator = ( const Singleton & source ) throw() ;


	        /// The internal single instance.
	        static Singleton * _InternalSingleton ;



	} ;

}



#endif // CEYLAN_SINGLETON_H_
