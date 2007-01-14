#ifndef CEYLAN_SMART_RESOURCE_H_
#define CEYLAN_SMART_RESOURCE_H_


#include "CeylanResource.h"         // for inheritance
#include "CeylanMeasurable.h"       // for inheritance
#include "CeylanClonable.h"         // for inheritance



namespace Ceylan
{



	/**
	 * Smart Resources are advanced resources, able to determine their size 
	 * and clone themselves at will, for example so that they can be cached
	 * by a SmartResourceManager.
	 *
	 * All resources implement the Measurable interface, and therefore should
	 * be able to evaluate the size in memory they are occupying, so that the
	 * cache can adapt its policy with regard to the memory it uses for them.
	 *
	 * All resources implement the Clonable interface too, since the cache may
	 * have to store only copies of submitted resources, and may have to give
	 * the caller a copy (a clone) of them.
	 * Otherwise the cached resource would be modified too as the original one
	 * is used afterwards, or could be even silently deallocated while still 
	 * in cache, depending of their use by the caller.
	 *
	 */
	class CEYLAN_DLL SmartResource : 
		public Resource, public Measurable, public Clonable
	{
	
		public:
		
			
			/// Creates a new resource.
			SmartResource() throw() ;
			
			/// Virtual destructor.
			virtual ~SmartResource() throw() ;
	
			
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
			SmartResource( const SmartResource & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 *
			 */			 
			SmartResource & operator = ( const SmartResource & source ) 
				throw() ;
				
			
	} ;

}


#endif // CEYLAN_SMART_RESOURCE_H_
