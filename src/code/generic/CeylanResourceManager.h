#ifndef CEYLAN_RESOURCE_MANAGER_H_
#define CEYLAN_RESOURCE_MANAGER_H_


#include "CeylanException.h"        // for inheritance

#include "CeylanUtils.h"            // for Ceylan::emergencyShutdown
#include "CeylanResource.h"         // for Ceylan::Resource
#include "CeylanOperators.h"        // for string operations


#include <string>


namespace Ceylan
{


	
	/**
	 * Exception to be raised when a Resource manager encounters an abnormal
	 * situation.
	 *
	 */
	class ResourceManagerException : public Ceylan::Exception
	{
	
		public:
		
			explicit ResourceManagerException( const std::string & reason )
				throw() ;
				
			virtual ~ResourceManagerException() throw() ;
	
	} ;
	
	
	
	/**
	 * Manages a set of Resource instances : the Resource manager can store 
	 * and afterwards retrieve resources on behalf of the caller, which will
	 * associate a key, whose type is user-defined, to each resource.
	 *
	 * The Resource manager takes ownership of the Resources it is given, 
	 * stores them all and, on request, thanks to the key, delivers to the
	 * caller only const references to these resources. 
	 *
	 * Such managers are especially designed to facilitate the memory 
	 * management of static resources.
	 *
	 * A Resource manager handles the life cycle of its resources : it takes
	 * their ownership, which means it will delete them when itself deleted or
	 * flushed.
	 * From the user's point of view, giving a Resource to the manager is an
	 * alternative to deleting the resource.
	 * By no means should the caller delete a managed Resource. Modifying them
	 * after having given them to the manager is not recommended : the caller
	 * should forget any pointer or reference to the Resources it sent to the
	 * manager.
	 *
	 * The resources can be submitted to the manager at the time when their
	 * state must be kept, including prior to any use or after they already 
	 * have been used, at the moment when they would have been deallocated
	 * should there be no cache.  
	 *
	 * The Resource manager has no quota to respect, it will store all given
	 * Resources regardless of the resulting size in memory, and will make 
	 * them available (as const), as long as the manager exists : it will
	 * never forget any resource while still alive.
	 *
	 * There are different use cases for such a cache, depending on what is
	 * to be done with the cached resources, which translates into the need 
	 * for cached resources to be cloned or not.
	 *
	 * If we take the example of a font rendering system, then this type of
	 * cache could be useful when the blitting of a glyph is requested : first
	 * the specified glyph is rendered into a new surface, then it is blitted
	 * on, say, the screen. 
	 * Instead of deallocating the surface after use, the blit function could
	 * pass it to an appropriate resource manager, which would take ownership
	 * of it.
	 * If, later, the same blitting is requested again, then the blit function 
	 * could start by asking the cache for this prerendered glyph. The cache,
	 * if fed as described before, should be able to provide it as a 'const'
	 * resource, which could be used directly for the targeted blit. 
	 *
	 * This will work as long as the user ensures that none of these 'const'
	 * resources is be used after the manager deleting. 
	 *
	 * As no direct inheritance relationship can exist between the basic and
	 * smart managers (for example, the 'takeOwnershipOf' method should not 
	 * have the same parameters, respectively it should be 'Resource' and
	 * 'SmartResource'), this abstract class is needed.
	 *
	 * In all templates inheriting from this one, the data members (ex :
	 * _cacheHits) should be specified as 'this->_cacheHits' or
	 * 'Ceylan::ResourceManager<Key>::_cacheHits', since otherwise the compiler
	 * would not search for these member names in this mother class : it may
	 * not contain a '_cacheHits' member for all 'Key' choice, because of
	 * template specialization that may occur.
	 *
	 */
	template <class Key>
	class ResourceManager : public Ceylan::TextDisplayable
	{
	
	
		public:
		
			
			/**
			 * Creates an abstract Resource manager.
			 *
			 */
			explicit ResourceManager() throw() ;
  
 
 			/**
			 * Virtual destructor, deletes all resources still in cache.
			 *
			 * @note Each child class should have its destructor call the
			 * 'flush' method.
			 */
 			virtual ~ResourceManager() throw() ;
	
					
			/**
			 * Returns whether the specified key is already associated with a
			 * Resource.
			 *
			 * Useful to avoid trying to overwrite a resource already 
			 * associated with a key.
			 *
			 */
			virtual bool isKeyAlreadyAssociated( const Key & key ) 
				const throw() = 0 ;
			
			
			/**
			 * Returns directly the resource associated with this key, if
			 * available in cache.
			 * Otherwise returns a null pointer.
			 *
			 * @return A 'const' resource since it must not be changed in any
			 * way by the caller (not modified, not deallocated, etc.) so that
			 * the version in cache remains untouched. 
			 * Similarly, no entry with the same key must be put in cache nor
			 * the cache itself must be deallocated while a returned resource 
			 * is in use, since it would result in the deallocation of this
			 * resource.
			 *
			 * @note The method itself cannot be 'const' since some metadata 
			 * in cache entries might be updated.
			 *
			 */
			 virtual const Resource * get( const Key & key ) throw() = 0 ;
			 						
			 
			/**
			 * Removes and deletes all resources currently in cache.
			 *
			 * Cache statistics are not modified.
			 *
			 * @note Should be called in the destructor of each child class.
			 *
			 */
			virtual void flush() throw() = 0 ;
						
			
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
			 * Records the total number of requested resources successfully
			 * found in cache.
			 *
			 */
			Ceylan::Uint32 _cacheHits ;
			
			
			/**
			 * Records the total number of requested resources not found in
			 * cache.
			 *
			 */
			Ceylan::Uint32 _cacheMisses ;
						


		
		private:
		
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			ResourceManager( const ResourceManager & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will be
			 * never called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			ResourceManager & operator = ( const ResourceManager & source )
				throw() ;
			
			
	} ;



	/// Public section : implementation.


	template <class Key>
	ResourceManager<Key>::ResourceManager<Key>() throw() :
		_cacheHits( 0 ),
		_cacheMisses( 0 )
	{
	
	}	

				
	template <class Key>
	ResourceManager<Key>::~ResourceManager<Key>() throw()
	{
		/*
		 * Cannot be called at this level since 'flush()' is
		 * implementation-dependent.
		 *
		 */
	}	
			
			
	template <class Key>
	const std::string ResourceManager<Key>::toString( VerbosityLevels level )
		const throw()
	{
		
		std::string res = "Abstract Resource manager" ;		

		if ( level == Ceylan::low )	
			return res ;
			
		Ceylan::Uint32 total = _cacheHits + _cacheMisses ;
		
		return res + ". The average cache success is " 
				+ Ceylan::toNumericalString( 
					static_cast<Ceylan::Uint8>( 
						( 100.0f * _cacheHits ) / total ) )
				+ "% (" + Ceylan::toString( _cacheHits ) + " cache hit(s) for " 
				+ Ceylan::toString( _cacheMisses ) + " cache misse(s))" ;
	}
	
	
}


#endif // CEYLAN_RESOURCE_MANAGER_H_

