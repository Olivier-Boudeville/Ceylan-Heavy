#ifndef CEYLAN_SMART_RESOURCE_MANAGER_H_
#define CEYLAN_SMART_RESOURCE_MANAGER_H_



#include "CeylanResourceManager.h"  // for inheritance

#include "CeylanSmartResource.h"    // for Ceylan::SmartResource



// Declarations are sorted by theme, definitions are sorted by interface.


// Usually no particular need to debug this code :
#define CEYLAN_DEBUG_SMART_RESOURCE_MANAGER 0


namespace Ceylan
{


	
	/**
	 * Manages a set of SmartResource instances : the smart Resource manager 
	 * can behave like a basic Resource manager, if the 'NeverDrop' policy is
	 * used, or it can provide other advanced policies. 
	 * For all policies, supplementary means of storing resources in the cache
	 * and retrieving them are provided. These new functionalities are allowed
	 * thanks to the ability of smart resources to compute themselves their 
	 * size and to be cloned. 
	 *
	 * Various ownerships and 'const'-ness for the submitted and returned
	 * resources can be chosen, so that only the necessary resource clones are
	 * created.
	 *
	 * Basically, besides the inherited way of storing a resource
	 * ('takeOwnershipOf' method), smart resources can be cloned, and have their
	 * clone stored into the cache, so that the caller keeps the ownership of
	 * the submitted resource and the possibility to modify it afterwards : the
	 * smart manager just reads the specified resource and, if its policy tells
	 * this resource can be cached, it makes a clone of it so that it can store
	 * this copy for later use, without changing in any way the specified
	 * resource and what the user can do with it. The 'scanForAddition' method
	 * corresponds to this alternative to the 'takeOwnershipOf' method.
	 *
	 * Similarly, to the inherited way of retrieving a resource from cache
	 * (the 'get' method, which returns only a 'const' resource) is added a new
	 * method ('getClone'), which returns a clone of the resource specified by 
	 * a key. The ownership of the clone is transferred to the caller, who
	 * will be able to modify it at will, and will have to delete it, once
	 * finished with it.
	 *
	 * Various cache policies can be chosen to limit the size in memory taken 
	 * by the resources cached by a smart manager. 
	 *
	 * If the smart Resource manager has to enforce a quota (an upper bound to
	 * the total memory size of cached resources), it will have to drop cached
	 * entries based on the current cache policy, whose role is to select which
	 * resources should be kept and which should be dropped.
	 *
	 * Specifying a policy enforcing a quota implies that resources can be
	 * dropped and thus that the application is able to regenerate them when
	 * needed : in this case the application can never rely on the cache to
	 * answer to a given request, even if the corresponding entry had been
	 * recently submitted to the cache. It is the price for memory control.
	 *
	 * As computing resource sizes may be demanding, they are recomputed only 
	 * on request ('updateSizes' method), otherwise the size taken into account
	 * for a resource is the one it had when lastly managed by the cache.
	 *
	 * A smart Resource manager handles the life cycle of the resources whose
	 * ownership has been given to it : it will deallocate them when itself
	 * deleted, or sooner for various reasons, including if having a quota to
	 * respect and needing some more room.
	 *
	 * The resources can be submitted to the manager at the time when their
	 * state must be kept, including prior to any use or after they already 
	 * have been used, at the moment when they would have been deallocated
	 * should there be no cache. 
	 * 
	 * For each use case, both the way of submitting the resource (hereby 
	 * called a 'put' operation, be it 'takeOwnershipOf' or 'scanForAddition'),
	 * and of retrieving them (a 'get' operation, be it 'get' or 'getClone'),
	 * have to be carefully chosen.
	 * All combinations are valid, there are just different use cases for such
	 * a cache, depending on what is to be done with the cached resources, 
	 * which translates into the need for cached resources to be cloned or not.
	 *
	 * If we take the example of a font rendering system, we saw with basic
	 * ResourceManager how a 'takeOwnershipOf'/'get' pair could be useful. 
	 * On the contrary of a mere blit, if a glyph is needed but will be altered
	 * after its creation, then when this surface is in some particular state,
	 * the user may would like to have it cached. In this case, the user will
	 * submit that surface to the cache, asking that the cache does not take 
	 * the ownership of it, but makes a clone of it, so that the user can keep
	 * on using his surface. This can be done thanks to the 'scanForAddition'
	 * method. If the cache is able to store this smart resource, then it will
	 * clone it and store the clone. The submitted surface will not be
	 * modified in any way. 
	 * If the key associated to this surface is requested afterwards, then, if
	 * still in cache, it will be returned, either as 'const Resource' or as a
	 * non-const resource (hence a clone of the initial clone), depending on
	 * the choice of the caller ('get' or 'getClone').  
	 *
	 * Such an organization allows to clone the resource only if needed, i.e.
	 * only when the manager knows for sure that the resource will be accepted
	 * in cache with regard to its state and the current cache policy being
	 * applied. Otherwise a resource could be cloned before knowing whether
	 * the clone can be accepted in cache, hence leading to useless cloning
	 * operations.
	 *
	 * The SmartResourceManager class should not be a child class of the
	 * BasicResourceManager class, since it must deal with smart resources only.
	 * However its 'NeverDrop' policy results in a behaviour very similar to
	 * the one of a basic Resource manager.
	 *
	 */
	template <class Key>
	class SmartResourceManager : public Ceylan::ResourceManager<Key>
	{
	
	
		public:
		
			
			/**
			 * Lists all available cache policies :
			 *
			 *  - 'NeverDrop' : no quota enforced, no resource dropped, except
			 * on manager deleting or explicit 'flush' request.
			 * If a 'put' operation would lead to an already cached resource
			 * being replaced by another resource associated to the same key,
			 * a ResourceManagerException is raised.
			 * Therefore the only cause for a non-cloned (const resource)
			 * returned by a 'get' operation to become invalid is the resource
			 * manager being deleted (or flushed) : its life cycle must
			 * therefore be correctly managed by the user.
			 *
			 *	- 'DropLessRequestedFirst' : the manager may drop resources for
			 * three reasons. 
			 * If the quota is reached, it will drop the less requested
			 * resources, no matter their size in memory (but if this policy 
			 * is applied as part of a 'put' operation, the newly cached
			 * resource will escape this first drop round). 
			 * If a resource is put with a key already associated with a
			 * previously put resource, the new resource will replace the
			 * previous one, which will be dropped.
			 * If the manager is deleted or flushed, all resources will be
			 * dropped.
			 *
			 * There is a drawback with such templated enumerations : user code
			 * cannot specify a cache policy independently of the type of the
			 * key chosen for the template instanciation.
			 * For example, a cache policy cannot be specified as
			 * SmartResourceManager::CachePolicy, it has to be for example
			 * SmartResourceManager<int>::CachePolicy. In this case,
			 * SmartResourceManager<float>::CachePolicy is a completely
			 * different type, which can be a bit inconvenient. 
			 * See OSDL::Font to have an example of issue and work-around.
			 *
			 */	
			enum CachePolicy { NeverDrop, DropLessRequestedFirst } ;
			
			
			/**
			 * Creates a new smart resource manager, which will store, take
			 * ownership of and make available the resources that will be put
			 * in it, with the 'NeverDrop' policy, i.e. with no concern for 
			 * size limit, as a basic ResourceManager would do.
			 *
			 * As no resource will be ever dropped until the manager is 
			 * flushed or deleted, it is the user responsibility to take care
			 * of what is put in cache, so that the total size in memory does
			 * not increase too much.
			 *
			 */
			explicit SmartResourceManager() throw() ;
 
 
			/**
			 * Creates a new smart resource manager, which will store and make
			 * available resources, with respect to the specified quota and the
			 * specified policy. 
			 *
			 * It implies dropping a smart Resource if the cache policy
			 * determines it.
			 *
			 * @param quota the upper bound of the total size of cached
			 * resources in memory, if the selected policy takes it into
			 * account. No default value is provided since it depends too much
			 * on the resources being actually cached.
			 *
			 * @param policy the cache policy to enforce.
			 *
			 */
			explicit SmartResourceManager( System::Size quota, 
				CachePolicy policy = DropLessRequestedFirst ) throw() ;
 
 
 			/// Virtual destructor, deletes all owned resources still in cache.
 			virtual ~SmartResourceManager() throw() ;
	
			
			
			// Two different 'put' operations.
	
	
			/**
			 * Puts, if possible, a clone of specified resource in cache,
			 * associated with specified key, without taking ownership of the
			 * specified resource.
			 *
			 * With the 'NeverDrop' policy, the specified resource will be
			 * cloned and put in cache, provided the specified key is not
			 * already associated. If it was the case, then nothing would be
			 * done, the association would be left as it was.
			 *
			 * With the 'DropLessRequestedFirst' policy, the smart Resource
			 * manager will do its best to add a clone of the resource in 
			 * cache : if there is not enough space, the less requested
			 * resources already in cache will be dropped until the new smart
			 * resource clone can fit into the cache, provided it is simply
			 * possible, i.e. the resource size is smaller than the quota.
			 * Otherwise the resource will be trivially rejected and will not
			 * be cloned.
			 *
			 * In all cases, the smart manager will handle on its own the life
			 * cycle of its clones, and will perform their deleting depending 
			 * on the cache policy, but no later than its own deleting.
			 *
			 * @param key the key by which that resource should be retrieved.
			 * If this key is already associated with a resource, and, for the
			 * 'NeverDrop' policy, if the size of the new resource is smaller
			 * than the quota, the already cached resource is deallocated first,
			 * before being replaced by the new resource.
			 *
			 * @param smartResource the smart resource to put in cache. It may
			 * not be a dynamically allocated resource, since it will be either
			 * left alone or cloned, but never owned.
			 *
			 * @return true iff the specified resource could be cached. For
			 * example, if the size of the resource is higher than the manager
			 * quota, then the resource will not be stored and false will be
			 * directly returned. 
			 *
			 */
			virtual bool scanForAddition( const Key & key, 
				const SmartResource & smartResource ) throw() ;
			
			
	
			/**
			 * Puts specified resource in cache, associated with specified key,
			 * and takes ownership of it.
			 * 
			 * The Resource manager takes ownership of all the supplied
			 * resources and will perform their deleting whenever its current 
			 * policy requests it, or during its own deleting, or when the
			 * 'flush' method is called.
			 *
			 * If the policy is 'NeverDrop' and if the key is already 
			 * associated to a resource, then the association is left as is, 
			 * and a ResourceManagerException is raised.
			 *
			 * @param key the key by which that resource could be retrieved. 
			 * If this key is already associated with a resource, and if the
			 * cache policy is 'NeverDrop', an exception is raised. Otherwise
			 * the preceding entry is dropped and the new resource replaces it.
			 *
			 * @param resource the resource to put in cache. It must be a
			 * dynamically allocated resource (thanks to new, no automatic
			 * variable) since it will be deallocated by the cache when
			 * appropriate.
			 *
			 * @return true iff the specified resource could be cached.
			 * Even though the resource could not be cached, the manager will
			 * deallocate it, so that the caller can rely on the fact that in
			 * all cases he has nothing to do with the submitted resource after
			 * this call. 
			 * The caller notably must not delete the resource since the 
			 * manager will always handle the deleting itself.
			 *
			 * @throw ResourceManagerException if the specified key was already
			 * associated with a Resource, and if the policy is 'NeverDrop'.
			 *
			 * @see isKeyAlreadyAssociated
			 *
			 */
			virtual bool takeOwnershipOf( const Key & key, 
					const SmartResource & resource ) 
				throw( ResourceManagerException ) ;
			

					
			/**
			 * Tells whether the specified key is already associated with a
			 * Resource.
			 *
			 * Useful to avoid trying to overwrite a resource already 
			 * associated with a key.
			 *
			 */
			virtual bool isKeyAlreadyAssociated( const Key & key )
				const throw() ;



			// Two different 'get' operations.
	
	
			/**
			 * Returns a clone of the resource associated with this key, if
			 * available in cache.
			 * Otherwise returns a null pointer.
			 *
			 * Cloning, even if it is complex or resource-demanding, is
			 * necessary since with this method the caller should be able to 
			 * do anything with the returned resource, including modifying it
			 * and/or deallocating it independently from the resource 
			 * manager and its life cycle.
			 *
			 * @note This ethod cannot be 'const' since some metadata in cache
			 * entries might be updated.
			 *
			 */
			virtual SmartResource * getClone( const Key & key ) throw() ;
			

			/**
			 * Returns directly the smart resource associated with this key, 
			 * if available in cache.
			 * Otherwise returns a null pointer.
			 *
			 * The signature of this inherited method tells it returns
			 * Resource instances, but more precisely in the case of a smart
			 * manager it will be always SmartResource instances.
			 *
			 * @return A 'const' resource since it must not be changed in any
			 * way by the caller (not modified nor deallocated, etc.) so that
			 * the version in cache remains unaltered. 
			 * Similarly, no entry with the same key must be put in cache nor
			 * the cache itself must be deallocated while a returned resource 
			 * is in use, since it would result in the deallocation of this
			 * resource.
			 *
			 * @note The method itself cannot be 'const' since some metadata 
			 * in cache entries might be updated.
			 *
			 */
			 virtual const Resource * get( const Key & key ) throw() ;



			// Section about policies and sizes.

			
			/**
			 * Returns the smart resource manager current quota, in bytes.
			 * 
			 * @note A non-null quota does not imply resources can be dropped,
			 * the only criterion is the cache policy.
			 *
			 */
			virtual System::Size getQuota() const throw() ;
		

			/**
			 * Returns the cache policy currently being enforced.
			 *
			 */
			virtual CachePolicy getCachePolicy() const throw() ;
			
					
			/**
			 * Returns the approximate size of the memory used by all currently
			 * cached resources, in bytes.
			 * 
			 * The size of the smart resource manager itself is not taken into
			 * account in the sum.
			 * In most cases its size is negligible compared to the total 
			 * resource size.
			 *
			 * @note The sizes taken into account are the ones that are already
			 * available, no special recomputing is performed.
			 *
			 * @see updateSizes
			 *
			 */
			virtual System::Size getFootprint() const throw() ;
		
		
		
			// Various cache management facilities.
			
			
			/**
			 * Updates the cache metadata : recomputes the size in memory of
			 * each cache entry, and applies the selected cache policy.
			 *
			 * @see updateSizes, applyPolicy
			 *
			 */
			virtual void update() throw() ;
			
			
			/**
			 * Recomputes the size in memory of each cache entry.
			 *
			 */
			virtual void updateSizes() throw() ;


			/**
			 * Applies the current policy on cache entries.
			 *
			 * @note The size which is taken into account is based onto the
			 * metadata of the entries, which may be out-of-date. 
			 * If one wants the cache policy to rely on accurate sizes, the
			 * 'updateSizes' method should be called first.
			 * 
			 */
			virtual void applyPolicy() throw() ;
			
			 
			/**
			 * Removes and deletes all resources currently in cache.
			 *
			 * Cache statistics are not modified.
			 *
			 */
			virtual void flush() throw() ;
						
			
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
		
		
			// Forward declaration.
			struct CacheEntry ;
			
			
			
			/**
			 * Applies the current policy.
			 *
			 * @param lastCached, if non null, indicates that the policy is
			 * being applied just upon this last resource has been put in cache,
			 * and therefore this resource should be preserved from this first
			 * shrink. 
			 * If lastCached is null, the policy will be applied uniformly
			 * against all cache entries.
			 *
			 */
			virtual void applyPolicy( const SmartResource * lastCached ) 
				throw() ;
			

			/**
			 * Applies the 'DropLessRequestedFirst' policy on current cached
			 * entries.
			 *
			 */
			virtual void applyDropLessRequestedFirst( 
				const SmartResource * lastCached = 0 ) throw() ;
			
			
			/**
			 * Adds a new cached entry, with the specified resource and key. 
			 * If the key is already associated with a cache entry, this cache
			 * entry will be deleted first, including its embedded resource.
			 *
			 * @param key the key for this new cache entry. If the key is
			 * already assigned in the cache, the previous entry is erased
			 * first.
			 *
			 * @param newResource the smart resource which should be cached.
			 *
			 * @param size the size of this resource, if not null (allows to
			 * avoid useless size recomputations, since they might be
			 * demanding). If null, the actual size will be computed on the fly.
			 *
			 * @throw ResourceManagerException if the policy is 'NeverDrop' 
			 * and the key is already associated to a resource. In this case,
			 * the association remains untouched.
			 *
			 */
			virtual void addEntry( const Key & key, 
					const SmartResource & newResource, 
					Ceylan::System::Size size = 0 ) 
				throw( ResourceManagerException ) ;
				
			
			/**
			 * Drops specified cache entry : helper for cache policy execution. 
			 *
			 * Should be called by all methods applying a policy such as
			 * 'applyDropLessRequestedFirst' (since this method does everything
			 * needed), and only by them (since it updates the statistics of
			 * drop-because-of-policy as well).
			 *
			 * @param pos the iterator pointing to the cache entry to drop.
			 *
			 */
			virtual void dropEntryDueToPolicy( typename std::map<Key, 
				CacheEntry *>::iterator pos ) throw() ;
			
			
			/**
			 * Drops specified cache entry. 
			 *
			 * @param pos the iterator pointing to the cache entry to drop.
			 *
			 */
			virtual void dropEntry( typename std::map<Key, 
				CacheEntry *>::iterator pos ) throw() ;
			
			
			/**
			 * The quota that would be enforced by this cache, provided that 
			 * a policy makes us of it.
			 *
			 */
			System::Size _quota ;
	

			
			/**
			 * The approximate total size of all cached resources, as computed
			 * after last resource inspection and updated on drop/put
			 * operations.
			 *
			 */
			System::Size _totalSize ;
			
			
			/// The policy enforced by this cache.
			CachePolicy _policy ;
			
			
			/**
			 * Records the total number of resources dropped by this manager
			 * because of cache policy (counting neither resource replacements
			 * because of key collision nor the final drops caused by manager
			 * deleting).
			 *
			 */
			Ceylan::Uint32 _droppedByPolicy ;
			
			
			/// Metadata associated to a cached resource.
			struct CacheEntry
			{
			
			
				/// A pointer to the Resource itself.
				const SmartResource * _resource ;
				
				/**
				 * The number of times that this resource was already
				 * requested through this cache.
				 *
				 */
				Uint32 _requestCount ;
				
				/// The number of times this smart resource had to be cloned.
				Uint32 _cloneCount ;	
							
				/// The size in memory of this resource.
				System::Size _size ;
			
			
			} ;	
			
			
			// The cache itself.
			std::map<Key, CacheEntry *> _entries ;
			
			
			
		
		private:
		
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 */			 
			SmartResourceManager( const SmartResourceManager & source ) 
				throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator 
			 * is called, implicitly or not.
			 * 
			 *
			 */			 
			SmartResourceManager & operator = ( 
				const SmartResourceManager & source ) throw() ;
			
			
	} ;



	// Public section : implementation.


	template <class Key>
	SmartResourceManager<Key>::SmartResourceManager() throw() :
		ResourceManager<Key>(),
		_quota( 0 ),
		_totalSize( 0 ),
		_policy( NeverDrop ),
		_droppedByPolicy( 0 ),
		_entries()
	{
	
	}	

		
	template <class Key>
	SmartResourceManager<Key>::SmartResourceManager( System::Size quota,
		CachePolicy policy )
			throw() :
		ResourceManager<Key>(),
		_quota( quota ),
		_totalSize( 0 ),
		_policy( policy ),
		_droppedByPolicy( 0 ),
		_entries()
	{
	
	}	
		
		
	template <class Key>
	SmartResourceManager<Key>::~SmartResourceManager<Key>() throw()
	{
	
		/**
		 * There does not seem to exist any way of calling 'flush' from 
		 * the abstract mother class, so that no child class could forget to
		 * call it.
		 *
		 */
		flush() ; 
	}	
			

	
	// New methods provided by the smart Resource manager.
	
	
	template <class Key>
	System::Size SmartResourceManager<Key>::getQuota() const throw()
	{
		return _quota ;
	}	


	template <class Key>
	typename Ceylan::SmartResourceManager<Key>::CachePolicy 
		SmartResourceManager<Key>::getCachePolicy() const throw()
	{
		return _policy ;
	}	
					
					
	template <class Key>
	System::Size SmartResourceManager<Key>::getFootprint() const throw()
	{
	
		/*
		 * A call to 'updateSizes' should not be necessary since the total
		 * size should be updated at each drop/put operation.
		 *			
		 */
		
		return _totalSize ;
		
	}
	

	template <class Key>
	bool SmartResourceManager<Key>::scanForAddition( const Key & key, 
		const SmartResource & smartResource ) throw()
	{


		/*
		 * Code very similar to the one of the 'takeOwnershipOf', which could 
		 * be called by this method with a clone of the resource, at the 
		 * expense of useless operations.
		 * Duplicating code seems to be simpler, sharing would involve adding
		 * a policy-dependent 'bool canBeCached( const SmartResource &
		 * smartResource ) const throw()'.
		 *
		 */
		 
		System::Size resourceSize = smartResource.getSizeInMemory() ;
		
		// Trivial rejection : can drop and should drop -> drop.
		if ( _policy != NeverDrop && resourceSize > _quota )
			return false ;
		
		
		/*
		 * Do nothing if the policy is 'NeverDrop' and the key is already
		 * associated : otherwise this new resource would replace the previous
		 * one, which would then be dropped despite the policy.
		 *
		 */		
		if ( _policy == NeverDrop && isKeyAlreadyAssociated( key ) )
			return false ;
				
				
		// Incorporates unconditionally this resource in cache :	
		
		
		SmartResource & clone = * dynamic_cast<SmartResource *>(
			& smartResource.clone() ) ;
		
		/*
		 * Records the new entry, with a clone of the resource (hence should
		 * have the same size).
		 * No exception should be raised here since the key has already been
		 * checked for any association already set, if policy is 'NeverDrop'.
		 *
		 */
		addEntry( key, clone, resourceSize ) ;


		/*
		 * Reshuffles entries against cache policy, avoid cache starvation by
		 * encouraging turn-over (newcomers are preserved from first cache
		 * shrink) :
		 *
		 */
		applyPolicy( & clone ) ;
		
		return true ;
		
	}


	template <class Key>
	bool SmartResourceManager<Key>::takeOwnershipOf( const Key & key, 
		const SmartResource & smartResource ) throw( ResourceManagerException )
	{

		/*
		 * Code very similar to the one of the 'scanForAddition', which may
		 * call this method with a clone of the resource, at the expense of
		 * useless operations.
		 * Duplicating code seems to be simpler, sharing would involve adding
		 * a policy-dependent 'bool canBeCached( const SmartResource &
		 * smartResource ) const throw()'.
		 *
		 */
		 
		System::Size resourceSize = smartResource.getSizeInMemory() ;
		
		// Trivial rejection : can drop and should drop -> drop.
		if ( _policy != NeverDrop && resourceSize > _quota )
			return false ;
			
			
		// Incorporates unconditionally this resource in cache :	
		
		
		// Records the new entry, with directly the provided resource :
		addEntry( key, smartResource, resourceSize ) ;

		/*
		 * Reshuffles entries against cache policy, avoid cache starvation by
		 * encouraging turn-over (newcomers are preserved from first cache
		 * shrink) :
		 *
		 */
		applyPolicy( & smartResource ) ;
		
		return true ;
		
	
	}
		

	template <class Key>
	SmartResource * SmartResourceManager<Key>::getClone( const Key & key )
		throw()
	{
	
		// Look-up entry, if any :
		typename std::map<Key, CacheEntry *>::const_iterator it = 
			_entries.find( key ) ;
		
		if ( it == _entries.end() )
			return 0 ;
		
		// Found, let's clone it :
		this->_cacheHits++ ;
		(*it).second->_requestCount++ ;

#if CEYLAN_DEBUG_SMART_RESOURCE_MANAGER
		
		SmartResource * res = dynamic_cast<SmartResource *>( 
			& (*it).second->_resource->clone() ) ;
			
		if ( res != 0 )
			return res ;
		else
		{
			Ceylan::emergencyShutdown( "SmartResourceManager<Key>::getClone : "
				"clone cannot be casted back to Smart resource for "
				+ (*it).second->_resource->toString() ) ;
			// Avoid warning :	
			return res ;		
		}			
		
#else // CEYLAN_DEBUG_SMART_RESOURCE_MANAGER
		
		return dynamic_cast<SmartResource *>( 
			& (*it).second->_resource->clone() ) ;
		
#endif // CEYLAN_DEBUG_SMART_RESOURCE_MANAGER

	}
	

	
	template <class Key>
	void SmartResourceManager<Key>::update() throw()
	{
		updateSizes() ;
		applyPolicy() ;
	}

	
	template <class Key>
	void SmartResourceManager<Key>::updateSizes() throw()
	{
	
		System::Size resourceSize ;		
		_totalSize = 0 ;
		
		for ( typename std::map<Key, CacheEntry *>::const_iterator it =
			_entries.begin() ; it != _entries.end(); it++ )
		{
			resourceSize = (*it).second->_resource->getSizeInMemory() ;
			(*it).second->_size = resourceSize ;
			_totalSize += resourceSize ;
		}
		
	
	}
	
		
	template <class Key>
	void SmartResourceManager<Key>::applyPolicy() throw()
	{
		applyPolicy( /* no newcomer in cache */ 0 ) ;
	}



	
	// Section of overriding methods inherited from ResourceManager.	


	template <class Key>
	bool SmartResourceManager<Key>::isKeyAlreadyAssociated( const Key & key )
		const throw()
	{

		// Look-up any previous entry :
		return ( _entries.find( key ) != _entries.end() ) ;

	}


	template <class Key>
	const Resource * SmartResourceManager<Key>::get( const Key & key ) throw()
	{
	
		typename std::map<Key, CacheEntry *>::const_iterator it = 
			_entries.find( key ) ;
			
		if ( it != _entries.end() )
		{
			this->_cacheHits++ ;
			(*it).second->_requestCount++ ;
			return (*it).second->_resource ;
		}
		else
		{
			// Cache miss :
			this->_cacheMisses++ ;
			return 0 ;
		}	
	}


	
	
	template <class Key>
	void SmartResourceManager<Key>::flush() throw()
	{
	
		/*
		 * Could use dropEntry as well, but removing elements from a STL
		 * container while iterating on it might be dangerous.
		 *
		 */
		
		for ( typename std::map<Key, CacheEntry *>::const_iterator it =
			 _entries.begin() ; it != _entries.end(); it++ )
		{
		
			// Delete the resource :
			delete (*it).second->_resource ;
			
			// Delete the cache entry :
			delete (*it).second ;
		}
		
		_entries.clear() ;
		_totalSize = 0 ;
		
	}
	

	template <class Key>
	const std::string SmartResourceManager<Key>::toString( 
		VerbosityLevels level ) const throw()
	{
		
		std::string res = "Smart resource manager applying " ;
		
		switch( _policy )
		{
		
			case NeverDrop:
				res += "the 'Never drop'" ;
				break ;
				
			case DropLessRequestedFirst:
				res += "the 'Drop less requested first'" ;
				break ;
			
			default:
				res += "an unknown (hence abnormal)" ;			
				break ;	
		}
		
		res += " cache policy" ;
		
		if ( _policy != NeverDrop )
			res += ", with a memory size quota of " 
				+ Ceylan::toString( _quota ) + " bytes" ;
		
		if ( level == Ceylan::low )
			return res ;
			
		res += ". It is currently managing " ;
				
		System::Size resourceCount = _entries.size() ;
		
		if ( resourceCount == 0 )
			res += "no resource" ;
		else	
			res += Ceylan::toString( resourceCount ) + " resource(s), "
				"for a total estimated size of "
				+ Ceylan::toString( _totalSize ) + " bytes" ;
				
		if ( _droppedByPolicy == 0 )
			res += ". No resource dropped because of cache policy" ;
		else
			res += ". " + Ceylan::toString( _droppedByPolicy ) 
				+ " resource(s) dropped because of cache policy" ;
				 	
		Ceylan::Uint32 total = this->_cacheHits + this->_cacheMisses ;
		
		if ( total == 0 )
			res += ". No resource request processed for the moment" ;	
		else
			res += ". The average cache success is " 
				+ Ceylan::toNumericalString( 
					static_cast<Ceylan::Uint8>( 
						( 100.0f * this->_cacheHits ) / total ) )
				+ "% (" + Ceylan::toString( this->_cacheHits ) 
				+ " cache hit(s) for " 
				+ Ceylan::toString( this->_cacheMisses ) + " cache misse(s))" ;


		if ( level == Ceylan::medium )	
			return res ;
		
		if ( resourceCount == 0 )
			return res ;
			
		res += ". Displaying current cached entrie(s) : " ;
		
		Ceylan::Uint32 count = 0 ;
		std::list<std::string> entries ;
			
		for ( typename std::map<Key, CacheEntry *>::const_iterator it =
			 _entries.begin() ; it != _entries.end(); it++ )
		{
		
			count++ ;
			entries.push_back( "[Entry #" + Ceylan::toString( count ) 
				+ "] : size = "	+ Ceylan::toString( (*it).second->_size ) 
				+ ", request count = "	
				+ Ceylan::toString( (*it).second->_requestCount ) 
				+ ", resource description = '"	
				+ (*it).second->_resource->toString( Ceylan::low )
				+ "'" ) ; 
		}	
		
		return res + Ceylan::formatStringList( entries ) ;
		
	}
	
	
	
	// Protected section : implementation.



	template <class Key>
	void SmartResourceManager<Key>::applyPolicy( 
		const SmartResource * lastCached ) throw()
	{
	
		switch( _policy )
		{
		
			case NeverDrop:
				// No 'applyNeverDrop()', since nothing to do.
				break ;
				
			case DropLessRequestedFirst:
				applyDropLessRequestedFirst( lastCached ) ;
				break ;
				
			default:
				Ceylan::emergencyShutdown(
					"SmartResourceManager<Key>::applyPolicy	: "
					"unknown cache policy requested." ) ;
				break ;	
		}

	}


	template <class Key>	
	void SmartResourceManager<Key>::applyDropLessRequestedFirst( 
		const SmartResource * lastCached ) throw()
	{
		
		
		Uint32 minRequestCount = 0 ;
		Uint32 minRequestCurrent ;
		

		typename std::map<Key, CacheEntry *>::iterator minIterator ;
		bool minAlreadyFound ;
		
		
		// While too big, shrink :
		while ( getFootprint() > _quota )
		{
		
		
			/*
			 * One entry has to be dropped during this iteration. 
			 * It will be the first encountered one among the ones with the
			 * lowest request count, the lastCached one (if any) excluded, to
			 * give it a chance. 
			 * There is always at least another resource to drop before
			 * lastCached, since lastCached is smaller than the cache quota
			 * (otherwise it would have been trivially rejected in the 'put'
			 * method and no policy would have been applied) and 
			 * nevertheless the footprint is higher than the quota.	
			 * The 'worst' situation is thus a cache with only the lastCached
			 * entry left.
			 *
			 */
			 
			minAlreadyFound = false ;
			
			for ( typename std::map<Key, CacheEntry *>::iterator it =
				 _entries.begin(); it != _entries.end(); it++ )
			{
			
				/*
				 * Ignores new entry lastCached 
				 * (if any : null lastCached -> test always true) :
				 *
				 */
				if ( (*it).second->_resource != lastCached )
				{
				
					minRequestCurrent = (*it).second->_requestCount ;

					if ( minRequestCurrent == 0 )
					{
						// Trivial drop :
						minIterator = it ;
						break ;
					}
					
					// Select the entry with the fewer accesses :
					if ( ! minAlreadyFound )
					{
						// Take first encountered as comparison entry :
						minRequestCount = minRequestCurrent ;
						minIterator = it ;
						minAlreadyFound = true ;
					}
					else
					{
						// A better candidate ?
						if ( minRequestCurrent < minRequestCount )
						{
							minRequestCount = minRequestCurrent ;
							minIterator = it ;
						}
					}	
				}	
			}
		
			/*
			 * We hereby have the first found entry among the entries of lowest
			 * request count, let's drop it :
			 *
			 */
			dropEntryDueToPolicy( minIterator ) ;
			
		
		}
		
	}
	
	
	template <class Key>	
	void SmartResourceManager<Key>::addEntry( const Key & key, 
			const SmartResource & newResource, Ceylan::System::Size size ) 
		throw( ResourceManagerException )	
	{
	
		// Erases the content of any previous entry associated with this key :
		typename std::map<Key, CacheEntry *>::iterator it =
			_entries.find( key ) ;
			
		if ( it != _entries.end() )
		{
		
			if ( _policy == NeverDrop )
				throw ResourceManagerException(
					"SmartResourceManager<Key>::addEntry : "
					"specified key was already associated to a resource, "
					"and the current policy, 'NeverDrop', prevents from "
					"removing already associated resource." ) ; 
		
			/*
			 * Inconsistency risk since the already computed size might have
			 * changed since then : better use the same estimation even if the
			 * actual size changed, since every call to getSizeInMemory should
			 * lead to an update of _totalSize. They should thus match at 
			 * all times, even if 'updateSizes' is called in between.
			 *
			 * Delete this resource, since the manager owns it in all 'put'
			 * cases :
			 *
			 */
			dropEntry( it ) ; 
			
		}
		
		// Records the new entry :
		CacheEntry * newEntry = new CacheEntry() ;
		
		// Here we must be dealing with the clone only :
		newEntry->_resource = & newResource ;
		newEntry->_requestCount = 0 ;
		
		if ( size == 0 )
			size = newResource.getSizeInMemory() ;
			
		newEntry->_size = size ;
		
		_entries[ key ] = newEntry ;
		
		_totalSize += size ;
		
	
	}
		
	
	template <class Key>	
	void SmartResourceManager<Key>::dropEntryDueToPolicy( 
		typename std::map<Key, CacheEntry *>::iterator pos ) throw()
	{	

		dropEntry( pos ) ;
		_droppedByPolicy++ ;
		
	}		


	template <class Key>	
	void SmartResourceManager<Key>::dropEntry( 
		typename std::map<Key, CacheEntry *>::iterator pos ) 
		throw()
	{	
	
		// Update cache size :
		_totalSize -= (*pos).second->_size ;
		
		// Delete embedded resource :
		delete (*pos).second->_resource ;
		
		// Remove and delete the cache entry :
		delete (*pos).second ;
		_entries.erase( pos ) ;
		
	}		



} // namespace Ceylan



#endif // CEYLAN_SMART_RESOURCE_MANAGER_H_
