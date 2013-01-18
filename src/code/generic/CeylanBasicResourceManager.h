/* 
 * Copyright (C) 2003-2013 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#ifndef CEYLAN_BASIC_RESOURCE_MANAGER_H_
#define CEYLAN_BASIC_RESOURCE_MANAGER_H_


#include "CeylanResourceManager.h"  // for inheritance

#include "CeylanResource.h"         // for Ceylan::Resource

#include "CeylanStringUtils.h"      // for Ceylan::formatStringList
#include "CeylanSystem.h"           // for Ceylan::System::Size
#include "CeylanOperators.h"        // for string operations


#include <map>
#include <list>
#include <string>
#include <iostream>                 // for ostringstream



namespace Ceylan
{


	
	/**
	 * Manages basically a set of Resource instances: the basic Resource
	 * manager can store and afterwards retrieve resources on behalf of the
	 * caller, which will be able to associate a key, whose type is
	 * user-defined, to each resource.
	 *
	 * The basic Resource manager takes ownership of the Resources it is given,
	 * it stores them all and, on request, thanks to the key, delivers to the
	 * caller only const references to these resources. 
	 *
	 * Such managers are especially designed to facilitate the memory 
	 * management of static user resources, such as sounds, textures, maps, 
	 * geometries, etc.
	 *
	 * A Resource manager handles the life cycle of its resources: it takes
	 * their ownership, which means it will delete them when itself deleted.
	 * From the user's point of view, giving a Resource to the manager thanks
	 * to the 'takeOwnershipOf' method is an alternative to deleting the
	 * resource. By no means should the caller delete a managed Resource.
	 * Modifying them after having given them this way to the manager is not
	 * recommended: the caller should forget any pointer or reference to the
	 * Resources it sent to the manager, so that these resources remain
	 * unaltered and can be directly retrieved 'as are' by various cache users.
	 *
	 * The resources can be submitted to the manager at the time when their
	 * state must be kept, including prior to any use or after they already 
	 * have been used, at the moment when they would have been deallocated,
	 * should there be no cache.  
	 *
	 * The basic Resource manager has no quota to respect, it will store all
	 * given Resources regardless of the resulting size in memory, and will 
	 * make them available (as const resources), as long as the manager exists:
	 * a basic manager will never forget any resource while still alive.
	 *
	 * There are different use cases for such a manager, depending on what is 
	 * to be done with the cached resources, which translates into the need 
	 * for cached resources to be cloned or not.
	 *
	 * If we take the example of a font rendering system, then this type of
	 * cache could be useful when the blitting of a glyph is requested: first
	 * the specified glyph is rendered into a new surface, then it is blitted
	 * on, say, the screen. Instead of deallocating the surface after use, the
	 * blit function could pass it to an appropriate resource manager, which
	 * would take ownership of it. If later the same blitting is requested
	 * again, then the blit function could start by asking the cache for this
	 * prerendered glyph. The cache, if fed as described before, should be able
	 * to provide it as a 'const' resource, which could be used directly for
	 * the targeted blit. 
	 *
	 * This will work as long as the user ensures that none of these 'const'
	 * resources is used after the manager is deleted. 
	 *
	 * @note No CEYLAN_DLL declaration for templates.
	 *
	 */
	template <typename Key>
	class BasicResourceManager : public Ceylan::ResourceManager<Key>
	{
	
	
		public:
		
			
			/**
			 * Creates a new Resource manager, which will store, take ownership
			 * of and make available the resources that it will be given, with
			 * no concern for size limit.
			 *
			 * As no resource will be ever dropped until the manager is deleted,
			 * it is the user responsibility to take care of how much data is
			 * put in cache, so that the total size in memory does not increase
			 * too much.
			 *
			 */
			explicit BasicResourceManager() ;
  
  
 
 			/// Virtual destructor, deletes all resources still in cache.
 			virtual ~BasicResourceManager() throw() ;
	
		
		
			/**
			 * Puts specified resource in cache, associated with specified key,
			 * and takes ownership of it.
			 * 
			 * The basic Resource manager takes ownership of all the supplied
			 * resources and will perform their deleting only during its own
			 * deleting, or when the 'flush' method is called.
			 *
			 * @param key the key by which that resource could be retrieved. 
			 * If this key is already associated with a resource, an exception
			 * is raised.
			 *
			 * @param resource the resource to put in cache. It must be a
			 * dynamically allocated resource (thanks to new, no automatic
			 * variable) since it will be deallocated by the cache when deemed
			 * appropriate.
			 *
			 * @throw ResourceManagerException if the specified key was already
			 * associated with a Resource.
			 *
			 * @see isKeyAlreadyAssociated
			 *
			 */
			virtual void takeOwnershipOf( const Key & key, 
				const Resource & resource ) ;
			
			
			
			/**
			 * Tells whether the specified key is already associated with a
			 * Resource.
			 *
			 * Useful to avoid trying to overwrite a resource already 
			 * associated with a key.
			 *
			 */
			virtual bool isKeyAlreadyAssociated( const Key & key ) const ;
			
			
			
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
			 * Maybe the volatile keyword should be used for such attributes.
			 *
			 */
			 virtual const Resource * get( const Key & key ) ;
			 						
			 
			 
			/**
			 * Removes and deletes all resources currently in cache.
			 *
			 * Cache statistics are not modified.
			 *
			 */
			virtual void flush() ;
						
			
			
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
			 	Ceylan::VerbosityLevels level = Ceylan::high ) const ;

			
			
			
		protected:
		
					
/* 
 * Takes care of the awful issue of Windows DLL with templates.
 *
 * @see Ceylan's developer guide and README-build-for-windows.txt 
 * to understand it, and to be aware of the associated risks. 
 * 
 */
#pragma warning( push )
#pragma warning( disable : 4251 )

			/// Stores the association between a key and a resource.			
			std::map<Key, const Resource *> _entries ;

#pragma warning( pop ) 			


		private:
		
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined constructor
			 * is called, implicitly or not.
			 * 
			 */			 
			BasicResourceManager( const BasicResourceManager & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be 
			 * called.
			 *
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 * 
			 */			 
			BasicResourceManager & operator = ( 
				const BasicResourceManager & source ) ;
			
			
	} ;




	// Public section: implementation.


	template <typename Key>
	BasicResourceManager<Key>::BasicResourceManager() :
		ResourceManager<Key>(),
		_entries()
	{
	
	}	


				
	template <typename Key>
	BasicResourceManager<Key>::~BasicResourceManager() throw()
	{
	
		/**
		 * There does not seem to exist any way of calling 'flush' from the
		 * abstract mother class so that no child class could forget to call it.
		 *
		 */
		flush() ; 
		
	}	
			
			
		
	template <typename Key>
	void BasicResourceManager<Key>::takeOwnershipOf( const Key & key, 
		const Resource & resource ) 	
	{

		// Look-up any previous entry:
		typename std::map<Key, const Resource *>::const_iterator it =
			_entries.find( key ) ;
			
		if ( it != _entries.end() )
		{
			/*
			 * Key type not known, using streams to get as much information as
			 * possible:
			 *
			 */
			std::ostringstream os ;
			os << key ;
			throw ResourceManagerException(
				"BasicResourceManager<Key>::takeOwnershipOf: key '"
				+ os.str() + "' already associated with ressource '" 
				+ resource.toString() + "'." ) ;
		}	
			
		_entries[ key ] = & resource ;
				
	}



	template <typename Key>
	bool BasicResourceManager<Key>::isKeyAlreadyAssociated( const Key & key )
		const
	{

		// Look-up any previous entry:
		return ( _entries.find( key ) != _entries.end() ) ;

	}
	


	template <typename Key>
	const Resource * BasicResourceManager<Key>::get( const Key & key )
	{
	
		typename std::map<Key, const Resource *>::const_iterator it =
			_entries.find( key ) ;
			
		if ( it != _entries.end() )
		{
			this->_cacheHits++ ;
			return (*it).second ;
		}
		else
		{
			this->_cacheMisses++ ;
			return 0 ;
		}	
		
	}

	
	
	template <typename Key>
	void BasicResourceManager<Key>::flush()
	{
	
		for ( typename std::map<Key, const Resource *>::const_iterator it =
			_entries.begin(); it != _entries.end(); it++ )
		{
			delete (*it).second ;
		}
		
		_entries.clear() ;
		
	}
	
	

	template <typename Key>
	const std::string BasicResourceManager<Key>::toString( 
		VerbosityLevels level ) const
	{
		
		std::string res = "Basic Resource manager currently managing " ;
				
		System::Size resourceCount = _entries.size() ;
		
		if ( resourceCount == 0 )
			res += "no resource" ;
		else	
			res += Ceylan::toString( 
				static_cast<Ceylan::Uint32>( resourceCount ) ) 
				+ " resource(s)" ;
				
		if ( level == Ceylan::low )	
			return res ;

				 	
		Ceylan::Uint32 total = this->_cacheHits + this->_cacheMisses ;
		
		if ( total == 0 )
			res += ". No resource request processed for the moment" ;	
		else
			res += ". The average cache success is " 
				+ Ceylan::toNumericalString( static_cast<Ceylan::Uint8>( 
					( 100.0f * this->_cacheHits ) / total ) )
				+ "% (" + Ceylan::toString( this->_cacheHits ) 
				+ " cache hit(s) for " 
				+ Ceylan::toString( this->_cacheMisses ) + " cache miss(es))" ;


		if ( level == Ceylan::medium )	
			return res ;
		
		if ( resourceCount == 0 )
			return res ;
			
		res += ". Displaying current cached entrie(s): " ;
		
		Ceylan::Uint32 count = 0 ;
		std::list<std::string> entries ;
			
		for ( typename std::map<Key, const Resource *>::const_iterator it =
			_entries.begin(); it != _entries.end(); it++ )
		{
		
			count++ ;
			entries.push_back( "[Entry #" + Ceylan::toString( count ) 
				+ "] resource description = '"	
				+ (*it).second->toString( Ceylan::low )	+ "'" ) ; 
				
		}	
		
		return res + Ceylan::formatStringList( entries ) ;
		
	}
	
	
}



#endif // CEYLAN_BASIC_RESOURCE_MANAGER_H_

