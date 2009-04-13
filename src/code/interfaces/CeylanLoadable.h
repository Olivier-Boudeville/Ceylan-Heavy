/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
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


#ifndef CEYLAN_LOADABLE_H_
#define CEYLAN_LOADABLE_H_


#include "CeylanException.h"   // for inheritance
#include "CeylanFile.h"        // for File
#include "CeylanLogPlug.h"     // for LogPlug


#include <string>



namespace Ceylan
{



	/// Exception to be raised whenever a loadable operation fails.
	class CEYLAN_DLL LoadableException: public Ceylan::Exception
	{
	
		public:
		
			explicit LoadableException( const std::string & message ) throw() ;
			
			virtual ~LoadableException() throw() ;
			
	
	} ;
	
	
	
	
	/*
	 * The overall goal is to rely on a Loadable mother class, that can be
	 * used simply, while, thanks to the Loadable<Content> template, to be
	 * able to retrieve the content directly, with its correct type.
	 *
	 * User is expected to subclass the template, so that she can rely on a 
	 * useful abstraction:
	 *
	 * @example class MyOwnContent: public Loadable<MyContent> {...} ;
	 * MyOwnContent anExample( aPath ) ;
	 * anExample.getContent() etc.
	 * std::list<Loadable> myList ;
	 * myList.push_back( anExample ) ;
	 *
	 * @see testCeylanLoadable.cc
	 *
	 */
	 
	 
	
    /**
     * Interface that every object which can be loaded dynamically from file
	 * should implement. 
     *
	 * @see the associated child template, Loadable<Content>.
	 *
	 * @see testCeylanLoadable.cc
	 *
     */
    class CEYLAN_DLL Loadable
    {

        public:
		
		

			/**
			 * Creates a new loadable instance, whose content file is designated
			 * by the specified filename, but does not load anything.
			 *
			 * @param contentFilePath the path to the file storing the content
			 * to be loaded.
			 *
			 * @throw LoadableException if the operation failed.
			 *
			 */
			explicit Loadable( const std::string & contentFilePath ) 
				throw( LoadableException ) ;
			
			
			
			/// Virtual destructor.
			virtual ~Loadable() throw() ;
			
			
            /**
			 * Loads the content of this instance from file.
			 *
			 * @return true iff the content had to be actually loaded (otherwise
			 * it was already loaded and nothing was done).
			 *
			 * @throw LoadableException whenever the loading fails.
			 *
			 */
            virtual bool load() throw( LoadableException ) = 0 ;
		
		
            /**
			 * Unloads the content of this instance.
			 *
			 * @return true iff the content had to be actually unloaded
			 * (otherwise it was not already available and nothing was done).
			 *
			 * @throw LoadableException whenever the unloading fails.
			 *
			 */
            virtual bool unload() throw( LoadableException ) = 0 ;
		

			/**
			 * Returns the path to the associated content file.
			 *
			 */
			virtual const std::string & getContentPath() const throw() ;



		protected:
		
		
			/// The path to the file used for content loading.
			std::string _contentPath ;					
		
		
			
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Loadable( const Loadable & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			Loadable & operator = ( const Loadable & source ) throw() ;
			

    } ;




	/**
	 * Loadable template, useful to have a way of retrieving the loaded content
	 * directly according to its specific type.
	 *
	 * @note No CEYLAN_DLL declaration for templates.
	 *
	 * A class inheriting from this instanciated template just has to implement
	 * the load and unload method accordingly.
	 *
	 * @see testCeylanLoadable.cc for an example of such implementation.
	 *
	 */
	template <typename Content>
	class LoadableWithContent: public Loadable
	{
	
	
		public:
		
		
			/**
			 * Creates a new loadable instance, whose content file is designated
			 * by the specified filename, but does not load anything.
			 *
			 */
			explicit LoadableWithContent( const std::string & contentFilePath ) 
				throw( LoadableException ) ;
				
				
			/**
			 * Virtual destructor.
			 *
			 * @note Does not delete the content as this operation is usually
			 * content-specific.
			 *
			 */
			virtual ~LoadableWithContent() throw() ;
			
			
			
            /**
			 * Loads the content of this instance from file.
			 *
			 * @return true iff the content had to be actually loaded (otherwise
			 * it was already loaded and nothing was done).
			 *
			 * @throw LoadableException whenever the loading fails.
			 *
			 * @note This method is still pure virtual here, as loading the
			 * file in memory would not be enough: a Content object is still 
			 * to be created, and we cannot expect that each Content type is 
			 * a class, and has a constructor taking exactly, for example, a
			 * Ceylan::Byte pointer.
			 *
			 */
            virtual bool load() throw( LoadableException ) = 0 ;
		
		
            /**
			 * Unloads the content of this instance.
			 *
			 * @return true iff the content had to be actually unloaded
			 * (otherwise it was not already available and nothing was done).
			 *
			 * @throw LoadableException whenever the unloading fails.
			 *
			 * @note This method is still pure virtual here, as unloading the
			 * content can be specific to the content.
			 *
			 */
            virtual bool unload() throw( LoadableException ) = 0 ;


            /**
			 * Reloads the content of this instance.
			 *
			 * @param forceLoad if true, then any previously loaded content
			 * will be deleted before an unconditional loading is performed.
			 * If false, then if there were no content already loaded, no 
			 * loading will be performed (content availability will be kept
			 * as was), but if the content was loaded, it will be unloaded and
			 * reloaded.
			 *
			 * @return true iff a reload had to take place.
			 *
			 * @throw LoadableException whenever the reloading fails.
			 *
			 */
            virtual bool reload( bool forceLoad = false ) 
				throw( LoadableException ) ;


			
			/**
			 * Returns true iff the content is already loaded.
			 *
			 */
			virtual bool hasContent() const throw() ;
			
			
			/**
			 * Returns the already loaded content.
			 *
			 * @throw LoadableException if the operation failed, including if
			 * there is no available content to be returned (it will not be
			 * loaded if lacking).
			 *
			 * @note Ownership of the content is kept by this instance.
			 *
			 */
			virtual Content & getExistingContent() throw( LoadableException ) ;
			
			
			/**
			 * Returns a constant reference to the already loaded content.
			 *
			 * @throw LoadableException if the operation failed, including if
			 * there is no available content to be returned (it will not be
			 * loaded if lacking).
			 *
			 * @note Ownership of the content is kept by this instance.
			 *
			 */
			virtual const Content & getExistingContentAsConst() const 
				throw( LoadableException ) ;
			
			
			/**
			 * Returns the loaded content, either directly (if already
			 * available), otherwise after having loaded it.
			 *
			 * @throw LoadableException if the operation failed.
			 *
			 * @note Ownership of the content is kept by this instance.
			 *
			 */
			virtual Content & getContent() throw( LoadableException ) ;
			
			
			/**
			 * Returns a constant reference to the loaded content, either
			 * directly (if already available), otherwise after having loaded
			 * it.
			 *
			 * @throw LoadableException if the operation failed.
			 *
			 * @note Ownership of the content is kept by this instance.
			 *
			 * @note The method cannot be const as it may have to load the 
			 * content.
			 *
			 * @see getExistingContentAsConst
			 *
			 */
			virtual const Content & getContentAsConst() 
				throw( LoadableException ) ;
			
			

		protected:
		
		
			/// The loaded content, if any.
			Content * _content ;
						
			
				
		private:
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			LoadableWithContent( const LoadableWithContent & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 * The compiler should complain whenever this undefined operator
			 * is called, implicitly or not.
			 * 
			 */			 
			LoadableWithContent & operator = ( 
				const LoadableWithContent & source ) throw() ;

    } ;
			


	// Implementation of the LoadableWithContent template.
	
	
	template <typename Content>
	LoadableWithContent<Content>::LoadableWithContent( 
		const std::string & contentFilePath ) 
			throw( LoadableException ):
		Loadable( contentFilePath ),
		_content( 0 )		 				
	{
	
		/*
		 * Cannot preload here as the load method is abstract.
		 * Instanciable child classes should offer to preload:
		
		if ( preLoad )
			load() ;
		
		 */	
	}
	
	
	
	template <typename Content>
	LoadableWithContent<Content>::~LoadableWithContent() throw()
	{
	
	
		/*
		 * Cannot unload here as the unload method is abstract.
		 * Instanciable child classes should deallocate any loaded content:
		
		if ( hasContent() )
			unload() ;
		
		 */	
		
		// Cannot deallocate, but can warn:
		if ( _content != 0 )
			Ceylan::Log::LogPlug::error( 
				"LoadableWithContent<Content> destructor: "
				"still existing content has been found" ) ;
					
	}
	
	
	
	template <typename Content>
	bool LoadableWithContent<Content>::reload( bool forceLoad ) 
		throw( LoadableException )	
	{
	
		if ( hasContent() )
		{
		
			// Does not depend on forceLoad:
			unload() ;
			load() ;
			
			return true ;
			
		}
		else
		{
		
			// No prior content.
			if ( forceLoad )
				load() ;
			
			return forceLoad ; 
		
		}
		
	}
	
	
	
	template <typename Content>
	bool LoadableWithContent<Content>::hasContent() const throw()
	{
	
		return ( _content != 0 ) ;
		
	}


	
	template <typename Content>
	Content & LoadableWithContent<Content>::getExistingContent() 
		throw( LoadableException ) 
	{
	
		if ( ! hasContent() )
			throw LoadableException( 
				"LoadableWithContent<Content>::getExistingContent failed: "
				"no content available" ) ;
				
		return	* _content ;
			
	}
		
		
	template <typename Content>
	const Content & LoadableWithContent<Content>::getExistingContentAsConst()
		const throw( LoadableException ) 
	{
	
		if ( ! hasContent() )
			throw LoadableException( 	
				"LoadableWithContent<Content>::getExistingContentAsConst "
				"failed: no content available" ) ;
				
		return	* _content ;
			
	}
		
		

	template <typename Content>
	Content & LoadableWithContent<Content>::getContent() 
		throw( LoadableException )	
	{

		if ( ! hasContent() )
			load() ;
		
		return	* _content ;
				
	}
	
		
	template <typename Content>
	const Content & LoadableWithContent<Content>::getContentAsConst()
		throw( LoadableException )	
	{

		if ( ! hasContent() )
			load() ;
		
		return	* _content ;
				
	}
		
		

}


#endif // CEYLAN_LOADABLE_H_

