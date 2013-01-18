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


#include "CeylanPlugin.h"


#include "CeylanFile.h"         // for Exists
#include "CeylanDirectory.h"    // for IsAbsolutePath
#include "CeylanOperators.h"    // for toString
#include "CeylanLogPlug.h"      // for LogPlug



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H



/*
 * Using libltdl is preferred to using old-school ld_*, which are not 
 * portable.
 *
 * Note that libltdl is not deemed thread-safe, see: lt_dlmutex_lock, etc.
 *
 */



extern "C"
{

#ifdef CEYLAN_USES_LTDL_H
#include "ltdl.h"               // for lt_dlinit, etc.
#endif // CEYLAN_USES_LTDL_H

#ifdef CEYLAN_USES_STRING_H
#include <string.h>             // for memcpy
#endif // CEYLAN_USES_STRING_H

}


#include <list>
using std::list ;

using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;



#ifdef CEYLAN_USES_LTDL_H

// Avoid exposing system-dependent pthread_mutex_t in the headers:
struct Plugin::SystemSpecificPluginHandle
{
	lt_dlhandle _handle ;
} ;

#endif // CEYLAN_USES_PTHREAD_H




PluginException::PluginException( const std::string & message ) :
	Ceylan::ModuleException( message )
{

}


PluginException::~PluginException() throw()
{

}




Ceylan::System::FileLocator Plugin::PluginLocator ;

const std::string Plugin::SymbolMarker= CEYLAN_SYMBOL_MARKER ;

Ceylan::Uint16 Plugin::PluginSystemInitialized = 0 ;



Plugin::Plugin( const string & filename, bool autoPrefix ) :
	Module(),	
	_pluginHandle( 0 ),
	_filename( filename ),
	_autoPrefix( autoPrefix )
{


#if CEYLAN_USES_PLUGINS
 	
	_pluginHandle = new SystemSpecificPluginHandle ;
	_pluginHandle->_handle = 0 ;
	
	
 	if ( PluginSystemInitialized == 0 )
	{
		
		
		/// FIXME: is overriding of any use ?
		
		//lt_dlmalloc = (lt_ptr_t (*) (size_t))::xmalloc ;
      	//lt_dlfree   = (void (*) (lt_ptr_t))::free ;

	
		/*
		 * Make sure preloaded modules are initialized.
		 * The call has been commented out since it was making fail test
		 * programs generated by the configure script in ceylan.m4 with:
		 * undefined reference to 'lt_preloaded_symbols'.
		 * No special work-around tried for the moment.
		 *
		 */
		//LTDL_SET_PRELOADED_SYMBOLS() ;
		
		if ( ::lt_dlinit() != 0 )
			throw PluginException( "Plugin constructor: initialization of "
				" ltdl failed: " + string( ::lt_dlerror() ) ) ; 
				
				
		const list<string> & paths = PluginLocator.getPaths() ;
		
		for ( list<string>::const_iterator it = paths.begin() ;
			it != paths.end(); it++ )
		{
			if ( ::lt_dladdsearchdir( (*it).c_str() ) != 0 )
				throw PluginException( "Plugin constructor: "
					"error while specifying plugin search directory '"
					+ (*it) + "': " + string( ::lt_dlerror() ) ) ;
		
		}	
	
	}
	
	// One more user:
	PluginSystemInitialized++ ;
	
		 
	/*
	 * If the executable using this module was linked with the 
	 * -export-dynamic flag, then the global symbols in the executable 
	 * will also be used to resolve references in the module.
	 *
	 */
	_pluginHandle->_handle =::lt_dlopenext( filename.c_str() ) ;
	
	if ( _pluginHandle->_handle == 0 ) 
		throw PluginException( "Plugin constructor: "
			"error while loading library '" + filename + "': "
			+ string( ::lt_dlerror() ) + "." ) ; 
	
	retrieveMetadata() ;
		
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin constructor: "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}



Plugin::~Plugin() throw()
{


#if CEYLAN_USES_PLUGINS
 	

	close() ;
	
	PluginSystemInitialized-- ;
	
	if ( PluginSystemInitialized == 0 )
	{
	
		if ( ::lt_dlexit() != 0 )
			LogPlug::error( "Plugin destructor: lt_dlexit failed: "
				+ string( ::lt_dlerror() ) ) ; 
	
	}
	
#endif // CEYLAN_USES_PLUGINS	
	
	
}



string Plugin::getEmbeddedName()
{

#if CEYLAN_USES_PLUGINS

		
	if ( _pluginHandle->_handle != 0 )
	{
		
		const lt_dlinfo * info =::lt_dlgetinfo( _pluginHandle->_handle ) ;
		
		if ( info == 0 )
			throw PluginException( "Plugin::getEmbeddedName failed: "
				+ string( ::lt_dlerror() ) ) ;
		
		return info->name ;
					
	}
	else
	{
		throw ModuleException( "Plugin::getEmbeddedName failed: "
			"no handle available." ) ;
	}
	

#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::getEmbeddedName: "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}



string Plugin::getFilename() const
{

#if CEYLAN_USES_PLUGINS

	if ( _pluginHandle->_handle != 0 )
	{
		
		const lt_dlinfo * info =::lt_dlgetinfo( _pluginHandle->_handle ) ;
		
		if ( info == 0 )
			throw PluginException( "Plugin::getFilename failed: "
				+ string( ::lt_dlerror() ) ) ;
		
		return info->filename ;
				
	}
	else
		throw ModuleException( "Plugin::getFilename failed: "
			"no handle available." ) ;

#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::getFilename: "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}



Plugin::ReferenceCount Plugin::getReferenceCount() const 
{

#if CEYLAN_USES_PLUGINS

	if ( _pluginHandle->_handle != 0 )
	{
		
		const lt_dlinfo * info =::lt_dlgetinfo( _pluginHandle->_handle ) ;
		
		if ( info == 0 )
			throw PluginException( "Plugin::getReferenceCount failed: "
				+ string( ::lt_dlerror() ) ) ;
		
		return info->ref_count ;
				
	}
	else
		throw ModuleException( "Plugin::getReferenceCount failed: "
			"no handle available." ) ;

#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::getReferenceCount: "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}



bool Plugin::isOpen() const 
{

#if CEYLAN_USES_PLUGINS

	return ( ( _pluginHandle != 0 ) && ( _pluginHandle->_handle != 0 ) ) ;

#else // CEYLAN_USES_PLUGINS	

	return false ;
	
#endif // CEYLAN_USES_PLUGINS	
	
}
		
		
	
void * Plugin::getDataSymbol( const string & dataName ) const 
{

#if CEYLAN_USES_PLUGINS

	if ( ! isOpen() )
		throw PluginException( "Plugin::getDataSymbol: "
			"loading symbol '" + dataName 
			+ "' requested whereas plugin not opened." ) ; 
	
	// Note that the symbol is not especially prefixed: libltdl does it.
	
    lt_ptr f =::lt_dlsym( _pluginHandle->_handle, dataName.c_str() ) ;

	if ( f == 0 )
		throw PluginException( "Plugin::getDataSymbol: "
			"symbol '" + dataName + "' not found in '"
			+ _filename + "'." ) ; 
	
	return f ;
	
	
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::getDataSymbol: "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}



Plugin::BasicFunctionPointer Plugin::getFunctionSymbol( 
	const string & functionName ) const
{

#if CEYLAN_USES_PLUGINS

	if ( ! isOpen() )
		throw PluginException( "Plugin::getFunctionSymbol: "
			"loading symbol '" + functionName 
			+ "' requested whereas plugin not opened." ) ; 
	
	// Note that the symbol is not especially prefixed: libltdl does it.
	
    lt_ptr f =::lt_dlsym( _pluginHandle->_handle, functionName.c_str() ) ;

	if ( f == 0 )
		throw PluginException( "Plugin::getFunctionSymbol: "
			"symbol '" + functionName + "' not found in '"
			+ _filename + "'." ) ; 
	
	/*
	 * Ugly hack as ISO C++ does not accept conversions from void * to
	 * function pointers.
	 *
	 * Other solutions would be: 
	 *    - conversion to integral types
	 *    - union cast
	 */
	BasicFunctionPointer returned ;
	
	// This check could be performed at compile-time:
	if ( sizeof(BasicFunctionPointer) != sizeof(lt_ptr) )
		throw PluginException( "Plugin::getFunctionSymbol: "
			"unable to convert from pointer to function (size is "
			+ Ceylan::toString( sizeof(BasicFunctionPointer) ) 
			+ ") to pointer to object lt_ptr (size is "
			+ Ceylan::toString( sizeof(lt_ptr) ) + "." ) ;
	
	::memcpy( &returned, &f, sizeof(BasicFunctionPointer) ) ;  	
			 
	return returned ;
	
	
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::getFunctionSymbol: "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}



void Plugin::close()
{

#if CEYLAN_USES_PLUGINS

	if ( _pluginHandle !=0 )
	{
		if ( _pluginHandle->_handle != 0 )
		{
			if ( ::lt_dlclose( _pluginHandle->_handle ) == 0 )
			{
				_pluginHandle->_handle = 0 ;
			}	
			else
			{
				throw PluginException( "Plugin::close: " 
					+ string( ::lt_dlerror() ) ) ;
			}		
		}
		delete _pluginHandle ;
		_pluginHandle = 0 ;
		
	}			
				
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::close: "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS

}



void Plugin::makeResident()
{

#if CEYLAN_USES_PLUGINS
	
	if ( ::lt_dlmakeresident(_pluginHandle->_handle ) == -1 )
		throw PluginException( "Plugin::makeResident failed: "
			+ string( ::lt_dlerror() ) ) ; 
		
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::makeResident: "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	  
}



bool Plugin::isResident() const
{

#if CEYLAN_USES_PLUGINS

	int res =::lt_dlisresident(_pluginHandle->_handle ) ;
	
	if ( res == -1 )
		throw PluginException( "Plugin::isResident failed: "
			+ string( ::lt_dlerror() ) ) ; 
	
	return ( res == 1 ) ;
	  
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::isResident: "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS

}



const string Plugin::toString( VerbosityLevels level ) const 
{

    string res ;

    res = "Plugin named '" + getName() + "', corresponding to the library '" 
		+ getFilename() + "' (user specified: '" + _filename + "'), which " ;
	
	if ( isOpen() )
		res += "has been already opened" ;
	else
		res += "has not been opened yet" ;


    if ( level == low )
        return res ;
	
	res += ". " ;
	
	
	if ( _autoPrefix )	
		res += "The plugin loader expects symbols to be prefixed "
			"according to module conventions" ;
	else
		res += "The plugin loader does not expect prefixed symbols" ;
			
    if ( level == medium )
        return res ;

	return res + ". " + Module::toString( level ) ;
	
}



void Plugin::retrieveMetadata()		
{

	setName( getEmbeddedName() ) ;
	
	setDescription( * static_cast<const std::string *>( 
		getDataSymbol( "Description" ) ) ) ;
	
	setHomePage( * static_cast<const std::string *>( 
		getDataSymbol( "Url" ) ) ) ;
		
	setAuthor( * static_cast<const std::string *>( 
		getDataSymbol( "Author" ) ) ) ;
		
	setAuthorMail( * static_cast<const std::string *>( 
		getDataSymbol( "AuthorMail" ) ) ) ;

	try
	{	
	
		Version versionFromPlugin( * static_cast<const std::string *>( 
			getDataSymbol( "Version" ) ) ) ;
			
		setVersion( versionFromPlugin ) ;
		
	}
	catch( const Ceylan::VersionException & e )
	{
		throw PluginException( "Plugin::retrieveMetadata: "
			"getting version failed: " + e.toString() ) ;
	}

	setLicence(  * static_cast<const std::string *>( 
		getDataSymbol( "Licence" ) ) ) ;
		
}

