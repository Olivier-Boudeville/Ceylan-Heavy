#include "CeylanPlugin.h"


#include "CeylanFile.h"         // for Exists
#include "CeylanDirectory.h"    // for IsAbsolutePath



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


//#define CEYLAN_USES_DEPRECATED_DL_SYM


extern "C"
{

#ifdef CEYLAN_USES_DEPRECATED_DL_SYM

// _handle was void *

#ifdef CEYLAN_USES_DLFCN_H
#include <dlfcn.h>
#endif // CEYLAN_USES_DLFCN_H

#endif // CEYLAN_USES_DEPRECATED_DL_SYM


#ifdef CEYLAN_USES_LTDL_H
#include <ltdl.h>               // for lt_dlinit
#endif // CEYLAN_USES_LTDL_H


}

#include <list>
using std::list ;

using std::string ;

using namespace Ceylan ;

#ifdef CEYLAN_USES_LTDL_H

// Avoid exposing system-dependent pthread_mutex_t in the headers :
struct Plugin::SystemSpecificPluginHandle
{
	lt_dlhandle _handle ;
} ;

#endif // CEYLAN_USES_PTHREAD_H


PluginException::PluginException( const std::string & message ) throw() :
	Ceylan::ModuleException( message )
{

}


PluginException::~PluginException() throw()
{

}




Ceylan::System::FileLocator Plugin::PluginLocator ;

Ceylan::Uint16 Plugin::PluginSystemInitialized = 0 ;



Plugin::Plugin( const string & filename, bool resolveAllNow, 
		bool makeAvailable ) throw( PluginException ) :
	Module(),	
	_pluginHandle( 0 ),
	_filename( filename )
{


#if CEYLAN_USES_PLUGINS
 	
	_pluginHandle = new SystemSpecificPluginHandle ;
	_pluginHandle->_handle = 0 ;
	
	
 	if ( PluginSystemInitialized == 0 )
	{
		
		
		/// FIXME : to be kept ?
		lt_dlmalloc = (lt_ptr_t (*) (size_t)) ::xmalloc ;
      	lt_dlfree   = (void (*) (lt_ptr_t)) ::free ;
	
		// Make sure preloaded modules are initialized :
		LTDL_SET_PRELOADED_SYMBOLS() ;
		
		// FIXME : lt_dlerror interpretation ?
		if ( ::lt_dlinit() != 0 )
			throw PluginException( "Plugin constructor : initialization of "
				" ltdl failed : " + toString( ::lt_dlerror() ) ) ; 
				
				
		const list<string> & paths = PluginLocator.getPaths() ;
		
		for ( list<string>::const_iterator it = paths.begin() ;
			it != paths.end(); it++ )
		{
			if ( ::lt_dladdsearchdir( (*it).c_str() ) != 0 )
				throw PluginException( "Plugin constructor : "
					"error while specifying plugin search directory '"
					+ (*it) + "' : " + toString( ::lt_dlerror() ) ) ;
		
		}	
	
	}
	
	// One more user :
	PluginSystemInitialized++ ;
	
	
	/*
	// Search for the plugin :
	if ( System::Directory::IsAbsolutePath( filename ) )
	{
	
		try
		{
		
			if ( ! File::Exists( filename ) )
				throw PluginException( "Plugin constructor : could not find '"
				+ filename + "'." ) ) ;
		}		
		catch( const FileException & e )
		{
			
			throw PluginException( "Plugin constructor : "
				"error while performing look-up for plugin '"
				+ name + "' : " + e.toString() ) ;
		}
		
	}
	else
	{
		
		try
		{
		
			filename = PluginLocator::find( filename ) ;
		
		}
		catch( const FileLocatorException & e )
		{
		
			throw PluginException( "Plugin constructor : could not find '"
				+ filename + "' in plugin locator : " + e.toString() ) ;
		
		}		
	
	}
	
	*/
	// Here filename should contain the right path to the plugin.
	
	
 	/* 
	 * RTLD_LAZY leads to resolving symbols only when they are just to be
	 * executed, not now as with RTLD_NOW.
	 * 
	 *  RTLD_GLOBAL is to export loaded symbols for next libraries.
	 *
	if ( makeAvailable )
	{
	
		if ( resolveAllNow )
			_pluginHandle->_handle = ::dlopen( 
				filename.c_str(), RTLD_NOW | RTLD_GLOBAL ) ;
		else
			_pluginHandle->_handle = ::dlopen( 
				filename.c_str(), RTLD_LAZY | RTLD_GLOBAL  ) ;
		
	}
	else
	{

		if ( resolveAllNow )
			_pluginHandle->_handle = ::dlopen( 
				filename.c_str(), RTLD_NOW ) ;
		else
			_pluginHandle->_handle = ::dlopen( 
				filename.c_str(), RTLD_LAZY ) ;
	
	}
     */
	
	_pluginHandle->_handle = ::lt_dlopenext( filename.c_str() ) ;
	
	if ( _pluginHandle->_handle == 0 ) 
		throw PluginException( "Plugin constructor : "
			"error while loading library '" + filename + string( "' with " )
			+ ( resolveAllNow ? "instant" : "deferred" ) 
			+ string( " symbol resolution, with loaded symbols "
			+ ( makeAvailable ? "" : "not" ) 
			+ "made available to next loaded libraries : "
			+ string( ::dlerror() ) + "." ) ; 
	
	// FIXME : add the reading of plugin name, version, etc.
	
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin constructor : "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}


Plugin::~Plugin() throw()
{


#if CEYLAN_USES_PLUGINS
 	

	// FIXME : see lt_ptr_t, lt_dlforeach, lt_dlinfo, lt_dlgetinfo
	close() ;
	
	PluginSystemInitialized-- ;
	
	if ( PluginSystemInitialized == 0 )
	{
	
		::lt_dlclose( _pluginHandle->_handle ) ;
	
	}
	
#endif // CEYLAN_USES_PLUGINS	
	
	
}


bool Plugin::isOpen() const throw()
{

#if CEYLAN_USES_PLUGINS

	return ( ( _pluginHandle != 0 ) && ( _pluginHandle->_handle != 0 ) ) ;

#else // CEYLAN_USES_PLUGINS	

	return false ;
	
#endif // CEYLAN_USES_PLUGINS	
	
}
		
	
void * Plugin::getSymbol( const string & symbol ) const throw( PluginException )
{

#if CEYLAN_USES_PLUGINS

// Deprecated in favor of libltdl :
#ifdef CEYLAN_USES_DEPRECATED_DL_SYM

	void * f = ::dlsym( _handle, symbol.c_str() ) ;
	
	const char * error = ::dlerror() ;
	
	if ( error != 0 ) 
		throw PluginException( "Plugin::getSymbol : "
			"error while loading symbol '" + symbol + "' : "
			+ string( error ) + "." ) ; 
	
	if ( f == 0 ) 
		throw PluginException( "Plugin::getSymbol : "
			"symbol '" + symbol + "' not found in '"
			+ _filename + "'." ) ; 
	
	return f ;

#else // CEYLAN_USES_DEPRECATED_DL_SYM

	if ( ! isOpen() )
		throw PluginException( "Plugin::getSymbol : "
			"loading symbol '" + symbol 
			+ "' requested whereas plugin not opened." ) ; 
	
    void * f = ::lt_dlsym( _pluginHandle->_handle, symbol.c_str() ) ;

	if ( 
#endif // CEYLAN_USES_DEPRECATED_DL_SYM
	
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::getSymbol : "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}



void Plugin::close() throw( PluginException )
{

#if CEYLAN_USES_PLUGINS

	if ( _pluginHandle !=0 )
	{
		if ( _pluginHandle->_handle != 0 )
		{
			if ( ::dlclose( _pluginHandle->_handle ) == 0 )
			{
				_pluginHandle->_handle = 0 ;
			}	
			else
			{
				throw PluginException( "Plugin::close : " 
					+ string( ::dlerror() ) ) ;
			}		
		}
		delete _pluginHandle ;
		_pluginHandle = 0 ;
		
	}			
				
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::close : "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS

}


const string Plugin::toString( VerbosityLevels level ) const throw()
{

    string res ;

    res = "Plugin corresponding to the library '" 
		+ _filename + "', which " ;
	
	if ( isOpen() )
		res += "has been already opened" ;
	else
		res += "has not been opened yet" ;
	
	return res ;		

}

		
