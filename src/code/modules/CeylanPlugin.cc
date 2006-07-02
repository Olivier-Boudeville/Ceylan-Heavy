#include "CeylanPlugin.h"


#include "CeylanFile.h"         // for Exists
#include "CeylanDirectory.h"    // for IsAbsolutePath
#include "CeylanOperators.h"    // for toString
#include "CeylanLogPlug.h"      // for LogPlug



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


/*
 * libltdl is preferred to old-school ld_*, which are not portable.
 *
 * Note that libltdl is not deemed thread-safe, see : lt_dlmutex_lock, etc.
 *
 */


extern "C"
{

#ifdef CEYLAN_USES_LTDL_H
#include "ltdl.h"               // for lt_dlinit, etc.
#endif // CEYLAN_USES_LTDL_H

}


#include <list>
using std::list ;

using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;


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

const std::string Plugin::SymbolMarker= CEYLAN_SYMBOL_MARKER ;

Ceylan::Uint16 Plugin::PluginSystemInitialized = 0 ;



Plugin::Plugin( const string & filename, bool autoPrefix ) 
		throw( PluginException ) :
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
		
		
		/// FIXME : is overriding of any use ?
		
		//lt_dlmalloc = (lt_ptr_t (*) (size_t)) ::xmalloc ;
      	//lt_dlfree   = (void (*) (lt_ptr_t)) ::free ;

	
		/*
		 * Make sure preloaded modules are initialized.
		 * The call has been commented out since it was making fail test
		 * programs generated by the configure script in ceylan.m4 with :
		 * undefined reference to 'lt_preloaded_symbols'.
		 * No special work-around tried for the moment.
		 *
		 */
		//LTDL_SET_PRELOADED_SYMBOLS() ;
		
		if ( ::lt_dlinit() != 0 )
			throw PluginException( "Plugin constructor : initialization of "
				" ltdl failed : " + string( ::lt_dlerror() ) ) ; 
				
				
		const list<string> & paths = PluginLocator.getPaths() ;
		
		for ( list<string>::const_iterator it = paths.begin() ;
			it != paths.end(); it++ )
		{
			if ( ::lt_dladdsearchdir( (*it).c_str() ) != 0 )
				throw PluginException( "Plugin constructor : "
					"error while specifying plugin search directory '"
					+ (*it) + "' : " + string( ::lt_dlerror() ) ) ;
		
		}	
	
	}
	
	// One more user :
	PluginSystemInitialized++ ;
	
		 
	/*
	 * If the executable using this module was linked with the 
	 * -export-dynamic flag, then the global symbols in the executable 
	 * will also be used to resolve references in the module.
	 *
	 */
	_pluginHandle->_handle = ::lt_dlopenext( filename.c_str() ) ;
	
	if ( _pluginHandle->_handle == 0 ) 
		throw PluginException( "Plugin constructor : "
			"error while loading library '" + filename + "' : "
			+ string( ::lt_dlerror() ) + "." ) ; 
	
	retrieveMetadata() ;
		
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin constructor : "
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
			LogPlug::error( "Plugin destructor : lt_dlexit failed : "
				+ string( ::lt_dlerror() ) ) ; 
	
	}
	
#endif // CEYLAN_USES_PLUGINS	
	
	
}



string Plugin::getEmbeddedName() throw( PluginException )
{

#if CEYLAN_USES_PLUGINS

		
	if ( _pluginHandle->_handle != 0 )
	{
		
		const lt_dlinfo * info = ::lt_dlgetinfo( _pluginHandle->_handle ) ;
		
		if ( info == 0 )
			throw PluginException( "Plugin::getEmbeddedName failed : "
				+ string( ::lt_dlerror() ) ) ;
		
		return info->name ;
					
	}
	else
	{
		throw ModuleException( "Plugin::getEmbeddedName failed : "
			"no handle available." ) ;
	}
	


#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::getEmbeddedName : "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}


string Plugin::getFilename() const throw( ModuleException )
{

#if CEYLAN_USES_PLUGINS

	if ( _pluginHandle->_handle != 0 )
	{
		
		const lt_dlinfo * info = ::lt_dlgetinfo( _pluginHandle->_handle ) ;
		
		if ( info == 0 )
			throw PluginException( "Plugin::getFilename failed : "
				+ string( ::lt_dlerror() ) ) ;
		
		return info->filename ;
				
	}
	else
		throw ModuleException( "Plugin::getFilename failed : "
			"no handle available." ) ;

#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::getFilename : "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	
}


Plugin::ReferenceCount Plugin::getReferenceCount() const 
	throw( ModuleException )
{

#if CEYLAN_USES_PLUGINS

	if ( _pluginHandle->_handle != 0 )
	{
		
		const lt_dlinfo * info = ::lt_dlgetinfo( _pluginHandle->_handle ) ;
		
		if ( info == 0 )
			throw PluginException( "Plugin::getReferenceCount failed : "
				+ string( ::lt_dlerror() ) ) ;
		
		return info->ref_count ;
				
	}
	else
		throw ModuleException( "Plugin::getReferenceCount failed : "
			"no handle available." ) ;

#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::getReferenceCount : "
		"plugin feature not available" ) ;
		
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

	if ( ! isOpen() )
		throw PluginException( "Plugin::getSymbol : "
			"loading symbol '" + symbol 
			+ "' requested whereas plugin not opened." ) ; 
	
	// Note that the symbol is not especially prefixed : libltdl does it.
	
    lt_ptr f = ::lt_dlsym( _pluginHandle->_handle, symbol.c_str() ) ;

	if ( f == 0 )
		throw PluginException( "Plugin::getSymbol : "
			"symbol '" + symbol + "' not found in '"
			+ _filename + "'." ) ; 
	
	return f ;
	
	
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
			if ( ::lt_dlclose( _pluginHandle->_handle ) == 0 )
			{
				_pluginHandle->_handle = 0 ;
			}	
			else
			{
				throw PluginException( "Plugin::close : " 
					+ string( ::lt_dlerror() ) ) ;
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


void Plugin::makeResident() throw( PluginException )
{

#if CEYLAN_USES_PLUGINS
	
	if ( ::lt_dlmakeresident(_pluginHandle->_handle ) == -1 )
		throw PluginException( "Plugin::makeResident failed : "
			+ string( ::lt_dlerror() ) ) ; 
		
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::makeResident : "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS
	  
}


bool Plugin::isResident() const throw( PluginException )
{

#if CEYLAN_USES_PLUGINS

	int res = ::lt_dlisresident(_pluginHandle->_handle ) ;
	
	if ( res == -1 )
		throw PluginException( "Plugin::isResident failed : "
			+ string( ::lt_dlerror() ) ) ; 
	
	return ( res == 1 ) ;
	  
#else // CEYLAN_USES_PLUGINS

	throw PluginException( "Plugin::isResident : "
		"plugin feature not available" ) ;
		
#endif // CEYLAN_USES_PLUGINS

}


const string Plugin::toString( VerbosityLevels level ) const throw()
{

    string res ;

    res = "Plugin named '" + getName() + "', corresponding to the library '" 
		+ getFilename() + "' (user specified : '" + _filename + "'), which " ;
	
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


void Plugin::retrieveMetadata() throw( PluginException )		
{

	setName( getEmbeddedName() ) ;
	
	setDescription( * static_cast<const std::string *>( 
		getSymbol( "Description" ) ) ) ;
	
	setHomePage( * static_cast<const std::string *>( 
		getSymbol( "Url" ) ) ) ;
		
	setAuthor( * static_cast<const std::string *>( 
		getSymbol( "Author" ) ) ) ;
		
	setAuthorMail( * static_cast<const std::string *>( 
		getSymbol( "AuthorMail" ) ) ) ;

	try
	{	
		setVersion( Version(  * static_cast<const std::string *>( 
			getSymbol( "Version" ) ) ) ) ;
	}
	catch( const Ceylan::VersionException & e )
	{
		throw PluginException( "Plugin::retrieveMetadata : "
			"getting version failed : " + e.toString() ) ;
	}

	setLicence(  * static_cast<const std::string *>( 
		getSymbol( "Licence" ) ) ) ;
		
		
}
