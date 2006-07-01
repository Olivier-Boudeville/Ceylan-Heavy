#ifndef CEYLAN_PLUGIN_H_
#define CEYLAN_PLUGIN_H_


#include "CeylanModule.h"           // for inheritance and ModuleException
#include "CeylanFileLocator.h"      // for plugin locator

#include <string>



namespace Ceylan
{



	/// Raised whenever a plugin-related operation failed.
	class PluginException : public Ceylan::ModuleException
	{
	
		public:
		
			PluginException( const std::string & message ) throw() ;
			virtual ~PluginException() throw() ;
	
	} ;



	/**
	 * Plugin encapsulation for Dynamic Shared Object (DSO) management.
	 *
	 * @note The plugin feature must be available to successfully create and
	 * use a Plugin object.
	 *
	 * @see Features::arePluginsSupported
	 *
	 */
	class Plugin : public Ceylan::Module
	{


		/**
		 * Opaque handle for forward-declared but undefined struct
		 * pointer to system plugin handle, used to avoid including 
		 * system-specific headers such as ltdl.
		 *
		 * Otherwise the API exposed by Ceylan would depend on these
		 * headers, then on a config.h that should then be installed 
		 * but may clash with others, and so on.
		 *
		 */
		struct SystemSpecificPluginHandle ;
		
		
		public:
	
		
		
			/**
			 * Constructs a reference on a plugin (DSO) by trying to load
			 * an appropriate file.
			 *
			 * Use isOpen() to see whether the plugin could be loaded
			 * succcessfully.
			 *
			 * @param filename the filename of the shared library that should
			 * be loaded. If it is not an absolute filename, the file is
			 * searched through system library search paths. For example, on
			 * GNU/Linux, it is searched first in directories specified by
			 * LD_LIBRARY_PATH, then in /etc/ld.so.cache, then in /lib,
			 * then in /usr/lib. If filename is an empty string, the binary
			 * loaded will be the main program itself.
			 *
			 * @param resolveAllNow if true requests that all undefined symbols
			 * are resolved during this call, instead of being resolved just
			 * before the actual code is executed.
			 * 
			 * @param makeAvailable if true, all of the external symbols of 
			 * the loaded library will be made available for next libraries
			 * to be loaded. 
			 *
			 * @throw PluginException if the plugin feature is not available
			 * or if the plugin could not be loaded successfully.
			 *
			 */
			explicit Plugin( const std::string & filename = "",
				bool resolveAllNow = false, bool makeAvailable = false ) 
				throw( PluginException ) ;
	
		
			/// Virtual destructor.
			virtual ~Plugin() throw() ;
			
			
			/**
			 * Tells whether the file has been loaded and opened as a
			 * shared object.
			 *
			 */
			bool isOpen() const throw() ;
	
	
			/**
			 * Returns the reference on the symbol <b>sym</b> within the
			 * opened plugin (DSO).
			 *
			 * @param symbol the symbol to look-up.
			 *
			 * @throw PluginException if the plugin feature is not available
			 * or if the plugin could not be loaded successfully.
			 *
			 */
			void * getSymbol( const std::string & symbol ) const
				throw( PluginException ) ;
		
			
			/**
			 * Closes previously opened shared object. 
			 *
			 * @throw PluginException if the plugin feature is not available
			 * or if the plugin could not be closed successfully.
			 *
			 */
			void close() throw( PluginException ) ;
		
		
			/// Returns the filename corresponding to this plugin.
			const std::string & getFileName() const ;


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
				VerbosityLevels level = high ) const throw() ;


			/**
			 * This locator keeps the plugin search paths available.
			 *
			 */
			static Ceylan::System::FileLocator PluginLocator ;



		protected:
		
		
			/**
			 * Tells whether the plugin system is currently initialized,
			 * by recording the current number of times the system was 
			 * requested minus the number of times it was released.
			 *
			 */
			static Ceylan::Uint16 PluginSystemInitialized ;
			
			

		private:
		


			/// The plugin handle returned by the loader.
			SystemSpecificPluginHandle * _pluginHandle ;
		
				
			/// The filename of the plugin.
			std::string _filename ;
	
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Plugin( const Plugin & source ) throw() ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 *
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 * 
			 */			 
			Plugin & operator = ( const Plugin & source ) throw() ;
		


	} ;


}


#endif // CEYLAN_PLUGIN_H_
