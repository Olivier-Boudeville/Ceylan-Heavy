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
	 * This class allows to load arbitrary, user-specified plugins at runtime.
	 * 
	 * @note The plugin feature must be available to successfully create and
	 * use a Plugin object.
	 *
	 * @see Features::arePluginsSupported
	 *
	 * @note Our plugin system relies on ltdl.
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
		
		typedef Ceylan::Uint32 ReferenceCount ;
		
		
		public:
	
		
		
			/**
			 * Constructs a reference on a plugin (DSO) by trying to load
			 * an appropriate file.
			 *
			 * Use isOpen() to see whether the plugin could be loaded
			 * succcessfully.
			 *
			 * @param filename the filename of the shared library that should
			 * be loaded. If the filename has no directory component, the 
			 * plugin will be searched through :
			 *   1. user-defined search path, see PluginLocator
			 *   2. libltdl's search path, which is the value of the 
			 * environment variable LTDL_LIBRARY_PATH
			 *   3. system-dependent library search path, for example on Linux
			 * it is LD_LIBRARY_PATH. 
			 * Different file name extensions will be appended to the file 
			 * name until the plugin is found : the libtool archive extension
			 * `.la', the extension used for native dynamic libraries on the
			 * host platform, e.g., `.so', `.sl', etc. This lookup strategy 
			 * was designed to allow programs that do not have knowledge 
			 * about native dynamic libraries naming conventions to be able 
			 * to open dynamically such libraries as well as libtool modules
			 * transparently. 
			 * If filename is an empty string, the 
			 * binary loaded will be the main program itself.
			 *
			 * @throw PluginException if the plugin feature is not available
			 * or if the plugin could not be loaded successfully.
			 *
			 */
			explicit Plugin( const std::string & filename = "" ) 
				throw( PluginException ) ;
	
		
			/// Virtual destructor.
			virtual ~Plugin() throw() ;
			
			
			
            /**
			 * Returns the plugin name, as read in the library.
			 *
			 * @throw ModuleException if the operation failed.
			 *
			 */
            virtual std::string getName() const throw( ModuleException ) ;


            /**
			 * This operation cannot be performed on plugin instances.
			 *
			 * @throw ModuleException whevener called.
			 *
			 */
            virtual void setName( const std::string & name )
				throw( ModuleException ) ;


            /**
			 * Returns the actual filename of this plugin.
			 *
			 * @throw ModuleException if the operation failed.
			 *
			 */
            virtual std::string getFilename() const throw( ModuleException ) ;

			
           /**
			 * Returns the current reference count of this plugin.
			 *
			 * This is a reference counter that describes how many times this
			 * plugin is currently loaded.
			 *
			 * @throw ModuleException if the operation failed.
			 *
			 */
            virtual ReferenceCount getReferenceCount() const 
				throw( ModuleException ) ;
			
			
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
		
		
			/**
			 * Mark this plugin so that it cannot be closed dynamically. 
			 * This can be useful if a plugin implements some core 
			 * functionality in the application, which would cause its code 
			 * to crash if removed.
			 *
			 * When loading the running binary as a plugin, it will always be
			 * marked as resident.
			 *
			 * @throw PluginException if the operation failed.
			 *
			 */
			void makeResident() throw( PluginException ) ;
			
			
			/**
			 * Checks whether this plugin has been marked as resident.
			 *
			 * @return true iff it has been marked as resident.
			 *
			 * @throw PluginException if the operation failed.
			 *
			 */
			bool isResident() const throw( PluginException ) ;
			
			
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
