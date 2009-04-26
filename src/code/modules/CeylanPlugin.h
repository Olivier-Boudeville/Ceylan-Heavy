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


#ifndef CEYLAN_PLUGIN_H_
#define CEYLAN_PLUGIN_H_


#include "CeylanModule.h"           // for inheritance and ModuleException
#include "CeylanFileLocator.h"      // for plugin locator

#include <string>


/// The marker used for symbols exported by the plugin.
#define CEYLAN_SYMBOL_MARKER "_LTX_"



/*
 * Is not accepted by the preprocessor:
 
#define CEYLAN_PLUGIN_DECLARE_NAME(name) \
#define plugin_name name

 */	


	
/**
 * Use this macro to declare a symbol which is to be exported by the plugin,
 * so that it can be dynamically loaded.
 *
 * @example CEYLAN_PLUGIN_EXPORTED_SYMBOL(my_function) 
 *
 * int my_function() {...}
 *
 * Such declarations can be gathered into a header file to make an interface
 * for a plugin type, with different plugins implementing it.
 *
 * @note The plugin name must be defined previously.
 *
 * @see ceylan-test-plugin.cc
 *
 * Is not accepted by the preprocessor:
 *
#define CEYLAN_PLUGIN_EXPORTED_SYMBOL(symbol)\
	#define symbol plugin_name##CEYLAN_SYMBOL_MARKER##symbol
 */


	
/**
 * Use this macro to declare a symbol which is to remain internal to the 
 * plugin, i.e. that is not to be exported.
 *
 * @example CEYLAN_PLUGIN_INTERNAL_SYMBOL(my_function) 
 *
 * int my_function() {...}
 *
 * @note The plugin name must be defined previously.
 *
 * @see ceylan-test-plugin.cc
 *
 * Is not accepted by the preprocessor:
 *
#define CEYLAN_PLUGIN_INTERNAL_SYMBOL(symbol)\
	#define symbol _##plugin_name##_symbol
 */




namespace Ceylan
{



	/// Raised whenever a plugin-related operation failed.
	class CEYLAN_DLL PluginException: public Ceylan::ModuleException
	{
	
		public:
		
			PluginException( const std::string & message ) ;
			
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
	class CEYLAN_DLL Plugin: public Ceylan::Module
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
		
		
		
		/**
		 * Pointer to function, used to distinguish pointer-to-function loaded
		 * symbols from pointer-to-object ones.
		 *
		 * In ISO C++, those two kinds of pointers are especially risky to 
		 * cast to each other.
		 *
		 * No 'extern "C"' declaration seems needed.
		 *
		 */
		typedef void * (*BasicFunctionPointer) (void) ;
	
	
		
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
			 * plugin will be searched through:
			 *   1. user-defined search path, see PluginLocator
			 *   2. libltdl's search path, which is the value of the 
			 * environment variable LTDL_LIBRARY_PATH
			 *   3. system-dependent library search path, for example on Linux
			 * it is LD_LIBRARY_PATH. 
			 * Different file name extensions will be appended to the file 
			 * name until the plugin is found: the libtool archive extension
			 * `.la', the extension used for native dynamic libraries on the
			 * host platform, e.g., `.so', `.sl', etc. This lookup strategy 
			 * was designed to allow programs that do not have knowledge 
			 * about native dynamic libraries naming conventions to be able 
			 * to open dynamically such libraries as well as libtool modules
			 * transparently. 
			 * If filename is an empty string, the 
			 * binary loaded will be the main program itself.
			 *
			 * @param autoPrefix if true, then if a symbol 'foo' of a plugin
			 * named 'my-Plugin' is requested, then the actually loaded symbol
			 * will be 'my_Plugin_LTX_foo'. It allows all plugins to implement
			 * the same API while still being able to be able to be loaded
			 * simultaneously. Otherwise their name would clash and prevent
			 * from linking.
			 *
			 * @throw PluginException if the plugin feature is not available
			 * or if the plugin could not be loaded successfully.
			 *
			 */
			explicit Plugin( const std::string & filename = "",
				bool autoPrefix = true ) ;
	
		
			/// Virtual destructor.
			virtual ~Plugin() throw() ;
			
			
			
            /**
			 * Returns the plugin-embedded name, as read in the library.
			 *
			 * @throw ModuleException if the operation failed.
			 *
			 */
            virtual std::string getEmbeddedName() ;



            /**
			 * Returns the actual filename of this plugin.
			 *
			 * @throw ModuleException if the operation failed.
			 *
			 */
            virtual std::string getFilename() const ;

			
			
           /**
			 * Returns the current reference count of this plugin.
			 *
			 * This is a reference counter that describes how many times this
			 * plugin is currently loaded.
			 *
			 * @throw ModuleException if the operation failed.
			 *
			 */
            virtual ReferenceCount getReferenceCount() const ;
		
			
			
			/**
			 * Tells whether the file has been loaded and opened as a
			 * shared object.
			 *
			 */
			bool isOpen() const ;
	
	
	
			/**
			 * Returns the reference on the data symbol <b>dataName</b> 
			 * within the opened plugin (DSO).
			 *
			 * @param symbol the data (non-function) symbol to look-up.
			 *
			 * @throw PluginException if the plugin feature is not available
			 * or if the plugin could not be loaded successfully.
			 *
			 * @see getFunctionSymbol, testCeylanPlugin.cc
			 *
			 */
			void * getDataSymbol( const std::string & dataName ) const ;



			/**
			 * Returns the reference on the symbol <b>functionName</b> 
			 * within the opened plugin (DSO).
			 *
			 * @param symbol the symbol to look-up.
			 *
			 * @throw PluginException if the plugin feature is not available
			 * or if the plugin could not be loaded successfully.
			 *
			 * @see getDataSymbol, testCeylanPlugin.cc
			 *
			 * @example:
			 * typedef Ceylan::Uint32 TestFunction( 
			 *	const std::string & message ) ;
			 * TestFunction * readFunction = reinterpret_cast<TestFunction *>(
			 *   myPlugin.getFunctionSymbol( "my_test_function" ) ) ;
			 *
			 */
			BasicFunctionPointer getFunctionSymbol( 
					const std::string & functionName ) const ;
		
		
			
			/**
			 * Closes previously opened shared object. 
			 *
			 * @throw PluginException if the plugin feature is not available
			 * or if the plugin could not be closed successfully.
			 *
			 */
			void close() ;
		
		
		
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
			void makeResident() ;
			
			
			
			/**
			 * Checks whether this plugin has been marked as resident.
			 *
			 * @return true iff it has been marked as resident.
			 *
			 * @throw PluginException if the operation failed.
			 *
			 */
			bool isResident() const ;
			
			
			
			/**
			 * Returns the filename corresponding to this plugin, from the
			 * loader point of view.
			 *
			 * This name (ex: ceylan-test-plugin.so) can be different from the
			 * user-specified one (ex:ceylan-test-plugin).
			 *
			 */
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
				VerbosityLevels level = high ) const ;



			/**
			 * This locator keeps the plugin search paths available.
			 *
			 */
			static Ceylan::System::FileLocator PluginLocator ;



			/**
			 * The characters that will be added automatically between the
			 * plugin name and the loaded symbols if auto-prefix is on.
			 * 
			 * For example, retrieving the symbol 'my_test_constant' in
			 * plugin named 'ceylan-test-plugin' will cause the look-up of
			 * symbol 'ceylan_test_plugin_LTX_my_test_constant'.
			 *
			 */
			static const std::string SymbolMarker  ; 
			
			
			
			 
		protected:
					

			/**
			 * Reads from plugin the usual module metadata, such as 
			 * plugin description, author, version, licence, etc.
			 *
			 * @throw PluginException if the operation failed.
			 *
			 */
			virtual void retrieveMetadata() ;
			
			
			
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
		
				
			/// The user-specified filename of the plugin.
			std::string _filename ;
	
		
			/**
			 * Tells whether the plugin loader expects symbols to be
			 * prefixed according to CEYLAN_PLUGIN_* macros.
			 *
			 */
			bool _autoPrefix ;
		
		
			/**
			 * Copy constructor made private to ensure that it will never be 
			 * called.
			 *
			 * The compiler should complain whenever this undefined 
			 * constructor is called, implicitly or not.
			 * 
			 */			 
			Plugin( const Plugin & source ) ;
			
			
			/**
			 * Assignment operator made private to ensure that it will never be
			 * called.
			 *
			 * The compiler should complain whenever this undefined operator is
			 * called, implicitly or not.
			 * 
			 */			 
			Plugin & operator = ( const Plugin & source ) ;
		


	} ;


}



#endif // CEYLAN_PLUGIN_H_

