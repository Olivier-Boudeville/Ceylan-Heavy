#ifndef CEYLAN_FEATURES_H_
#define CEYLAN_FEATURES_H_


#include "CeylanException.h"  // for Ceylan::Exception
#include "CeylanTypes.h"      // for Ceylan::Flags


#include <string>


namespace Ceylan
{


	/**
	 * Optional features can be requested and used, if available.
	 *
	 * Current known features are :
	 *  - regular expression support
	 *  - multithreading support
	 *  - networking support
	 *  - file descriptor support
	 *  - symbolic links support
	 *  - advanced file attributes (for opening and permissions) support
	 *  - read/write file lock support
	 *  - advanced process management
	 *	- plugin support
	 *
	 */
	namespace Features
	{
	
	
	
		/**
		 * To be raised whenever a feature is not available on the target
		 * platform for the currently linked Ceylan library.
		 *
		 */
		class FeatureNotAvailableException : public Ceylan::Exception
		{
	
			public:
		
				explicit FeatureNotAvailableException( 
					const std::string & message ) throw() ;
				virtual ~FeatureNotAvailableException() throw() ;
	
		} ;


		/**
		 * This is the list of Ceylan features that might be available.
		 * They are often available on a per-platform basis, even though each
		 * available feature can be disabled on request, at the library build
		 * time.
		 *
		 * @example The recommended usage for user code is the following :
		 * Ceylan::CheckForSupportedFeatures( MyFeature 
		 *	  | MyOtherFeature | AnotherWantedFeature ) ;
		 *
		 * If at least one feature is not available, a self-documented
		 * FeatureNotAvailableException will be thrown accordingly.
		 *
		 */
	
	
		/**
		 * Designates the regular expression support feature.
		 *
		 * If this feature is enabled, then the Ceylan library will make use
		 * of the regex-provided primitives whenever needed.
		 *
		 */ 
		extern const Ceylan::Flags RegularExpressions ;


		/**
		 * Designates the multithreading support feature.
		 *
		 * If this feature is enabled, then the Ceylan library will provide
		 * a basic thread support. 
		 *
		 * @note This do not imply that the Ceylan library itself will be
		 * multithread-proof, reentrant and/or protected against concurrent
		 * accesses.
		 *
		 */ 
		extern const Ceylan::Flags Multithreading ;


		/**
		 * Designates the network support feature.
		 *
		 * If this feature is enabled, then the Ceylan library will provide
		 * a basic networking support, including abstractions of low-level
		 * communication objects (namey, sockets) and higher-level base
		 * classes, such as generic clients, servers, and basic custom-made
		 * RPC-style primitives, a.k.a. Ceylan middleware.
		 *
		 */
		extern const Ceylan::Flags Networking ;


		/**
		 * Designates the file descriptor feature.
		 *
		 * If this feature is enabled, then the Ceylan library will be able 
		 * to provide file descriptor management, including select operations,
		 * direct copies, InputStream abstraction for files, sockets, pipes,
		 * etc.
		 *
		 */
		extern const Ceylan::Flags FileDescriptors ;


		/**
		 * Designates the symbolic link feature.
		 *
		 * If this feature is enabled, then the Ceylan library will be able 
		 * to provide symbolic management management, including creation,
		 * dereferencement, etc.
		 *
		 */
		extern const Ceylan::Flags SymbolicLinks ;


		/**
		 * Designates the advanced file attribute feature, which includes
		 * the management, for filesystem elements, of additional :
		 *   - opening flags (non-blocking/synchronous) 
		 *   - file attributes (beyond 'owner' permissions : 'group'
		 * and 'other').
		 *
		 */
		extern const Ceylan::Flags AdvancedFileAttributes ;


		/**
		 * Designates the file lock feature, for reading and/or writing.
		 *
		 */
		extern const Ceylan::Flags FileLocks ;


		/**
		 * Designates the advanced process management feature, for 
		 * fine-grained featureful process control.
		 *
		 */
		extern const Ceylan::Flags AdvancedProcessManagement ;


		/**
		 * Designates the plugin feature, for dynamic loading a shared objects.
		 *
		 */
		extern const Ceylan::Flags Plugins ;



	
		/**
		 * Checks that the version of the Ceylan library currently linked
		 * supports the specified feature(s).
		 *
		 * @param featuresFlag an OR'd flag of the requested features, for 
		 * example : 'MyFeature | MyOtherFeature'
		 *
		 * @throw FeatureNotAvailableException if at least one of the specified
		 * features is not supported.
		 *
		 */
		void checkForSupportedFeatures( Ceylan::Flags featuresFlag ) 
			throw( FeatureNotAvailableException ) ;
	
	
		/// Tells whether regular expressions are supported.
		bool areRegularExpressionsSupported() throw() ;
	
		/// Tells whether multithreading is supported.
		bool isMultithreadingSupported() throw() ;
	
		/// Tells whether network operations are supported.
		bool isNetworkingSupported() throw() ;
	
		/// Tells whether file descriptors are supported.
		bool areFileDescriptorsSupported() throw() ;
	
		/// Tells whether symbolic links are supported.
		bool areSymbolicLinksSupported() throw() ;
	
		/// Tells whether advanced file attributes are supported.
		bool areAdvancedFileAttributesSupported() throw() ;
	
		/// Tells whether file locks are supported.
		bool areFileLocksSupported() throw() ;
		
		/// Tells whether advanced process management is supported.
		bool isAdvancedProcessManagementSupported() throw() ;
	
		/// Tells whether plugins are supported.
		bool arePluginsSupported() throw() ;
	
	
		/**
		 * Returns a textual description of the optional features available 
		 * with the Ceylan version currently linked.
		 *
		 */
		const std::string describeAvailableFeatures() throw() ;
		
		
 	}
	
}


#endif // CEYLAN_FEATURES_H_
