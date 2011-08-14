/*
 * Copyright (C) 2003-2011 Olivier Boudeville
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
	 * Current known features are:
	 *
	 *  - regular expression support
	 *  - multithreading support
	 *  - networking support
	 *  - file descriptor support
	 *  - symbolic links support
	 *  - advanced file attributes (for opening and permissions) support
	 *  - read/write file lock support
	 *  - advanced process management
	 *  - plugin support
	 *
	 */
	namespace Features
	{



		/**
		 * To be raised whenever a feature is not available on the target
		 * platform for the currently linked Ceylan library.
		 *
		 */
		class CEYLAN_DLL FeatureNotAvailableException : public Ceylan::Exception
		{

			public:

				explicit FeatureNotAvailableException(
					const std::string & message ) ;

				virtual ~FeatureNotAvailableException() throw() ;

		} ;




		/**
		 * This is the list of Ceylan features that might be available.
		 *
		 * They are often available on a per-platform basis, even though each
		 * available feature can be disabled on request, at the library build
		 * time.
		 *
		 * @example The recommended usage for user code is the following:
		 * Ceylan::CheckForSupportedFeatures( MyFeature
		 *    | MyOtherFeature | AnotherWantedFeature ) ;
		 *
		 * If at least one feature is not available, a self-documented
		 * FeatureNotAvailableException will be thrown accordingly.
		 *
		 */


		/**
		 * Designates the regular expression support feature.
		 *
		 * If this feature is enabled, then the Ceylan library will make use of
		 * the regex-provided primitives whenever needed.
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags RegularExpressions ;



		/**
		 * Designates the multithreading support feature.
		 *
		 * If this feature is enabled, then the Ceylan library will provide a
		 * basic thread support.
		 *
		 * @note This do not imply that the Ceylan library itself will be
		 * multithread-proof, reentrant and/or protected against concurrent
		 * accesses.
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags Multithreading ;



		/**
		 * Designates the network support feature.
		 *
		 * If this feature is enabled, then the Ceylan library will provide a
		 * basic networking support, including abstractions of low-level
		 * communication objects (namey, sockets) and higher-level base classes,
		 * such as generic clients, servers, and basic custom-made RPC-style
		 * primitives, a.k.a. Ceylan middleware.
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags Networking ;



		/**
		 * Designates the file descriptor feature.
		 *
		 * If this feature is enabled, then the Ceylan library will be able to
		 * provide file descriptor management, including select operations,
		 * direct copies, InputStream abstraction for files, sockets, pipes,
		 * etc.
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags FileDescriptors ;



		/**
		 * Designates the symbolic link feature.
		 *
		 * If this feature is enabled, then the Ceylan library will be able to
		 * provide symbolic management management, including creation,
		 * dereferencement, etc.
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags SymbolicLinks ;



		/**
		 * Designates the advanced file attribute feature, which includes
		 * the management, for filesystem elements, of additional:
		 *
		 *   - opening flags (non-blocking/synchronous)
		 *
		 *   - file attributes (beyond 'owner' permissions: 'group' and
		 * 'other').
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags AdvancedFileAttributes ;



		/**
		 * Designates the file lock feature, for reading and/or writing.
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags FileLocks ;



		/**
		 * Designates the advanced process management feature, for fine-grained
		 * featureful process control.
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags AdvancedProcessManagement ;



		/**
		 * Designates the plugin feature, for dynamic loading a shared objects.
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags Plugins ;



		/**
		 * Designates the signal feature, for UNIX-style process signals.
		 *
		 */
		extern CEYLAN_DLL const Ceylan::Flags Signals ;




		/**
		 * Checks that the version of the Ceylan library currently linked
		 * supports the specified feature(s).
		 *
		 * @param featuresFlag an OR'd flag of the requested features, for
		 * example: 'MyFeature | MyOtherFeature'
		 *
		 * @throw FeatureNotAvailableException if at least one of the specified
		 * features is not supported.
		 *
		 */
		CEYLAN_DLL void checkForSupportedFeatures( Ceylan::Flags featuresFlag );



		/// Tells whether regular expressions are supported.
		CEYLAN_DLL bool areRegularExpressionsSupported() ;


		/// Tells whether multithreading is supported.
		CEYLAN_DLL bool isMultithreadingSupported() ;


		/// Tells whether network operations are supported.
		CEYLAN_DLL bool isNetworkingSupported() ;


		/// Tells whether file descriptors are supported.
		CEYLAN_DLL bool areFileDescriptorsSupported() ;


		/// Tells whether symbolic links are supported.
		CEYLAN_DLL bool areSymbolicLinksSupported() ;


		/// Tells whether advanced file attributes are supported.
		CEYLAN_DLL bool areAdvancedFileAttributesSupported() ;


		/// Tells whether file locks are supported.
		CEYLAN_DLL bool areFileLocksSupported() ;


		/// Tells whether advanced process management is supported.
		CEYLAN_DLL bool isAdvancedProcessManagementSupported() ;


		/// Tells whether plugins are supported.
		CEYLAN_DLL bool arePluginsSupported() ;


		/// Tells whether signals are supported.
		CEYLAN_DLL bool areSignalsSupported() ;



		/**
		 * Returns a textual description of the optional features available with
		 * the Ceylan version currently linked.
		 *
		 */
		CEYLAN_DLL const std::string describeAvailableFeatures() ;


	}


}



#endif // CEYLAN_FEATURES_H_
