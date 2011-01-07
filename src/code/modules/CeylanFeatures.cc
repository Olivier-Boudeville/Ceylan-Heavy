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


#include "CeylanFeatures.h"

#include "CeylanStringUtils.h"
#include "CeylanOperators.h"


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H


#include <list>
using std::list ;


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Features ;



// Binary masks for feature selection must not overlap.


/*
 * The regular expression support feature, linked with preprocessor symbol
 * CEYLAN_USES_REGEX.
 *
 * Equal to 0b00000000000000000000000000000001.
 *
 */
const Flags Features::RegularExpressions = 0x00000001 ; 



/*
 * The multithreading support feature, linked with preprocessor symbol
 * CEYLAN_USES_THREADS
 *
 * Equal to 0b00000000000000000000000000000010.
 *
 */
const Flags Features::Multithreading = 0x00000002 ; 



/*
 * The network support feature, linked with preprocessor symbol
 * CEYLAN_USES_NETWORK.
 *
 * Equal to 0b00000000000000000000000000000100.
 *
 */
const Flags Features::Networking = 0x00000004 ; 



/*
 * The file descriptor support feature, linked with preprocessor symbol
 * CEYLAN_USES_FILE_DESCRIPTORS.
 *
 * Equal to 0b00000000000000000000000000001000.
 *
 */
const Flags Features::FileDescriptors = 0x00000008 ; 



/*
 * The symbolic link support feature, linked with preprocessor symbol
 * CEYLAN_USES_SYMBOLIC_LINKS.
 *
 * Equal to 0b00000000000000000000000000010000.
 *
 */
const Flags Features::SymbolicLinks = 0x00000010 ; 



/*
 * The advanced file attribute feature, linked with preprocessor symbol
 * CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES.
 *
 * Equal to 0b00000000000000000000000000100000.
 *
 */
const Flags Features::AdvancedFileAttributes = 0x00000020 ; 



/*
 * The symbolic link support feature, linked with preprocessor symbol
 * CEYLAN_USES_FILE_LOCKS.
 *
 * Equal to 0b00000000000000000000000001000000.
 *
 */
const Flags Features::FileLocks = 0x00000040 ; 



/*
 * The advanced process management feature, linked with preprocessor symbol
 * CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT.
 *
 * Equal to 0b00000000000000000000000010000000.
 *
 */
const Flags Features::AdvancedProcessManagement = 0x00000080 ; 



/*
 * The plugin feature, linked with preprocessor symbol
 * CEYLAN_USES_PLUGINS.
 *
 * Equal to 0b00000000000000000000000100000000.
 *
 */
const Flags Features::Plugins = 0x00000100 ; 



/*
 * The signal feature, linked with preprocessor symbol
 * CEYLAN_USES_SIGNALS.
 *
 * Equal to 0b00000000000000000000001000000000.
 *
 */
const Flags Features::Signals = 0x00000200 ; 




FeatureNotAvailableException::FeatureNotAvailableException( 
		const std::string & message ) :
	Ceylan::Exception( message )
{

}



FeatureNotAvailableException::~FeatureNotAvailableException() throw()
{

}
	


void Features::checkForSupportedFeatures( Flags featuresFlag )
{
	
	string endOfMessage = " feature not supported by the version "
		"of the Ceylan library currently linked." ;
	
	// Test in turn all specified features:
	
	if ( featuresFlag & RegularExpressions )
	{
		if ( ! areRegularExpressionsSupported() )
			throw FeatureNotAvailableException( "Regular expression" 
				+ endOfMessage ) ;			
	}
	
	
	
	if ( featuresFlag & Multithreading )
	{
		if ( ! isMultithreadingSupported() )
			throw FeatureNotAvailableException( "Multithreading" 
				+ endOfMessage ) ;			
	}
	
	
	
	if ( featuresFlag & Networking )
	{
		if ( ! isNetworkingSupported() )
			throw FeatureNotAvailableException( "Networking" + endOfMessage ) ;
			
	}
	
	
	
	if ( featuresFlag & FileDescriptors )
	{
		if ( ! areFileDescriptorsSupported() )
			throw FeatureNotAvailableException( "File descriptor" 
				+ endOfMessage ) ;			
	}
	
	
	
	if ( featuresFlag & SymbolicLinks )
	{
		if ( ! areSymbolicLinksSupported() )
			throw FeatureNotAvailableException( "Symbolic link" 
				+ endOfMessage ) ;			
	}
	
	
	
	if ( featuresFlag & AdvancedFileAttributes )
	{
		if ( ! areAdvancedFileAttributesSupported() )
			throw FeatureNotAvailableException( "Advanced file attribute" 
				+ endOfMessage ) ;			
	}
	
	
	
	if ( featuresFlag & FileLocks )
	{
		if ( ! areFileLocksSupported() )
			throw FeatureNotAvailableException( "File lock" + endOfMessage ) ;	
	}



	if ( featuresFlag & AdvancedProcessManagement )
	{
		if ( ! isAdvancedProcessManagementSupported() )
			throw FeatureNotAvailableException( 
				"Advanced process management" + endOfMessage ) ;			
	}
	
	
	
	if ( featuresFlag & AdvancedProcessManagement )
	{
		if ( ! arePluginsSupported() )
			throw FeatureNotAvailableException( "Plugin" + endOfMessage ) ;
			
	}
	
	
	
	if ( featuresFlag & Signals )
	{
		if ( ! areSignalsSupported() )
			throw FeatureNotAvailableException( "Signal" + endOfMessage ) ;
			
	}
	
	
}



bool Features::areRegularExpressionsSupported() 
{

#if CEYLAN_USES_REGEX
	return true ;
#else // CEYLAN_USES_REGEX 
	return false ; 	
#endif // CEYLAN_USES_REGEX 

}



bool Features::isMultithreadingSupported() 
{

#if CEYLAN_USES_THREADS
	return true ;
#else // CEYLAN_USES_THREADS
	return false ; 	
#endif // CEYLAN_USES_THREADS

}



bool Features::isNetworkingSupported() 
{

#if CEYLAN_USES_NETWORK
	return true ;
#else // CEYLAN_USES_NETWORK
	return false ; 	
#endif // CEYLAN_USES_NETWORK

}



bool Features::areFileDescriptorsSupported() 
{

#if CEYLAN_USES_FILE_DESCRIPTORS
	return true ;
#else // CEYLAN_USES_FILE_DESCRIPTORS
	return false ; 	
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}



bool Features::areSymbolicLinksSupported() 
{

#if CEYLAN_USES_SYMBOLIC_LINKS
	return true ;
#else // CEYLAN_USES_SYMBOLIC_LINKS
	return false ; 	
#endif // CEYLAN_USES_SYMBOLIC_LINKS

}



bool Features::areAdvancedFileAttributesSupported() 
{

#if CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES
	return true ;
#else // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES
	return false ; 	
#endif // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

}



bool Features::areFileLocksSupported() 
{

#if CEYLAN_USES_FILE_LOCKS
	return true ;
#else // CEYLAN_USES_FILE_LOCKS
	return false ; 	
#endif // CEYLAN_USES_FILE_LOCKS

}



bool Features::isAdvancedProcessManagementSupported() 
{

#if CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT
	return true ;
#else // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT
	return false ; 	
#endif // CEYLAN_USES_ADVANCED_PROCESS_MANAGEMENT

}



bool Features::arePluginsSupported() 
{

#if CEYLAN_USES_PLUGINS
	return true ;
#else // CEYLAN_USES_PLUGINS
	return false ; 	
#endif // CEYLAN_USES_PLUGINS

}



bool Features::areSignalsSupported() 
{

#if CEYLAN_USES_SIGNALS
	return true ;
#else // CEYLAN_USES_SIGNALS
	return false ; 	
#endif // CEYLAN_USES_SIGNALS

}


const std::string Features::describeAvailableFeatures()  
{
		
	list<string> featureList ;
	
	if ( areRegularExpressionsSupported() )
		featureList.push_back( "Regular expressions are supported" ) ;
	else
		featureList.push_back( "Regular expressions not supported" ) ;

		
	if ( isMultithreadingSupported() )
		featureList.push_back( "Multithreading is supported" ) ;
	else
		featureList.push_back( "Multithreading not supported" ) ;


	if ( isNetworkingSupported() )
		featureList.push_back( "Networking supported" ) ;
	else
		featureList.push_back( "Networking not supported" ) ;
		
		
	if ( areFileDescriptorsSupported() )
		featureList.push_back( "File descriptors are supported" ) ;
	else
		featureList.push_back( "File descriptors not supported" ) ;
		
		
	if ( areSymbolicLinksSupported() )
		featureList.push_back( "Symbolic links are supported" ) ;
	else
		featureList.push_back( "Symbolic links not supported" ) ;


	if ( areAdvancedFileAttributesSupported() )
		featureList.push_back( "Advanced file attributes are supported" ) ;
	else
		featureList.push_back( "Advanced file attributes not supported" ) ;


	if ( areFileLocksSupported() )
		featureList.push_back( "File locks are supported" ) ;
	else
		featureList.push_back( "File locks not supported" ) ;


	if ( isAdvancedProcessManagementSupported() )
		featureList.push_back( "Advanced process management is supported" ) ;
	else
		featureList.push_back( "Advanced process management not supported" ) ;


	if ( arePluginsSupported() )
		featureList.push_back( "Plugins are supported" ) ;
	else
		featureList.push_back( "Plugins are not supported" ) ;


	if ( areSignalsSupported() )
		featureList.push_back( "Signals are supported" ) ;
	else
		featureList.push_back( "Signals are not supported" ) ;


	return "Summary of optional features available with "
		"the Ceylan library currently linked:"
		+ Ceylan::formatStringList( featureList ) ;
			
}

