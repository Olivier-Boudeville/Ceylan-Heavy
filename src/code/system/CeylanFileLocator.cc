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


#include "CeylanFileLocator.h"

#include "CeylanFile.h"                  // for ExistsAsFileOrSymbolicLink
#include "CeylanDirectory.h"             // for JoinPath
#include "CeylanStringUtils.h"           // for formatStringList
#include "CeylanLogPlug.h"               // for LogPlug
#include "CeylanEnvironmentVariables.h"  // for getEnvironmentVariable


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"                // for CEYLAN_DEBUG_DEMANGLE, etc.
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h"   // for iprintf, CEYLAN_DS_LOG
#endif // CEYLAN_ARCH_NINTENDO_DS


using std::string ;
using std::list ;

using namespace Ceylan::System ;
using namespace Ceylan::Log ;


/*
 * UNIX path separator is ':', Windows one is ';'.
 *
 */


FileLocatorException::FileLocatorException( const std::string & message )
		throw():
	SystemException( message )
{

}


FileLocatorException::~FileLocatorException() throw()
{

}




FileLocator::FileLocator() throw():
	_paths()
{

}


FileLocator::FileLocator( const string & variableName, char separator ) 
		throw():
	_paths()
{

#if CEYLAN_ARCH_NINTENDO_DS

	/*
	 * No environment variables on the DS, so no path to consider:
	 
	LogPlug::warning( "FileLocator constructor: running on the DS, "
		"hence environment variable '" + variableName + "' is ignored." ) ;
	 *
	 * However this log message has been commented out, as file locators are
	 * often created in global variables (ex: C++ static initializers), which
	 * implies that the log system may not be started yet.
	 *
	 */
	 	
#else // CEYLAN_ARCH_NINTENDO_DS

	addPathsFromEnvironmentVariable( variableName, separator ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 


FileLocator::~FileLocator() throw()
{

}



bool FileLocator::addPath( const string & newPath ) throw() 
{
	
	for ( list<string>::const_iterator it = _paths.begin(); 
			it != _paths.end(); it++ )
		if ( (*it) == newPath )
			return false ;
			
	_paths.push_back( newPath ) ;
	
	return true ;	
		
}



bool FileLocator::addPaths( const std::list<std::string> & paths ) throw() 
{
	
	bool res = false ;
	
	for ( list<string>::const_iterator it = paths.begin(); 
			it != paths.end(); it++ )
		res |= addPath( (*it ) ) ;
	
	return res ;		
	
}



bool FileLocator::addPathsFromEnvironmentVariable( 
	const std::string & variableName, char separator ) throw()
{	

	return addPaths( Ceylan::split( getEnvironmentVariable( variableName ),
		separator ) ) ;
		
}

	
	
bool FileLocator::removePath( const string & pathToRemove ) throw()
{

	for ( list<string>::const_iterator it = _paths.begin(); 
			it != _paths.end() ; it++ )
		if ( (*it) == pathToRemove )
		{
			// Iterator will not be used anymore afterwards:
			_paths.remove( pathToRemove ) ;
			return true ;
		}	
	
	return false ;		
	
}



string FileLocator::find( const string & filename ) const 
	throw( FileLocatorException ) 
{

	string fullPath ;
	
	for ( list<string>::const_iterator it = _paths.begin(); 
		it != _paths.end(); it++ )
	{
	
		fullPath = Directory::JoinPath( (*it), filename ) ;
		
		//LogPlug::debug( "FileLocator::find: testing '" + fullPath + "'." ) ;
		
		if ( File::ExistsAsFileOrSymbolicLink( fullPath ) )
			return fullPath ;
	}	
	
	throw FileLocatorException( "File '" + filename 
		+ "' could not be found through following Locator: " + toString() ) ;
		
}



const std::list<std::string> & FileLocator::getPaths() const throw()
{

	return _paths ;
	
}



const string FileLocator::toString( Ceylan::VerbosityLevels level ) const
	throw()
{
	
	if ( _paths.empty() ) 
		return "Empty File locator" ;

	return "File locator with following registered directories: "
		+ Ceylan::formatStringList( _paths ) ;
				 
}

