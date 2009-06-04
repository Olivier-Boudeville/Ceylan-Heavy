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


#include "CeylanFileSystemManager.h"

#include "CeylanStandardFileSystemManager.h"  // for StandardFileSystemManager
#include "CeylanLibfatFileSystemManager.h"    // for LibfatFileSystemManager

#include "CeylanLogPlug.h"     // for LogPlug
#include "CeylanFile.h"        // for File
#include "CeylanOperators.h"   // for toString



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



/*
 * Implementation notes.
 *
 * Some file and directory operations are made available here, so that
 * they are virtual methods (able to be overriden in FileSystemManager
 * child classes) rather than static methods (non-overridable) in 
 * classes File, Directory, etc. (filesystem implementations may have
 * to define them specifically).
 * But for the ease of programming, these FileSystemManager virtual
 * methods will be made available through File and Directory static
 * methods, in charge of selecting automatically the correct filesystem
 * implementation and calling its corresponding virtual method.
 * This allows the user programs to call only File::MyMethod instead of
 * calling something like GetDefaultFileSystemManager().myMethod or,
 * worst, GetStandardFileSystemManager().myMethod: less readable, more
 * error cases to manage, and the user code would not be cross-platform.
 *
 * The current organization is to centralize in FileSystemManager child
 * classes the non-static implementations of all the methods that 
 * would be static for a given file or directory child class: these
 * implementations must not be static (so that they are overridable) 
 * hence cannot be defined directly from File and Directory abstract
 * classes (otherwise polymorphism could not be used, and these two
 * abstract classes would have to be tightly coupled to their child
 * classes). Obviously non-static methods are to be called from
 * instances that can be neither File nor Directory instances.
 * 
 */

using std::string ;
using std::list ;

using namespace Ceylan::System ;
using namespace Ceylan::Log ;


const string FileSystemManager::DefaultAliasForCurrentDirectory = "."  ;
const string FileSystemManager::DefaultAliasForParentDirectory  = ".." ;


FileSystemManager * FileSystemManager::_CurrentDefaultFileSystemManager = 0 ;





// Public section.


		
// Static methods directly exposed to the user.



// For FileSystemManager:


bool FileSystemManager::ExistsAsEntry( const std::string & entryPath ) 
{

	// Let FileSystemManagerDelegatingException and EntryLookupFailed:
	return GetAnyDefaultFileSystemManager().existsAsEntry( entryPath ) ;
	
}



void FileSystemManager::CreateSymbolicLink( const string & linkTarget, 
	const string & linkName )
{

	// Let FileSystemManagerDelegatingException and SymlinkFailed:
	GetAnyDefaultFileSystemManager().createSymbolicLink( linkTarget,
		linkName ) ;
		
}



time_t FileSystemManager::GetEntryChangeTime( const string & entryPath )
{

	// Let FileSystemManagerDelegatingException and GetChangeTimeFailed:
	return GetAnyDefaultFileSystemManager().getEntryChangeTime( entryPath ) ;
		
}
			
			
				
						
// Accessors to FilesystemManager constants.


const string & FileSystemManager::GetRootDirectoryPrefix() 	
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getRootDirectoryPrefix() ;
	
}


						
Ceylan::Latin1Char FileSystemManager::GetSeparator() 	
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getSeparator() ;
	
}


						
string FileSystemManager::GetSeparatorAsString() 
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getSeparatorAsString() ;
	
}

	
						
const string & FileSystemManager::GetAliasForCurrentDirectory()	
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getAliasForCurrentDirectory() ;
	
}
	
	
						
const string & FileSystemManager::GetAliasForParentDirectory() 
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getAliasForParentDirectory() ;
	
}
						
		
		
	
					
										
// Implementation virtual methods.						



// For FileSystemManager (other methods are pure virtual):


const std::string & FileSystemManager::getAliasForCurrentDirectory() const
{

	// Made to be overriden if necessary:
	return DefaultAliasForCurrentDirectory ;
	
}
					
	
					
const std::string & FileSystemManager::getAliasForParentDirectory() const
{

	// Made to be overriden if necessary:
	return DefaultAliasForParentDirectory ;
	
}
					


std::string FileSystemManager::getSeparatorAsString() const
{

	return Ceylan::toString( getSeparator() ) ;
	
}





						
// For File (other methods are pure virtual):



std::string FileSystemManager::transformIntoValidFilename( 
	const string & rawFilename )
{

	// For MS-DOS/Windows, one may look at gcc 'dosck' as well.
	
	string result ;

	Ceylan::Uint32 characterCount = 0 ;

	// Remove all leading dots '.':
	while ( rawFilename[ characterCount ] == '.' )
		characterCount++ ;

	// (preferred to: for( string::const_iterator it...)

	// Substitute any space " ", slash "/" or back-slash "\" by a dash "-":

	StringSize nameSize = rawFilename.size() ;
	
	for ( ; characterCount < nameSize ; characterCount++ )
	{
		switch( rawFilename[ characterCount ] )
		{

			case ' ':
				result += "-" ;
				break ;

			case '/':
				result += "-" ;
				break ;

			case '\\':
				result += "-" ;
				break ;

			/*
			 * This is not strictly needed on most systems, but it is 
			 * convenient to avoid ':' in HTML file references 
			 * (<a href="xx::yyy.html">... not recommended)
			 *
			 */
			case ':':
				result += "_" ;
				break ;

			default:
				result += rawFilename[ characterCount ] ;
				break ;

		}

	}

	return result ;
	
}

	
	
bool FileSystemManager::diff( const std::string & firstFilename,
	const std::string & secondFilename )
{

	try
	{
	
	
		if ( ! existsAsFileOrSymbolicLink( firstFilename ) )
		{
	
			LogPlug::warning( "FileSystemManager::diff: first file '" 
				+ firstFilename	+ "' does not exist." ) ;
			
			return false ;	
		
		}
	
	
		if ( ! existsAsFileOrSymbolicLink( secondFilename ) )
		{

			LogPlug::warning( "FileSystemManager::diff: first file '" 
				+ secondFilename + "' does not exist." ) ;
		
			return false ;
			
		}
	
		Size commonSize = getSize( firstFilename ) ;
	
		if ( commonSize != getSize( secondFilename ) )
		{
	
			LogPlug::warning( "FileSystemManager::diff: " 
				"the two files do not have the same size." ) ;
		
			return false ;	
		
		}

	
		/*
		LogPlug::debug( "Common size is " 
			+ Ceylan::toString( static_cast<Ceylan::Uint32>( commonSize ) )
			+ " bytes." ) ;
		 */

		File & first  = openFile( firstFilename ) ;
		File & second = openFile( secondFilename ) ;
	
		for ( Size i = 0; i < commonSize; i++ )
		{
	
			if ( first.readUint8() != second.readUint8() )
				return false ;
			
		}
	
		return true ;
		
	} 
	catch( const SystemException & e )
	{
	
		throw FileDiffFailed( "FileSystemManager::diff failed: " 
			+ e.toString() ) ;
			
	}
	
		
}
		
	

					



// For Directory (other methods are pure virtual):



void FileSystemManager::removeLeadingSeparator( std::string & path )
{

	if ( path[ path.size() - 1 ] == getSeparator() )
		path.erase( path.size() - 1, 1 ) ;

}


					
list<string> FileSystemManager::splitPath( const string & path )
{

	/*
	 * Not wanting to use in-out argument such as 
	 * 'splitPath( list<string> & nodes ...)', we have to return it. 
	 * We are therefore unable to return directly the number of elements.
	 *
	 */

	list<string> res ;
	string currentWord ;

	/*
	 * With an initial value of false, "/mnt/..." would lead to
	 * [ 'mnt', '...' ] instead of the expected [ '', 'mnt', '...' ].
	 * Doing so is needed to make a difference between "/mnt/..." and "mnt/...".
	 *
	 */

	bool inEntry = true ;

	Latin1Char separator = getSeparator() ;
	
	// Extract each part of the specified path thanks to separators:
	for( string::const_iterator it = path.begin(); it != path.end(); it++ )
	{
	
		if ( (*it) == separator )
		{
			// We went through each character of the entry word.
			if ( inEntry )
				res.push_back( currentWord ) ;
			inEntry = false ;
			currentWord.erase() ;
		}
		else
		{
			// Adds the current character to the current entry name.
			currentWord += *it ;
			inEntry = true ;
		}
		
	}

	// Add last word if needed.
	if ( inEntry )
		res.push_back( currentWord ) ;

	return res ;

}



string FileSystemManager::joinPath( const list<string> & pathElements )
{

	list<string>::const_iterator it = pathElements.begin() ;
	string res = (*it) ;
	it++ ;

	while ( it != pathElements.end() )
	{
		res += getSeparatorAsString() + (*it ) ;
		it++ ;
	}

	return res ;

}



string FileSystemManager::joinPath( const string & firstPath, 
	const std::string & secondPath )
{

	return firstPath + getSeparatorAsString() + secondPath ;

}



void FileSystemManager::stripFilename( const string & path, string * base, 
	string * file )
{

	if ( base != 0 )
		base->erase() ;

	if ( file != 0 )
		file->erase() ;

	string::size_type p ;

	Latin1Char separator = getSeparator() ;

	// Finds first separator, starting from right to left:
	if ( ( p = path.rfind( separator ) ) != string::npos )
	{
		// Base is the characters between begin and last separator:
		if ( base != 0 )
			base->assign( path, 0, p ) ;

		// File is the leading part:
		if ( file != 0 )
			file->assign( path, p+1, path.size() - p - 1 ) ;
	}
	else
	{
		// No separator: no path, only file.
		if ( file != 0 )
			*file = path ;
	}
	
}





// FileSystemManager own section.


const string FileSystemManager::toString( Ceylan::VerbosityLevels level ) const
{

	return "Abstract filesystem manager" ;
	
}





// Static section to handle default filesystem manager.



bool FileSystemManager::IsDefaultFileSystemManagerSet()
{

	return ( _CurrentDefaultFileSystemManager != 0 ) ;
	
}



void FileSystemManager::SetDefaultFileSystemManager( 
	FileSystemManager & newDefaultFileSystemManager,
	bool deallocatePreviousIfAny )
{

	if ( deallocatePreviousIfAny )
    {
    
		/*
		 * As the current manager can be registered as well in a static
		 * class-specific variable, its destructor must perform every
		 * needed unsubscribing by itself.
		 *
		 */
		  
		if ( _CurrentDefaultFileSystemManager != 0 )
			delete _CurrentDefaultFileSystemManager ;
	
    }
    
	_CurrentDefaultFileSystemManager = & newDefaultFileSystemManager ;
		
}



void FileSystemManager::SetDefaultFileSystemManagerToPlatformDefault()
{

	// Common to all platforms:
	if ( _CurrentDefaultFileSystemManager != 0 )
		delete _CurrentDefaultFileSystemManager ;

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileSystemManagerException( 
		"FileSystemManager::SetDefaultFileSystemManagerToPlatformDefault: "
		"only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	/*
	 * Registers as well this manager in static _LibfatFileSystemManager.
	 * Let LibfatFileSystemManagerException propagate:
	 *
	 */
	_CurrentDefaultFileSystemManager =
		& LibfatFileSystemManager::GetLibfatFileSystemManager() ;

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS

	/*
	 * Registers as well this manager in static _StandardFileSystemManager.
	 * Let StandardFileSystemManagerException propagate:
	 *
	 */
	_CurrentDefaultFileSystemManager = 
		& StandardFileSystemManager::GetStandardFileSystemManager() ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS

}
	
	
						
FileSystemManager & FileSystemManager::GetExistingDefaultFileSystemManager() 
{

	if ( _CurrentDefaultFileSystemManager == 0 )
		throw FileSystemManagerException(
			"FileSystemManager::GetExistingDefaultFileSystemManager:"
			"no manager currently available" ) ;
			
	return *_CurrentDefaultFileSystemManager ;	
	
}
	


FileSystemManager & FileSystemManager::GetAnyDefaultFileSystemManager() 
{

	if ( _CurrentDefaultFileSystemManager == 0 )
		SetDefaultFileSystemManagerToPlatformDefault() ;
			
	return *_CurrentDefaultFileSystemManager ;	

}
	
					
	
void FileSystemManager::RemoveDefaultFileSystemManager()
{

	if ( _CurrentDefaultFileSystemManager != 0 )
	{
	
		delete _CurrentDefaultFileSystemManager ;
		_CurrentDefaultFileSystemManager = 0 ;
		
	}
	
}




// Protected section.


FileSystemManager::FileSystemManager()
{

}



FileSystemManager::~FileSystemManager() throw()
{

}

