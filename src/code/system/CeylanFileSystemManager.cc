#include "CeylanFileSystemManager.h"

//#include "CeylanLibfatFileSystemManager.h"    // for LibfatFileSystemManager
#include "CeylanStandardFileSystemManager.h"  // for StandardFileSystemManager

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
	throw( FileSystemManagerException )
{

	// Let FileSystemManagerDelegatingException and EntryLookupFailed:
	return GetAnyDefaultFileSystemManager().existsAsEntry( entryPath ) ;
	
}


void FileSystemManager::CreateSymbolicLink( const string & linkTarget, 
	const string & linkName ) throw( FileSystemManagerException )
{

	// Let FileSystemManagerDelegatingException and SymlinkFailed:
	GetAnyDefaultFileSystemManager().createSymbolicLink( linkTarget,
		linkName ) ;
		
}


time_t FileSystemManager::GetEntryChangeTime( const string & entryPath )
	throw( FileSystemManagerException )
{

	// Let FileSystemManagerDelegatingException and GetChangeTimeFailed:
	return GetAnyDefaultFileSystemManager().getEntryChangeTime( entryPath ) ;
		
}
			
			
				
						
// Accessors to FilesystemManager constants.


const string & FileSystemManager::GetRootDirectoryPrefix() 
	throw( FileSystemManagerDelegatingException )
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getRootDirectoryPrefix() ;
	
}

						
Ceylan::Latin1Char FileSystemManager::GetSeparator() 
	throw( FileSystemManagerDelegatingException )
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getSeparator() ;
	
}

						
string FileSystemManager::GetSeparatorAsString() 
	throw( FileSystemManagerDelegatingException )
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getSeparatorAsString() ;
	
}

						
const string & FileSystemManager::GetAliasForCurrentDirectory()	
	throw( FileSystemManagerDelegatingException )
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getAliasForCurrentDirectory() ;
	
}
	
						
const string & FileSystemManager::GetAliasForParentDirectory() 
	throw( FileSystemManagerDelegatingException )
{

	// Let FileSystemManagerDelegatingException propagate:
	return GetAnyDefaultFileSystemManager().getAliasForParentDirectory() ;
	
}
						
		
		
	
					
										
// Implementation virtual methods.						



// For FileSystemManager (other methods are pure virtual):


const std::string & FileSystemManager::getAliasForCurrentDirectory()
	const throw()
{

	// Made to be overriden if necessary:
	return DefaultAliasForCurrentDirectory ;
	
}
					
					
const std::string & FileSystemManager::getAliasForParentDirectory()
	const throw()
{

	// Made to be overriden if necessary:
	return DefaultAliasForParentDirectory ;
	
}
					


string FileSystemManager::getSeparatorAsString() const throw()
{

	return Ceylan::toString( getSeparator() ) ;
	
}





						
// For File (other methods are pure virtual):
	
	
bool FileSystemManager::diff( const std::string & firstFilename,
	const std::string & secondFilename ) throw( FileDiffFailed )
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



void FileSystemManager::removeLeadingSeparator( std::string & path ) throw()
{

	if ( path[ path.size() - 1 ] == getSeparator() )
		path.erase( path.size() - 1, 1 ) ;

}


					
list<string> FileSystemManager::splitPath( const string & path ) throw()
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



string FileSystemManager::joinPath( const list<string> & pathElements ) throw()
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
	const std::string & secondPath ) throw()
{

	return firstPath + getSeparatorAsString() + secondPath ;

}



void FileSystemManager::stripFilename( const string & path, string * base, 
	string * file ) throw()
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


const string FileSystemManager::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Abstract filesystem manager" ;
	
}





// Static section to handle default filesystem manager.



bool FileSystemManager::IsDefaultFileSystemManagerSet() throw()
{

	return ( _CurrentDefaultFileSystemManager != 0 ) ;
	
}


void FileSystemManager::SetDefaultFileSystemManager( 
		FileSystemManager & newDefaultFileSystemManager ) throw()
{

	if ( _CurrentDefaultFileSystemManager != 0 )
		delete _CurrentDefaultFileSystemManager ;
	
	_CurrentDefaultFileSystemManager = & newDefaultFileSystemManager ;
		
}


void FileSystemManager::SetDefaultFileSystemManagerToPlatformDefault()
	throw( FileSystemManagerException )
{

	if ( _CurrentDefaultFileSystemManager != 0 )
		delete _CurrentDefaultFileSystemManager ;

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileSystemManagerException( 
		"FileSystemManager::SetDefaultFileSystemManagerToPlatformDefault: 
		"only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	/* FIXME
	_CurrentDefaultFileSystemManager = new LibfatFileSystemManager() ;
*/

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS

	// Let StandardFileSystemManagerException propagate:
	_CurrentDefaultFileSystemManager = new StandardFileSystemManager() ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS

}
	
						
FileSystemManager & FileSystemManager::GetExistingDefaultFileSystemManager() 
	throw( FileSystemManagerException )
{

	if ( _CurrentDefaultFileSystemManager == 0 )
		throw FileSystemManagerException(
			"FileSystemManager::GetExistingDefaultFileSystemManager:"
			"no manager currently available" ) ;
			
	return *_CurrentDefaultFileSystemManager ;	
	
}
	

FileSystemManager & FileSystemManager::GetAnyDefaultFileSystemManager() 
	throw( FileSystemManagerException )
{

	if ( _CurrentDefaultFileSystemManager == 0 )
		SetDefaultFileSystemManagerToPlatformDefault() ;
			
	return *_CurrentDefaultFileSystemManager ;	

}
					
	
void FileSystemManager::RemoveDefaultFileSystemManager() throw()
{

	if ( _CurrentDefaultFileSystemManager != 0 )
	{
	
		delete _CurrentDefaultFileSystemManager ;
		_CurrentDefaultFileSystemManager = 0 ;
		
	}
	
}



// Protected section.


FileSystemManager::FileSystemManager() throw( FileSystemManagerException )
{

}


FileSystemManager::~FileSystemManager() throw()
{

}

