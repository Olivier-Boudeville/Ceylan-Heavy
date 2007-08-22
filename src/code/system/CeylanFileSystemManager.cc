#include "CeylanFileSystemManager.h"

//#include "CeylanLibfatFileSystemManager.h"    // for LibfatFileSystemManager
#include "CeylanStandardFileSystemManager.h"  // for StandardFileSystemManager

#include "CeylanLogPlug.h"     // for LogPlug
#include "CeylanFile.h"        // for File
#include "CeylanOperators.h"   // for toString


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



using std::string ;
using std::list ;

using namespace Ceylan::System ;
using namespace Ceylan::Log ;


const string FileSystemManager::DefaultAliasForCurrentDirectory = "."  ;
const string FileSystemManager::DefaultAliasForParentDirectory  = ".." ;


FileSystemManager * FileSystemManager::_DefaultFileSystemManager = 0 ;



FileSystemManager::DirectoryOperationFailed::DirectoryOperationFailed( 
		const string & reason ) throw():
	FileSystemManagerException( reason )
{

}



FileSystemManager::TouchFailed::TouchFailed( const string & reason ) throw():
	FileSystemManagerException( reason )
{

}


FileSystemManager::RemoveFailed::RemoveFailed( const string & reason ) throw():
	FileSystemManagerException( reason )
{

}


FileSystemManager::SymlinkFailed::SymlinkFailed( const string & reason ) 
		throw():
	FileSystemManagerException( reason )
{

}
		
	
FileSystemManager::MoveFailed::MoveFailed( const string & reason ) throw():
	FileSystemManagerException( reason )
{

}
		
	
FileSystemManager::CopyFailed::CopyFailed( const string & reason ) throw():
	FileSystemManagerException( reason )
{

}

				
FileSystemManager::GetChangeTimeFailed::GetChangeTimeFailed( 
		const string & reason ) throw():
	FileSystemManagerException( reason )
{

}

		
FileSystemManager::CouldNotDuplicate::CouldNotDuplicate( const string & reason )
		throw():
	FileSystemManagerException( reason )
{

}
		
			
FileSystemManager::CouldNotStatEntry::CouldNotStatEntry( const string & reason )
		throw():
	FileSystemManagerException( reason )
{

}

		
FileSystemManager::DiffFailed::DiffFailed( const string & reason ) throw():
	FileSystemManagerException( reason )
{

}
	
		
	
	
bool FileSystemManager::diff( const std::string & firstFilename,
	const std::string & secondFilename ) throw( DiffFailed )
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

		File & first  = File::Open( firstFilename ) ;
		File & second = File::Open( secondFilename ) ;
	
		for ( Size i = 0; i < commonSize; i++ )
		{
	
			if ( first.readUint8() != second.readUint8() )
				return false ;
			
		}
	
		return true ;
		
	} 
	catch( const SystemException & e )
	{
	
		throw DiffFailed( "FileSystemManager::diff failed: " + e.toString() ) ;
			
	}
	
			
}
		
	

					
const string FileSystemManager::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Abstract filesystem manager" ;
	
}





// Directory-related section.



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

	
	// Extract each part of the specified path thanks to separators:
	for( string::const_iterator it = path.begin(); it != path.end(); it++ )
	{
	
		if ( (*it) == getSeparator() )
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

	// Finds first separator, starting from right to left:
	if ( ( p = path.rfind( getSeparator() ) ) != string::npos )
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



const std::string & FileSystemManager::getAliasForCurrentDirectory()
	const throw()
{

	return DefaultAliasForCurrentDirectory ;
	
}
					
					
const std::string & FileSystemManager::getAliasForParentDirectory()
	const throw()
{

	return DefaultAliasForParentDirectory ;
	
}
					


string FileSystemManager::getSeparatorAsString() const throw()
{

	return Ceylan::toString( getSeparator() ) ;
	
}



// Static section.
	

FileSystemManager & FileSystemManager::GetDefaultFileSystemManager() 
	throw( FileSystemManagerException )
{


#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileSystemManagerException( 
		"Ceylan::System::FileSystemManager::GetDefaultFileSystemManager: 
		"only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	/* FIXME
	if ( _DefaultFileSystemManager == 0 )
		_DefaultFileSystemManager = new LibfatFileSystemManager() ;
*/
	return *_DefaultFileSystemManager ;	

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS

	if ( _DefaultFileSystemManager == 0 )
		_DefaultFileSystemManager = new StandardFileSystemManager() ;

	return *_DefaultFileSystemManager ;	
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}
	
	
	
void FileSystemManager::RemoveDefaultFileSystemManager() throw()
{

	if ( _DefaultFileSystemManager != 0 )
	{
	
		delete _DefaultFileSystemManager ;
		_DefaultFileSystemManager = 0 ;
		
	}
	
}



// Private section.


FileSystemManager::FileSystemManager() throw( FileSystemManagerException )
{

}


FileSystemManager::~FileSystemManager() throw()
{

}

