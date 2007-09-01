#include "CeylanDirectory.h"

#include "CeylanRegularExpression.h" // for RegExp
#include "CeylanLogLight.h"          // for CEYLAN_LOG
#include "CeylanOperators.h"         // for toString
#include "CeylanStringUtils.h"       // for isLetter


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



#ifndef PATH_MAX
#define PATH_MAX 1024
#endif // PATH_MAX


/*
 * Implementation notes.
 *
 * @note All internal paths kept in directory references (this object)
 * should be absolute paths.
 *
 */
 
using std::string ;
using std::list ;

using namespace Ceylan::System ;




Directory & Directory::Create( const string & newDirectoryName ) 
	throw( CouldNotCreate )
{

}
	
					
Directory & Directory::Open( const string & directoryName ) 
	throw( OpenFailed )
{

}
	
	
void Directory::Remove( const string & directoryPath, bool recursive ) 
	throw( RemoveFailed )
{

}

	
Directory::~Directory() throw()
{

	// Nothing special here for abstract directories.

}


					
/*
 * Default implementations from this abstract Directory are slow but safe.
 * They can be overriden by tuned specialized implementations if necessary.
 *
 */



void Directory::goDown( const string & subdirectoryName ) 
	throw( ChangeDirectoryFailed )
{

	 
	if ( hasEntry( subdirectoryName ) )
	{
	
		try
		{
	
			FileSystemManager & fs = getCorrespondingFileSystemManager() ;
	
			string candidate = fs.joinPath( _path, subdirectoryName ) ;
				
			if ( fs.existsAsDirectory( candidate ) )
			{
			 
				_path = candidate ;
				return ;
			
			}
			else
			{
				throw ChangeDirectoryFailed( 
					"Directory::goDown failed for subdirectory '"
					+ subdirectoryName + "' from '" + _path + "': '"
					+ candidate + "' is not a directory." ) ;
			
			}	
			
		}	
		catch( const FileSystemManagerException & e )
		{
		
			throw ChangeDirectoryFailed( 
				"Directory::goDown failed for subdirectory '"
				+ subdirectoryName + "' from '" + _path + "': " 
				+ e.toString() ) ;
		}		

	}
	
	throw ChangeDirectoryFailed( 
		"Directory::goDown failed for non-existent subdirectory '"
		+ subdirectoryName + "' from '" + _path ) ;
	
}


bool Directory::isValid() const throw( DirectoryException )
{

	return ExistsAsDirectory( _path ) ;
			
}


const std::string & Directory::getPath() const throw()
{

	return _path ;
	
}


void Directory::removeLeadingSeparator() throw( DirectoryException )
{
	
	RemoveLeadingSeparator( _path ) ;

}


const string Directory::toString( Ceylan::VerbosityLevels level ) const throw()
{

	return "Abstract directory referring to path '" + _path + "'" ;
	
}




// Static section.


bool Directory::Exists( const string & directoryPath ) const
	throw( DirectoryException )
{

	try
	{
	
		return getCorrespondingFileSystemManager().existsAsDirectory( _path ) ;
			
	}	
	catch( const StatEntryFailed & e )
	{
		
		throw DirectoryLookupFailed( 
			"Directory::Exists failed: " + e.toString() ) ;
			
	}		
	
}

	
bool Directory::IsAValidDirectoryName( const string & directoryString ) 
	throw( DirectoryException )
{

	return getCorrespondingFileSystemManager().isAValidDirectoryName( 
		directoryString ) ;
				
}

	
void Directory::RemoveLeadingSeparator( std::string & path ) 
	throw( DirectoryException )
{

	getCorrespondingFileSystemManager().removeLeadingSeparator( _path ) ;
	
}
					
	
bool Directory::IsAbsolutePath( const string & path ) 
	throw( DirectoryException )
{

	return getCorrespondingFileSystemManager().isAbsolutePath( path ) ;
			
}


string Directory::GetCurrentWorkingDirectoryName() throw( DirectoryException )
{

	return 
		getCorrespondingFileSystemManager().getCurrentWorkingDirectoryName() ;
		
 
}


void Directory::ChangeWorkingDirectory( const string & newWorkingDirectory )
	throw( DirectoryException )
{

	return getCorrespondingFileSystemManager().changeWorkingDirectory(
		newWorkingDirectory ) ;
		
}



list<string> Directory::SplitPath( const string & path ) 
	throw( DirectoryException )
{

	/*
	 * Not wanting to use in-out argument such as 
	 * 'SplitPath( list<string> & nodes ...)', we have to return it. 
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

	Latin1Char separator = GetSeparator() ;
	
	// Extract each part of the specified path thanks to separators:
	for( string::const_iterator it = path.begin(); it != path.end(); it++ )
	{
		if ( *it == separator )
		{
			// We went through each character of the entry word.
			if ( inEntry )
				res.push_back( currentWord ) ;
			inEntry = false ;
			currentWord.erase();
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


string Directory::JoinPath( const list<string> & pathElements ) 
	throw( DirectoryException )
{

	list<string>::const_iterator it = pathElements.begin() ;
	string res = (*it) ;
	it++ ;

	Latin1Char separator = GetSeparator() ;

	while ( it != pathElements.end() )
	{
		res += separator + (*it ) ;
		it++ ;
	}

	return res ;

}


string Directory::JoinPath( const string & firstPath, 
	const std::string & secondPath ) throw( DirectoryException )
{

	return firstPath + GetSeparator() + secondPath ;

}



void Directory::StripFilename( const string & path, string * base, 
	string * file ) throw( DirectoryException )
{

	if ( base != 0 )
		base->erase() ;

	if ( file != 0 )
		file->erase() ;

	string::size_type p ;

	Latin1Char separator = GetSeparator() ;

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


Ceylan::Latin1Char Directory::GetSeparator() const throw( DirectoryException )
{

	return getCorrespondingFileSystemManager().getSeparator() ; 

}


string Directory::GetSeparatorAsString() throw( DirectoryException )
{

	return Ceylan::toString( GetSeparator() ) ;

}



void Directory::Create( const string & name ) 
	throw( Directory::CouldNotCreate )
{
	Directory d( name ) ;
	
	// FIXME
}





// Protected section.


Directory::Directory( const string & directoryName ) 
		throw( DirectoryException ):
	_path( directoryName )
{

	// Enforce the convention on _path:
	removeLeadingSeparator() ;
	
}

