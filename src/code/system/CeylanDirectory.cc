#include "CeylanDirectory.h"

#include "CeylanRegularExpression.h" // for RegExp
#undef CEYLAN_DEBUG_LOG
#define CEYLAN_DEBUG_LOG 1
#include "CeylanLogLight.h"          // for CEYLAN_LOG
#include "CeylanOperators.h"         // for toString
#include "CeylanStringUtils.h"       // for isLetter


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


// None of them is available in standard include :
extern "C"
{

#if CEYLAN_USES_UNISTD_H
#include <unistd.h>
#endif // CEYLAN_USES_UNISTD_H

#if CEYLAN_USES_DIRENT_H
#include <dirent.h>
#endif // CEYLAN_USES_DIRENT_H

#if CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for S_IRWXU
#endif // CEYLAN_USES_SYS_TYPES_H

#if CEYLAN_USES_SYS_STAT_H
#include <sys/stat.h>          // for S_ISDIR, stat, etc.
#endif // CEYLAN_USES_SYS_STAT_H

#if CEYLAN_USES_DIRECT_H
#include <direct.h>            // for _mkdir
#endif // CEYLAN_USES_DIRECT_H

}


#ifndef PATH_MAX
#define PATH_MAX 1024
#endif // PATH_MAX



using std::string ;
using std::list ;

using namespace Ceylan::System ;


#if CEYLAN_ARCH_WINDOWS

// "c:" is a drive, "c:\\" is a directory.
const string Directory::RootDirectoryPrefix   = "c:"   ;
const string Directory::CurrentDirectoryAlias = "."  ;
const string Directory::UpperDirectoryAlias   = ".." ;
const char   Directory::Separator             = '\\'  ;

#else // CEYLAN_ARCH_WINDOWS

const string Directory::RootDirectoryPrefix   = ""   ;
const string Directory::CurrentDirectoryAlias = "."  ;
const string Directory::UpperDirectoryAlias   = ".." ;
const char   Directory::Separator 	          = '/'  ;

#endif // CEYLAN_ARCH_WINDOWS


/// Flags used to create new directories :

#if CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

const mode_t basicDirectory = 
	S_IRWXU | S_IRGRP | S_IXGRP	| S_IROTH | S_IXOTH ;
	
#else // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES

#ifdef CEYLAN_USES_MKDIR_TWO_ARGS

// Currently not used on MinGW : 
const mode_t basicDirectory = S_IRWXU ;

#endif // CEYLAN_USES_MKDIR_TWO_ARGS

#endif // CEYLAN_USES_ADVANCED_FILE_ATTRIBUTES		


/*
 * ::stat is used often here.
 * Maybe check specifically CEYLAN_USES_SYS_STAT_H, short of having
 * 'CEYLAN_USES_STAT'.
 *
 */


Directory::Directory() throw()
{

	_path = GetCurrentWorkingDirectoryName() ;
	removeLeadingSeparator() ;

}


Directory::Directory( const string & name, bool createDirectory )
	throw( Directory::DirectoryException )
{

#ifdef CEYLAN_USES_MKDIR

	CEYLAN_LOG( "Creating directory reference for " + name ) ;

	_path = name ;


	// path should not finish with a separator :
	removeLeadingSeparator() ;

	if ( createDirectory && ! isValid() )
	{

		CEYLAN_LOG( "Directory " + _path 
			+ " does not exist yet and is to be created." ) ;

		/*
		 * Not a valid path, must be created, each intermediate
		 * directory will be created if necessary.
		 *
		 */

		string path ;

#if CEYLAN_ARCH_UNIX

		// Next split will eat leading '/' :
		if ( IsAbsolutePath( _path ) )
			path = RootDirectoryPrefix + Separator ;

#endif // CEYLAN_ARCH_UNIX

#if CEYLAN_ARCH_WINDOWS

		/*
		 * For Windows, for example we have to evaluate first 'c:\' 
		 * (not 'c:' which would never be found existing), then 
		 * 'c:\Documents and Settings' 
		 * (not 'c:\Documents and Settings\' which would never be found existing)
		 *
		 */
		bool firstPathElement = true ;

#endif // CEYLAN_ARCH_WINDOWS

		list<string> nodes = SplitPath( _path ) ;

		CEYLAN_LOG( "Directory::Directory : creating sub-elements." ) ;

		for ( list<string>::const_iterator it = nodes.begin(); 
			it != nodes.end(); it++ )
		{
		
			path += *it ;
			
#if CEYLAN_ARCH_WINDOWS
			if ( firstPathElement )
			{

				// 'c:' is never a directory, whereas 'C:\' is.
				path += Directory::Separator ;
				firstPathElement = false ;
			}
#endif // CEYLAN_ARCH_WINDOWS


			CEYLAN_LOG( "Directory::Directory : examining " + path ) ;

			if ( ! Exists( path ) )
			{

				CEYLAN_LOG( "Directory::Directory : creating " + path ) ;

				// Mingw's mkdir takes only parameter :
								
#ifdef CEYLAN_USES_MKDIR_TWO_ARGS

				if ( ::mkdir( path.c_str(), basicDirectory ) == -1 )
				
#else // CEYLAN_USES_MKDIR_TWO_ARGS

#ifdef CEYLAN_USES__MKDIR

				if ( ::_mkdir( path.c_str() /* no basicDirectory argument */ ) 
					== -1 )

#else // CEYLAN_USES__MKDIR 

				if ( ::mkdir( path.c_str() /* no basicDirectory argument */ ) 
					== -1 )
					
#endif // CEYLAN_USES__MKDIR 

#endif // CEYLAN_USES_MKDIR_TWO_ARGS			
					throw CouldNotCreate( path + " : " + explainError() ) ;
			}

			path += Separator ;

		}

	}

	if ( ! IsAbsolutePath( _path ) )
		_path = GetCurrentWorkingDirectoryName() + Separator + _path ;

	CEYLAN_LOG( "Directory reference to " + _path + " done." ) ;

#else // CEYLAN_USES_MKDIR

	throw DirectoryException( "Directory constructor : operation not "
		"available on this platform." ) ;
		
#endif // CEYLAN_USES_MKDIR

}


Directory::~Directory() throw()
{

}


bool Directory::isValid() const throw()
{
	return Exists( _path ) ;
}


bool Directory::hasEntry( const string & name ) const throw()
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;
	string tmp = _path + Separator + name ;
	return ::stat( tmp.c_str(), & buf ) == 0 ;

#else // CEYLAN_USES_STAT

	struct _stat buf ;
	string tmp = _path + Separator + name ;
	return ::_stat( tmp.c_str(), & buf ) == 0 ;

#endif // CEYLAN_USES_STAT
	
}


void Directory::getSubdirectories( list<string> & subDirectories ) 
	const throw( DirectoryException )
{

#ifdef CEYLAN_USES_DIRENT_H

	DIR * d = ::opendir( _path.c_str() ) ;

	if ( d == 0 )
		throw DirectoryException( "Directory::getSubdirectories : open : "
			+ explainError() ) ;

	struct dirent * de = 0 ;
	struct stat buf ;

	while( ( de = ::readdir( d ) ) != 0 )
	{
		string name = de->d_name ;
		string fullname = _path + Separator + name ;

		// Selects only real subdirectories :
		if ( name != CurrentDirectoryAlias
				&& name != UpperDirectoryAlias
				&& ::stat( fullname.c_str(), & buf ) == 0
				&& S_ISDIR( buf.st_mode ) )
			subDirectories.push_back( name ) ;
	}
	
	
	/*
	 * errno could be checked here :
	 
	if ( errno != 0 )
		throw DirectoryException( "Directory::getSubdirectories : read : "
			+ explainError() ) ;
	 *
	 */
	 
	if ( ::closedir( d ) == -1 )
		throw DirectoryException( "Directory::getSubdirectories : close : "
			+ explainError() ) ;


#else // CEYLAN_USES_DIRENT_H

	// @portme Use FindFirstFile(), FindNextFile(), and FindClose() Win32 API functions.

	throw DirectoryException( "Directory::getSubdirectories : "
		"not available on this platform." ) ;
	
#endif // CEYLAN_USES_DIRENT_H

}


void Directory::getEntries( list<string> & entries ) const 
	throw( DirectoryException )
{

#ifdef CEYLAN_USES_DIRENT_H

	DIR * d = ::opendir( _path.c_str() ) ;

	if ( d == 0 )
		return ;

	struct dirent * de = 0 ;

	while( ( de = ::readdir( d ) ) != 0 )
	{
		string name = de->d_name ;
		if ( name != CurrentDirectoryAlias 
				&& name != CurrentDirectoryAlias )
			entries.push_back( de->d_name ) ;
	}

	/*
	 * errno could be checked here :
	 
	if ( errno != 0 )
		throw DirectoryException( "Directory::getEntries : read : "
			+ explainError() ) ;
	 *
	 */

	if ( ::closedir( d ) == -1 )
		throw DirectoryException( "Directory::getEntries : close : "
			+ explainError() ) ;
	
#else // CEYLAN_USES_DIRENT_H
	
	// @portme Use FindFirstFile(), FindNextFile(), and FindClose() Win32 API functions.

	throw DirectoryException( "Directory::getEntries : "
		"not available on this platform." ) ;
	
#endif // CEYLAN_USES_DIRENT_H

}


void Directory::goDown( const string & path ) throw( ChangeDirectoryFailed )
{

	if ( hasEntry( path ) )
	{
		_path += path ;
		return ;
	}
	throw ChangeDirectoryFailed( path ) ;
	
}


const string Directory::toString( Ceylan::VerbosityLevels level ) const throw()
{
	return "Directory refers to " + _path ;
}


bool Directory::IsAValidDirectoryName( const string & directoryString ) throw()
{

#if CEYLAN_USES_REGEX

	string directoryPattern = "^[" + RootDirectoryPrefix 
		+ Separator + "]{1,1}" ;

	Ceylan::RegExp target( directoryString ) ;

	return target.matches( directoryPattern ) ;
	
#else // CEYLAN_USES_REGEX

	// A priori correct :
	return true ;
	
#endif // CEYLAN_USES_REGEX

}


void Directory::removeLeadingSeparator() throw()
{
	RemoveLeadingSeparator( _path ) ;
}


void Directory::RemoveLeadingSeparator( std::string & path ) throw()
{
	if ( path[ path.size() - 1 ] == Separator )
		path.erase( path.size() - 1, 1 ) ;
}


bool Directory::IsAbsolutePath( const string & path ) throw()
{

	if ( path.size() == 0 )
		return false ;

	/*
	 * Starts with separator, or with prefix (if prefix is used) :
	 * absolute path.
	 *
	 */
#if CEYLAN_RUNS_ON_WINDOWS

	/* Was :

	if ( ! RootDirectoryPrefix.empty() )
		if ( path.find( RootDirectoryPrefix, 0 ) == 0 )
			return true ;
	 */

	// Prefix is : a drive letter + ':\', ex : 'c:\'
	if ( ( Ceylan::isLetter( path[0] ) ) && path[1] == ':' )
		return true ;

	return false ;

#else // CEYLAN_RUNS_ON_WINDOWS


	if ( path[0] == Separator )
		return true ;


	return false ;

#endif // CEYLAN_RUNS_ON_WINDOWS

}


string Directory::GetCurrentWorkingDirectoryName() throw( DirectoryException )
{

#ifdef CEYLAN_USES_GETCWD

	/*
	 * With following automatic variable, frame size is deemed 
	 * 'too large for reliable stack checking' :
	 
	char buf[ PATH_MAX + 1 ] ;
	 
	 * Another solution would be to use a static string, but this method 
	 * would not be reentrant anymore.
	 *
	 * Hence using dynamic allocation, even if slower :
	 *
	 */

	char * buf = new char[ PATH_MAX + 1 ] ;
	
	if ( ::getcwd( buf, PATH_MAX ) )
	{
		string res( buf ) ;
		delete buf ;
		
		return res ;
		
	}
	else
	{
		delete buf ;
	
		throw DirectoryException( "GetCurrentWorkingDirectoryName : "
			"unable to determine current directory : "
		 	+ explainError() ) ;
	}		
		 
#else // CEYLAN_USES_GETCWD

#ifdef CEYLAN_USES__GETCWD


	/*
	 * With following automatic variable, frame size is deemed 
	 * 'too large for reliable stack checking' :
	 
	char buf[ PATH_MAX + 1 ] ;
	 
	 * Another solution would be to use a static string, but this method 
	 * would not be reentrant anymore.
	 *
	 * Hence using dynamic allocation, even if slower :
	 *
	 */

	char * buf = new char[ PATH_MAX + 1 ] ;
	
	if ( ::_getcwd( buf, PATH_MAX ) )
	{
		string res( buf ) ;
		delete buf ;
		
		return res ;
		
	}
	else
	{
		delete buf ;
	
		throw DirectoryException( "GetCurrentWorkingDirectoryName : "
			"unable to determine current directory : "
		 	+ explainError() ) ;
	}		

#else // CEYLAN_USES__GETCWD

	throw DirectoryException( "GetCurrentWorkingDirectoryName : "
		"not available on this platform" ) ;

#endif // CEYLAN_USES__GETCWD

#endif // CEYLAN_USES_GETCWD		
 
}


void Directory::ChangeWorkingDirectory( const string & newWorkingDirectory )
	throw( DirectoryException )
{

#ifdef CEYLAN_USES_CHDIR

	if ( ::chdir( newWorkingDirectory.c_str() ) != 0 )

#else // CEYLAN_USES_CHDIR

#ifdef CEYLAN_USES__CHDIR

	if ( ::_chdir( newWorkingDirectory.c_str() ) != 0 )

#else // CEYLAN_USES__CHDIR

	throw DirectoryException( "Ceylan::Directory::ChangeWorkingDirectory : "
			"not supported on this platform" ) ;

#endif // CEYLAN_USES__CHDIR
		throw DirectoryException( "Ceylan::Directory::ChangeWorkingDirectory : "
			"unable to change current working directory to "
			+ newWorkingDirectory + " : " + explainError() ) ;

#endif // CEYLAN_USES_CHDIR

}


list<string> Directory::SplitPath( const string & path ) throw()
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

	// Extract each part of the specified path thanks to separators :
	for( string::const_iterator it = path.begin(); it != path.end(); it++ )
	{
		if ( *it == Separator )
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


string Directory::JoinPath( const list<string> & pathElements ) throw()
{

	list<string>::const_iterator it = pathElements.begin() ;
	string res = (*it) ;
	it++ ;

	while ( it != pathElements.end() )
	{
		res += Separator + (*it ) ;
		it++ ;
	}

	return res ;

}


string Directory::JoinPath( const string & firstPath, 
	const std::string & secondPath ) throw()
{

	return firstPath + Separator + secondPath ;

}



void Directory::StripFilename( const string & path, string * base, 
	string * file ) throw()
{

	if ( base != 0 )
		base->erase() ;

	if ( file != 0 )
		file->erase() ;

	string::size_type p ;

	// Finds first separator, starting from right to left :
	if ( ( p = path.rfind( Separator ) ) != string::npos )
	{
		// Base is the characters between begin and last separator :
		if ( base != 0 )
			base->assign( path, 0, p ) ;

		// File is the leading part :
		if ( file != 0 )
			file->assign( path, p+1, path.size() - p - 1 ) ;
	}
	else
	{
		// No separator : no path, only file.
		if ( file != 0 )
			*file = path ;
	}
}


bool Directory::Exists( const string & path ) throw( DirectoryException )
{

	CEYLAN_LOG( "Directory::Exists called for " + path ) ;

#ifdef CEYLAN_USES_STAT

	struct stat buf ;
	return ::stat( path.c_str(), & buf ) == 0 && S_ISDIR( buf.st_mode ) ;

#else // CEYLAN_USES_STAT

#ifdef CEYLAN_USES__STAT

	struct _stat buf ;

	if ( ::_stat( path.c_str(), & buf ) == 0 )
	{

		CEYLAN_LOG( "Directory::Exists : " + path + " is a directory entry" ) ;

		if ( buf.st_mode & _S_IFDIR )
		{

			CEYLAN_LOG( "Directory::Exists : " + path + " exists and is a directory" ) ;
			return true ;

		}
		else
		{

			CEYLAN_LOG( "Directory::Exists : " + path + " exists but is not a directory" ) ;
			return false ;

		}
	}
	else
	{

		CEYLAN_LOG( "Directory::Exists : " + path + " is not a directory entry" ) ;
		return false ;

	}

#else // CEYLAN_USES__STAT

	throw DirectoryException( "Directory::Exists : "
			"operation not supported on this platform." ) ;

#endif // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}


void Directory::Create( const string & name ) throw( Directory::CouldNotCreate )
{
	Directory d( name ) ;
}


void Directory::Remove( const string & name, bool recursive ) 
	throw( Directory::CouldNotRemove )
{

#ifdef CEYLAN_USES_RMDIR


// The case without stat and _stat not managed :

#ifdef CEYLAN_USES_STAT

	static struct stat buf ;

#else // CEYLAN_USES__STAT

	static struct _stat buf ;

#endif // CEYLAN_USES__STAT

	if ( name.empty() )
		throw CouldNotRemove( "(void directory specified)" ) ;

	string path = name ;

	RemoveLeadingSeparator( path ) ;


	if ( recursive )
	{

		Directory d( path, DoNotCreate ) ;

		list<string> nodes ;
		d.getEntries( nodes ) ;

		for ( list<string>::const_iterator it = nodes.begin(); 
			it != nodes.end(); it++ )
		{

			string newPath = path + Separator + *it ;

#ifdef CEYLAN_USES_STAT

			if ( ::stat( newPath.c_str(), & buf ) == 0 )

#else // CEYLAN_USES_STAT

			if ( ::_stat( newPath.c_str(), & buf ) == 0 )

#endif // CEYLAN_USES_STAT

			{
			
				// Unlinks symlinks and files :
#if CEYLAN_USES_SYMBOLIC_LINKS
				if ( S_ISLNK( buf.st_mode ) || ! S_ISDIR( buf.st_mode ) )
#else // CEYLAN_USES_SYMBOLIC_LINKS

#ifdef CEYLAN_ARCH_WINDOWS
				if ( ! ( buf.st_mode & _S_IFDIR ) )
#else // CEYLAN_ARCH_WINDOWS
				if ( ! S_ISDIR( buf.st_mode ) )
#endif // CEYLAN_ARCH_WINDOWS

#endif // CEYLAN_USES_SYMBOLIC_LINKS
				{


#ifdef CEYLAN_USES_UNLINK
					if ( ::unlink( newPath.c_str() ) )
#else // CEYLAN_USES_UNLINK
					if ( ::_unlink( newPath.c_str() ) )
#endif // CEYLAN_USES_UNLINK
						throw CouldNotRemove( newPath +  " : " 
							+ explainError() ) ;
				}
				else

				// Deletes directories :
#ifdef CEYLAN_ARCH_WINDOWS
				if ( buf.st_mode & _S_IFDIR )
#else // CEYLAN_ARCH_WINDOWS
				if ( S_ISDIR( buf.st_mode ) )
#endif // CEYLAN_ARCH_WINDOWS
				{
					// Recursive call.
					Remove( newPath ) ;
				}
			}
			else
			{
				throw CouldNotRemove( newPath ) ;
			}
		}

		if ( ::rmdir( path.c_str() ) )
			throw CouldNotRemove( path +  " : " + explainError() ) ;
	}
	else // not a recursive remove
	{
		if ( ::rmdir( path.c_str() ) )
			throw CouldNotRemove( path + " : " + explainError() ) ;
	}

#else // CEYLAN_USES_RMDIR

	throw CouldNotRemove( "(not available on this platform)" ) ;

#endif // CEYLAN_USES_RMDIR


}


time_t Directory::GetEntryChangeTime( const string & name ) 
	throw( DirectoryException )
{


#ifdef CEYLAN_USES_STAT
	
	struct stat buf ;

	if ( ::stat( name.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

#else // CEYLAN_USES_STAT

#if CEYLAN_USES__STAT

	struct _stat buf ;

	if ( ::_stat( name.c_str(), & buf ) == 0 )
		return buf.st_ctime ;

#else // CEYLAN_USES__STAT

	throw DirectoryException( "Directory::GetEntryChangeTime : "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

	throw DirectoryException( "Directory::GetEntryChangeTime : "
		"unable to get last change time for entry " + name + "." ) ;

}

