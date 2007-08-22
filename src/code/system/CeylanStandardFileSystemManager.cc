#include "CeylanStandardFileSystemManager.h"



#include "CeylanStandardFile.h"      // for StandardFile
#include "CeylanStringUtils.h"       // for StringSize
#include "CeylanOperators.h"         // for toString
#include "CeylanRegularExpression.h" // for RegExp


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


#include <fstream>   // for this class
using std::ifstream ;
using std::ios ;

using std::string ;


// Not available in their C++ form:
extern "C"
{


#ifdef CEYLAN_USES_SYS_STAT_H
#include <sys/stat.h>          // for mode_t
#endif // CEYLAN_USES_SYS_STAT_H

#ifdef CEYLAN_USES_UTIME_H
#include <utime.h>             // for utime     
#endif // CEYLAN_USES_UTIME_H

#ifdef CEYLAN_USES_SYS_UTIME_H
#include <sys/utime.h>         // for utime     
#endif // CEYLAN_USES_SYS_UTIME_H


}



using namespace Ceylan::System ;


StandardFileSystemManager *
	StandardFileSystemManager::_StandardFileSystemManager = 0 ;


#if CEYLAN_ARCH_WINDOWS

// "c:" is a drive, "c:\\" is a directory.
const string StandardFileSystemManager::RootDirectoryPrefix   = "c:" ;
const Ceylan::Latin1Char StandardFileSystemManager::Separator = '\\' ;

#else // CEYLAN_ARCH_WINDOWS

const string StandardFileSystemManager::RootDirectoryPrefix   = ""   ;
const Ceylan::Latin1Char StandardFileSystemManager::Separator = '/'  ;

#endif // CEYLAN_ARCH_WINDOWS



// Existence test section.


bool StandardFileSystemManager::existsAsFileOrSymbolicLink( 
	const string & filename ) const throw( CouldNotStatEntry )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;


#if CEYLAN_USES_SYMBOLIC_LINKS

	// Regular file or link:
	return ( ::stat( filename.c_str(), & buf ) == 0 
		&& ( S_ISREG( buf.st_mode ) || S_ISLNK( buf.st_mode ) ) ) ;
		
#else // CEYLAN_USES_SYMBOLIC_LINKS 

	// Regular file:
	return ( ::stat( filename.c_str(), & buf ) == 0 
		&& S_ISREG( buf.st_mode ) ) ;
	
#endif // CEYLAN_USES_SYMBOLIC_LINKS

#else // CEYLAN_USES_STAT

#ifdef CEYLAN_USES__STAT
	
	struct _stat buf ;

	return ( ::_stat( filename.c_str(), & buf ) == 0 
		&& ( buf.st_mode & _S_IFREG ) ) ;

#else // CEYLAN_USES__STAT

	throw CouldNotStatEntry(
		"StandardFileSystemManager::existsAsFileOrSymbolicLink: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT
	 	 
	 
}



bool StandardFileSystemManager::existsAsDirectory( 
	const string & directoryPath ) const throw( CouldNotStatEntry )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;
	
	return ::stat( directoryPath.c_str(), & buf ) == 0 
		&& ( S_ISDIR( buf.st_mode ) ) ;

#else // CEYLAN_USES_STAT

#ifdef CEYLAN_USES__STAT

	struct _stat buf ;

	if ( ::_stat( directoryPath.c_str(), & buf ) == 0 )
	{


		if ( buf.st_mode & _S_IFDIR )
		{

			CEYLAN_LOG( "StandardFileSystemManager::existsAsDirectory: "
				+ directoryPath + " exists and is a directory" ) ;
				
			return true ;

		}
		else
		{

			CEYLAN_LOG( "StandardFileSystemManager::existsAsDirectory: " 
				+ directoryPath + " exists but is not a directory" ) ;
				
			return false ;

		}
		
	}
	else
	{

		CEYLAN_LOG( "StandardFileSystemManager::existsAsDirectory: " 
			+ directoryPath + " is not a directory entry" ) ;
			
		return false ;

	}

#else // CEYLAN_USES__STAT

	throw CouldNotStatEntry( "StandardFileSystemManager::existsAsDirectory: "
			"operation not supported on this platform." ) ;

#endif // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT

}



bool StandardFileSystemManager::existsAsEntry( const string & entryPath ) 
	const throw( CouldNotStatEntry )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;
	
	return ( ::stat( entryPath.c_str(), & buf ) == 0 ) ;
	
#else // CEYLAN_USES_STAT

#ifdef CEYLAN_USES__STAT

	struct _stat buf ;
	
	return ( ::_stat( entryPath.c_str(), & buf ) == 0 ) ;


#else // CEYLAN_USES__STAT

	throw CouldNotStatEntry( 
		"StandardFileSystemManager::existsAsEntry: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__STAT

#endif // CEYLAN_USES_STAT
	
}




void StandardFileSystemManager::createSymbolicLink( 
	const string & linkTarget, const string & linkName )
	throw( SymlinkFailed )
{

#if CEYLAN_USES_SYMBOLIC_LINKS

	if ( ::symlink( linkTarget.c_str(), linkName.c_str() ) == -1 )
		throw SymlinkFailed( "StandardFileSystemManager::createSymbolicLink "
			" failed when creating link '" + linkName 
			+ "' which was to point to '" + linkTarget 
			+ "': " + System::explainError() ) ;
		
#else // CEYLAN_USES_SYMBOLIC_LINKS

	throw SymlinkFailed( "StandardFileSystemManager::createSymbolicLink: "
		"symbolic link feature not available" ) ;
		
#endif // CEYLAN_USES_SYMBOLIC_LINKS


}


void StandardFileSystemManager::remove( const string & filename ) 
	throw( RemoveFailed )
{

#ifdef CEYLAN_USES_UNLINK	

	if ( ::unlink( filename.c_str() ) != 0 )
		throw RemoveFailed( "StandardFileSystemManager::remove failed for '"
			+ filename + "': " + System::explainError() ) ;
		
#else // CEYLAN_USES_UNLINK

#ifdef CEYLAN_USES__UNLINK	

	if ( ::_unlink( filename.c_str() ) != 0 )
		throw RemoveFailed( 
			"StandardFileSystemManager::remove failed: file '" + filename 
			+ "': " + System::explainError() ) ;

#else // CEYLAN_USES__UNLINK

	throw RemoveFailed( "StandardFileSystemManager::remove: "
		"not available on this platform." ) ;

#endif // CEYLAN_USES__UNLINK

#endif // CEYLAN_USES_UNLINK	

		
}


void StandardFileSystemManager::move( const string & sourceFilename, 
	const string & targetFilename ) throw( MoveFailed )
{


#ifdef CEYLAN_USES_RENAME

	// rename can move as well:
	
	if ( ::rename( sourceFilename.c_str(), targetFilename.c_str() ) == -1 )
		throw MoveFailed( 
			"StandardFileSystemManager::move failed when renaming '"
			+ sourceFilename + "' into '" + targetFilename + "': " 
			+ System::explainError() ) ;
		
#else // CEYLAN_USES_RENAME


	// Renaming is copying and then removing the source file:
	try
	{
	
		copy( sourceFilename, targetFilename ) ;
		remove( sourceFilename ) ;
		
	}
	catch( const FileManagementException & e )
	{
	
		throw MoveFailed( "StandardFileSystemManager::move failed: " 
			+ e.toString() ) ;
		
	}	
	

#endif // CEYLAN_USES_RENAME
		
}



void StandardFileSystemManager::copy( const string & sourceFilename, 
	const string & targetFilename ) throw( CopyFailed )
{


#if CEYLAN_USES_FILE_DESCRIPTORS
	
	
	// saveAs only available with file descriptors for the moment.
	
	try 
	{
	
		File & f = File::Open( sourceFilename ) ;
		f.saveAs( targetFilename ) ;
		f.close() ;
		
	} 
	catch ( const File::FileException & e )
	{
		throw CopyFailed( 
			"StandardFileSystemManager::copy failed when copying '"
			+ sourceFilename + "' to '" + targetFilename + "': " 
			+ e.toString() ) ;
	}
	
	
#else // if CEYLAN_USES_FILE_DESCRIPTORS	

	/*
	 * This is only a hack with C++ streams.
	 *
	 * Another hack would be:
	 *
	 * File & sourceFile = File::Open( sourceFilename ) ;
	 * File & targetFile = File::Open( targetFilename ) ;
	 *
	 * targetFile._fstream << sourceFile._fstream.rdbuf() ;
	 *
	 *  - or something adapted from (slower ?) - 
	 *
	 * while ( true) 
     * {
	 *    char c ;
	 *	  cin.get(c) ;
	 *	  if ( cin.fail() )
	 *	      break;
	 *	  cout << c ;
	 * }
	 *
	 */
	
	try 
	{

		Size fileSize = getSize( sourceFilename ) ;
	
		File & sourceFile = File::Open( sourceFilename ) ;
		
		// Not knowing the permissions to set with C++ Standard Library: 
		File & targetFile = File::Create( targetFilename, 
			File::CreateToWriteBinary, File::OwnerReadWrite ) ;
		
		Size written = 0 ;
	
		Size bufferSize = ( fileSize > File::BigBufferSize ? 
			File::BigBufferSize : fileSize ) ;

		char * buf = new char[ bufferSize ] ;

		SignedSize readCount ;

		while ( written < fileSize )
		{
		
			Size toRead = fileSize - written ;

			if ( toRead > bufferSize )
				toRead = bufferSize ;

			try
			{	
				
				readCount = static_cast<SignedSize>( 
					sourceFile.read( buf, toRead ) ) ;
					
			}
			catch( const File::ReadFailed & e )
			{
			
				delete [] buf ;
				
				throw CopyFailed( "StandardFileSystemManager::copy "
					"failed when copying '"	+ sourceFilename + "' to '" 
					+ targetFilename + "': " + e.toString() ) ;
					
			}


			try
			{	
				
				targetFile.write( buf, readCount ) ;
				
			}	
			catch( const File::WriteFailed & e )
			{
			
				delete [] buf ;
				
				throw CopyFailed( "StandardFileSystemManager::copy "
					"failed when copying '" + sourceFilename + "' to '" 
					+ targetFilename + "': " + e.toString() ) ;
					
			}

			written += readCount ;
		}

		delete [] buf ;
	
		targetFile.close() ;
		sourceFile.close() ;
		
	} 
	catch ( const SystemException & e )
	{
	
		throw CopyFailed( "StandardFileSystemManager::copy "
			"failed when copying '"	+ sourceFilename + "' to '" 
			+ targetFilename + "': " + e.toString() ) ;
					
	}
	
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	
 	
}



Size StandardFileSystemManager::getSize( const string & filename ) 
	throw( CouldNotStatEntry )
{

#ifdef CEYLAN_USES_STAT

	struct stat buf ;

	if ( ::stat( filename.c_str(), & buf ) == 0 )
		return static_cast<Size>( buf.st_size ) ;

	throw CouldNotStatEntry( "StandardFileSystemManager::getSize: "
		"could not stat file '"	+ filename + "': " + System::explainError() ) ;

#else // CEYLAN_USES_STAT


#if CEYLAN_USES_FILE_DESCRIPTORS == 0 

	// One more hack with C++ streams:
	
	ifstream tmpFile ;
	
	tmpFile.open( filename.c_str(), ios::binary ) ;

	if ( ( ! tmpFile.is_open() ) || ( ! tmpFile.good() ) )
		throw CouldNotStatEntry( 
			"StandardFileSystemManager::getSize: failed for '" + filename
			+ "': error opening file, " 
			+ StandardFile::InterpretState( tmpFile ) ) ;
				
	tmpFile.seekg( 0, ios::end ) ;
	
	Size size = static_cast<Size>( tmpFile.tellg() ) ;
	
	tmpFile.close() ;
	
	return size ;
	
#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw CouldNotStatEntry( "StandardFileSystemManager::getSize: "
		"not available on this platform." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS 

#endif // CEYLAN_USES_STAT

}



void StandardFileSystemManager::touch( const string & filename ) 
	throw( TouchFailed )
{

#ifdef CEYLAN_USES_UTIME

	if ( ::utime( filename.c_str(), 0 ) )
		throw TouchFailed( "StandardFileSystemManager::touch failed for '" 
			+ filename + "': " + System::explainError() ) ;
		
#else // CEYLAN_USES_UTIME

#ifdef CEYLAN_USES__UTIME

	if ( ::_utime( filename.c_str(), 0 ) == -1 )
		throw TouchFailed( "StandardFileSystemManager::touch failed for '" 
			+ filename + "': " + System::explainError() ) ;

#else // CEYLAN_USES__UTIME

	throw TouchFailed( "StandardFileSystemManager::touch "
		"not available on this platform." ) ;
	
#endif // CEYLAN_USES__UTIME

#endif // CEYLAN_USES_UTIME

}



const string StandardFileSystemManager::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{

	return "Standard filesystem manager" ;
	
}
	
	


// Directory-related section.


bool StandardFileSystemManager::isAValidDirectoryName( 
	const string & directoryString ) throw()
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
			
			
void StandardFileSystemManager::removeLeadingSeparator( string & path ) throw()
{

	if ( path[ path.size() - 1 ] == Separator )
		path.erase( path.size() - 1, 1 ) ;

}	
		
			
bool StandardFileSystemManager::isAbsolutePath( const string & path ) throw()
{

	if ( path.empty() )
		return false ;

	/*
	 * Starts with separator, or with prefix (if prefix is used) :
	 * absolute path.
	 *
	 */
	 
#if CEYLAN_ARCH_WINDOWS

	/* Was :

	if ( ! RootDirectoryPrefix.empty() )
		if ( path.find( RootDirectoryPrefix, 0 ) == 0 )
			return true ;
	 */

	// Prefix is : a drive letter + ':\', ex : 'c:\'
	if ( ( Ceylan::isLetter( path[0] ) ) && ( path[1] == ':' ) )
		return true ;

	return false ;

#else // CEYLAN_ARCH_WINDOWS


	if ( path[0] == Separator )
		return true ;


	return false ;

#endif // CEYLAN_ARCH_WINDOWS

}	
			
			
std::string StandardFileSystemManager::getCurrentWorkingDirectoryName()	
	throw( DirectoryOperationFailed )
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
		delete [] buf ;
		
		return res ;
		
	}
	else
	{
	
		delete [] buf ;
	
		throw DirectoryOperationFailed(
			"StandardFileSystemManager::getCurrentWorkingDirectoryName: "
			"unable to determine current directory: " + explainError() ) ;
			
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
		delete [] buf ;
		
		return res ;
		
	}
	else
	{
	
		delete [] buf ;
	
		throw DirectoryOperationFailed(
			"StandardFileSystemManager::getCurrentWorkingDirectoryName: "
			"unable to determine current directory: " + explainError() ) ;
			
	}		

#else // CEYLAN_USES__GETCWD

	throw DirectoryOperationFailed(
		"StandardFileSystemManager::getCurrentWorkingDirectoryName: "
		"not available on this platform" ) ;

#endif // CEYLAN_USES__GETCWD

#endif // CEYLAN_USES_GETCWD		

}	


void StandardFileSystemManager::changeWorkingDirectory( 
	const string & newWorkingDirectory ) throw( DirectoryOperationFailed )
{

#ifdef CEYLAN_USES_CHDIR

	if ( ::chdir( newWorkingDirectory.c_str() ) != 0 )

#else // CEYLAN_USES_CHDIR

#ifdef CEYLAN_USES__CHDIR

	if ( ::_chdir( newWorkingDirectory.c_str() ) != 0 )

#else // CEYLAN_USES__CHDIR

	throw DirectoryOperationFailed(
		"StandardFileSystemManager::changeWorkingDirectory: "
		"not supported on this platform" ) ;

#endif // CEYLAN_USES__CHDIR

#endif // CEYLAN_USES_CHDIR

	throw DirectoryOperationFailed(
		"StandardFileSystemManager::changeWorkingDirectory: "
		"unable to change current working directory to "
		+ newWorkingDirectory + ": " + explainError() ) ;

}	




// Filesystem constants.



const string & StandardFileSystemManager::getRootDirectoryPrefix() const throw()
{

	return RootDirectoryPrefix ;
	
}

	    	   					
Ceylan::Latin1Char StandardFileSystemManager::getSeparator() const throw()
{

	return Separator ;
	
}




// Static section.


StandardFileSystemManager &
		StandardFileSystemManager::GetStandardFileSystemManager() 
	throw( StandardFileSystemManagerException )
{

	if ( _StandardFileSystemManager == 0 )
		_StandardFileSystemManager = new StandardFileSystemManager() ;
	
	return *_StandardFileSystemManager ;	
	
}
	
	
	
void StandardFileSystemManager::RemoveStandardFileSystemManager() throw()
{

	if ( _StandardFileSystemManager != 0 )
	{
	
		delete _StandardFileSystemManager ;
		_StandardFileSystemManager = 0 ;
		
	}
	
}



StandardFileSystemManager::StandardFileSystemManager() 
	throw( StandardFileSystemManagerException )
{

	// Nothing special to initialize this filesystem.

}


StandardFileSystemManager::~StandardFileSystemManager() throw()
{

	// Nothing special to switch off this filesystem.

}







// Protected section.



FileDescriptor StandardFileSystemManager::Duplicate( FileDescriptor fd ) 
	throw( CouldNotDuplicate )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	FileDescriptor newFD = ::dup( fd ) ;

	if ( newFD == -1 )
		throw CouldNotDuplicate( 
			"StandardFileSystemManager::Duplicate failed for file descriptor "
			+ Ceylan::toString( fd ) + "." ) ;

	return newFD ;

#else

	throw CouldNotDuplicate( "StandardFileSystemManager::Duplicate: "
		"file descriptor feature not available." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS	

}



const string StandardFileSystemManager::TransformIntoValidFilename( 
	const string & rawFilename ) throw()
{

	// For MS-DOS/Windows, one may look at gcc 'dosck' as well.
	
	string result ;

	Ceylan::Uint32 characterCount = 0 ;

	// Remove all leading dots '.' :
	while ( rawFilename[ characterCount ] == '.' )
		characterCount++ ;

	// (preferred to : for( string::const_iterator it...)

	// Substitute any space " ", slash "/" or back-slash "\" by a dash "-" :

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
			 * This is not strictly needed on most system, but it is 
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

	
