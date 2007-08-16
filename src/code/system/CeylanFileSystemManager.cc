#include "CeylanFileSystemManager.h"

#include "CeylanLibfatFileSystemManager.h"    // for LibfatFileSystemManager
#include "CeylanStandardFileSystemManager.h"  // for StandardFileSystemManager


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"           // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H




using std::string ;

using namespace Ceylan::System ;


FileSystemManager * FileSystemManager::_DefaultFileSystemManager = 0 ;



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
	
		
	
	
		
	

					
const string FileSystemManager::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Abstract filesystem manager" ;
	
}




// Static section.
	

FileSystemManager & FileSystemManager::GetDefaultFileSystemManager() 
	throw( FileSystemManagerException )
{


#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw FileSystemManagerException( 
		"Ceylan::System::FileSystemManager::GetDefaultFileSystemManager : 
		"only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

	if ( _DefaultFileSystemManager == 0 )
		_DefaultFileSystemManager = new LibfatFileSystemManager() ;

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

