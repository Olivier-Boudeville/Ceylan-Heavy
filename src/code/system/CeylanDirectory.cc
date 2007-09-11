#include "CeylanDirectory.h"


#include "CeylanFileSystemManager.h" // for FileSystemManager
#include "CeylanOperators.h"         // for toString


/*
 * The File factories (Create/Open) have to know all the Directory subclasses 
 * to be able to instantiate them:
 *
 */
 
#include "CeylanStandardDirectory.h" // for StandardDirectory
//#include "CeylanLibfatDirectory.h"   // for LibfatDirectory



#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H



/*
 * Implementation notes.
 *
 * @note All internal paths kept in directory references (this object)
 * should be absolute paths.
 *
 * Non-static methods must not use static methods, as they may use different
 * filesystem managers.
 *
 */
 
using std::string ;
using std::list ;

using namespace Ceylan::System ;



DirectoryDelegatingException::DirectoryDelegatingException( 
		const string & reason ) throw():
	DirectoryException( reason )
{

}




// Directory implementation section.



// Static section.


bool Directory::Exists( const string & directoryPath ) 
	throw( DirectoryException )
{

	// Let DirectoryLookupFailed and DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().existsAsDirectory(
		directoryPath ) ;
			
}


void Directory::Remove( const string & directoryPath, bool recursive ) 
	throw( DirectoryException )
{

	// Let DirectoryRemoveFailed and DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().removeDirectory(
		directoryPath, recursive ) ;

}


void Directory::Move( const string & sourceDirectoryname,
	const string & targetDirectoryname ) throw( DirectoryException )
{

	// Let DirectoryMoveFailed and DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().moveDirectory(
		sourceDirectoryname, targetDirectoryname ) ;

}	


void Directory::Copy( const std::string & sourceDirectoryname,
	const std::string & targetDirectoryname ) throw( DirectoryException )
{

	// Let DirectoryCopyFailed and DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().copyDirectory(
		sourceDirectoryname, targetDirectoryname ) ;

}	
	

time_t Directory::GetLastChangeTime( const std::string & directoryPath ) 
	throw( DirectoryException )
{

	/*
	 * Let DirectoryLastChangeTimeRequestFailed and 
	 * DirectoryDelegatingException propagate:
	 *
	 */
	return GetCorrespondingFileSystemManager().getLastChangeTimeDirectory(
		directoryPath ) ;

}	
	
																
bool Directory::IsAValidDirectoryName( const string & directoryString ) 
	throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().isAValidDirectoryPath( 
		directoryString ) ;
				
}

	
void Directory::RemoveLeadingSeparator( std::string & path ) 
	throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	GetCorrespondingFileSystemManager().removeLeadingSeparator( path ) ;
	
}
					
	
bool Directory::IsAbsolutePath( const string & path ) 
	throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().isAbsolutePath( path ) ;
			
}


string Directory::GetCurrentWorkingDirectoryName() throw( DirectoryException )
{

	// Let DirectoryGetCurrentFailed and DirectoryDelegatingException propagate:
	return 
		GetCorrespondingFileSystemManager().getCurrentWorkingDirectoryPath() ;
		
}


void Directory::ChangeWorkingDirectory( const string & newWorkingDirectory )
	throw( DirectoryException )
{

	// Let DirectoryChangeFailed and DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().changeWorkingDirectory(
		newWorkingDirectory ) ;
		
}



list<string> Directory::SplitPath( const string & path ) 
	throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().splitPath( path ) ;

}


string Directory::JoinPath( const list<string> & pathElements ) 
	throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().joinPath( pathElements ) ;

}


string Directory::JoinPath( const string & firstPath, 
	const std::string & secondPath ) throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().joinPath( firstPath,
		secondPath ) ;

}


void Directory::StripFilename( const string & path, string * base, 
	string * file ) throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().stripFilename( path, base ) ;

}


Ceylan::Latin1Char Directory::GetSeparator() throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	return GetCorrespondingFileSystemManager().getSeparator() ; 

}


string Directory::GetSeparatorAsString() throw( DirectoryException )
{

	// Let DirectoryDelegatingException propagate:
	return Ceylan::toString( GetSeparator() ) ;

}



// Constructors are in protected section.	


// Factory subsection.


Directory & Directory::Create( const string & newDirectoryName ) 
	throw( DirectoryException )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw SystemException( 
		"Ceylan::System::Directory::Create: only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

FIXME

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS

	// We are on classical computers, let's use the OS-provided facilities:	
	return StandardDirectory::Create( newDirectoryName ) ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS


}
	
					
Directory & Directory::Open( const string & directoryName ) 
	throw( DirectoryException )
{

#if CEYLAN_ARCH_NINTENDO_DS
		
#ifdef CEYLAN_RUNS_ON_ARM7

	throw SystemException( 
		"Ceylan::System::Directory::Create: only available on the ARM9." ) ;

#elif defined(CEYLAN_RUNS_ON_ARM9)

FIXME

#endif // CEYLAN_RUNS_ON_ARM7

	
#else // CEYLAN_ARCH_NINTENDO_DS

	// We are on classical computers, let's use the OS-provided facilities:	
	return StandardDirectory::Open( directoryName ) ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS

}
	
	

	
Directory::~Directory() throw()
{

	// Nothing special here for abstract directories.

}





					
/*
 * Instance methods.
 *
 * Default implementations from this abstract Directory can be overriden by
 * tuned specialized implementations if necessary.
 *
 */



void Directory::goDown( const string & subdirectoryName ) 
	throw( DirectoryChangeFailed )
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
			
				throw DirectoryChangeFailed( 
					"Directory::goDown failed for subdirectory '"
					+ subdirectoryName + "' from '" + _path + "': '"
					+ candidate + "' is not a directory." ) ;
			}
				
		}	
		catch( const DirectoryLookupFailed & e )
		{
		
			throw DirectoryChangeFailed( 
				"Directory::goDown failed for subdirectory '"
				+ subdirectoryName + "' from '" + _path + "': " 
				+ e.toString() ) ;
		}		
		catch( const DirectoryDelegatingException & e )
		{
		
			throw DirectoryChangeFailed( "Directory::goDown failed: " 
				+ e.toString() ) ;
		}		

	}
	
	throw DirectoryChangeFailed( 
		"Directory::goDown failed for non-existent subdirectory '"
		+ subdirectoryName + "' from '" + _path ) ;
	
}


bool Directory::isValid() const throw( DirectoryException )
{

	/*
	 * Should not call the static version, as it may use a different filesystem
	 * manager:
	 *
	 */
	return getCorrespondingFileSystemManager().existsAsDirectory( _path ) ;

}


const std::string & Directory::getPath() const throw()
{

	return _path ;
	
}


void Directory::removeLeadingSeparator() throw( DirectoryException )
{
	
	/*
	 * Should not call the static version, as it may use a different filesystem
	 * manager:
	 *
	 */
	return getCorrespondingFileSystemManager().removeLeadingSeparator( _path ) ;

}


const string Directory::toString( Ceylan::VerbosityLevels level ) const throw()
{

	return "Abstract directory referring to path '" + _path + "'" ;
	
}








// Protected section.


Directory::Directory( const string & directoryName ) 
		throw( DirectoryException ):
	_path( directoryName )
{

	// Enforce the convention on _path:
	removeLeadingSeparator() ;
	
}



FileSystemManager & Directory::GetCorrespondingFileSystemManager()
	throw( DirectoryDelegatingException )
{

	try
	{
	
		return FileSystemManager::GetAnyDefaultFileSystemManager() ;
	
	}
	catch( const FileSystemManagerException & e )
	{
		throw DirectoryDelegatingException(
			"Directory::GetCorrespondingFileSystemManager failed: "
			+ e.toString() ) ;
	}
	
	
}

