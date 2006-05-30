#include "CeylanFileLocator.h"

#include "CeylanFile.h"                  // for ExistsAsFileOrSymbolicLink
#include "CeylanDirectory.h"             // for JoinPath
#include "CeylanStringUtils.h"           // for formatStringList
#include "CeylanLogPlug.h"               // for LogPlug
#include "CeylanEnvironmentVariables.h"  // for getEnvironmentVariable

using std::string ;
using std::list ;

using namespace Ceylan::System ;
using namespace Ceylan::Log ;




FileLocatorException::FileLocatorException( const std::string & message ) throw() :
	SystemException( message )
{

}


FileLocatorException::~FileLocatorException() throw()
{

}



FileLocator::FileLocator() throw() :
	_paths()
{

}


FileLocator::FileLocator( const string & variableName, char separator ) throw() :
	_paths()
{
	addPathsFromEnvironmentVariable( variableName, separator ) ;
} 


FileLocator::~FileLocator() throw()
{

}


bool FileLocator::addPath( const string & newPath ) throw() 
{
	
	for ( list<string>::const_iterator it = _paths.begin(); it != _paths.end(); it++ )
		if ( (*it) == newPath )
			return false ;
			
	_paths.push_back( newPath ) ;
	
	return true ;		
}


bool FileLocator::addPaths( const std::list<std::string> & paths ) throw() 
{
	
	bool res = false ;
	
	for ( list<string>::const_iterator it = paths.begin(); it != paths.end(); it++ )
		res |= addPath( (*it ) ) ;
	
	return res ;		
	
}


bool FileLocator::addPathsFromEnvironmentVariable( const std::string & variableName,
	char separator ) throw()
{	
	return addPaths( Ceylan::split( getEnvironmentVariable( variableName ), separator ) ) ;
}

	
bool FileLocator::removePath( const string & pathToRemove ) throw()
{

	for ( list<string>::const_iterator it = _paths.begin(); it != _paths.end() ; it++ )
		if ( (*it) == pathToRemove )
		{
			_paths.remove( pathToRemove ) ;
			return true ;
		}	
	
	return false ;		
	
}


string FileLocator::find( const string & filename ) const throw( FileLocatorException ) 
{

	string fullPath ;
	
	for ( list<string>::const_iterator it = _paths.begin(); it != _paths.end(); it++ )
	{
		fullPath = Directory::JoinPath( (*it), filename ) ;
		
		//LogPlug::debug( "FileLocator::find : testing '" + fullPath + "'." ) ;
		
		if ( File::ExistsAsFileOrSymbolicLink( fullPath ) )
			return fullPath ;
	}	
	
	throw FileLocatorException( "File '" + filename 
		+ "' could not be found through following Locator : " + toString() ) ;
		
}


const string FileLocator::toString( Ceylan::VerbosityLevels level ) const throw()
{
	
	if ( _paths.empty() ) 
		return "Empty File locator" ;

	return "File locator with following registered directories : "
		+ Ceylan::formatStringList( _paths ) ;
				 
}
