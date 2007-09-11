#include "Ceylan.h"
using namespace Ceylan::System ;
using namespace Ceylan::Log ;


#include <iostream>
using std::cerr ;
using std::endl ;

#include <string>
using std::string ;

#include <list>
using std::list ;

#include <exception>




/**
 * Test of File and Directory classes.
 *
 * @see File, Directory, FileSystemManager and Holder template.
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder myLog( argc, argv ) ;


    try
    {

        LogPlug::info( "Testing File and Directory's implementations." ) ;

		const string absolutePath          = "/home/bob/wailers" ;		
		const string relativePath          = "three/little/birds" ;
		const string platformDependantPath = "c:\\Program Files\\redmond.com" ;
		const string invalidPath           = "something really weird" ;
		
		if ( ! Ceylan::Features::areRegularExpressionsSupported() )
			LogPlug::warning( "No regular expression support available, "
				"invalid paths may be deemed valid." ) ;
		
		LogPlug::info( "Testing whether '" + absolutePath 
			+ "' is a valid directory name: " + Ceylan::toString( 
				Directory::IsAValidDirectoryPath( absolutePath ) ) ) ;		
				
		LogPlug::info( "Testing whether '" + relativePath 
			+ "' is a valid directory name: " + Ceylan::toString(
				Directory::IsAValidDirectoryPath( relativePath ) ) ) ;
			
		LogPlug::info( "Testing whether '" + platformDependantPath 
			+ "' is a valid directory name: " + Ceylan::toString(
				Directory::IsAValidDirectoryPath( platformDependantPath ) ) ) ;
			
		LogPlug::info( "Testing whether '" + invalidPath 
			+ "' is a valid directory name: " + Ceylan::toString( 
				Directory::IsAValidDirectoryPath( invalidPath ) ) ) ;
			
		
		LogPlug::info( "Testing whether '" + absolutePath 
			+ "' is an absolute path: " 
			+ Ceylan::toString( Directory::IsAbsolutePath( absolutePath ) ) ) ;
			
		LogPlug::info( "Testing whether '" + relativePath 
			+ "' is an absolute path: " 
			+ Ceylan::toString( Directory::IsAbsolutePath( relativePath ) ) ) ;
			
		LogPlug::info( "Testing whether '" + platformDependantPath 
			+ "' is an absolute path: " 
			+ Ceylan::toString( 
				Directory::IsAbsolutePath( platformDependantPath ) ) ) ;
			
					
		Ceylan::Holder<Directory> d( Directory::Open() ) ;
		
        LogPlug::info( "By default a directory points towards "
			"current working directory: " + d->toString() ) ;
		
		if ( ! Directory::Exists( d->getPath() ) )
			throw Ceylan::TestException( "Current directory (" + d->toString() 
				+ ") is reported not to exist." ) ; 
		else
			LogPlug::info( "Current directory exists indeed." ) ;

		string testDir( FileSystemManager::GetRootDirectoryPrefix() ) ;
		
		LogPlug::info( "Does '" + testDir + "' exist ? " 
			+ Ceylan::toString( Directory::Exists( testDir ) ) ) ;

		testDir = FileSystemManager::GetRootDirectoryPrefix() 
			+ FileSystemManager::GetSeparator() ;
			
		LogPlug::info( "Does '" + testDir + "' exist ? " 
			+ Ceylan::toString( Directory::Exists( testDir ) ) ) ;

		// Test for english-based Windows systems:
		testDir = "c:\\Documents and Settings" ;
		
		LogPlug::info( "Does '" + testDir + "' exist ? " 
			+ Ceylan::toString( Directory::Exists( testDir ) ) ) ;


		// One may disable the directory removing at the end and run it again:
		string newDir = Directory::JoinPath( d->getPath(), "tmp" ) ;
			
		LogPlug::info( "Testing whether '" + newDir + "' exists..." ) ;
		
		bool dirExistedAlready = false ;
		
		if ( Directory::Exists( newDir ) )
		{
		
			LogPlug::info( "...yes" ) ;
			dirExistedAlready = true ;
			Directory * temp = & Directory::Open( newDir ) ;
			delete temp ;
			
		}
		else
		{
		
			LogPlug::info( "...no, creating it" ) ;	
			Directory * temp = & Directory::Create( newDir ) ;
			
			if ( ! Directory::Exists( newDir ) )
				throw Ceylan::TestException( 
					"Unable to create directory '" + newDir + "'." ) ;
			delete temp ;
					
		}		
		
		string targetFile = Directory::JoinPath( newDir, "Ceylan-rulez.txt" ) ;
		
		if ( File::Exists( targetFile ) )
		{
		
			LogPlug::info( "There already exists a directory entry for file "
				+ targetFile + ", removing it." ) ;
				
			File::Remove( targetFile ) ;	
			
		} 
		else
		{
			LogPlug::info( "There is no file named " + targetFile ) ;		
		}
		
		LogPlug::info( "Creating a new empty file named " + targetFile + "." ) ;
		
		Ceylan::Holder<File> newFileHolder( File::Create( targetFile ) ) ;
		
		LogPlug::info( "Updating thanks to touch the last "
			"access and modification times of file " + targetFile ) ;
			
		File::Touch( targetFile	) ;
		
		LogPlug::info( "Cleaning up: deleting file " + targetFile ) ;
		newFileHolder->remove() ;
		
		if ( ! dirExistedAlready )
		{
		
			LogPlug::info( "Cleaning up: deleting directory " + newDir
				+ " which should be empty now." ) ;
			Directory::Remove( newDir /* not recursive */) ;
			
		}
		
		const string name1 = "ASimpleName" ;
		
		LogPlug::info( "Transformation of '" + name1 
				+ "' into a (supposed) valid file name gives: '"
				+ File::TransformIntoValidFilename( name1 ) + "'." ) ;
		
		const string name2 = "A name with spaces" ;
		LogPlug::info( "Transformation of '" + name2 
				+ "' into a (supposed) valid file name gives: '"
				+ File::TransformIntoValidFilename( name2 ) + "'." ) ;
				
		const string name3 = "A / very ugly\\\\name" ;
		LogPlug::info( "Transformation of '" + name3 
				+ "' into a (supposed) valid file name gives: '"
				+ File::TransformIntoValidFilename( name3 ) + "'." ) ;
					
		const string firstPath = "/home/luke" ;
		const string secondPath = "deathstar/plan" ;
					
		LogPlug::info( "Directory::JoinPath( " + firstPath + "," 
			+ secondPath + ") returns "
			+ Directory::JoinPath( firstPath, secondPath ) ) ;
		
		
		list<string> toJoin ;
		toJoin.push_back( "" ) ;
		toJoin.push_back( "mnt" ) ;
		toJoin.push_back( "raid" ) ;
		toJoin.push_back( "md0" ) ;
		toJoin.push_back( "LOANI-0.3" ) ;
		
		string joined = Directory::JoinPath( toJoin ) ;
		LogPlug::info( 
			"Directory::JoinPath( [ '', 'mnt', 'raid', 'md0', 'LOANI-0.3' ] ) "
			" returns "	+ joined ) ;
		
		LogPlug::info( "Directory::SplitPath( " + joined 
			+ ") should return a void first element as expected: " 
			+ Ceylan::formatStringList( Directory::SplitPath( joined ) ) ) ;   
		
		
		// 'StripFilename' test.
		
		string first, second ;
		Directory::StripFilename( joined, & first, & second ) ;
		LogPlug::info( "StripFilename applied to '" + joined + "' returns '"
			+ first + "' and '" + second + "'." ) ; 
			
		LogPlug::info( "StripFilename applied to '" + joined 
			+ "' with first pointer being null returns, as 'basename', '"
			+ second + "'." ) ; 
		
		Directory::StripFilename( joined, & first, 0 ) ;
		LogPlug::info( "StripFilename applied to '" + joined 
			+ "' with second pointer being null returns, as 'dirname', '"
			+ first + "'." ) ; 
		
        LogPlug::info( "End of File and Directory test." ) ;


    }

    catch ( const Ceylan::Exception & e )
    {
        LogPlug::error( "Ceylan exception caught: "
        	 + e.toString( Ceylan::high ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        LogPlug::error( "Standard exception caught: " 
			 + std::string( e.what() ) ) ;
       	return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        LogPlug::error( "Unknown exception caught" ) ;
       	return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}

