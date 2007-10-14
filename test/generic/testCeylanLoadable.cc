#include "Ceylan.h"

#include <exception>

#include <string>
using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Log ;


/// This content example could be a native type, a struct, etc.
class MyContent: public Ceylan::TextDisplayable
{

	public:
	
		explicit MyContent( int i ) throw():
			_i( i )
		{
		
			LogPlug::trace( "Creating a MyContent with value " 
				+ Ceylan::toString( _i ) ) ;
			
		}
		
		
		virtual ~MyContent() throw()
		{
		
			LogPlug::trace( "Deleting a MyContent with value " 
				+ Ceylan::toString( _i ) ) ;
			
		}
		
		
		const string toString( Ceylan::VerbosityLevels level = Ceylan::high )
			const throw()
		{

			return "MyContent with value " + Ceylan::toString( _i ) ;
		
		}	
		
		
	protected:
	
		int _i ;	
		
} ;



/// Loads its MyContent instance when appropriate.
class MyOwnContent: public Ceylan::LoadableWithContent<MyContent>
{

	public:
	
		explicit MyOwnContent( const std::string & contentFilePath,
				bool preload = true ) throw():
			Ceylan::LoadableWithContent<MyContent>( contentFilePath )
		{

			LogPlug::trace( "Creating a MyOwnContent with path "
				+ getContentPath() ) ;
		
			if ( preload )
				load() ;
		
		}	
		
			
		virtual ~MyOwnContent() throw()
		{
		
			LogPlug::trace( "Deleting a MyOwnContent with path "
				+ getContentPath() ) ;
				
			if ( hasContent() )
				unload() ;
	
		}


		virtual bool load() throw( LoadableException )
		{
		
			LogPlug::trace( "MyOwnContent::load" ) ;
			
			if ( _content == 0 )
			{
			
				// Simulates loading:
				_content = new MyContent( 14 ) ;
				return true ;
			
			}
			
			return false ;	
			
		}
		
		
		virtual bool unload() throw( LoadableException )
		{
		
			LogPlug::trace( "MyOwnContent::unload" ) ;

			if ( _content != 0 )
			{
			
				/*
				 * Other unload implementations may have to call library
				 * functions, delete[], ::free, etc.
				 *
				 */
				delete _content ;
				_content = 0 ;
				return true ;
			
			}
			
			return false ;	
			
		}
		
		
} ;




/**
 * Test of Loadable facility.
 *
 * @see Loadable
 *	
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


    try
    {

 
		LogPlug::info( "Testing Loadable implementation." ) ;

		{
		
			LogPlug::trace( "Before instance creation" ) ;
			MyOwnContent testInstance( "/a/path" /* preload implied true */ ) ;
			
			
			LogPlug::trace( "Reading instance content (preloaded): " 
				+ testInstance.getExistingContent().toString() ) ;
			
			if ( testInstance.load() )
				throw TestException( 
					"No loading should be performed once preloaded." ) ;
			else		
				LogPlug::trace( 
					"Loading whereas preloaded, nothing done as expected." ) ;
				
			LogPlug::trace( "Unloading instance content" ) ;
			
			if ( testInstance.unload() )
				LogPlug::trace( "Unloading performed as expected." ) ;
			else		
				throw TestException( "No unloading performed, abnormal." ) ;
			
			if ( testInstance.unload() )
				throw TestException( 
					"No unloading should be performed once unloaded." ) ;
			else		
				LogPlug::trace( 
					"Unloading whereas unloaded, nothing done as expected." ) ;
			
			
			LogPlug::trace( "Reloading lastly to force deallocation "
				"on instance deletion." ) ;
			testInstance.load() ;
				
			LogPlug::trace( "Before instance deletion" ) ;
			
		}	
			
		
        LogPlug::info( "End of Loadable test." ) ;

	}
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught: "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught: " 
			 << e.what() << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( ... )
    {
        std::cerr << "Unknown exception caught" << std::endl ;
		return Ceylan::ExitFailure ;

    }

    return Ceylan::ExitSuccess ;

}

