#include "Ceylan.h"

using namespace Ceylan ;
using namespace Ceylan::Log ;


#include <exception>
#include <list>




class Example : public TextDisplayable
{

    public:
	
		Example( const std::string & name ) throw() ;
		virtual ~Example() throw() ;
		
        virtual const std::string toString( 
			Ceylan::VerbosityLevels level = high ) const throw() ;


  	private:
		
		std::string _name ;
		
} ;



Example::Example( const std::string & name ) throw() : _name( name )
{

}


Example::~Example() throw()
{

}


const std::string Example::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{
	return "My name is " + _name ;
}




/**
 * Test for TextDisplayable implementation.
 *
 * @see TextDisplayable, Displayable
 *
 *
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;

    try
    {


        LogPlug::info( "Testing TextDisplayable's implementation." ) ;

		Example myExample1( "Example one" ) ;
		
		LogPlug::info( "Here is the text output of "
			"our TextDisplayable example object : "
				+ myExample1.toString() ) ;
			 
		Example myExample2( "Example two" ) ;
		Example myExample3( "Example three" ) ;
		Example myExample4( "Example four" ) ;
		Example myExample5( "Example five" ) ;
		Example myExample6( "Example six" ) ;

		std::list<TextDisplayable *> myList ;
		
		myList.push_back( & myExample1 ) ;
		myList.push_back( & myExample2 ) ;
		myList.push_back( & myExample3 ) ;
		myList.push_back( & myExample4 ) ;
		myList.push_back( & myExample5 ) ;
		myList.push_back( & myExample6 ) ;
		
		LogPlug::info( "Displaying a list of Displayables : " 
			+ TextDisplayable::ToString( myList ) ) ;
		
		LogPlug::info( "End of TextDisplayable test." ) ;


	}
	
    catch ( const Ceylan::Exception & e )
    {
        std::cerr << "Ceylan exception caught : "
        	<< e.toString( Ceylan::high ) << std::endl ;
		return Ceylan::ExitFailure ;

    }

    catch ( const std::exception & e )
    {
        std::cerr << "Standard exception caught : " 
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
