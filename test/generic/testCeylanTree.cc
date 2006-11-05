#include "Ceylan.h"

#include <exception>

#include <string>
using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;


/// Test content, eligible for a tree, that contains a simple string.
class TestTreeStringContent : Ceylan::TextDisplayable
{
	
	public:

		TestTreeStringContent( const std::string & content ) throw() :
			_content( content )
		{

		}
		

		const std::string toString( Ceylan::VerbosityLevels level )
			const throw()
		{
			return "'" + _content + "'" ;
		}

	private:

		std::string _content ;

} ;


/**
 * Test of templated tree facility.
 *
 * @see Ceylan::Tree
 *	
 */
int main( int argc, char * argv[] )
{


	LogHolder logger( argc, argv ) ;


    try
    {

 
		LogPlug::info( "Testing templated Tree's implementation." ) ;

		LogPlug::info( "Creating a tree whose node content is a string." ) ;

		// No automatic variable, as ownership is taken by the tree :
		TestTreeStringContent * myContent = new TestTreeStringContent( 
			string( "I am the content of the root." ) ) ;

		typedef Tree<TestTreeStringContent> StringTree ;
			
		StringTree testTree( *myContent ) ;

		LogPlug::info( "Tree created : " + testTree.toString() ) ;
 
		TestTreeStringContent * myFirstSonContent = new TestTreeStringContent( 
			string( "I am the content of the first child." ) ) ;

		// No automatic variable, as ownership is taken by the tree :
		StringTree * testFirstSon = new StringTree( *myFirstSonContent ) ;

		LogPlug::info( "Subtree created : " + testTree.toString() ) ;

		testTree.addSon( *testFirstSon ) ;

		LogPlug::info( "Subtree added to Tree : " + testTree.toString() ) ;

		TestTreeStringContent * mySecondSonContent = new TestTreeStringContent( 
			string( "I am the content of the second child." ) ) ;
	
		StringTree * testSecondSon = new StringTree( *mySecondSonContent ) ;
		testTree.addSon( *testSecondSon ) ;

		LogPlug::info( "After a second subtree was added to Tree : " 
			+ testTree.toString() ) ;

		TestTreeStringContent * myThirdSonContent = new TestTreeStringContent( 
			string( "I am the content of the third child, "
				"I am the only son of second subtree." ) ) ;

		StringTree * testThirdSon = new StringTree( *myThirdSonContent ) ;
		testSecondSon->addSon( *testThirdSon ) ;

		LogPlug::info( "After a subtree was add to second subtree : " 
			+ testTree.toString() ) ;

        LogPlug::info( "End of templated Tree test." ) ;

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
