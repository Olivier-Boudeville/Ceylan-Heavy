/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "Ceylan.h"

#include <exception>

#include <string>
using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Log ;


class TestTreeStringContent ;


class TestTreeVisitor : public Ceylan::TreeVisitor<TestTreeStringContent>
{
	
	public:


		TestTreeVisitor() throw() 
		{

		}


		virtual ~TestTreeVisitor() throw() 
		{

		}


		void visit( Tree<TestTreeStringContent> & tree ) throw( VisitException )
		{
			LogPlug::info( "TestTreeVisitor::visit( Tree<Content> ) : "
				"traversing " + tree.toString() ) ;
		}
		
		
		void visit( TestTreeStringContent & content ) throw( VisitException )
		{
		
			/*
			 * Commented so that it avoids a double-dependence between
			 * TestTreeVisitor and TestTreeStringContent !
			 * 
			 
			LogPlug::info( "TestTreeVisitor::visit( Content ) : "
				"traversing " + content.toString() ) ;
				
			*/
		}
		

} ;



/// Test content, eligible for a tree, that contains a simple string.
class TestTreeStringContent : public Ceylan::TextDisplayable
{
	
	public:


		TestTreeStringContent( const std::string & content ) throw() :
			_content( content )
		{

		}
		
		
		virtual ~TestTreeStringContent() throw()
		{
		
		}
		
		
		virtual void accept( Visitor & visitor ) throw( VisitException )
		{
		
			TestTreeVisitor * actualVisitor = 
				dynamic_cast<TestTreeVisitor *>( & visitor ) ;
	
			if ( actualVisitor == 0 )
				throw VisitException( "TestTreeStringContent::accept failed : "
					"specified visitor (" + visitor.toString() 
					+ ") is not a test tree-enabled visitor." ) ;
			
			actualVisitor->visit( *this ) ;
			
		}
		
		
		const std::string toString( 
			Ceylan::VerbosityLevels level = Ceylan::high ) const throw()
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

		LogPlug::info( "Creating a tree visitor now." ) ;
		
		TestTreeVisitor testTreeVisitor ;

		LogPlug::info( "Applying this visitor to the tree, depth-first : "
			"order should be : first, third, second then root." ) ;
		testTree.traverseDepthFirst( testTreeVisitor,
			/* visitContent */ true ) ;

		LogPlug::info( "Applying the same visitor to the tree, breadth-first : "
			"order should be : root, first, second, third." ) ;
		testTree.traverseBreadthFirst( testTreeVisitor,
			/* visitContent */ true ) ;

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
