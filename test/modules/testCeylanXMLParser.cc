#include "Ceylan.h"
using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::XML ;


#include <iostream>	   // for cerr, endl
#include <exception>

#include <string>
using std::string ;




/**
 * Test of XML parsing services.
 *
 * @see CeylanXMLParser.h
 *
 */
int main( int argc, char * argv[] )
{

	LogHolder logger( argc, argv ) ;	


	try
	{


		LogPlug::info( "Testing XML parser." ) ;


		XMLParser myTestParser( "testCeylanXMLParser-output.xml" ) ;
		LogPlug::info( "Parser created : " + myTestParser.toString() ) ;
	
		string xmlFilename = "test-parser.xml" ;
		
		LogPlug::info( "Now, recreating programatically the content of '"
			+ xmlFilename + "'." ) ;
			
		/*
		 * The file content should be :
		 
<para width="100" height="200" color="brown">
	<b city="London">Hello world !
		<c length="14.1">Parse me</c>
		<c/>
	</b>
	
	<d>Someone listening ?</d>
	
</para>
		 
		 */
		 
		 	
		// Not an automatic variable, as the markup will be owned by the tree :
		XMLMarkup & myFirstMarkup = * new XMLMarkup( "para" ) ;
				
		myFirstMarkup.setAttribute( "width" , "100"   ) ;
		myFirstMarkup.setAttribute( "height", "200"   ) ;
		myFirstMarkup.setAttribute( "color" , "brown" ) ;

		LogPlug::info( "Markup created : " + myFirstMarkup.toString() ) ;


		// Not an automatic variable, as the tree will be owned by the parser :
		XMLParser::XMLTree & myTestTree = 
			* new XMLParser::XMLTree( myFirstMarkup ) ;

		XMLMarkup & mySecondMarkup = * new XMLMarkup( "b" ) ;	
		mySecondMarkup.setAttribute( "city", "London" ) ;
			
		XMLParser::XMLTree & myBTree = 
			*new XMLParser::XMLTree( mySecondMarkup ) ;
			
		myTestTree.addSon( myBTree ) ;

		// Not an automatic variable, as the markup will be owned by the tree :
		XMLText & myFirstText = * new XMLText( "Hello world !" ) ;
		LogPlug::info( "Text created : " + myFirstText.toString() ) ;

		XMLParser::XMLTree & myFirstTextTree = 
			*new XMLParser::XMLTree( myFirstText ) ;

		myBTree.addSon( myFirstTextTree ) ;
		
		XMLMarkup & myThirdMarkup = * new XMLMarkup( "c" ) ;	
		myThirdMarkup.setAttribute( "length", "14.1" ) ;

		XMLParser::XMLTree & myCTree = 
			*new XMLParser::XMLTree( myThirdMarkup ) ;
		
		myBTree.addSon( myCTree ) ;
		
		XMLText & mySecondText = * new XMLText( "Parse me" ) ;
	
		XMLParser::XMLTree & mySecondTextTree = 
			*new XMLParser::XMLTree( mySecondText ) ;
		
		myCTree.addSon( mySecondTextTree ) ;
		
		XMLMarkup & myFourthMarkup = * new XMLMarkup( "d" ) ;
			
		XMLParser::XMLTree & myDTree = 
			*new XMLParser::XMLTree( myFourthMarkup ) ;
		
		myTestTree.addSon( myDTree ) ;
		
		XMLText & myThirdText = * new XMLText( "Someone listening ?" ) ;
	
		XMLParser::XMLTree & myThirdTextTree = 
			*new XMLParser::XMLTree( myThirdText ) ;
	
		myDTree.addSon( myThirdTextTree ) ;
			
		
		LogPlug::info( "Tree created : " + myTestTree.toString() ) ;
		
		
		myTestParser.setXMLTree( myTestTree ) ;

		LogPlug::info( "Parser recorded tree : " + myTestParser.toString() ) ;
		
		
		// Storing the result in a XML file :
		myTestParser.saveToFile() ;
		
		
		LogPlug::info( "End of XML parser test." ) ;
		
		
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
