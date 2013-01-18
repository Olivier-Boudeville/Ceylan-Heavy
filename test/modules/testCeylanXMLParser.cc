/*
 * Copyright (C) 2003-2013 Olivier Boudeville
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
using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::XML ;


#include <iostream>   // for cerr, endl
#include <exception>

#include <string>
using std::string ;

#include <list>
using std::list ;




/**
 * Test of XML parsing services.
 *
 * @see CeylanXMLParser.h
 *
 * @see test/generic/testCeylanTree.cc
 *
 */
int main( int argc, char * argv[] )
{

  {

	LogHolder logger( argc, argv ) ;


	try
	{


	  LogPlug::info( "Testing XML parser." ) ;


	  bool testWritingXML = true ;
	  bool testReadingXML = true ;

	  string generatedFilename = "testCeylanXMLParser-generated.xml" ;
	  string processedFilename = "testCeylanXMLParser-processed.xml" ;


	  if ( testWritingXML )
	  {

		XMLParser myTestParser( generatedFilename ) ;
		LogPlug::info( "Parser created: " + myTestParser.toString() ) ;

		string xmlFilename = "test-parser.xml" ;

		LogPlug::info( "Now, recreating programatically the content of '"
		  + xmlFilename + "'." ) ;

		/*
		 * The file content could be:

		 <?xml version="1.0" encoding="ISO-8859-15"?>
		 <para color="brown" height="200" width="100">
		 <b_ex city="London">
		 Hello world!
		 <c_ex length="14.1">
		 Parse me
		 </c_ex>
		 <f_ex comment="I am a solo tag" />
		 </b_ex>
		 <d_ex>
		 Someone listening?
		 </d_ex>
		 <c_ex height="7.1">
		 </c_ex>
		 </para>

		 *
		 * However the f_ex solo tag was not added, as if the Ceylan parser is
		 * able to read it indeed, it would be written as '<f_ex comment="I am a
		 * solo tag"></f_ex>', which would make the diff fail, short of finding
		 * in written file the original solo mark-up.
		 *
		 */


		/*
		 * Not an automatic variable, as the markup will be owned by the tree:
		 *
		 */
		XMLMarkup & myFirstMarkup = * new XMLMarkup( "para" ) ;

		myFirstMarkup.setAttribute( "width" , "100"   ) ;
		myFirstMarkup.setAttribute( "height", "200"   ) ;
		myFirstMarkup.setAttribute( "color" , "brown" ) ;

		LogPlug::info( "Markup created: " + myFirstMarkup.toString() ) ;


		/*
		 * Not an automatic variable, as the tree will be owned by the parser:
		 *
		 */
		XMLParser::XMLTree & myTestTree =
		  * new XMLParser::XMLTree( myFirstMarkup ) ;

		XMLMarkup & mySecondMarkup = * new XMLMarkup( "b_ex" ) ;
		mySecondMarkup.setAttribute( "city", "London" ) ;

		XMLParser::XMLTree & myBTree =
		  * new XMLParser::XMLTree( mySecondMarkup ) ;

		myTestTree.addSon( myBTree ) ;

		/*
		 * Not an automatic variable, as the markup will be owned by the tree:
		 *
		 */
		XMLText & myFirstText = * new XMLText( "Hello world!" ) ;
		LogPlug::info( "Text created: " + myFirstText.toString() ) ;

		XMLParser::XMLTree & myFirstTextTree =
		  * new XMLParser::XMLTree( myFirstText ) ;

		myBTree.addSon( myFirstTextTree ) ;

		XMLMarkup & myThirdMarkup = * new XMLMarkup( "c_ex" ) ;
		myThirdMarkup.setAttribute( "length", "14.1" ) ;

		XMLParser::XMLTree & myCTree =
		  * new XMLParser::XMLTree( myThirdMarkup ) ;

		myBTree.addSon( myCTree ) ;

		XMLText & mySecondText = * new XMLText( "Parse me" ) ;

		XMLParser::XMLTree & mySecondTextTree =
		  * new XMLParser::XMLTree( mySecondText ) ;

		myCTree.addSon( mySecondTextTree ) ;

		XMLMarkup & myFourthMarkup = * new XMLMarkup( "d_ex" ) ;

		XMLParser::XMLTree & myDTree =
		  * new XMLParser::XMLTree( myFourthMarkup ) ;

		myTestTree.addSon( myDTree ) ;

		XMLText & myThirdText = * new XMLText( "Someone listening?" ) ;

		XMLParser::XMLTree & myThirdTextTree =
		  * new XMLParser::XMLTree( myThirdText ) ;

		myDTree.addSon( myThirdTextTree ) ;

		XMLMarkup & myFifthMarkup = * new XMLMarkup( "c_ex" ) ;
		myFifthMarkup.setAttribute( "height", "7.1" ) ;


		XMLParser::XMLTree & mySecondCTree =
		  * new XMLParser::XMLTree( myFifthMarkup ) ;
		myTestTree.addSon( mySecondCTree ) ;


		LogPlug::info( "Tree created: " + myTestTree.toString() ) ;


		myTestParser.setXMLTree( myTestTree ) ;

		LogPlug::info( "Parser recorded tree: "
		  + myTestParser.toString() ) ;


		// Storing the result in a XML file:
		myTestParser.saveToFile() ;


		LogPlug::info(
		  "Now using the tree is memory to perform searches." ) ;

		LogPlug::info( "Searching for the father content of '"
		  + encodeToHTML( myFirstText.toString() ) + "': it should be '"
		  + encodeToHTML( mySecondMarkup.toString() ) + "'." ) ;

		XMLElement * father = myTestTree.getFatherContent( myFirstText ) ;

		if ( father != & mySecondMarkup )
		{

		  if ( father != 0 )
			throw Ceylan::TestException(
			  "Searching for the father content of '"
			  + encodeToHTML( myFirstText.toString() )
			  + "' failed: it should be '"
			  + encodeToHTML( mySecondMarkup.toString() )
			  + "', but '"
			  + encodeToHTML( father->toString() )
			  + "' was returned." ) ;
		  else
			throw Ceylan::TestException(
			  "Searching for the father content of '"
			  + encodeToHTML( myFirstText.toString() )
			  + "' failed: it should be '"
			  + encodeToHTML( mySecondMarkup.toString() )
			  + "', but none was found." ) ;

		}
		else
		{

		  LogPlug::info( "Search succeeded." ) ;

		}


		LogPlug::info( "Searching for the father content of '"
		  + encodeToHTML( myFirstMarkup.toString() )
		  + "': none should be found, "
		  "as it is associated with the root node." ) ;

		father = myTestTree.getFatherContent( myFirstMarkup ) ;

		if ( father != 0 )
		  throw Ceylan::TestException(
			"No parent content should be found for a root node, "
			"whereas '" + encodeToHTML( father->toString() )
			+ "' was found." ) ;

		LogPlug::info( "No parent content found, that is correct." ) ;

		LogPlug::info( "Searching for the content of sons of '"
		  + encodeToHTML( mySecondMarkup.toString() )
		  + "'." ) ;

		list<XMLElement *> sonsContents ;
		myTestTree.appendSonsContentsOf( mySecondMarkup, sonsContents ) ;

		list<string> toDisplay ;

		for ( list<XMLElement *>::iterator it = sonsContents.begin() ;
			  it != sonsContents.end(); it++ )
		  toDisplay.push_back( encodeToHTML( (*it)->toString() ) ) ;

		LogPlug::info( "Child content of node associated with content '"
		  + encodeToHTML( mySecondMarkup.toString() ) + "' is: "
		  + formatStringList( toDisplay ) ) ;


		LogPlug::info( "Testing now XMLSearchingVisitor." ) ;

		string searchedMarkup = "c" ;
		XMLSearchingVisitor myTestVisitor( searchedMarkup ) ;
		myTestTree.accept( myTestVisitor ) ;

		list<XMLMarkup *> found = myTestVisitor.getMatchingMarkups() ;
		toDisplay.clear() ;

		for ( list<XMLMarkup *>::iterator it = found.begin() ;
			  it != found.end(); it++ )
		  toDisplay.push_back( encodeToHTML( (*it)->toString() ) ) ;

		LogPlug::info( "Found in test tree following markups matching '"
		  + searchedMarkup + "': "
		  + formatStringList( toDisplay ) ) ;

	  }


	  if ( testReadingXML )
	  {

		// Test of attribute parsing;

		LogPlug::info( "Testing the parsing of XML strings" ) ;

		XML::AttributeMap attributeMap ;

		// Whitespaces are intended:
		const string testAttributes =
		  "hello = \" world\" a b=\"1\" change c" ;

		XMLParser::ParseAttributeSequence( testAttributes, attributeMap ) ;

		LogPlug::info( "The parsing of '" + testAttributes
		  + "' results in: "
		  + Ceylan::formatStringMap( attributeMap,
			/* surroundByTicks */ true ) ) ;

		if ( attributeMap.size() != 5 )
		  throw Ceylan::TestException( "Parsed map has "
			+ Ceylan::toString(
			  static_cast<Ceylan::Uint32>( attributeMap.size() ) )
			+ " elements, instead of 5." ) ;

		// The corresponding file is supposed to exist:
		XMLParser myTestParser( generatedFilename ) ;
		LogPlug::info( "Parser created: " + myTestParser.toString() ) ;

		LogPlug::info( "Now, recreating a XML tree "
		  "from the content of '" + generatedFilename + "'." ) ;

		myTestParser.loadFromFile() ;

		LogPlug::info( "New parser state: " + myTestParser.toString() ) ;

		myTestParser.saveToFile( processedFilename ) ;

	  }


	  if ( testWritingXML && testReadingXML )
	  {

		LogPlug::info( "Checking that the two generated files "
		  "are exactly the same." ) ;

		if ( ! System::File::Diff( generatedFilename, processedFilename ) )
		  throw TestException( "The two generated XML files ('"
			+ generatedFilename + "' and '" + processedFilename
			+ "') are different." ) ;

	  }

	  LogPlug::info( "End of XML parser test." ) ;


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

  }

  Ceylan::shutdown() ;

  return Ceylan::ExitSuccess ;

}
