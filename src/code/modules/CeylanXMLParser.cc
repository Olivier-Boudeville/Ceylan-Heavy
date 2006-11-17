#include "CeylanXMLParser.h"

#include "CeylanTree.h"         // for Tree
#include "CeylanXMLElement.h"   // for XMLElement
#include "CeylanFile.h"         // for File
#include "CeylanXMLVisitor.h"   // for XMLVisitor types



using namespace Ceylan ;
using namespace Ceylan::System ;
using namespace Ceylan::XML ;

using std::string ;





XMLParserException::XMLParserException( const std::string & reason ) throw() : 
	XMLException( reason )
{

}


XMLParserException::~XMLParserException() throw()
{

}





// XMLParser section.



std::string XMLParser::DefaultEncoding = "ISO-8859-15" ;



XMLParser::XMLParser( const std::string & filename ) throw() :
	_filename( filename ),
	_parsedTree( 0 ),
	_encoding( DefaultEncoding )
{

}


XMLParser::~XMLParser() throw()
{

	if ( _parsedTree != 0 )
		delete _parsedTree ;

}


bool XMLParser::hasXMLTree() const throw()
{

	return ( _parsedTree != 0 ) ;
	
}


XMLParser::XMLTree & XMLParser::getXMLTree() const throw( XMLParserException )
{

	if ( _parsedTree == 0 )
		throw XMLParserException( "XMLParser::getXMLTree : "
			"no available tree" ) ;
		
	return *_parsedTree	;
	
}


void XMLParser::setXMLTree( XMLTree & newTree ) throw()
{

	if ( _parsedTree != 0 )
		delete _parsedTree ;
	
	_parsedTree = &newTree ;
		
}


void XMLParser::saveToFile() const throw( XMLParserException )
{


	if ( _parsedTree == 0 )
		throw XMLParserException( "XMLParser::saveToFile : "
			"no parsed tree to serialize." ) ;
			

	File xmlFile( _filename ) ; 

	// Prepare header : 
	string header = "<?xml version=\"1.0\" encoding=\""
		+ _encoding + "\"?>\n" ;
		
	xmlFile.write( header ) ;
	
	
	// Then 'SavingVisitor'.
	
	XMLSavingVisitor myVisitor( xmlFile ) ;
	
	//myVisitor.visit( *_parsedTree ) ;
	_parsedTree->accept( myVisitor ) ;
	
	xmlFile.close() ;
	
	
}


const string XMLParser::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "XML parser " ;

	if ( _parsedTree != 0 )
		res += "with following tree in memory : "
			+ _parsedTree->toString( level ) ;
	else
		res += "with no tree in memory" ;

	if ( level == Ceylan::low )
		return res ;

	if ( _filename.empty() )
		res += ". No serialization file defined" ;
	else
		res += ". Serialization file is " + _filename ;

	return res ;

}

