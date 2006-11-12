#include "CeylanXMLParser.h"

#include "CeylanTree.h"         // for Tree
#include "CeylanXMLElement.h"   // for XMLElement


using namespace Ceylan ;
using namespace Ceylan::XML ;

using std::string ;





XMLParserException::XMLParserException( const std::string & reason ) throw() : 
	XMLException( reason )
{

}


XMLParserException::~XMLParserException() throw()
{

}




XMLParser::XMLParser( const std::string & filename ) throw() :
	_filename( filename ),
	_parsedTree( 0 ),
	_markupStack()
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

