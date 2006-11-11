#include "CeylanXMLParser.h"

#include "CeylanTree.h"         // for Tree


using namespace Ceylan ;

using std::string ;


XMLParserException::XMLParserException( const std::string & reason ) throw() : 
	Ceylan::Exception( reason )
{

}


XMLParserException::~XMLParserException() throw()
{

}



XMLParser::XMLParser( const std::string & filename ) throw() :
	_filename( filename ),
	_parsedTree( 0 )
{

}


XMLParser::~XMLParser() throw()
{

	if ( _parsedTree != 0 )
		delete _parsedTree ;

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