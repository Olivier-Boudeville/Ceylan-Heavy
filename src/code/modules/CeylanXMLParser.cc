#include "CeylanXMLParser.h"

#include "CeylanTree.h"         // for Tree
#include "CeylanXML.h"          // for Latin1WithEuroEncoding
#include "CeylanXMLElement.h"   // for XMLElement
#include "CeylanFile.h"         // for File
#include "CeylanXMLVisitor.h"   // for XMLVisitor types
#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanOperators.h"    // for toString



using namespace Ceylan ;
using namespace Ceylan::Log ;
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



std::string XMLParser::DefaultEncoding = Latin1WithEuroEncoding ;



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
	
	_parsedTree->accept( myVisitor ) ;
	
	xmlFile.close() ;
	
	
}


void XMLParser::loadFromFile() throw( XMLParserException )
{

	if ( ! File::Exists( _filename ) )
		throw XMLParserException( "XMLParser::loadFromFile failed : "
			"file '" + _filename + "' does not exist." ) ;
	
	if ( _parsedTree != 0 )
	{
	
		delete _parsedTree ;
		_parsedTree = 0 ;
	
	}
	
	
	File input( _filename, /* opening flag */ File::Read ) ;
	
	Ceylan::Uint8 readChar ;
	input.skipWhitespaces( readChar ) ;
		
	if ( readChar != LowerThan )
		throw XMLParserException( "XMLParser::loadFromFile failed : "
			"first non-space character is not '" 
			+ Ceylan::toString( LowerThan )	+ "', read '" 
			+ Ceylan::toString( readChar ) + "' instead." ) ;

	// Read : '[whitespaces]<'.
	
	LowerThanSequence seq = InterpretLowerThanSequence( input, readChar ) ;
	
	if ( seq != XMLParser::Declaration )
		throw XMLParserException( "XMLParser::loadFromFile failed : "
			"expected to read an XML declaration, but read an "
			+ DescribeLowerThanSequence( seq ) ) ;
		
	// Read : '[whitespaces]<?', in readChar there is '?', which is dropped.
	InterpretXMLDeclaration( input ) ;
	
	// Read : '[whitespaces]<?xml version="1.0" [encoding..]?>[whitespaces]'.

	// Closing the file is automatic.
		
}


XMLParser::LowerThanSequence XMLParser::InterpretLowerThanSequence( 
		InputStream & input, 
		Ceylan::Uint8 & readChar ) 
	throw( InputStream::InputStreamException )
{

	readChar = input.readUint8() ;
	
	switch( readChar )
	{
	
		case '?':
			return XMLParser::Declaration ;
			break ;
			
		case '!':
			return XMLParser::Comment ;
			break ;
			
		case '/':
			return XMLParser::ClosingMarkup ;
			break ;
			
		default:
			// Nothing.			
			break ;
	}
	
	// Either OpeningMarkup	or UnexpectedElement here :
	if ( Ceylan::isLetter( readChar ) )
		return XMLParser::OpeningMarkup ;
	
	return XMLParser::UnexpectedElement ;
		
}


const std::string XMLParser::DescribeLowerThanSequence(
	LowerThanSequence sequence ) throw()
{
	
	switch( sequence ) 
	{
	
		case XMLParser::Declaration:
			return "XML declaration" ;
			break ;
			
		case XMLParser::Comment:
			return "XML comment" ;
			break ;
			
		case XMLParser::OpeningMarkup:
			return "XML opening markup" ;
			break ;
			
		case XMLParser::ClosingMarkup:
			return "XML closing markup" ;
			break ;
			
		case XMLParser::UnexpectedElement:
			return "unexpected XML element" ;
			break ;
			
		default:
			return "unknown sequence type (abnormal)" ;
			break ;
			
	}
	
	
}


void XMLParser::InterpretXMLDeclaration( InputStream & input ) 
	throw( InputStream::InputStreamException, XMLParserException )
{
	
	// '<?' already eaten.
	
	string res ;
	
	Ceylan::Uint8 readChar ;
	
	// Reads the full declaration in 'res', until second '?' :
	while ( ( readChar = input.readUint8() ) != '?' ) 
	{
		res += readChar ;
	}	
	
	// Final '>' :
	readChar = input.readUint8() ;
	
	if ( readChar != XML::HigherThan )
		throw XMLParserException( "XMLParser::InterpretXMLDeclaration : "
			"expected to finish XML declaration with '?>', read '" 
			+ res + Ceylan::toString( '?' ) 
			+ Ceylan::toString( readChar ) + "'." ) ;
	
	// res should contain now something like : 'xml version="1.0" ...'.
			
	if ( res.find( "xml", /* starting index */ 0 ) != 0 )
		throw XMLParserException( "XMLParser::InterpretXMLDeclaration : "
			"expected to find 'xml', read '" + res + "'." ) ;
	
	// Read '[?]xml', suppressing it :
	res = res.substr( 0, 3 ) ;
	
	
	/*
	 * res should contain now something like :
	 * '[whitespaces]version="1.0" ...'.
	 *
	 */
	
	AttributeMap declarationMap ;
	
	ParseAttributeSequence( res, declarationMap ) ;
	
	
	//ReadXMLName( input, res ) ;
	
	// Read '[?]xml [a name][whitespaces]'.	
	
			
}


void XMLParser::ParseAttributeSequence( const string & toBeParsed,
	AttributeMap & attributeMap ) throw( XMLParserException )
{

	LogPlug::debug( "XMLParser::ParseAttributeSequence : will parse '"
		+ toBeParsed + "'." ) ;
		
	StringSize size = toBeParsed.size() ;
	StringSize index = 0 ;
	
	string attributeName ;
	string attributeValue ;
	
	while ( index < size )
	{
	
		// Skip any leading whitespaces :
		while ( index < size && Ceylan::isWhitespace( toBeParsed[index] ) )
			index++ ;
	
		if ( index >= size )
			return ;
			
		// First character on an attribute should be a letter :
		if ( ! Ceylan::isLetter( toBeParsed[index] ) )
			throw XMLParserException( "XMLParser::ParseAttributeSequence : "
				"expecting first character of attribute name, read '"
				+ Ceylan::toString( toBeParsed[index] ) + "' instead." ) ;
		
		attributeName = toBeParsed[index] ;
		index++ ;
		
		if ( index >= size )
			return ;
		
		while ( index < size && ( ! Ceylan::isWhitespace( toBeParsed[index] ) ) 
			&& toBeParsed[index] != '=' )
		{
			attributeName += toBeParsed[index] ;
			index++ ;
		}
		
		LogPlug::debug( "XMLParser::ParseAttributeSequence : "
			"adding attribute name '" + attributeName + "'." ) ;
			
		attributeMap.insert( make_pair( attributeName, "" ) ) ;

		// Here the attribute name is recorded, maybe it has a value.
		
		if ( index >= size )
			return ;
		
		while ( index < size && Ceylan::isWhitespace( toBeParsed[index] ) )
			index++ ;

		if ( index >= size )
			return ;
		
		if ( toBeParsed[index] == '=' )
		{
		
			index++ ;
		
			while ( index < size && Ceylan::isWhitespace( toBeParsed[index] ) ) 
				index++ ;
			
			if ( index >= size )
				return ;
			
			// Let's retrieve the attribute value :
			if ( toBeParsed[index] != '"' )
				throw XMLParserException( "XMLParser::ParseAttributeSequence : "
					"expecting double quotes to begin attribute value, read '"
					+ Ceylan::toString( toBeParsed[index] ) + "' instead." ) ;
			index++ ;
			if ( index >= size )
				return ;
			
			while ( index < size && toBeParsed[index] != '"' )
			{
				attributeValue += toBeParsed[index] ;
				index++ ;
			}
			
			index++ ;
			
			LogPlug::debug( "XMLParser::ParseAttributeSequence : "
				"associating to attribute name '" + attributeName 
				+ "' the following attribute value : '" + attributeValue 
				+ "'." ) ;
				
			attributeMap[ attributeName ] = attributeValue ;
		
		
			if ( index >= size )
				return ;
			
		}
				
		attributeName.clear() ;
		attributeValue.clear() ;
		
	}
	
	
	
}

	
void XMLParser::ReadXMLName( InputStream & input, string & name,
		Ceylan::Uint8 & readChar ) 
	throw( InputStream::InputStreamException, XMLParserException )
{
	
	do
	{
		readChar = input.readUint8() ;
		if ( readChar == '=' || Ceylan::isWhitespace( readChar ) )
			break ;
		
		name += readChar ;
		
	}	
	while ( true ) ;
	
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

