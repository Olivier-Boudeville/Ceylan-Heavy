#include "CeylanXMLParser.h"

#include "CeylanTree.h"         // for Tree
#include "CeylanXML.h"          // for Latin1WithEuroEncoding
#include "CeylanXMLElement.h"   // for XMLElement
#include "CeylanFile.h"         // for File
#include "CeylanXMLVisitor.h"   // for XMLVisitor types
#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanOperators.h"    // for toString


#include <stack>   // for stack


using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::System ;
using namespace Ceylan::XML ;

using std::string ;
using std::stack ;





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


void XMLParser::saveToFile( const std::string & filename ) const 
	throw( XMLParserException )
{

	if ( _parsedTree == 0 )
		throw XMLParserException( "XMLParser::saveToFile : "
			"no parsed tree to serialize." ) ;
			
	string actualFileName ;
	
	if ( filename.empty() )
		actualFileName = _filename ;
	else
		actualFileName = filename ;
		
	File xmlFile( actualFileName ) ; 

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

	Ceylan::Uint8 remainder ;

	input.skipWhitespaces( remainder ) ;
	
	// Read : '[whitespaces]<?xml version="1.0" [encoding..]?>[whitespaces]'.

	if ( remainder != XML::LowerThan )
		throw XMLParserException( "XMLParser::loadFromFile failed : "
			"after declaration first non-whitespace character is not '<'." ) ;
			
	// Now reading the main part of the XML file :
	stack<string> markupStack ;
		
	// Parse until root markup is closed :
	handleNextElement( input, markupStack, _parsedTree, 
		remainder ) ;
	
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
	
		case XML::QuestionMark:
			return XMLParser::Declaration ;
			break ;
			
		case XML::ExclamationMark:
			return XMLParser::Comment ;
			break ;
			
		case XML::Slash:
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
	while ( ( readChar = input.readUint8() ) != XML::QuestionMark ) 
		res += readChar ;
	
	// Final '>' :
	readChar = input.readUint8() ;
	
	if ( readChar != XML::HigherThan )
		throw XMLParserException( "XMLParser::InterpretXMLDeclaration : "
			"expected to finish XML declaration with '?>', read '" 
			+ res + Ceylan::toString( XML::QuestionMark ) 
			+ Ceylan::toString( readChar ) + "'." ) ;
	
	// res should contain now something like : 'xml version="1.0" ...'.
			
	if ( res.find( "xml", /* starting index */ 0 ) != 0 )
		throw XMLParserException( "XMLParser::InterpretXMLDeclaration : "
			"expected to find 'xml', read '" + res + "'." ) ;
	
	// Read '[?]xml', suppressing it :
	res = res.substr( 3 ) ;
	
	
	/*
	 * res should contain now something like :
	 * '[whitespaces]version="1.0" ...'.
	 *
	 */
	
	AttributeMap declarationMap ;
	
	ParseAttributeSequence( res, declarationMap ) ;
	
	LogPlug::debug( "XMLParser::InterpretXMLDeclaration parsed : "
		+ Ceylan::formatStringMap( declarationMap ) ) ;
	
	AttributeMap::const_iterator it = declarationMap.find( "version" ) ;
	
	if ( it == declarationMap.end() )
		throw XMLParserException( "XMLParser::InterpretXMLDeclaration : "
			"no XML 'version' attribute found." ) ;

	if ( (*it).second != "1.0" )
		throw XMLParserException( "XMLParser::InterpretXMLDeclaration : "
			"only the 1.0 version of XML is supported, whereas the "
			+ (*it).second + " version was specified." ) ;
	
	it = declarationMap.find( "encoding" ) ;
	if ( it == declarationMap.end() )
		LogPlug::warning( "XMLParser::InterpretXMLDeclaration : "
			"no XML encoding attribute found in declaration, "
			"falling back to default one, " 
			+ XML::Latin1WithEuroEncoding + "." ) ;
	
	if ( (*it).second != XML::Latin1WithEuroEncoding )
		throw XMLParserException( 
			"XMLParser::InterpretXMLDeclaration : only the " 
			+ XML::Latin1WithEuroEncoding 
			+" encoding is supported, whereas the "
			+ (*it).second + " encoding was specified." ) ;		
			
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
		
		
		/*
		 * No 'if ( index >= size ) return ;' as a value-less one-letter 
		 * attribute can exist.
		 *
		 */
		
		while ( index < size && ( ! Ceylan::isWhitespace( toBeParsed[index] ) ) 
			&& toBeParsed[index] != XML::Equal )
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
		
		if ( toBeParsed[index] == XML::Equal )
		{
		
			index++ ;
		
			while ( index < size && Ceylan::isWhitespace( toBeParsed[index] ) ) 
				index++ ;
			
			if ( index >= size )
				return ;
			
			// Let's retrieve the attribute value :
			if ( toBeParsed[index] != XML::DoubleQuote )
				throw XMLParserException( "XMLParser::ParseAttributeSequence : "
					"expecting double quotes to begin attribute value, read '"
					+ Ceylan::toString( toBeParsed[index] ) + "' instead." ) ;
			index++ ;
			if ( index >= size )
				return ;
			
			while ( index < size && toBeParsed[index] != XML::DoubleQuote )
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


void XMLParser::handleNextElement( System::InputStream & input,
		stack<string> & markupStack, XMLTree * currentTree, 
		Ceylan::Uint8 & remainder )
	throw( System::InputStream::InputStreamException, XMLParserException )
{

	/*
	 * When this method is called, leading whitespaces should have been
	 * eaten, and first non-whitespace character should be already read and
	 * stored in 'remainder' variable.
	 *
	 */
	
	if ( remainder != XML::LowerThan )
	{
	
		// Should be a XML text element :
		string text ;
		text += static_cast<char>( remainder ) ;
		
		// Reads everything from the beginning of text to first '<' :
		while ( ( remainder = input.readUint8() ) != XML::LowerThan )
			text += remainder ;
		
		/*
		 * Removes any trailing whitespace so that writing an
		 * XML file, then reading it, then writing it leads to the same exact
		 * file (when there is no XML text ending with whitespaces).
		 *
		 */
		StringSize index = text.size() - 1 ;
		
		while ( index > 0 && Ceylan::isWhitespace( text[index] ) )
			index-- ;
		
		if ( index < text.size() - 1 )	
			text = text.substr( 0, index + 1 ) ;
			
		LogPlug::debug( "XMLParser::handleNextElement : "
			"creating XML text element from '" + text + "'." ) ;
			
		XMLText * newText = new XMLText( text ) ;
		
		XMLParser::XMLTree * newNode = new XMLParser::XMLTree( *newText ) ;
		
		if ( currentTree == 0 )
			throw XMLParserException( "XMLParser::handleNextElement : "
				"text '" + text + "' must be enclosed in markups." ) ;
		
		currentTree->addSon( *newNode ) ;
		
		handleNextElement( input, markupStack, currentTree, remainder ) ;
		
		return ;
		
	}
	
	
	// Here, we have either an opening or closing markup.
	
	LowerThanSequence seq = InterpretLowerThanSequence( input, remainder ) ;
	
	if ( seq != OpeningMarkup && seq != ClosingMarkup )
		throw XMLParserException( "XMLParser::handleNextElement : "
			"expecting opening or closing markup, found "
			+ DescribeLowerThanSequence( seq ) + "." ) ;
	
	// A opening or closing markup ! (common section)
	string markupName ;
	
	// If opening, first letter was already eaten, otherwise was '/' if closing.
	if ( seq == OpeningMarkup )
		markupName = remainder ;
	
	remainder = input.readUint8() ;
	
	while ( ! Ceylan::isWhitespace( remainder ) 
		&& remainder != XML::HigherThan )
	{
		markupName += remainder ;
		remainder = input.readUint8() ;	
	}
		
	LogPlug::debug( "XMLParser::handleNextElement : read markup '" 
		+ markupName + "'." ) ;
	
	if ( seq == OpeningMarkup )
	{
		
		XMLMarkup * newMarkup = new XMLMarkup( markupName ) ;
		markupStack.push( markupName ) ;
		
		// Did we stop on a '>' ?
		if ( remainder != XML::HigherThan )
		{
			
			// No, it was a whitespace, looking for any markup attributes :
			string restOfMarkup ;
			while ( ( remainder = input.readUint8() ) != XML::HigherThan )
				restOfMarkup += remainder ;
			
			ParseAttributeSequence( restOfMarkup, newMarkup->getAttributes() ) ;
			
			LogPlug::debug( "XMLParser::handleNextElement : "
				"read markup is now '"
				+ encodeToHTML( newMarkup->toString() ) + "'." ) ;
			
		} // else : we stop on a '>' : no attribute to retrieve.
		
		// Creating now the tree node containing that markup :
		XMLParser::XMLTree * newNode = new XMLParser::XMLTree( *newMarkup ) ;
		
		// Either a son of an already existing node, or the root :
		if ( currentTree != 0 )
		{
			LogPlug::debug( "XMLParser::handleNextElement : "
				"adding son to current parsed tree." ) ;
			currentTree->addSon( *newNode ) ;
			currentTree = newNode ;
		}	
		else
		{
			LogPlug::debug( "XMLParser::handleNextElement : "
				"creating a new parsed tree." ) ;
			setXMLTree( *newNode ) ;
			currentTree = _parsedTree ;
		}	
			
	}
	else 
	{
	
		// We have here a ClosingMarkup :
		string toBeClosed = markupStack.top() ;
		
		if ( toBeClosed != markupName )
			throw XMLParserException( "XMLParser::handleNextElement : "
				"expecting closing markup for '" + toBeClosed 
				+ "', found closing markup for '" + markupName + "'." ) ;
		
		LogPlug::debug( "XMLParser::handleNextElement : closing markup '"
			+ markupName + "' as expected." ) ;
			
		markupStack.pop() ;

		// Everything is parsed ?
		if ( markupStack.empty() )
			return ;
		
		// New current tree is the father :
		XMLTree * tempTree = _parsedTree->getFather( *currentTree ) ;
		
		if ( tempTree == 0 )
			throw XMLParserException( "XMLParser::handleNextElement : "
				"no father found for current tree '" + currentTree->toString()
				+ "' in parsed tree : '" + _parsedTree->toString() + "'." ) ;
		
		currentTree = tempTree	;
		
		// Go to the end of this markup : 
		if ( remainder != XML::HigherThan )
			while ( input.readUint8() != XML::HigherThan )
				;
				
	}


	input.skipWhitespaces( remainder ) ;
	
	handleNextElement( input, markupStack, currentTree, remainder ) ;
	
}
	
