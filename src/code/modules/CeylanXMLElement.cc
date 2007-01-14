#include "CeylanXMLElement.h"

#include "CeylanInputStream.h"    // for InputStream
#include "CeylanOutputStream.h"   // for OutputStream
#include "CeylanXMLVisitor.h"     // for (XML) Visitor


using namespace Ceylan ;
using namespace Ceylan::XML ;

using std::string ;
using std::map ;



/*
 * To add in future releases, if necessary :
 *  XMLBinary, with Ceylan::Byte * _data and Ceylan::Uint32 _size
 *
 */
 
 
// Exception section.


XMLElementException::XMLElementException( const std::string & reason ) throw() :
	XMLException( reason )
{

}


XMLElementException::~XMLElementException() throw()
{

}



// XMLElement section.


XMLElement::XMLElement() throw()
{

}


XMLElement::~XMLElement() throw()
{

}


void XMLElement::saveTo( System::OutputStream & output ) const
	throw( SerializationException )
{

	output.write( toString() ) ;

}




// XMLMarkup section.


XMLMarkup::XMLMarkup() throw():
	_name()
{

}


XMLMarkup::XMLMarkup( const MarkupName & name ) throw():
	_name( name )
{

}


XMLMarkup::~XMLMarkup() throw()
{

}


MarkupName XMLMarkup::getMarkupName() const throw()
{

	return _name ;
	
}


string XMLMarkup::getClosingMarkup() const throw()
{

	return "</" + _name + '>' ;
	
}


bool XMLMarkup::hasAttribute( const string & name ) const throw()
{
	return ( _attributes.find( name ) != _attributes.end() ) ;
}


string XMLMarkup::getAttribute( const string & name ) const throw()
{

	map<string,string>::const_iterator it = _attributes.find( name ) ;
	
	if ( it == _attributes.end() )
		return "" ;
	else
		return (*it).second ;	
	
}


void XMLMarkup::setAttribute( const string & name,
	const string & value ) throw()
{

	_attributes[ name ] = value ;
	
}


AttributeMap & XMLMarkup::getAttributes() throw()
{

	return _attributes ;
	
}


void XMLMarkup::loadFrom( System::InputStream & input ) 
	throw( SerializationException )
{

}


void XMLMarkup::accept( Visitor & visitor ) throw( VisitException )
{

	XMLVisitor * actualVisitor = dynamic_cast<XMLVisitor *>( & visitor ) ;
	
	if ( actualVisitor == 0 )
		throw VisitException( "XMLMarkup::accept failed : "
			"specified visitor (" + visitor.toString() 
			+ ") is not a XML-enabled visitor." ) ;
			
	actualVisitor->visit( *this ) ;
	
}


const string XMLMarkup::toString( Ceylan::VerbosityLevels level ) const throw()
{

	string res = "<" + _name ;
	
	for ( map<string,string>::const_iterator it = _attributes.begin();
			it != _attributes.end(); it++ )
		res += ' ' + (*it).first + "=\"" + (*it).second + '"' ;
		
	return res + ">" ;
	
}



// XMLText section.


XMLText::XMLText() throw():
	_text()
{

}


XMLText::XMLText( const std::string & text ) throw():
	_text( text )
{

}


XMLText::~XMLText() throw()
{

}


string XMLText::getText() const throw()
{

	return _text ;	
	
}


void XMLText::setText( const string & newText ) throw()
{

	_text = newText ;
	
}


void XMLText::loadFrom( System::InputStream & input ) 
	throw( SerializationException )
{

}


void XMLText::accept( Visitor & visitor ) throw( VisitException )
{

	XMLVisitor * actualVisitor = dynamic_cast<XMLVisitor *>( & visitor ) ;
	
	if ( actualVisitor == 0 )
		throw VisitException( "XMLText::accept failed : "
			"specified visitor (" + visitor.toString() 
			+ ") is not a XML-enabled visitor." ) ;
			
	actualVisitor->visit( *this ) ;
	
}


const string XMLText::toString( Ceylan::VerbosityLevels level ) const throw()
{

	return _text ;
	
}

