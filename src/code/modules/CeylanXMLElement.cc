#include "CeylanXMLElement.h"

#include "CeylanInputStream.h"    // for InputStream
#include "CeylanOutputStream.h"   // for OutputStream



using namespace Ceylan ;
using namespace Ceylan::XML ;

using std::string ;
using std::map ;



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



// XMLMarkup section.


XMLMarkup::XMLMarkup() throw():
	_name()
{

}


XMLMarkup::XMLMarkup( const std::string & name ) throw():
	_name( name )
{

}


XMLMarkup::~XMLMarkup() throw()
{

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


void XMLMarkup::saveTo( System::OutputStream & output ) 
	throw( SerializationException )
{

}


void XMLMarkup::loadFrom( System::InputStream & input ) 
	throw( SerializationException )
{

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


void XMLText::saveTo( System::OutputStream & output ) 
	throw( SerializationException )
{

}


void XMLText::loadFrom( System::InputStream & input ) 
	throw( SerializationException )
{

}


const string XMLText::toString( Ceylan::VerbosityLevels level ) const throw()
{

	return _text ;
	
}
