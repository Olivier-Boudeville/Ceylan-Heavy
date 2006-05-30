#include "CeylanTextIdentifier.h"


using std::string ;

using namespace Ceylan ;


TextIdentifier::TextIdentifier() throw() : 
	Identifier()
{

}


TextIdentifier::TextIdentifier( const std::string & id ) throw() : 
	Identifier(), 
	_id( id )
{

}


TextIdentifier::~TextIdentifier() throw()
{

}


bool TextIdentifier::operator==( const TextIdentifier & otherIdentifier )
	throw()
{
	return ( _id == otherIdentifier._id ) ;
}


const string TextIdentifier::toString( Ceylan::VerbosityLevels level ) 
	const throw() 
{
	return _id ;
}	
