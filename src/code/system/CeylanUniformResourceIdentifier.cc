#include "CeylanUniformResourceIdentifier.h"


using namespace Ceylan ;
using namespace Ceylan::URI ;

using std::string ;



const string Ceylan::URI::getProtocolName( const string & fullURI,
	const string & protocolSeparator ) throw()
{

	unsigned int endOfPrefix = fullURI.find( protocolSeparator ) ;

	if ( endOfPrefix == string::npos )
	{
			// No protocol separator found (such as ://), returns empty string
			return "" ;
	}

	return fullURI.substr( 0, endOfPrefix ) ;

}


const string Ceylan::URI::getEmbeddedURI( const string & fullURI,
	const string & protocolSeparator ) throw()
{

	unsigned int endOfPrefix = fullURI.find( protocolSeparator ) ;

	if ( endOfPrefix == string::npos )
	{
			/*
			 * No protocol separator found (such as ://), return
			 * whole string :
			 *
			 */
			return fullURI ;
	}

	return fullURI.substr( endOfPrefix + protocolSeparator.size() ) ;

}
