#include "CeylanRegularExpression.h"

#include "CeylanStringUtils.h"       // for Ceylan::Latin1Char, StringSize


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"            // for CEYLAN_USES_REGEX
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#ifdef CEYLAN_USES_REGEX_H
#include <regex.h>                   // for regex_t, regcomp, etc.
#endif // CEYLAN_USES_REGEX_H 

}


#include <sstream>
using std::stringstream ;

using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Features ;



RegExp::RegExp( const string & toAnalyze ) 
		throw( Features::FeatureNotAvailableException ) :
	_toAnalyze( toAnalyze )
{

#if CEYLAN_USES_REGEX

	// Nothing special to do.
	
#else // CEYLAN_USES_REGEX

	throw FeatureNotAvailableException( "RegExp constructor : "
		"regular expression support feature not available" ) ;
		
#endif // CEYLAN_USES_REGEX

}


RegExp::~RegExp() throw()
{
}


bool RegExp::isXMLName() const throw( Features::FeatureNotAvailableException )
{
	return matches( "^([a-z]|[A-Z]|[_]|[:]{1,1})" ) ;
}


bool RegExp::matches( const string & pattern ) const
	throw( Features::FeatureNotAvailableException )
{

#if CEYLAN_USES_REGEX

	regex_t re ;

	::regcomp( & re, pattern.c_str(), REG_EXTENDED | REG_NOSUB ) ;

	int status = ::regexec( & re, _toAnalyze.c_str(), 
		static_cast<StringSize>( 0 ), 0, 0 ) ;

	::regfree( & re ) ;

	return ( status == 0 ) ;
	
#else // CEYLAN_USES_REGEX

	throw FeatureNotAvailableException( "RegExp::matches : "
		"regular expression support feature not available" ) ;

#endif // CEYLAN_USES_REGEX

}

