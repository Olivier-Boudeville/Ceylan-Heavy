/* 
 * Copyright (C) 2003-2009 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


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

