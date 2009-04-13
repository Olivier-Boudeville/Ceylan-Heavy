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


#include "CeylanUniformResourceIdentifier.h"

#include "CeylanStringUtils.h" // for StringSize


using namespace Ceylan ;
using namespace Ceylan::URI ;

using std::string ;

const std::string Ceylan::URI::ProtocolSeparator( "://" ) ;



const string Ceylan::URI::getProtocolName( const string & fullURI ) throw()
{

	return getProtocolName( fullURI, ProtocolSeparator ) ;

}


const string Ceylan::URI::getProtocolName( const string & fullURI,
	const string & protocolSeparator ) throw()
{

	Ceylan::StringSize endOfPrefix = fullURI.find( protocolSeparator ) ;

	if ( endOfPrefix == string::npos )
	{
			// No protocol separator found (such as ://), returns empty string
			return "" ;
	}

	return fullURI.substr( 0, endOfPrefix ) ;

}


const string Ceylan::URI::getEmbeddedURI( const string & fullURI ) throw()
{

	return getEmbeddedURI( fullURI, ProtocolSeparator ) ;

}	


const string Ceylan::URI::getEmbeddedURI( const string & fullURI,
	const string & protocolSeparator ) throw()
{

	Ceylan::StringSize endOfPrefix = fullURI.find( protocolSeparator ) ;

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
