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


#include "CeylanModule.h"

#include "CeylanStringUtils.h"   // for formatStringList
#include "CeylanLogPlug.h"
#include "CeylanOperators.h"


#include <list>



using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;



ModuleException::ModuleException( const std::string & message ) :
	Ceylan::Exception( message )
{

}



ModuleException::~ModuleException() throw()
{

}




Module::Module( const string  & name, 
				const string  & description, 
				const string  & homepageURL,
                const string  & author, 
				const string  & authorMail,
                const Version & version, 
				const string  & licence ) :
	Object(),
	_name( name ),
	_description( description ),
	_homepageURL( homepageURL ),
	_author( author ),
	_authorMail( authorMail ),
	_version(),
	_licence( licence )
{
	
	setVersion( version ) ;

    send( "Creating " + toString( low ) ) ;

}



Module::Module() :
	Object(),
	_name(),
	_description(),
	_homepageURL(),
	_author(),
	_authorMail(),
	_version(),
	_licence()
{
		
    send( "Creating a blank module" ) ;
	
}



Module::~Module() throw()
{

    send( "Deleting " + toString( low ) ) ;

}



string Module::getName() const
{

    return _name ;
	
}



void Module::setName( const string & name )
{

    _name = name ;
	
}



string Module::getDescription() const 
{

    return _description ;
	
}



void Module::setDescription( const string & description ) 
{

    _description = description ;
	
}



string Module::getHomePage() const 
{

    return _homepageURL ;
	
}



void Module::setHomePage( const string & homePage ) 
{

    _homepageURL = homePage ;
	
}



string Module::getAuthor() const 
{

    return _author ;
	
}



void Module::setAuthor( const string & author ) 
{

    _author = author ;
	
}



string Module::getAuthorMail() const 
{

    return _authorMail ;
	
}



void Module::setAuthorMail( const string & authorMail ) 
{

    _authorMail = authorMail ;
	
}



const Version & Module::getVersion() const 
{

    return _version ;
	
}



void Module::setVersion( const Version & version ) 
{

	_version.setMajorNumber(   version.getMajorNumber()   ) ;
	_version.setMinorNumber(   version.getMinorNumber()   ) ;
	_version.setReleaseNumber( version.getReleaseNumber() ) ;
	
}




string Module::getLicence() const 
{

    return _licence ;
	
}



void Module::setLicence( const string & licence ) 
{

    _licence = licence ;
	
}



const string Module::toString( VerbosityLevels level ) const 
{

    string res ;

    res = "Module " + getName() + ", version " + _version.toString() ;

    if ( level == low )
        return res ;

	std::list<string> moduleList ;
	
	moduleList.push_back( "Description: " + _description + "." ) ;
	
	if ( TextDisplayable::GetOutputFormat() == TextDisplayable::html )
	{
	
		moduleList.push_back( "<a href=\"" + _homepageURL 
			+ "\" target=\"_blank\">Homepage</a>" ) ;
		moduleList.push_back( "<a href=\"mailto:" + _authorMail 
			+ "\">" + _author + "</a>" ) ;		
			
	}	
	else
	{
	
    	moduleList.push_back( "Home page: " + _homepageURL + "." ) ;
    	moduleList.push_back( "Author: " + _author + " (" 
			+ _authorMail + ")." ) ;
			
	}
			
    moduleList.push_back( "Released through licence " + _licence + "." ) ;

    return res + ": " + formatStringList( moduleList ) ;

}

