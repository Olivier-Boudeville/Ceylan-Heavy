#include "CeylanModule.h"

#include "CeylanOperators.h"
#include "CeylanStringUtils.h"   // for formatStringList


#include <list>



using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;



ModuleException::ModuleException( const std::string & message ) throw() :
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
				const string  & licence ) throw() :
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


Module::Module() throw() :
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



string Module::getName() const throw()
{
    return _name ;
}


void Module::setName( const string & name ) throw()
{
    _name = name ;
}



string Module::getDescription() const throw()
{
    return _description ;
}


void Module::setDescription( const string & description ) throw()
{
    _description = description ;
}



string Module::getHomePage() const throw()
{
    return _homepageURL ;
}


void Module::setHomePage( const string & homePage ) throw()
{
    _homepageURL = homePage ;
}



string Module::getAuthor() const throw()
{
    return _author ;
}


void Module::setAuthor( const string & author ) throw()
{
    _author = author ;
}



string Module::getAuthorMail() const throw()
{
    return _authorMail ;
}


void Module::setAuthorMail( const string & authorMail ) throw()
{
    _authorMail = authorMail ;
}



const Version & Module::getVersion() const throw()
{
    return _version ;
}


void Module::setVersion( const Version & version ) throw()
{
	_version.setMajorNumber(   version.getMajorNumber() ) ;
	_version.setMinorNumber(   version.getMinorNumber() ) ;
	_version.setReleaseNumber( version.getReleaseNumber() ) ;
}




string Module::getLicence() const throw()
{
    return _licence ;
}


void Module::setLicence( const string & licence ) throw()
{
    _licence = licence ;
}



const string Module::toString( VerbosityLevels level ) const throw()
{

    string res ;

    res = "Module " + _name + ", version " + _version.toString() ;

    if ( level == low )
        return res ;

	std::list<string> moduleList ;
	
	moduleList.push_back( "Description : " + _description + "." ) ;
	
	if ( TextDisplayable::GetOutputFormat() == TextDisplayable::html )
	{
	
		moduleList.push_back( "<a href=\"" + _homepageURL 
			+ "\" target=\"_blank\">Homepage</a>" ) ;
		moduleList.push_back( "<a href=\"mailto:" + _authorMail 
			+ "\">" + _author + "</a>" ) ;		
			
	}	
	else
	{
	
    	moduleList.push_back( "Home page : " + _homepageURL + "." ) ;
    	moduleList.push_back( "Author : " + _author + " (" 
			+ _authorMail + ")." ) ;
			
	}
			
    moduleList.push_back(" Released through licence " + _licence + "." ) ;

    return res + " : " + formatStringList( moduleList ) ;

}

