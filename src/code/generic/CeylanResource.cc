#include "CeylanResource.h"

#include "CeylanOperators.h"    // for string operators


using namespace Ceylan ;

using std::string ;


// Resource Exception.

ResourceException::ResourceException( const std::string & reason ) throw() :
	Exception( "Resource exception : " + reason )
{

}


ResourceException::~ResourceException() throw()
{

}



// The Resource class itself, mostly empty.

Resource::Resource() throw()
{

}


Resource::~Resource() throw()
{

}


const string Resource::toString( Ceylan::VerbosityLevels level ) const throw() 
{
	return "Ressource at '" 
		+ Ceylan::toString( static_cast<const void *>( this ) ) + "'" ;
}

