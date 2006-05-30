#include "CeylanResourceManager.h"

using namespace Ceylan ;



ResourceManagerException::ResourceManagerException( const std::string & reason )
		throw() :
	Exception( "ResourceManager exception : " + reason )
{

}


ResourceManagerException::~ResourceManagerException() throw()
{

}

