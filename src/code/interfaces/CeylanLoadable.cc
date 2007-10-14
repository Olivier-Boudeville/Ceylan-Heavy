#include "CeylanLoadable.h"


#include "CeylanOperators.h"            // for string operators


using namespace Ceylan ;



LoadableException::LoadableException( const std::string & message ) throw() :
	Exception( "Loadable exception: " + message )
{

}			


LoadableException::~LoadableException() throw()
{
			
}
			


// Implementation of the Loadable mother class.


Loadable::Loadable( const std::string & contentFilePath ) 
		throw( LoadableException ):
	_contentPath( contentFilePath )
{

}	



Loadable::~Loadable() throw()
{

}



const std::string & Loadable::getContentPath() const throw()
{

	return _contentPath ;
	
}

