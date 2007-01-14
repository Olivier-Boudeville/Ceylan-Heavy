#include "CeylanSmartResource.h"

#include "CeylanOperators.h"    // for string operators


using namespace Ceylan ;

using std::string ;



SmartResource::SmartResource() throw()
{

}


SmartResource::~SmartResource() throw()
{

}


const string SmartResource::toString( Ceylan::VerbosityLevels level ) 
	const throw() 
{
	return "Smart Ressource at '" 
		+ Ceylan::toString( static_cast<const void *>( this ) ) + "'" ;
}

