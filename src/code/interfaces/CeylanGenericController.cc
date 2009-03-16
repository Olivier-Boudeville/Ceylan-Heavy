#include "CeylanGenericController.h"

using namespace Ceylan ;

using std::string ;


BaseController::BaseController() throw()
{

}



BaseController::~BaseController() throw()
{

}



const string BaseController::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Base Controller" ;
	
}

