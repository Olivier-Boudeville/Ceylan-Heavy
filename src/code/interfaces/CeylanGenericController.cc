#include "CeylanGenericController.h"

using namespace Ceylan ;
using namespace Ceylan::MVC ;


using std::string ;


/* 
 * In this file following classes are implemented:
 *
 *   - BaseController
 */



// BaseController section.


BaseController::BaseController() throw()
{

}



BaseController::~BaseController() throw()
{

}



const string BaseController::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Base controller" ;
	
}

