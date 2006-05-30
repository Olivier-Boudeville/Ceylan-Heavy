#include "CeylanController.h"   


#include "CeylanModel.h"        // for Ceylan::Model

#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanOperators.h"    // for Ceylan::toString

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"         // for CEYLAN_DEBUG_EVENTS
#endif // CEYLAN_USES_CONFIG_H

#include <list>



using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;



Controller::Controller() throw() :
	CallableEventSource()
{

}


Controller::Controller( Model & model ) throw() :
	CallableEventSource()
{
	model.subscribeTo( * this ) ;
}


Controller::~Controller() throw()
{

	// Input device not owned.
	
#if CEYLAN_DEBUG_EVENTS
	LogPlug::debug( "Deleting Controller " + toString() ) ;
#endif // CEYLAN_DEBUG_EVENTS
	
}


const string Controller::toString( Ceylan::VerbosityLevels level ) 
	const throw() 
{

	string result ;
	
	if ( _listeners.empty() )
		return "MVC Controller has currently no model subscribed" ;
	else
		return "MVC Controller has currently " 
			+ Ceylan::toString( _listeners.size() ) + " model(s) subscribed" ;
				
}
