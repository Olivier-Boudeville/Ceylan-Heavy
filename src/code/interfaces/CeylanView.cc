#include "CeylanView.h"         // for Ceylan::View


#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanOperators.h"    // for Ceylan::toString

#include "CeylanModel.h"        // for Ceylan::Model

#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for CEYLAN_DEBUG_EVENTS
#endif // CEYLAN_USES_CONFIG_H


#include <list>


using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;



View::View() throw() :
	CallerEventListener()
{

}


View::View( Model & model ) throw() :
	CallerEventListener( static_cast<EventSource &>( model ) )
{

}


View::~View() throw()
{

#if CEYLAN_DEBUG_EVENTS
	LogPlug::debug( "Deleting View " + toString() ) ;
#endif // CEYLAN_DEBUG_EVENTS
	
}


const string View::toString( Ceylan::VerbosityLevels level ) const throw() 
{
	
	if ( _sources.empty() )
		return "MVC View currently not subscribed to any model" ;
	else
		return "MVC View currently subscribed to " 
			+ Ceylan::toString( _sources.size() ) +	" model(s)" ;
				
}
