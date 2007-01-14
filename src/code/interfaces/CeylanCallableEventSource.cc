#include "CeylanCallableEventSource.h"


#include "CeylanLogPlug.h"         // for LogPlug
#include "CeylanOperators.h"


using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;



CallableEventSource::CallableEventSource() throw() :
	EventSource()
{

}



CallableEventSource::CallableEventSource( EventListener & listener ) throw() :
	EventSource( listener )
{
	
}


CallableEventSource::~CallableEventSource() throw()
{
	// Nothing specific for callable sources.	 	
}



const string CallableEventSource::toString( Ceylan::VerbosityLevels level ) 
	const throw() 
{

	// The sentence makes sense :
	return "Callable Event Source. This specialized " 
		+ EventSource::toString( level ) ;

}


