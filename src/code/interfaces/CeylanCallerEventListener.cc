#include "CeylanCallerEventListener.h"


#include "CeylanLogPlug.h"      // for LogPlug
#include "CeylanOperators.h"    // for Ceylan::toString
#include "CeylanEventSource.h"  // for EventSource



using std::string ;
using std::list ;


using namespace Ceylan ;
using namespace Ceylan::Log ;



CallerEventListener::CallerEventListener() throw() :
	EventListener()
{

}


CallerEventListener::CallerEventListener( EventSource & source ) throw() :
	EventListener( source )
{

}


CallerEventListener::~CallerEventListener() throw() 
{

}


const string CallerEventListener::toString( Ceylan::VerbosityLevels level ) const throw() 
{
	return "Caller " + EventListener::toString( level ) ;
}

