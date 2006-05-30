#include "CeylanEvent.h"


#include "CeylanEventSource.h"    // for EventSource::toString


using std::string ;

using namespace Ceylan ;



EventException::EventException( const std::string & reason ) throw() :
	Ceylan::Exception( "Event exception : " + reason )
{

}


EventException::~EventException() throw()
{

}



Event::Event( EventSource & source ) throw() :
	_source( & source )
{

}


Event::~Event() throw()
{
	// _source not owned.
}


const string Event::toString( Ceylan::VerbosityLevels level ) const throw() 
{
	return "Event originating from source " + _source->toString() ;
}
