#include "CeylanProtocolEndpoint.h"

#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanMarshaller.h"         // for Marshaller



using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Middleware ;


				
ProtocolException::ProtocolException( const string & message ) throw() : 
	MiddlewareException( message )
{

}


ProtocolException::~ProtocolException() throw()			
{

}



				
ProtocolEndpoint::ProtocolEndpoint( Marshaller & marshaller ) throw() :
	TextDisplayable(),
	_marshaller( & marshaller )
{

}


ProtocolEndpoint::~ProtocolEndpoint() throw()
{
	
	delete _marshaller ;

}


const string ProtocolEndpoint::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Protocol endpoint using following marshaller : "
		+ _marshaller->toString( Ceylan::low ) ;
	
}

