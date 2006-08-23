#include "CeylanProtocolEndpoint.h"

#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanInputOutputStream.h"  // for InputOutputStream
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




				
ProtocolEndpoint::ProtocolEndpoint( System::InputOutputStream & stream,
		Marshaller & marshaller ) throw() :
	TextDisplayable(),
	_stream( & stream ),
	_marshaller( & marshaller )
{

}


ProtocolEndpoint::~ProtocolEndpoint() throw()
{

	// _stream not owned, hence not deallocated.
	
	delete _marshaller ;

}


const string ProtocolEndpoint::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Protocol endpoint using as underlying stream "
		+ _stream->toString( level ) + " with following marshaller : "
		+ _marshaller->toString( level ) ;
	
}

