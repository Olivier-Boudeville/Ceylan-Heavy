#include "CeylanMarshaller.h"

#include "CeylanLogPlug.h"            // for LogPlug
#include "CeylanInputOutputStream.h"  // for InputOutputStream



using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Log ;
using namespace Ceylan::Middleware ;


				
MarshallException::MarshallException( const string & message ) throw() : 
	MiddlewareException( message )
{

}

MarshallException::~MarshallException() throw()			
{

}


DecodeException::DecodeException( const string & message ) throw() : 
	MarshallException( message )
{

}

DecodeException::~DecodeException() throw()			
{

}


EncodeException::EncodeException( const string & message ) throw() : 
	MarshallException( message )
{

}

EncodeException::~EncodeException() throw()			
{

}




Marshaller::Marshaller( System::InputOutputStream & lowerLevelStream ) throw() :
	TextDisplayable(),
	_lowerLevelStream( & lowerLevelStream )
{

}


Marshaller::~Marshaller() throw()
{

	// _lowerLevelStream not owned.
	
}


const string Marshaller::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Marshaller using as underlying lower-level stream "
		+ _lowerLevelStream->toString( level )  ;
	
}


