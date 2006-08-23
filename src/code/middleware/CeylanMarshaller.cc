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




Marshaller::Marshaller() throw() :
	TextDisplayable()
{

}


Marshaller::~Marshaller() throw()
{

}


const string Marshaller::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "Marshaller" ;
	
}


