#include "CeylanMiddleware.h"


using std::string ;

using namespace Ceylan ;
using namespace Ceylan::Middleware ;



MiddlewareException::MiddlewareException( const string & message ) throw() :
	 Ceylan::Exception( message )
{

}


MiddlewareException::~MiddlewareException() throw()
{

}

