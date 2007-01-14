#include "CeylanXML.h"


using namespace Ceylan ;
using namespace Ceylan::XML ;

using std::string ;



XMLException::XMLException( const std::string & reason ) throw():
	Ceylan::Exception( reason )
{

}


XMLException::~XMLException() throw()
{

}

