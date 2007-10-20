#include "CeylanTestException.h"

using std::string ;

using namespace Ceylan ;



TestException::TestException( const string & reason ) throw() :
	Exception ( "Test failure: " + reason )
{

}



TestException::~TestException() throw()
{

}

