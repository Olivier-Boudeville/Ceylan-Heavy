#include "CeylanLog.h"

using std::string ;


using namespace Ceylan::Log ;


/// Let's define in a centralized way informations shared at the framework level.


LogException::LogException( const std::string & reason ) throw() :
 	Ceylan::Exception( reason ) 
{

}

	
LogException::~LogException() throw()
{

}

