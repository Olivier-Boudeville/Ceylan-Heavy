#include "CeylanLogTransport.h"


using std::string ;

using namespace Ceylan::Log ;



LogTransport::LogTransport() throw() 
{


}


LogTransport::~LogTransport() throw() 
{

}

const string LogTransport::toString( Ceylan::VerbosityLevels level ) const throw() 
{
	return "This LogTransport is a pure intermediary between a LogSource and a LogListener" ; 

}	
