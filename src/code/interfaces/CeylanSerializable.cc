#include "CeylanSerializable.h"


using std::string ;

using namespace Ceylan ;



SerializationException::SerializationException( 
		const std::string & reason ) throw() :
	Ceylan::Exception( reason ) 
{

}	


SerializationException::~SerializationException() throw()
{

}





