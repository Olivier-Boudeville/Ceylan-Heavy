#include "CeylanLinear.h"


using namespace Ceylan::Maths::Linear ;


LinearException::LinearException( const std::string & message ) throw() 
	: MathsException( "Linear exception : " + message ) 
{

}


LinearException::~LinearException() throw() 
{

}

