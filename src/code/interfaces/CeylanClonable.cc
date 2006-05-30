#include "CeylanClonable.h"


#include "CeylanOperators.h"            // for string operators


using namespace Ceylan ;



ClonableException::ClonableException( const std::string & message ) throw() :
	Exception( "Clonable exception : " + message )
{

}			


ClonableException::~ClonableException() throw()
{
			
}
			
