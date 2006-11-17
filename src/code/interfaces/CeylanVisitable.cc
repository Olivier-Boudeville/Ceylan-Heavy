#include "CeylanVisitable.h"

using namespace Ceylan ;



VisitException::VisitException( const std::string & reason ) throw():
	Ceylan::Exception( reason ) 
{

}


VisitException::~VisitException() throw()
{

}




Visitable::Visitable() throw()
{

}


Visitable::~Visitable() throw()
{

}



/*
 * Example of what the method should look like in child classes :
 *
 
void Visitable::accept( Visitor & visitor ) throw( VisitException )
{

	visitor.visit( *this ) ;
	
}

*/

