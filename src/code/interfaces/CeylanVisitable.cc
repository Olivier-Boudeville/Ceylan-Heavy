#include "CeylanVisitable.h"

using namespace Ceylan ;


Visitable::Visitable() throw()
{

}


void Visitable::accept( Visitor & visitor ) throw( VisitException )
{
	//visitor.visit( *this ) ;
}


const std::string Visitable::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{
	return "Visitable instance" ;
}