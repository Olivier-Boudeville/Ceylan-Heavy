#include "CeylanFunctor.h"

using std::string ;

using namespace Ceylan ;



Functor::Functor() throw()
{

}


Functor::~Functor() throw()
{

}


const string Functor::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{
	return "Basic functor" ;
}

