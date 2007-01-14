#include "CeylanPoint.h"   


using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Maths::Linear ;



Point::Point() throw() 
{
}


Point::~Point() throw() 
{
}


void Point::nullify() throw()
{
	setAllElementsTo( 0 ) ;
}


const string Point::toString( VerbosityLevels level ) const throw()
{
	return "Abstract point" ;
}

