#include "CeylanVector.h"   


using std::string ;


using namespace Ceylan ;
using namespace Ceylan::Maths ;
using namespace Ceylan::Maths::Linear ;



Vector::Vector() throw() 
{
}


Vector::~Vector() throw() 
{
}


void Vector::nullify() throw()
{
	setAllElementsTo( 0 ) ;
}

	
const string Vector::toString( VerbosityLevels level ) const throw()
{

	return "Abstract vector" ;
				
}

