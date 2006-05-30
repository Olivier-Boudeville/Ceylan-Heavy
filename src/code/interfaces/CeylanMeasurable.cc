#include "CeylanMeasurable.h"

#include "CeylanOperators.h"

using std::string ;

using namespace Ceylan ;


Measurable::Measurable() throw()
{

}


Measurable::~Measurable() throw()
{

}


const std::string Measurable::toString( Ceylan::VerbosityLevels level ) const throw()
{
	return "This Measurable uses " + Ceylan::toString( getSizeInMemory() ) + " bytes in memory" ;
}

