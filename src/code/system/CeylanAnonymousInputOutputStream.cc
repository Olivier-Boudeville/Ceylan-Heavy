#include "CeylanAnonymousInputOutputStream.h"

#include "CeylanOperators.h"
#include "CeylanLogPlug.h"


using std::string ;
using std::list ;


using namespace Ceylan::System ;
using namespace Ceylan::Log ;



AnonymousInputOutputStream::AnonymousInputOutputStream()
		throw( Stream::StreamException )
{

}


AnonymousInputOutputStream::~AnonymousInputOutputStream() throw()
{
			
}


const std::string AnonymousInputOutputStream::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{

	return "AnonymousInputOutputStream" ;
		
}
