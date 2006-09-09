#include "CeylanInputOutputStream.h"



using namespace Ceylan::System ;



InputOutputStream::InputOutputStream( bool blocking ) throw() :
	InputStream( blocking ),
	OutputStream( blocking )
{

}


InputOutputStream::~InputOutputStream() throw()
{

}


const std::string InputOutputStream::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	return "InputOutputStream, which is both an" 
		+ InputStream::toString( level ) + ", and an "
		+ OutputStream::toString( level ) ;

}
