#include "CeylanAnonymousInputOutputStream.h"

#include "CeylanOperators.h"
#include "CeylanLogPlug.h"


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H




using std::string ;
using std::list ;


using namespace Ceylan::System ;
using namespace Ceylan::Log ;


AnonymousInputOutputStream::AnonymousInputOutputStream( FileDescriptor fd )
		throw( StreamException ):
	_fdes( fd )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw StreamException( "AnonymousInputOutputStream constructor : "
		"file descriptor feature not available." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}


AnonymousInputOutputStream::~AnonymousInputOutputStream() throw()
{

}


StreamID AnonymousInputOutputStream::getInputStreamID() const
{

	return static_cast<StreamID>( _fdes ) ;
}


StreamID AnonymousInputOutputStream::getOutputStreamID() const
{

	return static_cast<StreamID>( _fdes ) ;
}


const std::string AnonymousInputOutputStream::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{

	return "AnonymousInputOutputStream whose file descriptor is "
		+ Ceylan::toString( _fdes ) ;
		
}
