#include "CeylanAnonymousInputStream.h"

#include "CeylanOperators.h"
#include "CeylanLogPlug.h"


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H




using std::string ;
using std::list ;


using namespace Ceylan::System ;
using namespace Ceylan::Log ;


AnonymousInputStream::AnonymousInputStream( FileDescriptor fd )
		throw( StreamException ):
	_fdes( fd )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw StreamException( "AnonymousInputStream constructor : "
		"file descriptor feature not available." ) ;
		
#endif // CEYLAN_USES_FILE_DESCRIPTORS

}


AnonymousInputStream::~AnonymousInputStream() throw()
{

}


StreamID AnonymousInputStream::getInputStreamID() const
	throw( InputStreamException )
{

	return static_cast<StreamID>( _fdes ) ;
}


const std::string AnonymousInputStream::toString( 
	Ceylan::VerbosityLevels level ) const throw()
{

	return "AnonymousInputStream whose file descriptor is "
		+ Ceylan::toString( _fdes ) ;
		
}
