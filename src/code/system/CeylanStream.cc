#include "CeylanStream.h"


#include "CeylanOperators.h"   // for toString


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


// Not available in their C++ form :
extern "C"
{


#ifdef CEYLAN_USES_ERRNO_H
#include <errno.h>             // for errno
#endif // CEYLAN_USES_ERRNO_H

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>            // for close on OpenBSD
#endif // CEYLAN_USES_UNISTD_H

}


#include <cerrno>


using namespace Ceylan::System ;



Stream::Stream( bool blocking ) throw():
	_isBlocking( blocking )
{

}
		
		
Stream::~Stream() throw()
{

}


bool Stream::isBlocking() const throw()
{

	return _isBlocking ;
	
}


const std::string Stream::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	if ( _isBlocking )
		return "Blocking Stream" ;
	else	
		return "Non-blocking Stream" ;
		
}

	
bool Stream::Close( FileDescriptor & fd ) throw( CloseException )
{

	if ( fd > 0 )
	{
	
		int volatile ret ; 
		
		do
		{
			ret = ::close( fd ) ;
		}	
		while ( ( ret == -1 ) && ( errno == EINTR ) ) ;
		
		if ( ret == -1 )
		{
			
			switch( errno )
			{
			
				case EBADF:
					throw Stream::CloseException( "Ceylan::System::close : "
						"file descriptor " + Ceylan::toString( fd ) 
						+ " is invalid." ) ;
						
				case EIO:
					throw Stream::CloseException( "Ceylan::System::close : "
						"an I/O error occurred with file descriptor " 
						+ Ceylan::toString( fd ) + "." ) ;
							
				default:
					throw Stream::CloseException( "Ceylan::System::close : "
						"unexpected error occurred with file descriptor " 
						+ Ceylan::toString( fd ) + "." ) ;
						
			}	
		} 
		
		fd = 0 ;
		
		return true ;
		
	}

	return false ;

}


void Stream::setBlocking( bool newStatus ) 
	throw( NonBlockingNotSupportedException )
{

	// To be overriden by child classes supporting it (ex : sockets) :
	if ( newStatus == false )
		throw NonBlockingNotSupportedException( 
			"Stream::setBlocking : this stream cannot be put in "
			"non-blocking mode." ) ;

}

	
