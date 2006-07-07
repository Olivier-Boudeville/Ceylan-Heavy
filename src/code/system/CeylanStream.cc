#include "CeylanStream.h"


#include "CeylanOperators.h"   // for toString


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


#ifdef CEYLAN_USES_ERRNO_H
#include <errno.h>             // for errno
#endif // CEYLAN_USES_ERRNO_H



using namespace Ceylan::System ;


Stream::Stream() throw()
{

}
		
Stream::~Stream() throw()
{

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
