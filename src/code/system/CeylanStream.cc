#include "CeylanStream.h"


#include "CeylanLogPlug.h"                    // for Log primitives
#include "CeylanOperators.h"   // for toString


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h"    // for irqInit and al (ARM9)
#endif // CEYLAN_ARCH_NINTENDO_DS



// Not available in their C++ form :
extern "C"
{


#if CEYLAN_ARCH_NINTENDO_DS

#include "fat.h"                          // for Chishm's libfat

#include <fcntl.h> 
#include <unistd.h> 

#endif // CEYLAN_ARCH_NINTENDO_DS


#ifdef CEYLAN_USES_ERRNO_H
#include <errno.h>             // for errno
#endif // CEYLAN_USES_ERRNO_H

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>            // for close on OpenBSD
#endif // CEYLAN_USES_UNISTD_H

}


#include <cerrno>


using namespace Ceylan::Log ;
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


#if CEYLAN_USES_FILE_DESCRIPTORS

	//LogPlug::trace( "Stream::Close: begin" ) ;
	
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

#else // CEYLAN_USES_FILE_DESCRIPTORS

	throw Stream::CloseException( "Ceylan::System::close : "
		"file descriptor feature not available" ) ;

#endif // CEYLAN_USES_FILE_DESCRIPTORS


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

	
