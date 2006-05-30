#include "CeylanInputStream.h"

#include "CeylanThread.h"
#include "CeylanOperators.h"
#include "CeylanLogPlug.h"


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H



extern "C"
{

#ifdef CEYLAN_USES_SYS_TYPES_H
//#include <sys/types.h>
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_STRINGS_H
#include <strings.h>
#endif // CEYLAN_USES_STRINGS_H

#ifdef CEYLAN_USES_SYS_SELECT_H
#include <sys/select.h>
#endif // CEYLAN_USES_SYS_SELECT_H

}

#include <ctime>




using std::string ;
using std::list ;


using namespace Ceylan::System ;
using namespace Ceylan::Log ;


InputStream::InputStream() throw() :
	_isSelected( false )
{

}


InputStream::~InputStream() throw()
{

}


void InputStream::setSelected( bool newStatus ) throw()
{
	_isSelected = newStatus ;
}





int InputStream::Select( list<InputStream*> & is ) 
	throw( InputStream::SelectFailedException )
{


#if CEYLAN_USES_FILE_DESCRIPTORS

	if ( is.size() == 0 )
		return 0 ;

	// Creates the set of waiting file descriptors.
	fd_set waitFDSet ;
	FD_ZERO( & waitFDSet ) ;

	// Aggregates them and blanks their selected status.
	FileDescriptor maxFD = -1 ;

	for ( list<InputStream*>::iterator it = is.begin(); it != is.end(); it++ )
	{
		if ( *it )
		{
			FileDescriptor fd = (*it)->getInputStreamID() ;
			FD_SET( fd, & waitFDSet ) ;
			(*it)->setSelected( false ) ;
			if ( fd > maxFD )
				maxFD = fd ;
		}
	}

	/// Maximum number of select attempts.
	const int selectAttemptCount = 5 ;

	/**
	 * Each thread will wait for selectWaitingTime seconds before 
	 * retrying select.
	 *
	 */
	const int selectWaitingTime = 2 ;

	int attemptCount = selectAttemptCount ;

	for ( ; attemptCount; attemptCount-- )
	{
		if ( ::select( maxFD + 1, & waitFDSet, 0, 0, 0 ) < 0 )
		{

			LogPlug::debug( "No InputStream selected ("
				+ explainError( getError() )
				+ string( "), retrying in " )
				+ selectWaitingTime
				+ string( " ..." ) ) ;
			Thread::sleep( selectWaitingTime ) ;
		}
		else
		{
			break ;
		}
	}

	if ( attemptCount == 0 )
		throw SelectFailedException( string( "After " )
			+ selectAttemptCount
			+ " attempts, still no InputStream selected." ) ;


	int selectedCount = 0 ;
	for ( list<InputStream*>::iterator it = is.begin(); it != is.end(); it++ )
	{
		if ( *it && ( FD_ISSET( (*it)->getInputStreamID(), & waitFDSet ) ) )
		{
			selectedCount++ ;
			(*it)->setSelected( true ) ;
		}
	}

	return selectedCount ;

#else // CEYLAN_USES_FILE_DESCRIPTORS
	
	// No file descriptors available on platforms such as Windows :
	throw SelectFailedException( "InputStream::Select : "
		"File descriptor feature not available on this platform." ) ;
	
#endif // CEYLAN_USES_FILE_DESCRIPTORS	
	
}


int InputStream::Test( list<InputStream*> & is ) 
	throw( InputStream::SelectFailedException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	if ( is.size() == 0 )
		return 0 ;

	// Asks to return immediatly.
	struct timeval tv ;
	tv.tv_sec  = 0 ;
	tv.tv_usec = 0 ;


	fd_set waitFDSet ;
	FD_ZERO( & waitFDSet ) ;

	FileDescriptor maxFD = -1 ;

	// Erase selected status.
	for ( list<InputStream*>::iterator it = is.begin(); it != is.end(); it++ )
	{
		if ( *it )
		{
			FileDescriptor fd = (*it)->getInputStreamID() ;
			FD_SET( fd, & waitFDSet ) ;
			(*it)->setSelected( false ) ;
			if ( fd > maxFD )
				maxFD = fd ;
		}
	}

	if ( ::select( maxFD + 1, & waitFDSet, 0, 0, & tv ) < 0 )
	{
		throw SelectFailedException(
			"InputStream select failed in non-blocking test method : "
			+ explainError( getError() ) ) ;
	}

	int selectedCount = 0 ;
	for ( list<InputStream*>::iterator it = is.begin(); it != is.end(); it++ )
	{
		if ( *it && ( FD_ISSET( (*it)->getInputStreamID(), & waitFDSet ) ) )
		{
			selectedCount++ ;
			(*it)->setSelected( true ) ;
		}
	}

	return selectedCount ;
	
#else // if CEYLAN_USES_FILE_DESCRIPTORS
	
	// No file descriptors available on platforms such as Windows :
	throw SelectFailedException( "InputStream::Test : "
		"File descriptor feature not available on this platform." ) ;
	
#endif // if CEYLAN_USES_FILE_DESCRIPTORS	

}
