#include "CeylanInputStream.h"

#include "CeylanThread.h"      // for Sleep
#include "CeylanOperators.h"   // for toString
#include "CeylanLogPlug.h"     // for LogPlug
#include "CeylanTypes.h"       // for Ceylan::Uint16, etc.
#include "CeylanEndianness.h"  // for ceylan_bswap_*, etc.
#include "CeylanNetwork.h"     // for Windows getSocketError



/*
 * Implementation note :
 *
 * the endianness of the system is detected by CEYLAN_RUNS_ON_LITTLE_ENDIAN.
 *
 * Ceylan chosed to order its serialized mutli-byte datatypes according to
 * the little endian convention (Least Significant Byte first), despite it 
 * is the opposite of the usual case (network order is defined as being big
 * endian).
 *
 * The reason for that is that most modern platforms Ceylan is to be run on
 * are little endian, so it spares heavy useless swapping in this general
 * case.
 *
 * 
 */


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H



extern "C"
{

#ifdef CEYLAN_USES_SYS_TYPES_H
#include <sys/types.h>         // for select
#endif // CEYLAN_USES_SYS_TYPES_H

#ifdef CEYLAN_USES_UNISTD_H
#include <unistd.h>            // for select
#endif // CEYLAN_USES_UNISTD_H

#ifdef CEYLAN_USES_STRINGS_H
#include <strings.h>           // for select
#endif // CEYLAN_USES_STRINGS_H

#ifdef CEYLAN_USES_SYS_SELECT_H
#include <sys/select.h>        // for select
#endif // CEYLAN_USES_SYS_SELECT_H

#ifdef CEYLAN_USES_WINSOCK2_H
#include <winsock2.h>          // for select
#endif // CEYLAN_USES_WINSOCK2_H

}


#include <ctime>
#include <cerrno>  // for EBADF



using std::string ;
using std::list ;


using namespace Ceylan::System ;
using namespace Ceylan::Log ;
using namespace Ceylan ;


InputStream::InputStream( bool blocking ) throw() :
	Stream( blocking ),	
	_isSelected( false ),
	_isFaulty( false )
{

}


InputStream::~InputStream() throw()
{

}


bool InputStream::isSelected() const throw()
{
	return _isSelected ;
}


bool InputStream::isFaulty() const throw()
{
	return _isFaulty ;
}	


const std::string InputStream::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "InputStream whose ID is " 
		+ Ceylan::toString( getInputStreamID() ) + ", which is " ;
		
	if ( ! isSelected() )
		res += "not " ;
		 
	res += "selected, and which is " ;

	if ( ! isFaulty() )
		res += "not " ;
	
	res += "faulty" ;
	
	res += ". This is a " + Stream::toString( level ) ;

	return res ;
	
}


// Read section.



Size InputStream::read( Ceylan::Byte * buffer, Size length )
	throw( ReadFailedException )
{

	throw ReadFailedException( "InputStream::read failed : "
		"this method should have been subclassed." ) ;
		
}


void InputStream::clearInput() throw( InputStream::ReadFailedException )
{
	
	Ceylan::Byte b ;

	while ( hasAvailableData() )
		read( &b, 1 ) ;

}



/*
 * The endianness of the target system is to be read from the configure step, 
 * the symbol is CEYLAN_RUNS_ON_LITTLE_ENDIAN.
 *
 */
				


// Read integer types subsection.


Ceylan::Sint8 InputStream::readSint8() 
	throw( ReadFailedException, EOFException )
{
	
	const Ceylan::Uint8 TypeSize = 1 ;
	
	Ceylan::Byte tempBuffer[ TypeSize ] ;
		
	Size readCount = read( tempBuffer, TypeSize ) ;
	
	if ( readCount < TypeSize ) 
		throw EOFException( "InputStream::readSint8" ) ;
	
	return * reinterpret_cast<Ceylan::Sint8 *>( tempBuffer ) ;
			
}


Ceylan::Uint8 InputStream::readUint8() 
	throw( ReadFailedException, EOFException )
{

	
	const Ceylan::Uint8 TypeSize = 1 ;
	
	Ceylan::Byte tempBuffer[ TypeSize ] ;
		
	Size readCount = read( tempBuffer, TypeSize ) ;
	
	if ( readCount < TypeSize ) 
		throw EOFException( "InputStream::readUint8" ) ;
	
	return * reinterpret_cast<Ceylan::Uint8 *>( tempBuffer ) ;
			
}


Ceylan::Sint16 InputStream::readSint16() 
	throw( ReadFailedException, EOFException )
{

	const Ceylan::Uint8 TypeSize = 2 ;
	
	Ceylan::Byte tempBuffer[ TypeSize ] ;
		
	Size readCount = read( tempBuffer, TypeSize ) ;
	
	if ( readCount < TypeSize ) 
		throw EOFException( "InputStream::readSint16" ) ;
	
	Ceylan::Sint16 * ret = reinterpret_cast<Ceylan::Sint16 *>( tempBuffer ) ;
	

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return *ret ;
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return ceylan_bswap_16( *ret ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
	
		
}


Ceylan::Uint16 InputStream::readUint16() 
	throw( ReadFailedException, EOFException )
{

	const Ceylan::Uint8 TypeSize = 2 ;
	
	Ceylan::Byte tempBuffer[ TypeSize ] ;
		
	Size readCount = read( tempBuffer, TypeSize ) ;
	
	if ( readCount < TypeSize ) 
		throw EOFException( "InputStream::readUint16" ) ;
	
	Ceylan::Uint16 * ret = reinterpret_cast<Ceylan::Uint16 *>( tempBuffer ) ;
	

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return *ret ;
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return ceylan_bswap_16( *ret ) ;
	
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
	
		
}


Ceylan::Sint32 InputStream::readSint32() 
	throw( ReadFailedException, EOFException )
{

	const Ceylan::Uint8 TypeSize = 4 ;
	
	Ceylan::Byte tempBuffer[ TypeSize ] ;
		
	Size readCount = read( tempBuffer, TypeSize ) ;
	
	if ( readCount < TypeSize ) 
		throw EOFException( "InputStream::readSint32" ) ;
	
	Ceylan::Sint32 * ret = reinterpret_cast<Ceylan::Sint32 *>( tempBuffer ) ;
	

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return *ret ;
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return ceylan_bswap_32( *ret ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
	
		
}


Ceylan::Uint32 InputStream::readUint32() 
	throw( ReadFailedException, EOFException )
{

	const Ceylan::Uint8 TypeSize = 4 ;
	
	Ceylan::Byte tempBuffer[ TypeSize ] ;
		
	Size readCount = read( tempBuffer, TypeSize ) ;
	
	if ( readCount < TypeSize ) 
		throw EOFException( "InputStream::readUint32" ) ;
	
	Ceylan::Uint32 * ret = reinterpret_cast<Ceylan::Uint32 *>( tempBuffer ) ;
	

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return *ret ;
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return ceylan_bswap_32( *ret ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
	
		
}


Ceylan::Float32 InputStream::readFloat32() 
	throw( ReadFailedException, EOFException )
{

	const Ceylan::Uint8 TypeSize = 4 ;
	
	Ceylan::Byte tempBuffer[ TypeSize ] ;
		
	Size readCount = read( tempBuffer, TypeSize ) ;
	
	if ( readCount < TypeSize ) 
		throw EOFException( "InputStream::readFloat32" ) ;
	
	Ceylan::Float32 * ret = reinterpret_cast<Ceylan::Float32 *>( tempBuffer ) ;
	

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return *ret ;
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return ceylan_bswap_32( *ret ) ;
	
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
	
		
}


Ceylan::Float64 InputStream::readFloat64() 
	throw( ReadFailedException, EOFException )
{

	const Ceylan::Uint8 TypeSize = 8 ;
	
	Ceylan::Byte tempBuffer[ TypeSize ] ;
		
	Size readCount = read( tempBuffer, TypeSize ) ;
	
	if ( readCount < TypeSize ) 
		throw EOFException( "InputStream::readFloat64" ) ;
	
	Ceylan::Float64 * ret = reinterpret_cast<Ceylan::Float64 *>( tempBuffer ) ;
	

#if CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return *ret ;
	
#else // CEYLAN_RUNS_ON_LITTLE_ENDIAN

	return ceylan_bswap_64( *ret ) ;
	
#endif // CEYLAN_RUNS_ON_LITTLE_ENDIAN
	
		
}


void InputStream::readString( std::string & result ) 
	throw( ReadFailedException, EOFException )
{


	/*
	 * The protocol used to marshal/demarshall a string is to specify first
	 * its length in characters, then to make the characters follow this 
	 * size.
	 *
	 */
	 
	Uint16 stringSize = readUint16() ;
	
#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
	LogPlug::debug( "InputStream::readString : string size is "
		+ Ceylan::toString( stringSize ) + " characters." ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS
	 
	 
	// Blanks the result :
	result.erase() ;
	
	if ( stringSize == 0 )
		return ;
	
	// The size above which the heap is preferred to the stack :
	const Sint16 thresholdSize = 255 ; 	
	
	if ( stringSize < thresholdSize )
	{
	
		// Use a simple automatic buffer :
	
		Ceylan::Byte tempBuffer[thresholdSize] ;

		Size readSize = read( tempBuffer, stringSize ) ;

		if ( readSize < stringSize )
			throw EOFException( "In InputStream::readString." ) ;
		
		// Null-terminated string :
		tempBuffer[stringSize] = 0 ;
		
		result = tempBuffer ;	
				
	}
	else
	{
	
	
		/*
		 * Use a dedicated dynamically created buffer :
		 *
		 * (Byte type is char)
		 *
		 */
		
		Ceylan::Byte * tempBuffer = new Ceylan::Byte[ stringSize + 1 ] ;
		
		Size readSize = read( tempBuffer, stringSize ) ;
		
		if ( readSize < stringSize )
		{
			delete tempBuffer ;
			throw EOFException( "In InputStream::readString." ) ;
		}	
		
		// Null-terminated string :
		tempBuffer[stringSize] = 0 ;
		
		result = tempBuffer ;
		delete tempBuffer ;
		
		
	}
	
	
}


Ceylan::Uint16 InputStream::Select( list<InputStream*> & is ) 
	throw( InputStream::SelectFailedException )
{

#if CEYLAN_ARCH_WINDOWS

#if CEYLAN_USES_NETWORK

	// Used only for sockets :
	if ( is.empty() )
		return 0 ;

	// Creates the set of waiting file descriptors.
	fd_set waitFDSet ;
	FD_ZERO( & waitFDSet ) ;

	// Aggregates them and blanks their selected status.
	FileDescriptor maxFD = /* unsigned, on Windows */ 0 ;

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

	
	/// Maximum number of select attempts.

	/**
	 * Each thread will wait for selectWaitingTime seconds before 
	 * retrying a blocking select.
	 *
	 */
	const Ceylan::Uint8 selectWaitingTime = 2 ;

	Ceylan::Uint8 attemptCount = selectAttemptCount ;

	int selectedCount ;
	
	for ( ; attemptCount; attemptCount-- )
	{
		
			
		// Will block for ever until an error or some data is coming :
		selectedCount = ::select( 
			/* number of FD */ maxFD + 1, 
			/* for reading  */ & waitFDSet, 
			/* for writing  */ 0, 
			/* for errors   */ 0, 
			/* for time-out */ 0 ) ;
		
		if ( selectedCount == SOCKET_ERROR )
		{
	
			// Error :
			Network::SocketError error = Network::getSocketError() ;
			if ( error == WSAEBADF || error == WSAENOTSOCK )
			{
			
				LogPlug::error( "InputStream::Select : there seems to be "
					"at least one bad file descriptor in the list, "
					"checking them to flag them as faulty ("
					+ ( ( error == WSAEBADF ) ? 
						string( "WSAEBADF" ) : string( "WSAENOTSOCK" ) )
					+ ")." ) ;
				
				for ( list<InputStream*>::iterator it = is.begin(); 
					it != is.end(); it++ )
				{
				
					fd_set testFDSet ;
					FD_ZERO( & testFDSet ) ;
					
					FileDescriptor testedFD = (*it)->getInputStreamID() ;
					FD_SET( testedFD, & testFDSet ) ;
					
					// No waiting desired :
					struct timeval timeout ;
					timeout.tv_sec  = 0 ;
           			timeout.tv_usec = 0 ;
					
					if ( ::select( 
						/* number of FD */ testedFD + 1, 
						/* for reading  */ &testFDSet, 
						/* for writing  */ 0, 
						/* for errors   */ 0, 
						/* for time-out */ &timeout ) == SOCKET_ERROR )
					{

						error = Network::getSocketError() ;
						if ( error == WSAEBADF || error == WSAENOTSOCK )
						{
					
							LogPlug::error( "InputStream::Select : "
								"following stream is faulty : " + (*it)->toString()
								+ ", flagging it as such ("	
								+ ( ( error == WSAEBADF ) ? 
									string( "WSAEBADF" ) : string( "WSAENOTSOCK" ) )
								+ ")." ) ;
							(*it)->setFaulty( true ) ;
						
						}
							
					}
				
				}
			}
			else
			{
			
				LogPlug::error( "InputStream::Select : "
					"no InputStream selected ("	+ Network::explainSocketError()
					+ "), retrying in " + Ceylan::toString( selectWaitingTime )
					+ " second(s) ..." ) ;
					
				Thread::Sleep( selectWaitingTime ) ;
					
			}
				
			
		}
		else
		{
			if ( selectedCount > 0 )
				break ;
		}
		
	} // for : more select attempts left ?

	if ( attemptCount == 0 )
		throw SelectFailedException( string( "After " )
			+ selectAttemptCount
			+ " attempts, still no InputStream selected." ) ;


	selectedCount = 0 ;
	for ( list<InputStream*>::iterator it = is.begin(); it != is.end(); it++ )
	{
		if ( *it && ( FD_ISSET( (*it)->getInputStreamID(), & waitFDSet ) ) )
		{
			selectedCount++ ;
			(*it)->setSelected( true ) ;
		}
	}

	return selectedCount ;


#else // CEYLAN_USES_NETWORK

	throw InputStream::SelectFailedException( 
		"InputStream::Select operation not supported on Windows "
		"when the network feature is disabled." ) ;

#endif // CEYLAN_USES_NETWORK

#else // CEYLAN_ARCH_WINDOWS

#if CEYLAN_USES_FILE_DESCRIPTORS


	if ( is.empty() )
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

	
	/// Maximum number of select attempts.

	/**
	 * Each thread will wait for selectWaitingTime seconds before 
	 * retrying a blocking select.
	 *
	 */
	const Ceylan::Uint8 selectWaitingTime = 2 ;

	Ceylan::Uint8 attemptCount = selectAttemptCount ;

	int selectedCount ;
	
	for ( ; attemptCount; attemptCount-- )
	{
		
			
		// Will block for ever until an error or some data is coming :
		selectedCount = ::select( 
			/* number of FD */ maxFD + 1, 
			/* for reading  */ & waitFDSet, 
			/* for writing  */ 0, 
			/* for errors   */ 0, 
			/* for time-out */ 0 ) ;
		
		if ( selectedCount < 0 )
		{
	
			// Error :
			
			if ( System::getError() == EBADF )
			{
			
				LogPlug::error( "InputStream::Select : there seems to be "
					"at least one bad file descriptor in the list, "
					"checking them to flag them as faulty." ) ;
				
				for ( list<InputStream*>::iterator it = is.begin(); 
					it != is.end(); it++ )
				{
				
					fd_set testFDSet ;
					FD_ZERO( & testFDSet ) ;
					
					FileDescriptor testedFD = (*it)->getInputStreamID() ;
					FD_SET( testedFD, & testFDSet ) ;
					
					// No waiting desired :
					struct timeval timeout ;
					timeout.tv_sec  = 0 ;
           			timeout.tv_usec = 0 ;
					
					if ( ( ::select( testedFD + 1, 
							&testFDSet, 0, 0, &timeout ) < 0 )
						&& ( System::getError() == EBADF ) )
					{
					
						LogPlug::error( "InputStream::Select : "
							"following stream is faulty : " + (*it)->toString()
							+ ", flagging it as such." ) ;
						(*it)->setFaulty( true ) ;
							
					}
				
				}
			}
			else
			{
			
				LogPlug::error( "InputStream::Select : "
					"no InputStream selected ("	+ explainError()
					+ "), retrying in " + Ceylan::toString( selectWaitingTime )
					+ " second(s) ..." ) ;
					
				Thread::Sleep( selectWaitingTime ) ;
					
			}
				
			
		}
		else
		{
			if ( selectedCount > 0 )
				break ;
		}
		
	} // for : more select attempts left ?

	if ( attemptCount == 0 )
		throw SelectFailedException( string( "After " )
			+ selectAttemptCount
			+ " attempts, still no InputStream selected." ) ;


	selectedCount = 0 ;
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

#endif // CEYLAN_ARCH_WINDOWS

}



Ceylan::Uint16 InputStream::Test( list<InputStream*> & is ) 
	throw( InputStream::SelectFailedException )
{

#if CEYLAN_USES_FILE_DESCRIPTORS

	if ( is.empty() )
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
	
		// Error :
			
		if ( System::getError() == EBADF )
		{
			
			LogPlug::error( "InputStream::Test : there seems to be "
				"at least one bad file descriptor in the list, "
				"checking them to flag them as faulty." ) ;
				
			for ( list<InputStream*>::iterator it = is.begin(); 
				it != is.end(); it++ )
			{
				
				fd_set testFDSet ;
				FD_ZERO( & testFDSet ) ;
					
				FileDescriptor testedFD = (*it)->getInputStreamID() ;
				FD_SET( testedFD, & testFDSet ) ;
				
				// No waiting desired :
				struct timeval timeout ;
				timeout.tv_sec  = 0 ;
           		timeout.tv_usec = 0 ;
					
				if ( ( ::select( testedFD + 1, 
						&testFDSet, 0, 0, &timeout ) < 0 )
					&& ( System::getError() == EBADF ) )
				{
					
					LogPlug::error( "InputStream::Select : "
						"following stream is faulty : " + (*it)->toString()
						+ ", flagging it as such." ) ;
				
					(*it)->setFaulty( true ) ;
							
				}
				
			}
			
		}
		else // a real error :
		{
				
			throw SelectFailedException(
				"InputStream select failed in non-blocking test method : "
				+ explainError( getError() ) ) ;
		}
		
		
	}


	Ceylan::Uint16 selectedCount = 0 ;
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



// protected section.


void InputStream::setSelected( bool newStatus ) throw()
{

#if CEYLAN_DEBUG_LOW_LEVEL_STREAMS
	if ( ( _isSelected == false ) && ( newStatus == true ) )
		LogPlug::debug( "InputStream::setSelected : selecting descriptor #"
			+ Ceylan::toString( getInputStreamID() ) ) ;
#endif // CEYLAN_DEBUG_LOW_LEVEL_STREAMS
			
	_isSelected = newStatus ;
	
}


void InputStream::setFaulty( bool newFaultyState ) throw()
{
	_isFaulty = newFaultyState ;
}


void InputStream::setStreamID( StreamID newInputStreamID ) 
	throw( InputStreamException )
{
	throw InputStreamException( "InputStream::setStreamID : "
		"operation not supportedby this class." ) ;
}

