#include "CeylanFIFO.h"

#include "CeylanLogPlug.h"             // for Log primitives
#include "CeylanOperators.h"           // for toNumericalString, etc.
#include "CeylanSystem.h"              // for ConvertToNonCacheable
#include "CeylanUtils.h"               // for waitForKey


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"              // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


#if CEYLAN_ARCH_NINTENDO_DS
#include "CeylanConfigForNintendoDS.h" // for FIFO defines, etc.
#endif // CEYLAN_ARCH_NINTENDO_DS


#include "CeylanARM7Codes.h"           // for ARM7 status and error values
#include "CeylanIPCCommands.h"         // for IPC command management



using std::string ;

using namespace Ceylan::System ;
using namespace Ceylan::Log ;
using namespace Ceylan ;



#if CEYLAN_DEBUG_FIFO

#define CEYLAN_LOG_FIFO(message) LogPlug::debug(message)

#else // CEYLAN_DEBUG_FIFO

#define CEYLAN_LOG_FIFO(message) 

#endif // CEYLAN_DEBUG_FIFO


// CEYLAN_SAFE_FIFO defined for both ARMs in CeylanARM7Codes.h


/*
 * Implementation notes:
 * 
 * @see libnds include/nds/ipc.h for defines.
 * @see http://www.neimod.com/dstek/dstek2.xml#Interprocessor%20Communication
 *
 * @note If ever a variable is read in the main function and in an IRQ handler,
 * by all means declare it as volatile. The other ARM is not the only cause
 * of using this qualifier, it is useful for IRQ handler as well !
 *
 * Code called from an IRQ handler (ex: handle*) does not need to be 
 * specifically protected, execution-wise, as interrupts are disabled during
 * them, but code called from a sent request (ex: sent*) have to disable
 * interrupts, using preferably SetEnabledInterrupts, otherwise commands might
 * collide in the FIFO.
 *
 */
 

// No FIFO registered at start-up:
FIFO * FIFO::_FIFO = 0 ;



FIFO::FIFOException::FIFOException( const string & reason ) throw():
	SystemException( reason )
{

}


FIFO::FIFOException::~FIFOException() throw()
{

}



			

FIFO::FIFOFull::FIFOFull( const string & reason ) throw():
	FIFO::FIFOException( reason )
{

}
		

FIFO::FIFOEmpty::FIFOEmpty( const string & reason ) throw():
	FIFO::FIFOException( reason )
{

}

				


FIFO::FIFO() throw( FIFOException ):
	_arm7StatusWordPointer( 0 ),
	_arm7ErrorCodePointer( 0 ),
	_localCommandCount( 0 ),
	_remoteCommandCount( 0 ),
	_processedCount( 0 ),
	_sentCount( 0 )
{

	// LogPlug::trace( "FIFO constructor" ) ;

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9
	
	if ( FIFO::_FIFO != 0 )
		throw FIFOException( "FIFO constructor failed: "
			"there is already a FIFO instance registered" ) ;

	/*
	 * Two approaches here to avoid the ARM7 sees obsolete values in the 
	 * main RAM while the correct ones sit in the ARM9 cache only:
	 *
	 *   - method #1 (CEYLAN_USES_CACHE_FLUSH = 1), currently working:
	 * we write values as usual (hence they are in the ARM9 cache) and then we
	 * flush that cache to force their writing in the main RAM
	 *
	 *   - method #2 (CEYLAN_USES_CACHE_FLUSH = 0), currently *not* working:
	 * we write values in the non-cacheable mirror RAM space (hence the ARM9
	 * cache is bypassed and values are directly available for the ARM7 in the
	 * main RAM) and then we invalidate the cache so that it will not by 
	 * flushed over the correct values we just set
	 * 
	 */
#define CEYLAN_USES_CACHE_FLUSH 1


	/*
	 * @note A 'new' is forced instead of a class member, as if the FIFO is 
	 * created as an automatic variable, this variable would be in the ARM9
	 * stack, hence out of range for the ARM7.
	 *
	 */

#if CEYLAN_USES_CACHE_FLUSH

	/*
	 * Too many times the handshake failed because either the status or the 
	 * error code had been updated by the ARM7, despite using the volatile
	 * keyword, DC_FlushRange/DC_InvalidateRange and reading from non-cacheable
	 * mirror.
	 *
	 * For increased safety we now reserve a full cache line were both ARM7
	 * variables are placed, and only them.
	 *
	 */
	
	// 32-byte cache line: 
	Ceylan::Byte * cacheLine = CacheProtectedNew( sizeof( ARM7StatusWord ) +
		sizeof( ARM7ErrorCode ) ) ;   
	
	// First the status word:
	
	_arm7StatusWordPointer = (volatile ARM7StatusWord* volatile) cacheLine ;
		
	(*_arm7StatusWordPointer) = NoStatusAvailable ;

	if ( getLastARM7StatusWord() != NoStatusAvailable )
		LogPlug::warning( "FIFO constructor: "
			"ARM7 status setting failed before flushing cache" ) ;

	/*
	 * Not flushed directly, as may affect the error code if flushing
	 * the cache line and if this line contains both ARM7 variables.
	 *
	 */	

	// Then the error code:
	
	_arm7ErrorCodePointer = (volatile ARM7StatusWord* volatile) (
		cacheLine + sizeof( ARM7StatusWord ) );
	
	(*_arm7ErrorCodePointer) = NoErrorAvailable ;

	atomicSleep() ;
	atomicSleep() ;
	atomicSleep() ;
	atomicSleep() ;
	atomicSleep() ;
	
	if ( getLastARM7ErrorCode() != NoErrorAvailable )
		LogPlug::warning( "FIFO constructor: "
			"ARM7 error setting failed before flushing cache" ) ;

	//More specific than 'DC_FlushAll() ;':
	DC_FlushRange( (void*) _arm7StatusWordPointer, sizeof(ARM7StatusWord) ) ;

	if ( getLastARM7StatusWord() != NoStatusAvailable )
		throw FIFOException( "FIFO constructor: "
			"ARM7 status setting failed after flushing cache" ) ;

	//More specific than 'DC_FlushAll() ;':
	DC_FlushRange( (void*) _arm7ErrorCodePointer, sizeof(ARM7ErrorCode) ) ;
	
	if ( getLastARM7ErrorCode() != NoErrorAvailable )
		throw FIFOException( "FIFO constructor: "
			"ARM7 error setting failed after flushing cache" ) ;

	DC_InvalidateRange( (void*) _arm7StatusWordPointer, 
		sizeof(ARM7StatusWord) ) ;
		
	DC_InvalidateRange( (void*) _arm7ErrorCodePointer, 
		sizeof(ARM7ErrorCode) ) ;
		
	
#else // CEYLAN_USES_CACHE_FLUSH


	/*
	 * Does not work, because the cache overwrites the mirrored data. 
	 * So we set the correct values in the mirror and try to invalidate the
	 * cache, but apparently the correct values are overwritten by the 
	 * deprecated ones, from the cache, nevertheless.
	 *
	 */
	
	ARM7StatusWord * tempStatus = new ARM7StatusWord ;
	
	_arm7ErrorCodePointer  = /* (volatile ARM7StatusWord*) */
		ConvertToNonCacheable<ARM7StatusWord>( tempStatus ) ;

	(*_arm7StatusWordPointer) = NoStatusAvailable ;
	*tempStatus = NoStatusAvailable ;
	
	DC_InvalidateAll() ;
	//DC_InvalidateRange( (void *) tempStatus, sizeof( ARM7StatusWord ) ) ;
	
	if ( getLastARM7StatusWord() != NoStatusAvailable )
		throw FIFOException( "FIFO constructor: "
			"ARM7 status setting failed, read: "
			+ Ceylan::toString( getLastARM7StatusWord() ) ) ;


	ARM7ErrorCode * tempError = new ARM7ErrorCode ;

	_arm7ErrorCodePointer  = /* (volatile ARM7StatusWord*) */
		ConvertToNonCacheable<ARM7ErrorCode>( tempError ) ;

	(*_arm7ErrorCodePointer) = NoErrorAvailable ;
	*tempError = NoErrorAvailable ;

	DC_InvalidateAll() ;
	//DC_InvalidateRange( (void *) tempError, sizeof( ARM7ErrorCode ) ) ;

	if ( getLastARM7StatusWord() != NoErrorAvailable )
		throw FIFOException( "FIFO constructor: "
			"ARM7 error setting failed, read: "
			+ Ceylan::toString( getLastARM7ErrorCode() ) ) ;

#endif // CEYLAN_USES_CACHE_FLUSH



	// Interrupts will be used here:
	System::InitializeInterrupts() ;


	/*
	 * FIFO not empty IRQ (IRQ_FIFO_NOT_EMPTY) not used anymore, as 
	 * IRQ_IPC_SYNC is considered more appropriate.
	 *
	 
	irqSet( IRQ_FIFO_NOT_EMPTY, SyncHandlerForFIFO ) ; 
    irqEnable( IRQ_FIFO_NOT_EMPTY ) ;
	
	 */


	/*
	 * Fully managed by the Ceylan FIFO system:
	 *
	 */
	irqSet( IRQ_IPC_SYNC, SyncHandlerForFIFO ) ; 
	 
	 
	/*
	 * Fail-over VBlank handler added here, as needed at least for atomic
	 * sleep.
	 *
	 * It may be associated with an user-specific handler for other
	 * purposes later, as long as it is not disabled (otherwise atomic sleeps
	 * would freeze forever).
	 *
	 */
	irqSet( IRQ_VBLANK, 0 ) ;
	
	// Unleashes these IRQ: 
    irqEnable( IRQ_IPC_SYNC | IRQ_VBLANK ) ;

	// Last action is to register this FIFO:
	_FIFO = this ;		


	if ( getLastARM7StatusWord() != NoStatusAvailable )
		throw FIFOException( "FIFO constructor: "
			"ARM7 status setting failed, read finally: "
			+ Ceylan::toString( getLastARM7StatusWord() )  ) ;
		
		
	if ( getLastARM7ErrorCode() != NoErrorAvailable )
		throw FIFOException( "FIFO constructor: "
			"ARM7 error setting failed, read finally: "
			+ Ceylan::toString( getLastARM7ErrorCode() )  ) ;

	
#else // CEYLAN_RUNS_ON_ARM9

	throw FIFO::FIFOException( "FIFO constructor failed: "
		"not available on the ARM7." ) ;

#endif // CEYLAN_RUNS_ON_ARM9
		
		
#else // CEYLAN_ARCH_NINTENDO_DS

	throw FIFO::FIFOException( "FIFO constructor failed: "
		"not available on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



FIFO::~FIFO() throw()
{

	// LogPlug::trace( "FIFO destructor" ) ;
	
	/*
	 * Must have been called beforehand, otherwise pure method calls could
	 * be issued:
	
	deactivate() ;

	 */
	 
	_FIFO = 0 ;

	// Will deallocate both ARM7 variables:
	CacheProtectedDelete( (Ceylan::Byte*) _arm7StatusWordPointer ) ;

	_arm7StatusWordPointer = 0 ;
	_arm7ErrorCodePointer = 0 ;

	/*
	
	if ( _arm7StatusWordPointer != 0 )
		delete _arm7StatusWordPointer ;
	
	_arm7StatusWordPointer = 0 ;
	
	
	if ( _arm7ErrorCodePointer != 0 )
		delete _arm7ErrorCodePointer ;
		
	_arm7ErrorCodePointer = 0 ;
		
	*/
	
}



void FIFO::activate() throw( FIFOException )
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	// LogPlug::trace( "FIFO activate" ) ;

	ARM7StatusWord initialStatus = getLastARM7StatusWord() ;
	
	ARM7ErrorCode initialError = getLastARM7ErrorCode() ;
	
	CEYLAN_LOG_FIFO( "FIFO::activate: before sending command, "
		"the ARM7 status word is equal to " 
		+ Ceylan::toString( initialStatus ) + ", the error code is "
		+ Ceylan::toString( initialError ) ) ;
		
		
	if ( initialStatus != NoStatusAvailable )
		throw FIFOException( "FIFO::activate: "
			"ARM7 status word not set to correct initial value: read "
			+ Ceylan::toString( initialStatus ) + ", instead of "
			+ Ceylan::toString( NoStatusAvailable ) ) ;

	if ( initialError != NoErrorAvailable )
		throw FIFOException( "FIFO::activate: "
			"ARM7 error code not set to correct initial value: read "
			+ Ceylan::toString( initialError ) + ", instead of "
			+ Ceylan::toString( NoErrorAvailable ) ) ;


	// Reset counters if needed: 
	
	_localCommandCount = 0 ;
	_remoteCommandCount = 0 ;
	
	_processedCount = 0 ;
	_sentCount = 0 ;
	
	
	/*
	 * REG_IPC_FIFO_CR is the FIFO *control* register, and:
	 *  - IPC_FIFO_ENABLE enables the FIFO
	 *  - IPC_FIFO_SEND_CLEAR flushes the send FIFO
	 *  - (not used anymore) IPC_FIFO_RECV_IRQ request an IRQ to be triggered 
	 * on a transition, on the receive FIFO, from empty to not empty	 
	 *
	 */
  	REG_IPC_FIFO_CR = IPC_FIFO_ENABLE | IPC_FIFO_SEND_CLEAR ;

	
	/*
	 * IPC_SYNC_IRQ_ENABLE allows the ARM7 to trigger IPC_SYNC IRQ on this
	 * ARM9:
	 *
	 * ('=', not '|=', to nullify the rest of the register, not expecting to
	 * write on ARM7 settings)
	 * 
	 */
	REG_IPC_SYNC = IPC_SYNC_IRQ_ENABLE ;
		
	
	/*
	 * Send the address of the variables to be used by the ARM7
	 * to report its status and last error, with relevant command identifier.
	 *
	 * Uses and updates _localCommandCount:
	 *
	 */
	FIFOElement commandElement = prepareFIFOCommand( 
		SendARM7StatusAndErrorReportAddress ) ;

	CEYLAN_LOG_FIFO( "FIFO::activate: sending IPC activate report command." ) ;
		
	/*
	 * LogPlug::debug( "Init command is: " 
	 	+ DescribeCommand( commandElement ) ) ;
	 */	
	
	InterruptMask previous = SetEnabledInterrupts( AllInterruptsDisabled ) ;
	
	writeBlocking( commandElement ) ;
	
	CEYLAN_LOG_FIFO( "FIFO::activate: sending address of the status word." ) ;
			
	writeBlocking( 
		/* address of the status word */ 
		reinterpret_cast<FIFOElement>( _arm7StatusWordPointer ) ) ;

	CEYLAN_LOG_FIFO( "FIFO::activate: sending address of the error word." ) ;
			
	writeBlocking( 
		/* address of the error word */ 
		reinterpret_cast<FIFOElement>( _arm7ErrorCodePointer ) ) ;
	
	SetEnabledInterrupts( previous ) ;
	
	notifyCommandToARM7() ;

	
	// Wait for at most a whole second:
	Ceylan::Uint8 VBlankCount = 60 ;
		
	/*
	 * getLastARM7StatusWord might return the starting value 
	 * (NoStatusAvailable) or 0 (StatusVoluntarilyLeftBlank) which is found
	 * in the FIFO before the ARM7 inits it.
	 *
	 * @note Even with pointers to volatile, the ARM9 still from times to times
	 * fail to grab the correct value in RAM, maybe confused by its cache. So
	 * the values have to be invalidated in the cache again and again.
	 *
	 * Waiting for the ARM7 to update these variables:
	 *
	 */
	while ( ( ( getLastARM7StatusWord() == NoStatusAvailable ) 
			|| ( getLastARM7ErrorCode() == NoErrorAvailable ) )
		&& ( VBlankCount > 0 ) )
	{

		sendSynchronizeInterruptToARM7() ;
	
#if CEYLAN_DEBUG_FIFO
		
		//if ( VBlankCount % 10 == 0 )
			LogPlug::debug( "act: status " 
				+ Ceylan::toString( getLastARM7StatusWord() ) + ", error "
				+ Ceylan::toString( getLastARM7ErrorCode() ) ) ;
				
#endif // CEYLAN_DEBUG_FIFO
		
		atomicSleep() ;
		VBlankCount-- ;

	}	
	
	
	if ( VBlankCount == 0 )
		throw FIFOException( "FIFO::activate: "
			"time-out reached while waiting for ARM7 update" ) ;

	// Not really used currently, as relying on the command embedded count;
	FIFOCommandCount arm7Count = GetARM7ProcessedCount() ;
	
	if ( arm7Count != 1 )
		LogPlug::error( "FIFO::activate: ARM7 processed count expected "
			"to be equal to 1, found: "
			+ Ceylan::toNumericalString( arm7Count ) ) ;
			
		
	if ( getLastARM7StatusWord() != ARM7Running )
		throw FIFOException( "FIFO::activate: ARM7 status updated, "
			"but is not in expected running state: "
			+ interpretLastARM7StatusWord() ) ;

	if ( getLastARM7ErrorCode() != NoError )
		throw FIFOException( "FIFO::activate: ARM7 error updated, "
			"but is not in expected no-error state: "
			+ interpretLastARM7ErrorCode() ) ;
	
	
	CEYLAN_LOG_FIFO( "FIFO successfully activated, ARM handshake completed" ) ;

	
#endif // CEYLAN_RUNS_ON_ARM9

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



void FIFO::deactivate() throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	CEYLAN_LOG_FIFO( "FIFO deactivate, stopping IPC system" ) ;


	InterruptMask previous = SetEnabledInterrupts( AllInterruptsDisabled ) ;
	
	/*
	 * Send  relevant command identifier for IPC shutdown:
	 * (remaining FIFO bytes not used)
	 *
	 */
	writeBlocking( prepareFIFOCommand( ShutdownIPC ) ) ;

	SetEnabledInterrupts( previous ) ;

	notifyCommandToARM7() ;

	
	// Wait for at most a whole second:
	Ceylan::Uint8 VBlankCount = 60 ;
	
	
	while ( ( getLastARM7StatusWord() != ARM7IPCShutdown ) 
		&& ( VBlankCount > 0 ) )
	{

#if CEYLAN_DEBUG_FIFO

		if ( VBlankCount % 10 == 0 )
			LogPlug::trace( "deactivating: " + interpretLastARM7StatusWord() ) ;

#endif // CEYLAN_DEBUG_FIFO
		
		atomicSleep() ;
		VBlankCount-- ;
	
	}	
	
	
	if ( VBlankCount == 0 )
		LogPlug::error( "FIFO::deactivate ended on a time-out" ) ;
	else
		CEYLAN_LOG_FIFO( "ARM7 shutdown successful" ) ;
	
	// Disable IPC sync IRQ:
	irqDisable( IRQ_IPC_SYNC ) ;
 	REG_IPC_SYNC &= ~IPC_SYNC_IRQ_ENABLE ;

	// Disable FIFO:
	REG_IPC_FIFO_CR = REG_IPC_FIFO_CR &	~IPC_FIFO_ENABLE ;   	


#endif // CEYLAN_RUNS_ON_ARM9
		
#else // CEYLAN_ARCH_NINTENDO_DS

	throw FIFO::FIFOException( "FIFO constructor failed: "
		"not available on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



// Status word and error code section.


ARM7StatusWord FIFO::getLastARM7StatusWord() throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

	if ( _arm7StatusWordPointer == 0 )
		return NoStatusVariableAvailable ;

	// Try to bypass this very annoying ARM9 cache:
	volatile ARM7StatusWord * temp = (volatile ARM7StatusWord *)
		( ( (Ceylan::Uint32) _arm7StatusWordPointer) | 0x400000 ) ;

	return ( *temp ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

	// Dummy to allow compilation, will never be used:
	return 1 ;
		
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



string FIFO::interpretLastARM7StatusWord() throw()
{

	ARM7StatusWord status = getLastARM7StatusWord() ;
	
	switch( status )
	{
	
		case StatusVoluntarilyLeftBlank:
			return "unexpected unassigned null value for ARM7 status word" ;
			break ;
			
		case ARM7Running:
			return "ARM7 running normally" ;
			break ;
			
		case ARM7InError:
			return "ARM7 in error: " + interpretLastARM7ErrorCode() ;
			break ;
		
		case ARM7IPCShutdown:
			return "ARM7 IPC shutdown" ;
			break ;
			
		case NoStatusAvailable:
			return "no ARM7 status available" ;
			break ;

		case NoStatusVariableAvailable:
			return "ARM7 status variable not set" ;
			break ;
			
		case StatusReset:
			return "ARM7 status had been reset" ;
			break ;
			
		default:
			return "unexpected ARM7 status word (" 
				+ Ceylan::toString( status ) + ")" ;
			break ;
	
	}
	
}



ARM7ErrorCode FIFO::getLastARM7ErrorCode() throw()
{

	// Null pointer results in NoError (0):
	if ( _arm7ErrorCodePointer == 0 )
		return NoErrorVariableAvailable ;
	
	// Try to bypass this very annoying ARM9 cache:
	volatile ARM7ErrorCode * temp = (volatile ARM7ErrorCode *)
		( ( (Ceylan::Uint32) _arm7ErrorCodePointer) | 0x400000 ) ;

	return ( *temp ) ; 
	
}



string FIFO::interpretLastARM7ErrorCode() throw()
{

	ARM7ErrorCode error = getLastARM7ErrorCode() ;
	
	switch( error )
	{
	
		case ErrorVoluntarilyLeftBlank:
			return "unexpected unassigned null value for ARM7 error code" ;
			break ;

		case UnexpectedSystemCommand:
			return "ARM7 received an unknown system (Ceylan-specific) command" ;
			break ;
			
		case UnexpectedApplicationCommand:
			return "ARM7 received an unknown user "
				"(application-specific) command" ;
			break ;
			
		case FIFOErrorWhileReading:
			return "ARM7 encountered a FIFO error while reading" ;
			break ;
			
		case FIFOErrorWhileWriting:
			return "ARM7 encountered a FIFO error while writing" ;
			break ;
			
		case NoError:
			return "ARM7 has no error registered" ;
			break ;

		case NoErrorVariableAvailable:
			return "ARM7 error variable not set" ;
			break ;
		
		case CommandOverlapping:	
			return "ARM7 prevented attempt of command overlapping" ;
			break ;
			
		case UnexpectedBehaviour:	
			return "ARM7 received an HelloToTheARM7 (i.e. null) command" ;
			break ;
			
		case IPCAlreadyStarted:	
			return 
				"ARM7 received an IPC start command whereas already started" ;
			break ;
			
		case IPCAlreadyStopped:	
			return "ARM7 received an IPC stop command whereas not running" ;
			break ;
			
		case AwokenWithNothingToRead:	
			return "ARM7 had its sync IRQ triggered "
				"whereas its input FIFO was empty" ;
			break ;
			
		case IncorrectInitialStatus:	
			return "ARM7 detected its initial status, set by the ARM9, "
				"is incorrect" ;
			break ;
			
		case IncorrectInitialError:	
			return "ARM7 detected its initial error code, set by the ARM9, "
				"is incorrect" ;
			break ;
			
		case UnexpectedCommandCount:	
			return "ARM7 encountered a command embedding an unexpected count" ;
			break ;
			
		case IncorrectApplicationAnswer:	
			return "ARM7 received an incorrect application-specific answer" ;
			break ;
			
		case FIFOTimeOutWhileReading:	
			return "ARM7  made a time-out while reading" ;
			break ;
			
		case FIFOTimeOutWhileWriting:	
			return "ARM7  made a time-out while writing" ;
			break ;
			
		default:
			return "unexpected ARM7 error code (" 
				+ Ceylan::toString( error ) + ")" ;
			break ;
	
	}

}



const std::string FIFO::toString( Ceylan::VerbosityLevels level ) 
	const throw()
{

	string res = "FIFO with " ;
	
	if ( ! dataAvailableForReading() )
		res += "no " ;	
	
	res += "data available for reading, and with " ;
	
	if ( ! spaceAvailableForWriting() )
		res += "no " ;
		
	res += "space available for writing" ;
	
	if ( level == Ceylan::low )			
		return res ;
		
	if ( _arm7StatusWordPointer == 0 )
		res += ". State variables not available" ;
	
	res += ". 8-bit overall (truncated) local command count is " 
		+ Ceylan::toString( _localCommandCount ) + ", remote one is "
		+ Ceylan::toString( _remoteCommandCount ) + ", processed count is "
		+ Ceylan::toString( getProcessedCount() ) + ", sent count is "
		+ Ceylan::toString( getSentCount() ) + ", ARM7 processed count is "
		+ GetARM7ProcessedCount() ;
		
	return res ;
	
}



FIFOCommandID FIFO::GetFIFOCommandIDFrom( const FIFOElement & element ) throw()
{

	return ( element & 0xff000000 ) >> 24 ;
	
}



FIFOCommandCount FIFO::GetFIFOCommandCountFrom( 
	const FIFOElement & element ) throw()
{

	return ( element & 0x00ff0000 ) >> 16 ;
	
}



FIFOCommandCount FIFO::GetARM7ProcessedCount() throw()
{
	
#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	// IPC Remote Status is in bits 0-3:
	return REG_IPC_SYNC & 0x0f ;

#endif // CEYLAN_RUNS_ON_ARM9
		
	// Dummy to allow compilation, will never be used:
	return 1 ;

#else // CEYLAN_ARCH_NINTENDO_DS

	// Dummy to allow compilation, will never be used:
	return 1 ;

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



FIFOCommandCount FIFO::GetARM9ProcessedCount() throw()
{
	
#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	// IPC Local Status is in bits 8-11:
	return ( REG_IPC_SYNC & 0x0f00 ) >> 8 ;
	
#endif // CEYLAN_RUNS_ON_ARM9
		
	// Dummy to allow compilation, will never be used:
	return 1 ;

#else // CEYLAN_ARCH_NINTENDO_DS

	// Dummy to allow compilation, will never be used:
	return 1 ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



void FIFO::VBlankHandlerForFIFO() 
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	// Not used currently, as not registered as a handler.
	
	// 'if ( dataAvailableForReading() )', but we are static here:
	if ( ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) )
	{
	
		/*
		LogPlug::debug( 
			"FIFO::VBlankHandlerForFIFO triggered command handling" ) ;
		*/
			
		ManageReceivedCommand() ;
	
	}

#endif // CEYLAN_RUNS_ON_ARM9
		
#endif // CEYLAN_ARCH_NINTENDO_DS
				
}





// Protected section.


void FIFO::handleReceivedApplicationCommand( FIFOCommandID commandID, 
	FIFOElement firstElement ) throw()
{

	/*
	 * This basic implementation, meant to be overriden, does not call any
	 * command handler, just reports unexpected commands:
	 *
	 */
	handleUnexpectedApplicationCommand( commandID ) ;
	
}



void FIFO::handleReceivedCommand() throw()
{

#if CEYLAN_ARCH_NINTENDO_DS


	FIFOElement firstElement ;
	FIFOCommandID id ;
	
	/*
	 * During the management of this command, notifications of incoming FIFO
	 * elements will be ignored (as CommandInProgress, the protection flag,
	 * is true in the unique caller of this method, ManageReceivedCommand), 
	 * so that the next elements of the command can be read without being 
	 * taken for new commands. 
	 * But it may hide real new commands that could occur after the first 
	 * command performed its last read, but before it returned. 
	 * Thus a 'while' loop instead of a simple 'if'.
	 *
	 */	
	
	
	while ( dataAvailableForReading() )
	{
	 		
	 	// readBlocking instead of read: increased safety ?
		firstElement = readBlocking() ;

#if CEYLAN_SAFE_FIFO

	 	FIFOCommandCount count = GetFIFOCommandCountFrom( firstElement ) ;
		
		if ( count != _remoteCommandCount )
		{
		
			LogPlug::error( "FIFO::handleReceivedCommand: "
				"unexpected embedded command count of "
				+ Ceylan::toNumericalString( count ) + " where "
				+ Ceylan::toNumericalString( _remoteCommandCount ) 
				+ " was expected." ) ;
				
			return ;	
			
		}
		
		_remoteCommandCount++ ;

#endif // CEYLAN_SAFE_FIFO
		
	
		id = GetFIFOCommandIDFrom( firstElement ) ;
	
		if ( id > 127 )
		{
				
			// It is an application-specific command, relay it:
			handleReceivedApplicationCommand( id, firstElement ) ;
	
		}
		else
		{
	
			// Here we are dealing with a system-specific (Ceylan) command.
			switch( id )
			{
	
				case HelloToTheARM9:
					LogPlug::info( "The ARM7 says hello to the ARM9 !" ) ;
					break ;
		
				default:
					LogPlug::error( "FIFO::handleReceivedCommand: "
						"unexpected command: " + Ceylan::toNumericalString( id )
						+ ", ignored." ) ;
					break ;		
	
			}
		
		}
		
		
		// Each command processed is tracked (let the 4-bit variable overflow): 
		incrementProcessedCount() ;
		
	
	} // end while
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



void FIFO::handleUnexpectedApplicationCommand( FIFOCommandID commandID ) throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

	LogPlug::error( "handleUnexpectedApplicationCommand called: "
		"unexpected application-specific command identifier: " 
		+ Ceylan::toNumericalString( commandID ) ) ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}


	
FIFOElement FIFO::prepareFIFOCommand( FIFOCommandID id ) throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_SAFE_FIFO

	FIFOElement res = ( id << 24 ) | ( _localCommandCount << 16 ) ;
	
	// Prepare for next command:
	_localCommandCount++ ;
	
	return res ;

#else // CEYLAN_SAFE_FIFO

	FIFOElement res = id << 24 ;
		
	return res ;

#endif // CEYLAN_SAFE_FIFO

#else // CEYLAN_ARCH_NINTENDO_DS

	// Dummy to allow compilation, will never be used:
	return 0 ;

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



void FIFO::notifyCommandToARM7() throw()
{

	_sentCount++ ;
	sendSynchronizeInterruptToARM7() ;
	
}



void FIFO::sendSynchronizeInterruptToARM7() throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	//LogPlug::trace( "FIFO::sendSynchronizeInterruptToARM7 begin" ) ;
	
	// Triggers an IRQ on the ARM7 and specifies the local processed count:
	REG_IPC_SYNC = (REG_IPC_SYNC & 0xf0ff) | (getProcessedCount() << 8) 
		| IPC_SYNC_IRQ_REQUEST ;

	//LogPlug::trace( "FIFO::sendSynchronizeInterruptToARM7 end" ) ;

#endif // CEYLAN_RUNS_ON_ARM9

#endif // CEYLAN_ARCH_NINTENDO_DS

}



bool FIFO::dataAvailableForReading() const throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

	return ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

	// Dummy to allow compilation, will never be used:
	return false ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



bool FIFO::spaceAvailableForWriting() const throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

	return ! ( REG_IPC_FIFO_CR & IPC_FIFO_SEND_FULL ) ;

#else // CEYLAN_ARCH_NINTENDO_DS

	// Dummy to allow compilation, will never be used:
	return 1 ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS

}



FIFOElement FIFO::read() throw( FIFOException )
{

	// LogPlug::trace( "FIFO read" ) ;

#if CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::read: FIFO in error before reading" ) ;

#endif // CEYLAN_SAFE_FIFO

	
	if ( ! dataAvailableForReading() )
		throw FIFOEmpty( "Cannot read from an empty FIFO" ) ;
	
	FIFOElement res = REG_IPC_FIFO_RX ;


#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::read: FIFO in error after reading" ) ;

#endif // CEYLAN_SAFE_FIFO
		
	return res ;
	
#else // CEYLAN_ARCH_NINTENDO_DS

	// Dummy to allow compilation, will never be used:
	return 1 ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



FIFOElement FIFO::readBlocking() throw( FIFOException )
{

	// LogPlug::trace( "FIFO readBlocking" ) ;

#if CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( 
			"FIFO::readBlocking: FIFO in error before waiting" ) ;

#endif // CEYLAN_SAFE_FIFO


	Ceylan::Uint32 attemptCount = 100000 ;
	
	// Active waiting preferred to atomicSleep():

	while ( ! dataAvailableForReading() && attemptCount > 0 )
		 attemptCount-- ;

	if ( attemptCount == 0 )
	{
	
		LogPlug::warning( "FIFO::readBlocking: never ending ?" ) ;

		// Triggers the ARM7 if it can help to make some FIFO room:
		sendSynchronizeInterruptToARM7() ;
		
		while ( ! dataAvailableForReading() )
			;
	
	}		

#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::readBlocking: "
			"FIFO in error after waiting but before reading" ) ;

#endif // CEYLAN_SAFE_FIFO


	FIFOElement res = REG_IPC_FIFO_RX ;


#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::readBlocking: "
			"FIFO in error after reading" ) ;

#endif // CEYLAN_SAFE_FIFO

	return res ;

#else // CEYLAN_ARCH_NINTENDO_DS

	// Dummy to allow compilation, will never be used:
	return 1 ;

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



void FIFO::write( FIFOElement toSend ) throw( FIFOException )
{

	// LogPlug::trace( "FIFO write" ) ;

#if CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::write: FIFO in error before writing" ) ;

#endif // CEYLAN_SAFE_FIFO

	
	if ( ! spaceAvailableForWriting() )
		throw FIFOFull( "Cannot write to a full FIFO" ) ;

	
	REG_IPC_FIFO_TX = toSend ;

	
#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::write: FIFO in error after writing" ) ;

#endif // CEYLAN_SAFE_FIFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



void FIFO::writeBlocking( FIFOElement toSend ) throw( FIFOException )
{

	// LogPlug::trace( "FIFO writeBlocking" ) ;

#if CEYLAN_ARCH_NINTENDO_DS

#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( 
			"FIFO::writeBlocking: FIFO in error before waiting" ) ;

#endif // CEYLAN_SAFE_FIFO

	
	
	Ceylan::Uint32 attemptCount = 100000 ;
	
	// Active waiting preferred to atomicSleep():

	while ( ! spaceAvailableForWriting() && attemptCount > 0 )
		 attemptCount-- ;
	
	
	if ( attemptCount == 0 )
	{
	
		LogPlug::warning( "FIFO::writeBlocking: never ending ?" ) ;
		
		// Triggers the ARM7 if it can help to make some FIFO room:
		sendSynchronizeInterruptToARM7() ;
		
		while ( ! spaceAvailableForWriting() )
			;
			
	}
	

#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::writeBlocking: "
			"FIFO in error after waiting but before writing" ) ;

#endif // CEYLAN_SAFE_FIFO
	
	
	REG_IPC_FIFO_TX = toSend ;


#if CEYLAN_SAFE_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::writeBlocking: "
			"FIFO in error after writing" ) ;

#endif // CEYLAN_SAFE_FIFO
	
#endif // CEYLAN_ARCH_NINTENDO_DS

}



FIFOCommandCount FIFO::getProcessedCount() const throw()
{

	return ( _processedCount & 0x0f ) ;
	
}



FIFOCommandCount FIFO::getSentCount() const throw()
{

	return ( _sentCount & 0x0f ) ;

}



void FIFO::incrementProcessedCount() throw() 
{

#if CEYLAN_ARCH_NINTENDO_DS

	_processedCount++ ;
	
	// Updates the local processed count in IPC register:
	REG_IPC_SYNC = (REG_IPC_SYNC & 0xf0ff) | (getProcessedCount() << 8) ;

#endif // CEYLAN_ARCH_NINTENDO_DS
		
}




// Static section.


void FIFO::SyncHandlerForFIFO() 
{
	
	//LogPlug::trace( "FIFO::SyncHandlerForFIFO called" ) ;

#if CEYLAN_ARCH_NINTENDO_DS
	
	// 'if ( dataAvailableForReading() )', but we are static here:
	if ( ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) )
		ManageReceivedCommand() ;

#endif // CEYLAN_ARCH_NINTENDO_DS
				
}



void FIFO::ManageReceivedCommand()
{

#if CEYLAN_ARCH_NINTENDO_DS

	/**
	 * Should always be called from a context where relevant interrupts have 
	 * already been disabled.
	 *
	 * @see IRQ handlers: VBlankHandlerForFIFO, SyncHandlerForFIFO
	 *
	 */
	
#if CEYLAN_SAFE_FIFO
	 
    /**
     * Tells whether a command is in progress: as soon as the ARM7
     * sends a multi-element FIFO command, the ARM9, after having
     * read the first element, might have its 
     * 'FIFO receive not empty' IRQ  triggered while still being 
     * processing this first command (quite likely, as the ARM7 will send these
	 * elements directly one after the other, with no delay inbetween). 
     *
     * To avoid this IRQ handler to be interrupted by itself 
     * (potentially reading next FIFO elements of the current
     * command as if they were new commands), this flag makes IRQ 
     * handlers return immediately if found true.
     * (increased safety beneath shutting down relevant IRQ)
	 *
     */
	static bool CommandInProgress = false ;


	// LogPlug::trace( "FIFO ManageReceivedCommand" ) ;
	
	/*
	 * Potential race conditions (mitigated a lot since IRQ are disabled):  
	 *  - if between the FIFO IRQ and the setting of 'CommandInProgress = true' 
	 * another FIFO IRQ is triggered, the second one might eat elements of the
	 * first one (hence the current call will block until next sending)
	 * [Not very likely, not very serious]
	 *  - if between the last read of '_FIFO->handleReceivedCommand()'
	 * and the following 'CommandInProgress = false' an element is sent on the
	 * ARM7, an IRQ may triggered but it will be lost (wrongly ignored). 
	 * [Not very likely, not very serious]
	 * Reading regularly the FIFO state, for example in the VBLANK, should
	 * increase the reliability of the IPC.
	 *
	 */
	if ( ! CommandInProgress )
	{
	
		CommandInProgress = true ;

#endif // CEYLAN_SAFE_FIFO
		
		
		if ( _FIFO != 0 )			
			_FIFO->handleReceivedCommand() ;

#if CEYLAN_SAFE_FIFO
		
		CommandInProgress = false ;	

	}
	else
	{
	
		LogPlug::warning( "FIFO::ManageReceivedCommand: "
			"concurrent calls hindered" ) ;
			
	}	
	
	/*
	 * else ( CommandInProgress is true) ignores the possible IRQ that 
	 * triggered that method, as this event (FIFO not empty) is expected to 
	 * be managed by the handleReceivedCommandhandler command.
	 *
	 * The IRQ handler that called this method is responsible for acknowledging
	 * the interrupt that triggered it.
	 *
	 */

#endif // CEYLAN_SAFE_FIFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
}



string FIFO::DescribeCommand( FIFOElement element ) throw()
{

#if CEYLAN_ARCH_NINTENDO_DS

	string res = "Command is equal to " + Ceylan::toString( element ) 
		+ ", i.e. " + Ceylan::toString( element, /* bitField */ true ) ;

#if CEYLAN_SAFE_FIFO
		
	res += ". Command ID is " 
		+ Ceylan::toNumericalString( GetFIFOCommandIDFrom( element ) )
		+ ", command count is " 
		+ Ceylan::toNumericalString( GetFIFOCommandCountFrom( element ) ) ;

#else // CEYLAN_SAFE_FIFO
	
	res += ". Command ID is " 
		+ Ceylan::toNumericalString( GetFIFOCommandIDFrom( element ) ) ;
		
#endif // CEYLAN_SAFE_FIFO
	
	return res ;	
	
#else // CEYLAN_ARCH_NINTENDO_DS

	return "(not available on this platform)" ;
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
}

