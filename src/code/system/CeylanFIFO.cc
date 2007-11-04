#include "CeylanFIFO.h"

#include "CeylanLogPlug.h"             // for Log primitives
#include "CeylanOperators.h"           // for toNumericalString
#include "CeylanSystem.h"              // for ConvertToNonCacheable


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


/*
 * Implementation notes:
 * 
 * @see libnds include/nds/ipc.h for defines.
 * @see http://www.neimod.com/dstek/dstek2.xml#Interprocessor%20Communication
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
	_arm7ErrorCodePointer( 0 )
{

	// LogPlug::trace( "FIFO constructor" ) ;
	
	if ( FIFO::_FIFO != 0 )
		throw FIFOException( "FIFO constructor failed: "
			"there is already a FIFO instance registered" ) ;


#if CEYLAN_ARCH_NINTENDO_DS

	_FIFO = this ;		

	// Interrupts will be used here:
	System::InitializeInterrupts() ;

	// FIFO IRQ not empty considered owned by the Ceylan FIFO system:
	irqSet( IRQ_FIFO_NOT_EMPTY, FIFOHandlerForFIFO ) ; 
    irqEnable( IRQ_FIFO_NOT_EMPTY ) ;

	/*
	 * Fail-over VBlank handler not managed here, as may be used for other
	 * purposes as well in the user code.
	 *
	 */
		
#else // CEYLAN_ARCH_NINTENDO_DS

	throw FIFO::FIFOException( "FIFO constructor failed: "
		"not available on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

	
}



FIFO::~FIFO() throw()
{

	// LogPlug::trace( "FIFO destructor" ) ;
	
	deactivate() ;
			
	_FIFO = 0 ;
	
}



void FIFO::activate() throw( FIFOException )
{

	// LogPlug::trace( "FIFO activate" ) ;

	/*
	 * REG_IPC_FIFO_CR is the FIFO *control* register, and:
	 *  - IPC_FIFO_ENABLE enables the FIFO
	 *  - IPC_FIFO_SEND_CLEAR flushes the send FIFO
	 *  - IPC_FIFO_RECV_IRQ request an IRQ to be triggered on a transition, on
	 * the receive FIFO, from empty to not empty	 
	 *
	 */
	REG_IPC_FIFO_CR = 
		IPC_FIFO_ENABLE | IPC_FIFO_SEND_CLEAR | IPC_FIFO_RECV_IRQ ;


	/*
	 * Contains an initial zero status word (zero is not a legal ARM7 status
	 * word.
	 *
	 * @note A new is forced instead of a class member, as if the FIFO is 
	 * created as an automatic variable, this variable would be in the ARM9
	 * stack, hence out of range for the ARM7.
	 *
	 * The address is translated so that it is not cached by the ARM9 data 
	 * cache.
	 *
	 * @note Both the status word and the error code *must* be Ceylan::Uint16
	 * here.
	 *
	 */
	Ceylan::Uint16 * arm7StateMemory = ConvertToNonCacheable<Ceylan::Uint16>(
		new Ceylan::Uint16[ 2 ] ) ;
		
	_arm7StatusWordPointer = &arm7StateMemory[0] ;
	_arm7ErrorCodePointer  = &arm7StateMemory[1] ;
	
	(*_arm7StatusWordPointer) = NoStatusAvailable ;
	
	(*_arm7ErrorCodePointer) = NoError ;
	
		
	/*
	 * Send this address to the ARM7 thanks to the relevant command identifier:
	 * Zeroed even if not necessary to avoid a warning.
	 */
	FIFOElement commandElement = 0 ;
	

	/**
	 * Send the address of the variables to be used by the ARM7
	 * to report its status and last error.
	 *
	 */
	FIFO::SetFIFOCommandIDTo( commandElement,
		SendARM7StatusAndErrorReportAddress ) ;
	
	writeBlocking( commandElement ) ;

	LogPlug::trace( "FIFO::activate: sending address of the status word." ) ;
			
	writeBlocking( 
		/* address of the status word (from which the error code is deduced */ 
		reinterpret_cast<FIFOElement>( _arm7StatusWordPointer ) ) ;
	
	// Wait for at most a whole second:
	Ceylan::Uint8 VBlankCount = 60 ;
	
	while ( ( getLastARM7StatusWord() == NoStatusAvailable ) 
		&& ( VBlankCount > 0 ) )
	{

		atomicSleep() ;
		VBlankCount-- ;
	
	}	
	
	if ( VBlankCount == 0 )
		throw FIFOException( "FIFO::activate: "
			"time-out reached while waiting for ARM7 status update" ) ;
			
	LogPlug::trace( "FIFO activate ended" ) ;
	
}



void FIFO::deactivate() throw()
{


	LogPlug::trace( "FIFO deactivate, stopping IPC system" ) ;

	/*
	 * Send this address to the ARM7 thanks to the relevant command identifier:
	 * Zeroed even if not necessary to avoid a warning.
	 */
	FIFOElement commandElement = 0 ;
	

	/**
	 * Send the address of the variables to be used by the ARM7
	 * to report its status and last error.
	 *
	 */
	FIFO::SetFIFOCommandIDTo( commandElement, ShutdownIPC ) ;
	
	writeBlocking( commandElement ) ;
	
	// Wait for at most a whole second:
	Ceylan::Uint8 VBlankCount = 60 ;
	
	while ( ( getLastARM7StatusWord() != NoStatusAvailable ) 
		&& ( VBlankCount > 0 ) )
	{

		LogPlug::trace( "shut " + interpretLastARM7StatusWord() ) ;
		atomicSleep() ;
		VBlankCount-- ;
	
	}	
	
	if ( VBlankCount == 0 )
		LogPlug::error( "FIFO::deactivate ended on a time-out" ) ;
		
	LogPlug::trace( "ARM7 shutdown" ) ;
	
	irqDisable( IRQ_FIFO_NOT_EMPTY ) ;
	
	REG_IPC_FIFO_CR &= ~( IPC_FIFO_ENABLE | IPC_FIFO_RECV_IRQ ) ;   	

	if ( _arm7StatusWordPointer != 0 )
		delete [] _arm7StatusWordPointer ;
	
	_arm7StatusWordPointer = 0 ;
	
		
/*	
 * Splendid bug: the two variables were allocated as an array, even if
 * previous delete (for _arm7StatusWordPointer) had no '[]', 
 * _arm7ErrorCodePointer was already deallocated, thus the following:
 *	
 *	if ( _arm7ErrorCodePointer != 0 )
 *		delete _arm7ErrorCodePointer ;
 *	
 *	_arm7ErrorCodePointer = 0 ;
 *
 * caused a double deallocation which was making the NoCashGBA emulator crash
 * (but not the DS).
 *
 */	
 
}




// Status word and error code section.


ARM7StatusWord FIFO::getLastARM7StatusWord() throw()
{

	if ( _arm7StatusWordPointer == 0 )
		return 0 ;
	
	return (*_arm7StatusWordPointer) ;
	
}



string FIFO::interpretLastARM7StatusWord() throw()
{

	ARM7StatusWord status = getLastARM7StatusWord() ;
	
	switch( status )
	{
	
		case NoStatusAvailable:
			return "no ARM7 status available" ;
			break ;
			
		case ARM7Running:
			return "ARM7 running normally" ;
			break ;
			
		case ARM7InError:
			return "ARM7 in error: " + interpretLastARM7ErrorCode() ;
			break ;
			
		default:
			return "unexpected ARM7 status word (" 
				+ Ceylan::toString( status ) + ")" ;
			break ;
	
	}
	
}



ARM7ErrorCode FIFO::getLastARM7ErrorCode() throw()
{

	if ( _arm7ErrorCodePointer == 0 )
		return 0 ;
	
	return (*_arm7ErrorCodePointer) ;
	
}



string FIFO::interpretLastARM7ErrorCode() throw()
{

	ARM7ErrorCode error = getLastARM7ErrorCode() ;
	
	switch( error )
	{
	
		case NoError:
			return "ARM7 has no error registered" ;
			break ;
			
		case UnexpectedSystemCommand:
			return "ARM7 received an unknown system (Ceylan-specific) command" ;
			break ;
			
		case UnexpectedApplicationCommand:
			return "ARM7 received an unknown user "
				"(application-specific) command" ;
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
				
	return res ;
	
}



FIFOCommandID FIFO::GetFIFOCommandIDFrom( const FIFOElement & element ) throw()
{

	return ( element & 0xff000000 ) >> 24 ;
	
}



void FIFO::SetFIFOCommandIDTo( FIFOElement & targetElement, FIFOCommandID id )
	throw()
{

	targetElement = ( targetElement & 0x00ffffff ) | ( id << 24 ) ;
	
}



void FIFO::VBlankHandlerForFIFO() 
{

	// 'if ( dataAvailableForReading() )', but we are static here:
	if ( ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) )
		ManageReceivedCommand() ;
	
	// Notify this interrupt has been managed:
	REG_IF |= IRQ_VBLANK ;		
		
			
}







// Protected section.



void FIFO::FIFOHandlerForFIFO() 
{

	// FIXME test useless
	
	// 'if ( dataAvailableForReading() )', but we are static here:
	if ( ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) )
		ManageReceivedCommand() ;

	// Notify this interrupt has been managed:
	REG_IF |= IRQ_FIFO_NOT_EMPTY ;		
				
}



void FIFO::handleReceivedCommand() throw()
{

	/*
	 * During the management of this command, notifications of incoming FIFO
	 * elements will be ignored (as CommandInProgress is true), so that
	 * the next elements of the command can be read without being taken for
	 * new commands. 
	 * But it may hide real new commands that could occur after the first 
	 * command performed its last read but before it returned. 
	 * Thus a while loop.
	 *
	 */	
	
	FIFOElement firstElement ;
	FIFOCommandID id ;
	
	while ( dataAvailableForReading() )
	{
	 
	 	// readBlocking instead of read: increased safety ?
		firstElement = readBlocking() ;
	
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
						"unexpected command: " 
						+ Ceylan::toNumericalString( id ) + ", ignored." ) ;
					break ;		
	
			}
		
		}
	
	} // end while
	
	
}



void FIFO::ManageReceivedCommand()
{

	REG_IE = REG_IE & ~( IRQ_VBLANK | IRQ_FIFO_NOT_EMPTY ) ;
	
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
     *
     */
	static bool CommandInProgress = false ;


	// LogPlug::trace( "FIFO ManageReceivedCommand" ) ;
	
	/*
	 * Potential race conditions:  
	 *  - if between the FIFO IRQ and the setting of 'CommandInProgress = true' 
	 * another FIFO IRQ is triggered, the second one might eat elements of the
	 * first one (hence the current call will block until next sending)
	 * [Not very likely, not very serious]
	 *  - if between the last read of '_FIFO->handleReceivedCommand()'
	 * and the following 'CommandInProgress = false' an IRQ is triggered, it 
	 * will be lost (wrongly ignored). [Not very likely, not very serious]
	 * Reading regularly the FIFO state, for example in the VBLANK, should
	 * increase the reliability of the IPC.
	 *
	 */
	if ( ! CommandInProgress )
	{
	
		CommandInProgress = true ;
		
		
		if ( _FIFO != 0 )			
			_FIFO->handleReceivedCommand() ;
		
		CommandInProgress = false ;	

	}
	
	
	/*
	 * else ignores the possible IRQ that triggered that method, as this 
	 * event (FIFO not empty) is expected to be managed by the
	 * handleReceivedCommandhandler command.
	 *
	 * The IRQ handler that called this method is responsible for acknoledging
	 * the interrupt that triggered it.
	 *
	 */

	REG_IE = REG_IE | ( IRQ_VBLANK | IRQ_FIFO_NOT_EMPTY ) ;
	
}



bool FIFO::dataAvailableForReading() const throw()
{

	return ! ( REG_IPC_FIFO_CR & IPC_FIFO_RECV_EMPTY ) ;
	
}



bool FIFO::spaceAvailableForWriting() const throw()
{

	return ! ( REG_IPC_FIFO_CR & IPC_FIFO_SEND_FULL ) ;

}



FIFOElement FIFO::read() throw( FIFOException )
{

	// LogPlug::trace( "FIFO read" ) ;

#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::read: FIFO in error before reading" ) ;

#endif // CEYLAN_DEBUG_FIFO


	if ( ! dataAvailableForReading() )
		throw FIFOEmpty( "Cannot read from an empty FIFO" ) ;
	
	FIFOElement res = REG_IPC_FIFO_RX ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::read: FIFO in error after reading" ) ;

#endif // CEYLAN_DEBUG_FIFO
	
	
	return res ;
	
}



FIFOElement FIFO::readBlocking() throw( FIFOException )
{

	// LogPlug::trace( "FIFO readBlocking" ) ;

#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( 
			"FIFO::readBlocking: FIFO in error before waiting" ) ;

#endif // CEYLAN_DEBUG_FIFO


	// Active waiting preferred to atomicSleep():
	while ( ! dataAvailableForReading() )
		 ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::readBlocking: "
			"FIFO in error after waiting but before reading" ) ;

#endif // CEYLAN_DEBUG_FIFO


	FIFOElement res = REG_IPC_FIFO_RX ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::readBlocking: "
			"FIFO in error after reading" ) ;

#endif // CEYLAN_DEBUG_FIFO


	return res ;
	
}



void FIFO::write( FIFOElement toSend ) throw( FIFOException )
{

	// LogPlug::trace( "FIFO write" ) ;

#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::write: FIFO in error before writing" ) ;

#endif // CEYLAN_DEBUG_FIFO


	if ( ! spaceAvailableForWriting() )
		throw FIFOFull( "Cannot write to a full FIFO" ) ;

	
	REG_IPC_FIFO_TX = toSend ;

	
#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::write: FIFO in error after writing" ) ;

#endif // CEYLAN_DEBUG_FIFO
	
}



void FIFO::writeBlocking( FIFOElement toSend ) throw( FIFOException )
{

	// LogPlug::trace( "FIFO writeBlocking" ) ;

#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( 
			"FIFO::writeBlocking: FIFO in error before waiting" ) ;

#endif // CEYLAN_DEBUG_FIFO


	// Active waiting preferred to atomicSleep():
	while ( ! spaceAvailableForWriting() )
		 ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::writeBlocking: "
			"FIFO in error after waiting but before writing" ) ;

#endif // CEYLAN_DEBUG_FIFO
	
	
	REG_IPC_FIFO_TX = toSend ;


#if CEYLAN_DEBUG_FIFO

	if ( REG_IPC_FIFO_CR & IPC_FIFO_ERROR )
		throw FIFOException( "FIFO::writeBlocking: "
			"FIFO in error after writing" ) ;

#endif // CEYLAN_DEBUG_FIFO
	
}

