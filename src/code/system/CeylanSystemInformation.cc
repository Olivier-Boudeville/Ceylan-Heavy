/* 
 * Copyright (C) 2003-2011 Olivier Boudeville
 *
 * This file is part of the Ceylan library.
 *
 * The Ceylan library is free software: you can redistribute it and/or modify
 * it under the terms of either the GNU Lesser General Public License or
 * the GNU General Public License, as they are published by the Free Software
 * Foundation, either version 3 of these Licenses, or (at your option) 
 * any later version.
 *
 * The Ceylan library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License and the GNU General Public License
 * for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License and the GNU General Public License along with the Ceylan library.
 * If not, see <http://www.gnu.org/licenses/>.
 *
 * Author: Olivier Boudeville (olivier.boudeville@esperide.com)
 *
 */


#include "CeylanSystemInformation.h"

#include "CeylanLogPlug.h"              // for LogPlug
#include "CeylanOperators.h"            // for toString


#include <string>


using std::string ;


using namespace Ceylan ;                // for UnsignedLongInteger
using namespace Ceylan::Log ;           // for LogPlug
using namespace Ceylan::System ;        // for SystemException


#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"               // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H




#if CEYLAN_ARCH_NINTENDO_DS

#include "CeylanConfigForNintendoDS.h"  // for FIFO defines, etc.
#include "CeylanFIFO.h"                 // for FIFO


// Needed to know free and used memory:
extern "C" 
{

#include <unistd.h>
#include <malloc.h>

}


/// End of static code and data:
extern u8 __end__[] ;        

/// farthest point to which the heap will grow:
extern u8 __eheap_end[] ;    

#endif // CEYLAN_ARCH_NINTENDO_DS




extern "C" 
{


#ifdef CEYLAN_USES_SYS_SYSINFO_H

// Solaris sysinfo.h does not even compile out-of-the-box (uint64_t not known).
#if CEYLAN_ARCH_SOLARIS == 0
#include <sys/sysinfo.h>           // for GNU/Linux sysinfo
#endif // CEYLAN_ARCH_SOLARIS

#endif // CEYLAN_USES_SYS_SYSINFO_H

// On Solaris sys/systeminfo.h provides only an unrelated sysinfo.

}



UnsignedLongInteger Ceylan::System::getSecondsSinceSystemBoot() 
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getSecondsSinceSystemBoot: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getSecondsSinceSystemBoot: "
			"unable to query system: "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( sysinfoStruct.uptime ) ;

#endif // CEYLAN_ARCH_SOLARIS

#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getSecondsSinceSystemBoot: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



Ceylan::Uint32 Ceylan::System::getTotalProcessCount() 
{ 

#if CEYLAN_ARCH_NINTENDO_DS

	return 1 ;
	
#else // CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getTotalProcessCount: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS
	
	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalProcessCount: "
			"unable to query system: "	+ explainError() ) ;
	
	return static_cast<Ceylan::Uint32>( sysinfoStruct.procs ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalProcessCount: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 




UnsignedLongInteger Ceylan::System::getTotalSystemMemorySize() 
{ 

#if CEYLAN_ARCH_NINTENDO_DS

	// Main RAM is 4 MB-KkB:
	return (4*1024 - 4) * 1024 ;
	
#else // CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getTotalSystemMemorySize: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalSystemMemorySize: "
			"unable to query system: "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.totalram ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalSystemMemorySize: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 



UnsignedLongInteger Ceylan::System::getFreeSystemMemorySize() 
{ 

#if CEYLAN_ARCH_NINTENDO_DS

	/*
	 * @see http://forum.gbadev.org/viewtopic.php?t=14438
	 *
	 * Thanks Cydrak!
	 *
	 */
   struct mallinfo mi = mallinfo() ;
   
   return mi.fordblks + __eheap_end - (u8*) sbrk(0) ;

#else // CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getFreeSystemMemorySize: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getFreeSystemMemorySize: "
			"unable to query system: "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.freeram ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getFreeSystemMemorySize: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 



UnsignedLongInteger Ceylan::System::getUsedSystemMemorySize() 
{ 

#if CEYLAN_ARCH_NINTENDO_DS

	/*
	 * @see http://forum.gbadev.org/viewtopic.php?t=14438
	 *
	 * Thanks Cydrak!
	 *
	 */
   struct mallinfo mi = mallinfo() ;

   return mi.uordblks ; 
   
#else // CEYLAN_ARCH_NINTENDO_DS
	
	throw SystemException( 
		"Ceylan::System::getUsedSystemMemorySize: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 



UnsignedLongInteger Ceylan::System::getTotalSwapMemorySize() 
{ 

#if CEYLAN_ARCH_NINTENDO_DS

	return 0 ;
	
#else // CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getTotalSwapMemorySize: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalSwapMemorySize: "
			"unable to query system: " + explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.totalswap ) ;
	
#endif // CEYLAN_ARCH_SOLARIS

#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalSwapMemorySize: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 



UnsignedLongInteger Ceylan::System::getFreeSwapMemorySize() 
{ 
	
#if CEYLAN_ARCH_NINTENDO_DS

	return 0 ;
	
#else // CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getFreeSwapMemorySize: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getFreeSwapMemorySize: "
			"unable to query system: "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.freeswap ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getFreeSwapMemorySize: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 



UnsignedLongInteger Ceylan::System::getTotalHighMemorySize() 
{ 

#if CEYLAN_ARCH_NINTENDO_DS

	return 0 ;
	
#else // CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getTotalHighMemorySize: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalHighMemorySize: "
			"unable to query system: "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.totalhigh ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalHighMemorySize: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 



UnsignedLongInteger Ceylan::System::getFreeHighMemorySize() 
{ 
	
#if CEYLAN_ARCH_NINTENDO_DS

	return 0 ;
	
#else // CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getFreeHighMemorySize: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getFreeHighMemorySize: "
			"unable to query system: " + explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.freehigh ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getFreeHighMemorySize: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 



UnsignedLongInteger Ceylan::System::getSharedMemorySize() 
{ 
	
#if CEYLAN_ARCH_NINTENDO_DS

	// Main RAM shared between the ARMs:
	return getTotalSystemMemorySize() ;
	
#else // CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getSharedMemorySize: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getSharedMemorySize: "
			"unable to query system: "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.sharedram ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getSharedMemorySize: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 



UnsignedLongInteger Ceylan::System::getBuffersMemorySize() 
{ 
	
#if CEYLAN_ARCH_NINTENDO_DS

	return 0 ;
	
#else // CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getBuffersMemorySize: "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getBuffersMemorySize: "
			"unable to query system: "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.bufferram ) ;

#endif // CEYLAN_ARCH_SOLARIS

#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getBuffersMemorySize: "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 





// User-data related section, notably for the Nintendo DS.



string Ceylan::System::GetUserName()
{ 

#if CEYLAN_ARCH_NINTENDO_DS

#ifdef CEYLAN_RUNS_ON_ARM9

	/*
	 * Heavily inspired of http://licklick.wordpress.com/2006/08/,
	 * thanks Rick Wong (Lick).
	 *
	 */	
	char name[11] ;
   
	// Gets ASCII-bits from UTF-16 name:
   
	for ( Ceylan::Uint32 i = 0; i < PersonalData->nameLen; i++ )
		name[i] = static_cast<char>( PersonalData->name[i] & 0xff ) ; 
 
	name[PersonalData->nameLen] = 0 ;
	
	return string( name ) ;
		
#else // CEYLAN_RUNS_ON_ARM9

	throw SystemException( "Ceylan::System::getUserName: "
		"not available on the ARM7." ) ;	
	
#endif // CEYLAN_RUNS_ON_ARM9

#else // CEYLAN_ARCH_NINTENDO_DS

	throw SystemException( "Ceylan::System::getUserName: "
		"not available on this platform." ) ;	

#endif // CEYLAN_ARCH_NINTENDO_DS
	
} 



BatteryStatus Ceylan::System::GetBatteryStatus()
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM9
	

	// Let the exceptions propagate:
	FIFO & fifo = FIFO::GetActivatedFIFO() ;
	
	fifo.sendBatteryStatusRequest() ;
	
	// Blocking wait:
	return fifo.getBatteryStatus() ;

					
#else // CEYLAN_RUNS_ON_ARM9

	throw SystemException( "GetBatteryStatus failed: "
		"not available on the ARM7." ) ;
	
#endif // CEYLAN_RUNS_ON_ARM9
		
#else // CEYLAN_ARCH_NINTENDO_DS

	throw SystemException( "GetBatteryStatus failed: "
		"not available on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}



DSType Ceylan::System::GetDSType()
{

#if CEYLAN_ARCH_NINTENDO_DS
	
#ifdef CEYLAN_RUNS_ON_ARM9
	

	// Let the exceptions propagate:
	FIFO & fifo = FIFO::GetActivatedFIFO() ;
	
	fifo.sendDSTypeRequest() ;
	
	// Blocking wait:
	return fifo.getDSType() ;

					
#else // CEYLAN_RUNS_ON_ARM9

	throw SystemException( "GetDSType failed: "
		"not available on the ARM7." ) ;
	
#endif // CEYLAN_RUNS_ON_ARM9
		
#else // CEYLAN_ARCH_NINTENDO_DS

	throw SystemException( "GetDSType failed: "
		"not available on this platform." ) ;

#endif // CEYLAN_ARCH_NINTENDO_DS

}




// OpenGL-related section.


/*
 * OpenGL contexts should never be lost, but it happens with buggy
 * vendor-specific OpenGL implementations (on purpose?).
 *
 * On Windows, the OpenGL contexts can be lost when:
 *   - window resizing/changing resolutions, including going to fullscreen
 *   - switching to another application
 *	 - changing color depth
 *
 * On Mac OS X, the OpenGL contexts may (not sure) be lost when:
 *   - window resizing/changing resolutions, including going to fullscreen
 *   - switching to another application
 *	 - changing color depth
 *
 * On GNU/Linux and other platforms, the contexts are not lost at random.
 * This is the default setting.
 *
 */


bool Ceylan::System::openGLContextsCanBeLost()
{

	return openGLContextsLostOnResize() 
		|| openGLContextsLostOnApplicationSwitch()
		|| openGLContextsLostOnColorDepthChange() ;

}



bool Ceylan::System::openGLContextsLostOnResize()
{
 
#if CEYLAN_ARCH_WINDOWS

	return true ;

#else // CEYLAN_ARCH_WINDOWS



#if CEYLAN_ARCH_MACOSX

	return true ;

#else // CEYLAN_ARCH_MACOSX

	return false ;
	
#endif // CEYLAN_ARCH_MACOSX

 
#endif // CEYLAN_ARCH_WINDOWS

}



bool Ceylan::System::openGLContextsLostOnApplicationSwitch() 
{
	 
#if CEYLAN_ARCH_WINDOWS

	return true ;

#else // CEYLAN_ARCH_WINDOWS



#if CEYLAN_ARCH_MACOSX

	return true ;

#else // CEYLAN_ARCH_MACOSX

	return false ;
	
#endif // CEYLAN_ARCH_MACOSX

 
#endif // CEYLAN_ARCH_WINDOWS

}



bool Ceylan::System::openGLContextsLostOnColorDepthChange() 
{

#if CEYLAN_ARCH_WINDOWS

	return true ;

#else // CEYLAN_ARCH_WINDOWS



#if CEYLAN_ARCH_MACOSX

	return true ;

#else // CEYLAN_ARCH_MACOSX

	return false ;
	
#endif // CEYLAN_ARCH_MACOSX

 
#endif // CEYLAN_ARCH_WINDOWS

}

