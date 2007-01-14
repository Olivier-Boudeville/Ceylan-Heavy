#include "CeylanSystemInformation.h"

#include "CeylanLogPlug.h"         // for LogPlug
#include "CeylanOperators.h"       // for toString


#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"          // for configure-time settings
#endif // CEYLAN_USES_CONFIG_H



#include <string>

using std::string ;


using namespace Ceylan ;           // for UnsignedLongInteger
using namespace Ceylan::Log ;      // for LogPlug
using namespace Ceylan::System ;   // for SystemException



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
	throw( SystemException )
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getSecondsSinceSystemBoot : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getSecondsSinceSystemBoot : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( sysinfoStruct.uptime ) ;

#endif // CEYLAN_ARCH_SOLARIS

#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getSecondsSinceSystemBoot : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



Ceylan::Uint32 Ceylan::System::getTotalProcessCount() 
	throw( SystemException )
{ 

#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getTotalProcessCount : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS
	
	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalProcessCount : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<Ceylan::Uint32>( sysinfoStruct.procs ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalProcessCount : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 




UnsignedLongInteger Ceylan::System::getTotalSystemMemorySize() 
	throw( SystemException )
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getTotalSystemMemorySize : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalSystemMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.totalram ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalSystemMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getFreeSystemMemorySize() 
	throw( SystemException )
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getFreeSystemMemorySize : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getFreeSystemMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.freeram ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getFreeSystemMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getTotalSwapMemorySize() 
	throw( SystemException )
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getTotalSwapMemorySize : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalSwapMemorySize : "
			"unable to query system : " + explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.totalswap ) ;
	
#endif // CEYLAN_ARCH_SOLARIS

#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalSwapMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getFreeSwapMemorySize() 
	throw( SystemException )
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getFreeSwapMemorySize : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getFreeSwapMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.freeswap ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getFreeSwapMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getTotalHighMemorySize() 
	throw( SystemException )
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getTotalHighMemorySize : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalHighMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.totalhigh ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalHighMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getFreeHighMemorySize() 
	throw( SystemException )
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getFreeHighMemorySize : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getFreeHighMemorySize : "
			"unable to query system : " + explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.freehigh ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getFreeHighMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getSharedMemorySize() 
	throw( SystemException )
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getSharedMemorySize : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getSharedMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.sharedram ) ;

#endif // CEYLAN_ARCH_SOLARIS
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getSharedMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getBuffersMemorySize() 
	throw( SystemException )
{ 
	
#ifdef CEYLAN_USES_SYSINFO

#if CEYLAN_ARCH_SOLARIS

	throw SystemException( 
		"Ceylan::System::getBuffersMemorySize : "
		"not available on Solaris platform." ) ;	

#else // CEYLAN_ARCH_SOLARIS

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getBuffersMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.bufferram ) ;

#endif // CEYLAN_ARCH_SOLARIS

#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getBuffersMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 

