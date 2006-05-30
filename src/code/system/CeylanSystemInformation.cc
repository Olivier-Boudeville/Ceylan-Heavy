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

#if CEYLAN_USES_SYS_SYSINFO_H
#include <sys/sysinfo.h>           // for GNU/Linux sysinfo
#endif // CEYLAN_USES_SYS_SYSINFO_H

}



UnsignedLongInteger Ceylan::System::getSecondsSinceSystemBoot() 
	throw( SystemException )
{ 
	
#if CEYLAN_USES_SYSINFO
	
	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getSecondsSinceSystemBoot : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( sysinfoStruct.uptime ) ;
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getSecondsSinceSystemBoot : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



Ceylan::Uint32 Ceylan::System::getTotalProcessCount() 
	throw( SystemException )
{ 

#if CEYLAN_USES_SYSINFO
	
	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalProcessCount : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<Ceylan::Uint32>( sysinfoStruct.procs ) ;
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalProcessCount : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 




UnsignedLongInteger Ceylan::System::getTotalSystemMemorySize() 
	throw( SystemException )
{ 
	
#if CEYLAN_USES_SYSINFO

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalSystemMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.totalram ) ;
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalSystemMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getFreeSystemMemorySize() 
	throw( SystemException )
{ 
	
#if CEYLAN_USES_SYSINFO

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getFreeSystemMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.freeram ) ;
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getFreeSystemMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getTotalSwapMemorySize() 
	throw( SystemException )
{ 
	
#if CEYLAN_USES_SYSINFO

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalSwapMemorySize : "
			"unable to query system : " + explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.totalswap ) ;
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalSwapMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getFreeSwapMemorySize() 
	throw( SystemException )
{ 
	
#if CEYLAN_USES_SYSINFO

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getFreeSwapMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.freeswap ) ;
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getFreeSwapMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 





UnsignedLongInteger Ceylan::System::getTotalHighMemorySize() 
	throw( SystemException )
{ 
	
#if CEYLAN_USES_SYSINFO

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getTotalHighMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.totalhigh ) ;
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getTotalHighMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getFreeHighMemorySize() 
	throw( SystemException )
{ 
	
#if CEYLAN_USES_SYSINFO

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getFreeHighMemorySize : "
			"unable to query system : " + explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.freehigh ) ;
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getFreeHighMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 




UnsignedLongInteger Ceylan::System::getSharedMemorySize() 
	throw( SystemException )
{ 
	
#if CEYLAN_USES_SYSINFO

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getSharedMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.sharedram ) ;
	
#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getSharedMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 



UnsignedLongInteger Ceylan::System::getBuffersMemorySize() 
	throw( SystemException )
{ 
	
#if CEYLAN_USES_SYSINFO

	struct sysinfo sysinfoStruct ;
	
	if ( ::sysinfo( & sysinfoStruct ) != 0 )
		throw SystemException( 
			"Ceylan::System::getBuffersMemorySize : "
			"unable to query system : "	+ explainError() ) ;
	
	return static_cast<UnsignedLongInteger>( 
		sysinfoStruct.mem_unit * sysinfoStruct.bufferram ) ;

#else // CEYLAN_USES_SYSINFO

	throw SystemException( 
		"Ceylan::System::getBuffersMemorySize : "
		"not available on this platform." ) ;	
	
#endif // CEYLAN_USES_SYSINFO
	
} 


