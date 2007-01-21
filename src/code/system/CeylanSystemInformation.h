#ifndef CEYLAN_SYSTEM_INFORMATION_H_
#define CEYLAN_SYSTEM_INFORMATION_H_



#include "CeylanSystem.h"   // for SystemException, etc.
#include "CeylanTypes.h"    // for UnsignedLongInteger, Ceylan::Uint32, etc.


#include <ctime>
#include <string>
#include <iosfwd>


/*
 * Some Ceylan-using libraries or programs have to know the endianess :
 * 
 * @note Defined to 1 if the platform is little endian, 0 otherwise.
 *
 */
#define CEYLAN_DETECTED_LITTLE_ENDIAN @CEYLAN_RUNS_ON_LITTLE_ENDIAN@


namespace Ceylan
{



	/**
	 * System query primitives.
	 *
	 * @note Load reporting could be added in cross-platform meaning
	 * can be found.
	 *
	 */
	namespace System
	{


		/**
		 * Returns the uptime, the number of seconds since the 
		 * system booted.
		 *
		 * @throw SystemException on error, or if not implemented 
		 * for this platform.
		 *
		 */
		CEYLAN_DLL UnsignedLongInteger getSecondsSinceSystemBoot() 
			throw( SystemException ) ; 



		/**
		 * Returns the current number of running processes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		CEYLAN_DLL Ceylan::Uint32 getTotalProcessCount() 
			throw( SystemException ) ; 




		/**
		 * Returns the total size of usable main memory, in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		CEYLAN_DLL UnsignedLongInteger getTotalSystemMemorySize() 
			throw( SystemException ) ; 


		/**
		 * Returns the size of currently available main memory, 
		 * in bytes.
		 *
		 * @throw SystemException on error, or if not implemented 
		 * for this platform.
		 *
		 */
		CEYLAN_DLL UnsignedLongInteger getFreeSystemMemorySize() 
			throw( SystemException ) ; 



		/**
		 * Returns the total size of swap memory, in bytes.
		 *
		 * @throw SystemException on error, or if not implemented 
		 * for this platform.
		 *
		 */
		CEYLAN_DLL UnsignedLongInteger getTotalSwapMemorySize() 
			throw( SystemException ) ; 


		/**
		 * Returns the size of currently available swap memory,
		 * in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		CEYLAN_DLL UnsignedLongInteger getFreeSwapMemorySize() 
			throw( SystemException ) ; 



		/**
		 * Returns the total size of usable high memory, in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		CEYLAN_DLL UnsignedLongInteger getTotalHighMemorySize() 
			throw( SystemException ) ; 


		/**
		 * Returns the size of currently available high memory,
		 * in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		CEYLAN_DLL UnsignedLongInteger getFreeHighMemorySize() 
			throw( SystemException ) ; 



		/**
		 * Returns the size of memory currently being shared, in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		CEYLAN_DLL UnsignedLongInteger getSharedMemorySize() 
			throw( SystemException ) ; 


		/**
		 * Returns the size of memory currently used by buffers,
		 * in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		CEYLAN_DLL UnsignedLongInteger getBuffersMemorySize() 
			throw( SystemException ) ; 

				
	}
	
	
}



#endif  // CEYLAN_SYSTEM_INFORMATION_H_
