#ifndef CEYLAN_SYSTEM_INFORMATION_H_
#define CEYLAN_SYSTEM_INFORMATION_H_



#include "CeylanSystem.h"   // for SystemException, etc.
#include "CeylanTypes.h"    // for UnsignedLongInteger, Ceylan::Uint32, etc.


#include <ctime>
#include <string>
#include <iosfwd>


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
		UnsignedLongInteger getSecondsSinceSystemBoot() 
			throw( SystemException ) ; 



		/**
		 * Returns the current number of running processes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		Ceylan::Uint32 getTotalProcessCount() 
			throw( SystemException ) ; 




		/**
		 * Returns the total size of usable main memory, in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		UnsignedLongInteger getTotalSystemMemorySize() 
			throw( SystemException ) ; 


		/**
		 * Returns the size of currently available main memory, 
		 * in bytes.
		 *
		 * @throw SystemException on error, or if not implemented 
		 * for this platform.
		 *
		 */
		UnsignedLongInteger getFreeSystemMemorySize() 
			throw( SystemException ) ; 



		/**
		 * Returns the total size of swap memory, in bytes.
		 *
		 * @throw SystemException on error, or if not implemented 
		 * for this platform.
		 *
		 */
		UnsignedLongInteger getTotalSwapMemorySize() 
			throw( SystemException ) ; 


		/**
		 * Returns the size of currently available swap memory,
		 * in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		UnsignedLongInteger getFreeSwapMemorySize() 
			throw( SystemException ) ; 



		/**
		 * Returns the total size of usable high memory, in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		UnsignedLongInteger getTotalHighMemorySize() 
			throw( SystemException ) ; 


		/**
		 * Returns the size of currently available high memory,
		 * in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		UnsignedLongInteger getFreeHighMemorySize() 
			throw( SystemException ) ; 



		/**
		 * Returns the size of memory currently being shared, in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		UnsignedLongInteger getSharedMemorySize() 
			throw( SystemException ) ; 


		/**
		 * Returns the size of memory currently used by buffers,
		 * in bytes.
		 *
		 * @throw SystemException on error, or if not implemented
		 * for this platform.
		 *
		 */
		UnsignedLongInteger getBuffersMemorySize() 
			throw( SystemException ) ; 

				
	}
	
	
}



#endif  // CEYLAN_SYSTEM_INFORMATION_H_
