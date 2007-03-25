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
 * @note Defined automatically (at configure-time) to 1 if the platform is
 * little endian, 0 otherwise (big endian).
 *
 */
#define CEYLAN_DETECTED_LITTLE_ENDIAN 1


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



		// OpenGL-related section.


		/**
		 * Tells whether the OpenGL contexts can be lost (and 
		 * therefore may have to be reloaded) under certain unexpected
		 * circumstances (ex : window resize, going to fullscreen,
		 * switching to another application, etc.) on the current platform,
		 * without specific notice.
		 *
		 * On some platforms, the OpenGL contexts can be lost when :
		 *  - window resizing/changing resolutions, including going to
		 * fullscreen
		 *  - switching to another application
		 *  - changing color depth
		 *
		 * @throw SystemException on error, or if not known for this platform.
		 *
		 */
		CEYLAN_DLL bool openGLContextsCanBeLost() throw( SystemException ) ; 



		/**
		 * Tells whether the OpenGL contexts are lost (and 
		 * therefore may have to be reloaded) when the window application is
		 * resized (resolution changed).
		 *
		 * @throw SystemException on error, or if not known for this platform.
		 *
		 */
		CEYLAN_DLL bool openGLContextsLostOnResize() throw( SystemException ) ; 


		/**
		 * Tells whether the OpenGL contexts are lost (and 
		 * therefore may have to be reloaded) when switching to another
		 * application.
		 *
		 * @throw SystemException on error, or if not known for this platform.
		 *
		 */
		CEYLAN_DLL bool openGLContextsLostOnApplicationSwitch() 
			throw( SystemException ) ; 


		/**
		 * Tells whether the OpenGL contexts are lost (and 
		 * therefore may have to be reloaded) when changing the color depth.
		 *
		 * @throw SystemException on error, or if not known for this platform.
		 *
		 */
		CEYLAN_DLL bool openGLContextsLostOnColorDepthChange() 
			throw( SystemException ) ; 


	}
	
	
}



#endif  // CEYLAN_SYSTEM_INFORMATION_H_
