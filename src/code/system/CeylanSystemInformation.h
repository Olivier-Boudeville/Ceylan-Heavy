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


#ifndef CEYLAN_SYSTEM_INFORMATION_H_
#define CEYLAN_SYSTEM_INFORMATION_H_



#include "CeylanSystem.h"   // for SystemException, etc.
#include "CeylanTypes.h"    // for UnsignedLongInteger, Ceylan::Uint32, etc.


#include <ctime>
#include <string>
#include <iosfwd>



/*
 * Some Ceylan-using libraries or programs have to know the endianess:
 *
 * @note Defined automatically (at configure-time) to 1 if the platform is
 * little endian, 0 otherwise (big endian).
 *
 * The real source file is CeylanSystemInformation.h.in, not
 * CeylanSystemInformation.h.
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
	CEYLAN_DLL UnsignedLongInteger getSecondsSinceSystemBoot() ;



	/**
	 * Returns the current number of running processes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 */
	CEYLAN_DLL Ceylan::Uint32 getTotalProcessCount() ;



	/**
	 * Returns the total size of usable main memory, in bytes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 */
	CEYLAN_DLL UnsignedLongInteger getTotalSystemMemorySize() ;



	/**
	 * Returns the size of currently available main memory,
	 * in bytes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 * @see getUsedSystemMemorySize
	 *
	 */
	CEYLAN_DLL UnsignedLongInteger getFreeSystemMemorySize() ;



	/**
	 * Returns the size of main memory being currently used,
	 * in bytes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 * @see getFreeSystemMemorySize
	 *
	 */
	CEYLAN_DLL UnsignedLongInteger getUsedSystemMemorySize() ;



	/**
	 * Returns the total size of swap memory, in bytes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 */
	CEYLAN_DLL UnsignedLongInteger getTotalSwapMemorySize() ;



	/**
	 * Returns the size of currently available swap memory,
	 * in bytes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 */
	CEYLAN_DLL UnsignedLongInteger getFreeSwapMemorySize() ;



	/**
	 * Returns the total size of usable high memory, in bytes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 */
	CEYLAN_DLL UnsignedLongInteger getTotalHighMemorySize() ;



	/**
	 * Returns the size of currently available high memory,
	 * in bytes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 */
	CEYLAN_DLL UnsignedLongInteger getFreeHighMemorySize() ;



	/**
	 * Returns the size of memory currently being shared, in bytes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 */
	CEYLAN_DLL UnsignedLongInteger getSharedMemorySize() ;



	/**
	 * Returns the size of memory currently used by buffers,
	 * in bytes.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 */
	CEYLAN_DLL UnsignedLongInteger getBuffersMemorySize() ;





	// User-data related section, notably for the Nintendo DS.



	/**
	 * Returns the name of the user, as retrieved from the system.
	 *
	 * @throw SystemException on error, or if not implemented
	 * for this platform.
	 *
	 * On the Nintendo DS, textual personal data is stored in
	 * UTF-16, hence the management of some non-ASCII characters might be
	 * incorrect.
	 *
	 * On the NoCachGBA emulator, an empty string is returned.
	 *
	 */
	CEYLAN_DLL std::string GetUserName() ;




	/**
	 * Describes the status of the battery (if any).
	 *
	 */
	enum BatteryStatus
	{

	  /// Indicates that the charge of a battery is still high.
	  WellCharged,

	  /// Indicates that the charge of a battery is getting quite low.
	  AlmostEmpty,

	  /// Indicates that the charge of a battery is not known.
	  BatteryStatusUnknown

	} ;



	/**
	 * Returns the current battery status.
	 *
	 * On the DS ARM9, the IPC system, namely the singleton FIFO, is
	 * expected to be already available (created and activated).
	 *
	 * @throw SystemException if the operation failed.
	 *
	 * @note On the DS ARM9, it is a synchronous operation: the ARM9 is
	 * blocked (polling) until the ARM7 answers.
	 *
	 */
	CEYLAN_DLL BatteryStatus GetBatteryStatus() ;




	/**
	 * Describes the type of a Nintendo DS.
	 *
	 */
	enum DSType
	{

	  /// The first DS release.
	  DSFat,

	  /// The update of the first DS.
	  DSLite,

	  /// Indicates that the type of the DS is not known.
	  DSTypeUnknown

	} ;


	/**
	 * Returns the type of the DS running the program.
	 *
	 * @throw SystemException if the operation failed.
	 *
	 * @note On the DS ARM9, it is a synchronous operation: the ARM9 is
	 * blocked (polling) until the ARM7 answers.
	 *
	 */
	CEYLAN_DLL DSType GetDSType() ;





	// OpenGL-related section.



	/**
	 * Tells whether the OpenGL contexts can be lost (and
	 * therefore may have to be reloaded) under certain unexpected
	 * circumstances (ex: window resize, going to fullscreen,
	 * switching to another application, etc.) on the current platform,
	 * without specific notice.
	 *
	 * On some platforms, the OpenGL contexts can be lost when:
	 *  - window resizing/changing resolutions, including going to
	 * fullscreen
	 *  - switching to another application
	 *  - changing color depth
	 *
	 * @throw SystemException on error, or if not known for this platform.
	 *
	 */
	CEYLAN_DLL bool openGLContextsCanBeLost() ;



	/**
	 * Tells whether the OpenGL contexts are lost (and
	 * therefore may have to be reloaded) when the window application is
	 * resized (resolution changed).
	 *
	 * @throw SystemException on error, or if not known for this platform.
	 *
	 */
	CEYLAN_DLL bool openGLContextsLostOnResize() ;



	/**
	 * Tells whether the OpenGL contexts are lost (and
	 * therefore may have to be reloaded) when switching to another
	 * application.
	 *
	 * @throw SystemException on error, or if not known for this platform.
	 *
	 */
	CEYLAN_DLL bool openGLContextsLostOnApplicationSwitch() ;



	/**
	 * Tells whether the OpenGL contexts are lost (and
	 * therefore may have to be reloaded) when changing the color depth.
	 *
	 * @throw SystemException on error, or if not known for this platform.
	 *
	 */
	CEYLAN_DLL bool openGLContextsLostOnColorDepthChange() ;



  }


}



#endif  // CEYLAN_SYSTEM_INFORMATION_H_
