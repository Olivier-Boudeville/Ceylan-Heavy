#ifndef CEYLAN_ENDIANNESS_H_
#define CEYLAN_ENDIANNESS_H_



// It is a private header, hence configuration settings can be used here :

#if CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"      // for configure-time feature settings
#endif // CEYLAN_USES_CONFIG_H


extern "C"
{

#ifdef CEYLAN_USES_BYTESWAP_H
#include <byteswap.h>          // for bswap_16, etc.
#endif // CEYLAN_USES_BYTESWAP_H

#ifdef CEYLAN_USES_MACHINE_BSWAP_H
#include <machine/bswap.h>     // for bswap_16, etc.
#endif // CEYLAN_USES_MACHINE_BSWAP_H

}

#include <cstdlib>             // for bswap



#ifdef CEYLAN_USES_BYTESWAP_H


#define ceylan_bswap_16(x) bswap_16(x) 

#define ceylan_bswap_32(x) bswap_32(x)

#define ceylan_bswap_64(x) bswap_64(x)



#else // CEYLAN_USES_BYTESWAP_H


// No built-in bswap_*, let's define ours :

#define ceylan_bswap_16(x) \
	 ((((x) >> 8) & 0xff)                        \
	| (((x) & 0xff) << 8)) 


#define ceylan_bswap_32(x) \
	 ((((x) & 0xff000000) >> 24)                 \
	| (((x) & 0x00ff0000) >>  8)                 \
	| (((x) & 0x0000ff00) <<  8)                 \
	| (((x) & 0x000000ff) << 24))



/*
 * This macro can only be used if a true native
 * 64-bit datatype is available on the running
 * platform, i.e. if CEYLAN_FAKES_64_BIT_TYPE
 * is not defined.
 *
 * Otherwise the Ceylan::byteswap functions should be used.
 *
 * Problem : on 32-bit Big Endian (ex : Ultra-60 Solaris 5.9), the 
 * ceylan_bswap_64 macro will need an operator '&' defined, as
 * Ceylan::Uint64:
 * error: no match for 'operator&' in '*tmp & 000000000...'
 * as there is no real 64-bit type, but only a faked struct
 * (see CeylanTypes.h).
 *
 */

// Native 64-bit support ? 
#ifndef CEYLAN_FAKES_64_BIT_TYPE

// Yes ? This macro is enough :

#define ceylan_bswap_64(x) \
	 ((((x) & 0xff00000000000000ull) >> 56)      \
	| (((x) & 0x00ff000000000000ull) >> 40)      \
	| (((x) & 0x0000ff0000000000ull) >> 24)      \
	| (((x) & 0x000000ff00000000ull) >>  8)      \
	| (((x) & 0x00000000ff000000ull) <<  8)      \
	| (((x) & 0x0000000000ff0000ull) << 24)      \
	| (((x) & 0x000000000000ff00ull) << 40)      \
	| (((x) & 0x00000000000000ffull) << 56)) 

// Otherwise : use Ceylan::byteswap

#endif // CEYLAN_FAKES_64_BIT_TYPE



namespace Ceylan
{


	/**
	 * Swaps the bytes of specified 64-bit number according to the conventions
	 * of a change of endianness. 
	 *
	 * @note This function is declared both for Ceylan::Uint64 and for
	 * Ceylan::Sint64. 
	 *
	 */
	inline void byteswap( Ceylan::Uint64 & toSwap ) throw()
	{

#ifdef CEYLAN_FAKES_64_BIT_TYPE

		/*
		 * Inspired from /usr/include/bits/byteswap.h on Linux :
		 *
		 * The conversion cannot be performed 'in place' (with no temporary
		 * variable) as one orginal attribute (hi or lo) would be overwritten
		 * by the converted other (lo or hi).
		 *
		 * @note Not tested yet.
		 *
		 */

		 Ceylan::Uint64 res ;
		 
		 res.hi = ceylan_bswap_32( toSwap.lo ) ;
		 res.lo = ceylan_bswap_32( toSwap.hi ) ;
		 
		 toSwap = res ;
		 
#else // CEYLAN_FAKES_64_BIT_TYPE

		// Should not be called in this case, as totally useless :
		return toSwap ;
		
#endif // CEYLAN_FAKES_64_BIT_TYPE
		 
	}


}


#endif // CEYLAN_USES_BYTESWAP_H




#endif // CEYLAN_ENDIANNESS_H_
