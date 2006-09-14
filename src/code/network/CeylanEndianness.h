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



#ifdef CEYLAN_USES_BYTESWAP_H



#define ceylan_bswap_16(x) bswap_16(x) 

#define ceylan_bswap_32(x) bswap_32(x)

#define ceylan_bswap_64(x) bswap_64(x)



#else // CEYLAN_USES_BYTESWAP_H



#define ceylan_bswap_16(x) ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8)) 

#define ceylan_bswap_32(x) \
	(((x) & 0xff000000) >> 24) | (((x) & 0x00ff0000) >>  8) | \
	(((x) & 0x0000ff00) <<  8) | (((x) & 0x000000ff) << 24))

#define ceylan_bswap_64(x) \
	 ((((x) & 0xff00000000000000ull) >> 56)      \
	| (((x) & 0x00ff000000000000ull) >> 40)      \
	| (((x) & 0x0000ff0000000000ull) >> 24)      \
	| (((x) & 0x000000ff00000000ull) >> 8)       \
	| (((x) & 0x00000000ff000000ull) << 8)       \
	| (((x) & 0x0000000000ff0000ull) << 24)      \
	| (((x) & 0x000000000000ff00ull) << 40)      \
	| (((x) & 0x00000000000000ffull) << 56)) 



#endif // CEYLAN_USES_BYTESWAP_H




#endif // CEYLAN_ENDIANNESS_H_
