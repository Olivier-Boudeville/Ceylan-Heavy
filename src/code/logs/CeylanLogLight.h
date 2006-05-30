#ifndef CEYLAN_LOG_LIGHT_H_
#define CEYLAN_LOG_LIGHT_H_


/*
 * This header is private and not meant to be installed.
 * Hence it can depend on CeylanConfig.h.
 *
 *
 */
 
#ifdef CEYLAN_USES_CONFIG_H
#include "CeylanConfig.h"       // for CEYLAN_DEBUG_LOG
#endif // CEYLAN_USES_CONFIG_H



/**
 * Very basic log facility made to debug ... the full Log system !
 * (bootstrap)
 *
 */
 

#if CEYLAN_DEBUG_LOG

#include <iostream>

#define CEYLAN_LOG(message) std::cerr << "[LogLight] " << message << std::endl << std::flush
 
#else // CEYLAN_DEBUG_LOG

#define CEYLAN_LOG(message)

#endif // CEYLAN_DEBUG_LOG



#endif // CEYLAN_LOG_LIGHT_H_
