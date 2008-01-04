#ifndef CEYLAN_SYSTEM_INCLUDES_H_
#define CEYLAN_SYSTEM_INCLUDES_H_


// Please include below all header files to be exported by module 'System'.


#include "CeylanAnonymousInputOutputStream.h"
#include "CeylanAnonymousInputStream.h"
#include "CeylanConsole.h"
#include "CeylanDirectory.h"
#include "CeylanEnvironmentVariables.h"
#include "CeylanFile.h"
#include "CeylanFileLocator.h"
#include "CeylanFileSystemCommon.h"
#include "CeylanFileSystemManager.h"
#include "CeylanInputOutputStream.h"
#include "CeylanInputStream.h"
#include "CeylanLibfatDirectory.h"
#include "CeylanLibfatFile.h"
#include "CeylanLibfatFileSystemManager.h"
#include "CeylanMemoryStream.h"
#include "CeylanMutex.h"
#include "CeylanOutputStream.h"
#include "CeylanPipe.h"
#include "CeylanProcess.h"
#include "CeylanRunnable.h"
#include "CeylanSignal.h"
#include "CeylanStandardDirectory.h"
#include "CeylanStandardFile.h"
#include "CeylanStandardFileSystemManager.h"
#include "CeylanStream.h"
#include "CeylanSynchronized.h"
#include "CeylanSystem.h"
#include "CeylanSystemInformation.h"
#include "CeylanThread.h"
#include "CeylanTypes.h"
#include "CeylanUniformResourceIdentifier.h"

/*
 * Private non-exported non-installed headers: 
 *   - CeylanEndianness.h: for byte-swapping 
 *   - CeylanIPCCommands.h: for commands exchanged internally by the DS ARMs
 * 
 * CeylanARM7Codes.h installed so that ARM7 status words and error codes
 * can be used on the ARM9 side.
 *
 */


#endif // CEYLAN_SYSTEM_INCLUDES_H_
