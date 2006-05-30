#ifndef CEYLAN_LOG_H_
#define CEYLAN_LOG_H_

#include <string>


#include "CeylanException.h"



namespace Ceylan
{


	namespace Log
	{


		/**
		 * This part of the Log system is meant to centralize common features needed
		 * by the actors of the Log framework.
		 *
		 * @see LogPlug
		 *
		 */
		 
		 
		/**
		 * Logging system.
		 *
		 * Ceylan's Log system uses interfaces defined in order to abstract, through an 
		 * uniform paradigm, various log implementations : as being able to swap log 
		 * implementations seamlessly is useful for programs and objects, they all 
		 * make use of this Log interface to send their log messages. The interface in turn
		 * delegate the log messages according to the registered log implementations, thanks
		 * to the LogPlug.
		 *
		 *
		 * To store thematically log messages, log channels are available. They are designed 
		 * to be subclassed by various implementations.

		 * Each message has also its own level of detail.
		 *
		 * The level of detail of a log message defines how important it is.
		 *
		 * Levels start from 1 (the most important level), and the more a level is high, 
		 * the less important the corresponding message is.
		 *
		 * Example : level 2 messages are considered as more important than level 3 ones.
		 *
		 * Log sources have also a level of detail, and they filter out their outgoing log
		 * messages if their own level of detail is smaller than the message's one.
		 * 
		 * Each Log listener has its own level of detail too (default : level 10).
		 *
		 * A Log listener will output any message whose level is equal or smaller than its current
		 * level of detail.
		 *
		 * Example : a listener with a registered level of detail of 4 will output a 
		 * message with a level of 3 or 4, but will not propagate a message whose level
		 * is 5 or greater.
		 *
		 * Incoming messages can be time-stamped if the Log channel option is set.
		 *
		 * The six default log channels are :
		 * 
		 *   1. info    : to gather informative messages
		 *   2. trace   : to know where the execution went
		 *	 3. debug   : to help troubleshooting
		 *   4. warning : to notify a non-critical execution property was not verified
		 *	 5. error   : to track down non-fatal abnormal behaviours
		 *   6. fatal   : to trace fatal abnormalities, just before the immediate failure
		 *	of the program.
		 *
		 * A shadow log channel exists, logroot, it is used by the log system itself.
		 *
		 */	
		 
		 
		/// Level of detail (lod) for log messages and channels.
		typedef unsigned int LevelOfDetail ;


		/// The default level of detail of a Log listener.
 		const LevelOfDetail DefaultLevelOfDetailForListener = 10  ;

 		/// The maximum level of detail for a Log message, should be equal to one.
 		const LevelOfDetail MaximumLevelOfDetailForMessage = 0 ;

 		/// The default level of detail for a Log message.
 		const LevelOfDetail DefaultLevelOfDetailForMessage = 5  ;


		/**
		 * The separator between the log protocol name and the corresponding URI.
		 * 
		 * Example : the log protocol separator in 'loggable://osdl.sourceforge.net' is '://'
		 *
		 */
		const std::string ProtocolSeparator = "://" ; 


		/// Exception by all actors of the log service.
	    class LogException: public Ceylan::Exception
		{

			public:
	
				explicit LogException( const std::string & reason ) throw() ;				
				virtual ~LogException() throw() ;

		} ;
		
	}

} 





#endif // CEYLAN_LOG_H_
