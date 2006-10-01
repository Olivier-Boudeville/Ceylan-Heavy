#ifndef CEYLAN_LOG_PLUG_H_
#define CEYLAN_LOG_PLUG_H_


#include "CeylanLog.h"

#include <string>


namespace Ceylan
{

	namespace Log
	{


		/// Used as plug endpoints, the Log messages come through them.
		class LogSource ;
		
		
		/**
		 * Where messages from a LogSource will be output : the first 
		 * part of the LogTransport-LogListener pair.
		 *
		 */
		class LogTransport ;
		
		
		/**
		 * Where messages from a LogSource will be received : the second 
		 * part of the LogTransport-LogListener pair, before the
		 * LogAggregator.
		 *
		 */
		class LogListener ;
		
		
		/**
		 * Where messages are finally user-friendly output. 
		 *
		 */
		class LogAggregator ;
		
		
		/**
		 * This class is in charge of managing the core of the Log system :
		 * it maintains a shared knowledge about all the implementation 
		 * classes for LogSource, LogTransport, LogListener and 
		 * LogAggregator, and links them together.
		 *
		 * Default standard log channels are registered through the 
		 * LogPlug too.
		 *
		 * LogPlug is the bridge between Log service users and the Log
		 * implementation that is in use.
		 *
		 * @note This class is mainly a clean container dedicated to the
		 * gathering of static methods. 
		 *
		 */
		class CEYLAN_DLL LogPlug
		{
		
			public:


		        /// Assigns a LogSource for the default log channel info.
		        static void SetInfoLogSource( LogSource & newInfoLogSource )
					 throw() ;


		        /// Returns the LogSource for the default log channel info.
		        static LogSource & GetInfoLogSource() throw() ;



		        /// Assigns a LogSource for the default log channel trace.
		        static void SetTraceLogSource( 
					LogSource & newTraceLogSource )	throw() ;


		        /// Returns the LogSource for default log channel trace.
		        static LogSource & GetTraceLogSource() throw() ;



		        /// Assigns a LogSource for the default log channel debug.
		        static void SetDebugLogSource( 
					LogSource & newDebugLogSource )	throw() ;


		        /// Returns the LogSource for default log channel debug.
		        static LogSource & GetDebugLogSource() throw() ;



		        /// Assigns a LogSource for the default log channel warning.
		        static void SetWarningLogSource( 
					LogSource & newWarningLogSource ) throw() ;


		        /// Returns the LogSource for default log channel warning.
		        static LogSource & GetWarningLogSource() throw() ;



		        /// Assigns a LogSource for the default log channel error.
		        static void SetErrorLogSource( 
					LogSource & newErrorLogSource ) throw() ;


		        /// Returns the LogSource for default log channel error.
		        static LogSource & GetErrorLogSource() throw() ;




		        /// Assigns a LogSource for the default log channel fatal.
		        static void SetFatalLogSource( 
					LogSource & newFatalLogSource ) throw() ;


		        /// Returns the LogSource for default log channel fatal.
		        static LogSource & GetFatalLogSource() throw() ;


				/**
				 * Tells whether the fatal channel of the Log Plug is
				 * initialized.
				 *
				 */
				static bool IsFatalLogSourceAvailable() throw() ;




		        /// Assigns a LogSource for the default log channel logroot.
		        static void SetLogRootLogSource( 
					LogSource & newLogRootLogSource ) throw() ;


		        /// Returns the LogSource for default log channel logroot.
		        static LogSource & GetLogRootLogSource() throw() ;



		        /// Assigns a LogSource for the default log channel logroot.
		        static void SetTransport( 
					LogTransport & newTransport ) throw() ;


		        /// Returns the log transport currently being used by the plug.
		        static LogTransport & GetTransport() throw() ;



		        /// Assigns a LogSource for the default log channel logroot.
		        static void SetListener( 
					LogListener & newListener ) throw() ;


		        /// Returns the log Listener currently being used by the plug.
		        static LogListener & GetListener() throw() ;



		        /// Assigns a LogSource for the default log channel logroot.
		        static void SetAggregator( 
					LogAggregator & newAggregator ) throw() ;


		        /// Returns the log Aggregator currently being used by the plug.
		        static LogAggregator & GetAggregator() throw() ;


				/**
				 * Verifies that the Log Plug is totally empty, no log source,
				 * transport, listener, aggregator being registered.
				 *
				 */
				static void CheckBlank() throw ( LogException ) ;
				
								
				
				/**
				 * Creates a basic plug, formed of the standard channels,
				 * provided a transport is already available.
				 *
				 */
				static void CreateBasicPlug() throw ( LogException ) ;
				
				
		        /**
		         * Starts the Log service and declares the name of the 
				 * source of the Log messages.
		         *
		         * @param plugCreator the name of the plug initiator, 
				 * for instance argv[0].
				 *
		         * @note To ensure all implementations can cope with the
				 * supplied name, prefer simple words for source name. 
				 * One constraint would be to take a sequence of
				 * characters which could stand as a filename, so that a
				 * file-based implementation of the log system could be used
				 * with no further encoding.
				 *
		         */
		        static void StartService( const std::string & plugCreator )
					throw ( LogException ) ;


		        /**
				 * Stops the Log service.
				 *
				 * @param warnIfAlreadyStopped if true, if the log system
				 * is not available when this method is called (either not
				 * launched or already stopped), then an error message is
				 * output. Otherwise this is a silent shutdown of the log
				 * system.
				 *
				 */
		        static void StopService( bool warnIfAlreadyStopped = true ) 
					throw() ;
				
				
				/// Returns the source name with initiated the log plug.
				static const std::string GetSourceName() throw() ;
				
				
				/// Returns some informations about the LogPlug's state.
	           	static const std::string ToString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) throw() ;
					
					
	        	/**
		         * Error message to be displayed on attempt of using the
				 * Log system, whereas the service has not been started yet
				 * (empty plug).
				 *
		         */
		        static const std::string LogSystemNotInitialized ;
		
		
			    /**
			     * Sends a message to the default log channel Info.
			     *
			     * This channel is to be used to send messages which
				 * should be read by the user, so its level of detail 
				 * should be high.
			     *
				 * @note Despite the method is static, its name does not
				 * start with an uppercased letter since it is deemed more
				 * readable that way.
				 *
			     */
			    static void info( const std::string & message,
			    		LevelOfDetail levelOfDetail 
							= Log::MaximumLevelOfDetailForMessage ) 
					throw( LogException ) ;


			    /**
			     * Sends a message to the default log channel trace.
			     *
			     * This channel is to be used to send messages describing
				 * the function calls. 
				 * This may, or may not, interest the user.
			     *
				 * @note Despite the method is static, its name does not
				 * start with an uppercased letter since it is deemed more
				 * readable that way.
			     *
			     */
			    static void trace( const std::string & message,
			    		LevelOfDetail levelOfDetail =
							Log::DefaultLevelOfDetailForMessage ) 
					throw( LogException ) ;


			    /**
			     * Sends a message to the default log channel debug.
			     *
			     * This channel is to be used to send messages helping the
				 * developer figuring out what happens in the system.
			     *
				 * @note Despite the method is static, its name does not
				 * start with an uppercased letter since it is deemed more
				 * readable that way.
			     *
			     */
			    static void debug( const std::string & message,
			    		LevelOfDetail levelOfDetail =
							Log::DefaultLevelOfDetailForMessage ) 
					throw( LogException ) ;


			    /**
			     * Sends a message to the default log channel warning.
			     *
			     * This channel is to be used to issue non critical 
				 * errors on abnormal events.
			     *
			     * Its level of detail should be high.
			     *
				 * @note Despite the method is static, its name does not
				 * start with an uppercased letter since it is deemed more
				 * readable that way.
			     *
			     */
			    static void warning( const std::string & message,
			    		LevelOfDetail levelOfDetail =
							Log::MaximumLevelOfDetailForMessage ) 
					throw( LogException ) ;


			    /**
			     * Sends a message to the default log channel error.
			     *
			     * This channel is to be used to issue critical errors
				 * on abnormal events.
			     *
			     * Its level of detail should be the highest, so that 
				 * it will never be missed.
			     *
				 * @note Despite the method is static, its name does not
				 * start with an uppercased letter since it is deemed more
				 * readable that way.
			     *
			     */
			    static void error( const std::string & message,
			    		LevelOfDetail levelOfDetail =
							Log::MaximumLevelOfDetailForMessage ) 
					throw( LogException ) ;


			    /**
			     * Sends a message to the default log channel fatal.
			     *
			     * This channel is to be used to trace fatal situtations,
				 * just before the immediate failure of the program.
			     *
			     * Its level of detail should be the highest, so that 
				 * it will never be missed.
			     *
				 * @note Despite the method is static, its name does not
				 * start with an uppercased letter since it is deemed more
				 * readable that way.
			     *
			     */
			    static void fatal( const std::string & message,
			    		LevelOfDetail levelOfDetail =
							Log::MaximumLevelOfDetailForMessage ) 
					throw( LogException ) ;
	
	
			    /**
			     * Sends a message to the special log channel logroot.
			     *
			     * This channel is to be used solely for internal log 
				 * system purpose.
			     *
			     * Its level of detail should be the highest, so that 
				 * it will never be missed.
			     *
				 * @note Despite the method is static, its name does not
				 * start with an uppercased letter since it is deemed more
				 * readable that way.
			     *
			     */
			    static void logroot( const std::string & message,
			    		LevelOfDetail levelOfDetail =
							Log::MaximumLevelOfDetailForMessage ) 
					throw( LogException ) ;
	
	
	
					
			protected:	
				 
				 
				/// Plug for the info channel. 
				static LogSource * InfoLogSource ;
				 
				/// Plug for the trace channel. 
				static LogSource * TraceLogSource ;
				 
				/// Plug for the debug channel. 
				static LogSource * DebugLogSource ;
				 
				/// Plug for the warning channel. 
				static LogSource * WarningLogSource ;
				 
				/// Plug for the error channel. 
				static LogSource * ErrorLogSource ;
				 
				/// Plug for the fatal channel. 
				static LogSource * FatalLogSource ;

				 
				/// Plug for the log root channel. 
				static LogSource * LogrootLogSource ;
				
				
				/// Plug for the Log transport.
				static LogTransport * Transport ;
				
				
				/**
				 * Plug for the possible Log Listener, if framework is 
				 * purely local.
				 *
				 */
				static LogListener * Listener ;
				
								
				/**
				 * Plug for the possible Log Aggregator, if framework is
				 * purely local.
				 *
				 */
				static LogAggregator * Aggregator ;
				
				
				/// Records the source name, which initiated the log session.
				static std::string SourceName ;
				
			
							
				/// LogPlug should not instanciated, even as a mother class.
				LogPlug() throw ( LogException )

#ifndef CEYLAN_RUNS_ON_WINDOWS
				/*
				 * g++ (gcc) needs this __attribute__ (otherwise a blocking 
				 * warning is issued), but Visual C++ does not understand it.
				 * As we are in a header file, only the CEYLAN_RUNS_ON_WINDOWS
				 * can be available here.
				 *
				 */
					__attribute__ ((noreturn))  
#endif // CEYLAN_RUNS_ON_WINDOWS
				;
				

				/// Virtual destructor.
				virtual ~LogPlug() throw() ;
				
				
				
			private:
				
				
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogPlug( const LogPlug & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogPlug & operator = ( const LogPlug & source ) throw() ;
				
					
		} ;	
		
	}

} 





#endif // CEYLAN_LOG_PLUG_H_
