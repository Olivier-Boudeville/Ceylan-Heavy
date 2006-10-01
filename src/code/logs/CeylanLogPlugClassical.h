#ifndef CEYLAN_LOG_PLUG_CLASSICAL_H_
#define CEYLAN_LOG_PLUG_CLASSICAL_H_


#include "CeylanLog.h"       // for LogException
#include "CeylanLogPlug.h"   // for LogPlug

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
		 * Where messages from a LogSource will be received : 
		 * the second part of the LogTransport-LogListener pair, before
		 * the LogAggregator.
		 *
		 */
		class LogListener ;
		
		
		/**
		 * Where messages are finally user-friendly output. 
		 *
		 */
		class LogAggregator ;
		
		
		
		/**
		 * This class implements the LogPlug interface, registers the 
		 * various instances of the log framework in their raw version,
		 * and creates the relevant standard channels.
		 *
		 * @see LogPlug
		 *
		 */
		class CEYLAN_DLL LogPlugClassical : public LogPlug
		{
		
			public:



		        /**
		         * Starts the Log classical service.
		         *
		         * @param plugInitiator the name of the speaker, for 
				 * instance argv[0].
				 *
				 * @param immediateWrite tells whether the classical 
				 * aggregator should store messages as soon as they arrive,
				 * or wait till asked before aggregating them.
				 *
				 * @param smart tells whether the classical aggregator 
				 * should be smart.
				 *
		         * @note Since log messages will be output to a file 
				 * named 'sourceName'.log, sourceName must be a valid 
				 * prefix file name.
				 *
		         */
		        static void StartService( const std::string & plugInitiator, 
					bool immediateWrite = true, bool smart = true ) 
						throw( LogException ) ;


		        /// Stops the Log classical service.
		        static void StopService() throw() ;
				
				
				/**
				 * Returns some informations about the classical LogPlug's
				 * state.
				 *
				 */
	           	static const std::string ToString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) throw() ;
				
				
					
	
			private:


				/* 
				
				Not even declared to avoid : 
				'warning: will never be executed'
				
				/// LogPlug classical should not be instanciated.
				LogPlugClassical() throw( LogException ) 
					__attribute__ ((noreturn)) ;
				
				/// This destructor will be never called.
				virtual ~LogPlugClassical() throw() ;
			
				*/
			
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */			 
				LogPlugClassical( const LogPlugClassical & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogPlugClassical & operator = ( 
					const LogPlugClassical & source ) throw() ;
				
	
		} ;	
		
	}

} 





#endif // CEYLAN_LOG_PLUG_CLASSICAL_H_
