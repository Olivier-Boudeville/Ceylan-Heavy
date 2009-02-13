#ifndef CEYLAN_LOG_PLUG_NULL_H_
#define CEYLAN_LOG_PLUG_NULL_H_


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
		 * Where messages from a LogSource will be output: the first 
		 * part of the LogTransport-LogListener pair.
		 *
		 */
		class LogTransport ;
		
		
		/**
		 * Where messages from a LogSource will be received: 
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
		 * various instances of the log framework in their null version,
		 * and creates the relevant standard channels.
		 *
		 * This plug just ignores all log messages and does not process nor
		 * store them in any way.
		 *
		 * Its purpose is to allow to run programs with the minimum overweight
		 * due to logs (in a context where recompiling the library is not an
		 * option).
		 *
		 * @see LogPlug
		 *
		 */
		class CEYLAN_DLL LogPlugNull: public LogPlug
		{
		
			public:



		        /**
		         * Starts the Log null service.
				 *
		         */
		        static void StartService() throw( LogException ) ;


		        /// Stops the Log null service.
		        static void StopService() throw() ;
				
				
				/**
				 * Returns some informations about the null LogPlug's
				 * state.
				 *
				 */
	           	static const std::string ToString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) throw() ;
				
				
					
	
			private:


			
				/**
				 * Copy constructor made private to ensure that it will 
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 *
				 */			 
				LogPlugNull( const LogPlugNull & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined 
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogPlugNull & operator = ( 
					const LogPlugNull & source ) throw() ;
				
	
		} ;	
		
	}

} 


#endif // CEYLAN_LOG_PLUG_NULL_H_

