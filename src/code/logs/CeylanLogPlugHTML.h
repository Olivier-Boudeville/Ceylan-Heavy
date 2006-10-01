#ifndef CEYLAN_LOG_PLUG_HTML_H_
#define CEYLAN_LOG_PLUG_HTML_H_


#include "CeylanLog.h"     // for LogException
#include "CeylanLogPlug.h" // for LogPlug

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
		 * Where messages from a LogSource will be received : the 
		 * second part of the LogTransport-LogListener pair, before
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
		 * various instances of the log framework in their HTML version, 
		 * and creates the relevant standard channels. 
		 *
		 * @note With this Log implementation, log informations will be 
		 * output in HTML format.
		 *
		 * A web page will correspond to each log channel and list its 
		 * messages.
		 *
		 * All web pages will be gathered by an HTML frameset allowing to 
		 * browse channels by picking their names.
		 *
		 * @example, a channel named 'Ceylan rocks' will be stored as
		 * 'Ceylan-rocks.html'.
		 *
		 * An object channel's name could be something similar to
		 * 'sonata/PID-21097/Example/0x80510a0'. 
		 *
		 * @see LogPlug
		 *
		 */
		class CEYLAN_DLL LogPlugHTML : public LogPlug
		{
		
			public:



		        /**
		         * Starts the Log HTML service.
		         *
		         * @param plugInitiator the name of the speaker, for 
				 * instance argv[0].
				 *
				 * @note plugInitiator will be used to create the
				 * directory which will be containing all HTML log output.
				 *
				 * @param smart tells whether the HTML aggregator should
				 * be smart.
				 *
		         */
		        static void StartService( const std::string & plugInitiator, 
					bool smart = true ) throw ( LogException ) ;


		        /// Stops the Log HTML service.
		        static void StopService() throw() ;
				
				
				/// Returns some informations about the HTML LogPlug's state.
	           	static const std::string ToString( 
					Ceylan::VerbosityLevels level = Ceylan::high ) 
						throw() ;
				
					
			protected:	
				 
				 
				 /**
				  * The suffix that will be added to <b>sourceName</b> to
				  * define the log directory.
				  *
				  */
				 static const std::string & LogDirectorySuffix ;
	
	
			private:
			
			
				/* 
				
				Not even declared to avoid : 
				'warning: will never be executed'
				/// LogPlug HTML should not be instanciated.
				LogPlugHTML() throw( LogException )
					__attribute__ ((noreturn)) ;

				
				/// This destructor will be never called.
				virtual ~LogPlugHTML() throw() ;
			
				*/
				
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogPlugHTML( const LogPlugHTML & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it will<
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogPlugHTML & operator = ( const LogPlugHTML & source )
					throw() ;
				
	
		} ;	
		
	}

} 





#endif // CEYLAN_LOG_PLUG_HTML_H_
