#ifndef CEYLAN_LOG_HOLDER_H_
#define CEYLAN_LOG_HOLDER_H_


#include "CeylanTextDisplayable.h" // for inheritance
#include "CeylanLog.h"             // for LogException

#include <string>



namespace Ceylan
{

	namespace Log
	{
	
	
		/**
		 * List of the log plugs available to the LogHolder :
		 *    - consolePlug : all logs are output in standard console streams,
		 * the default one being the standard log stream (clog), the others 
		 * being the standard output stream (cout) and the standard error
		 * stream (cerr). Associated command-line option : '--consolePlug'.
		 *    - classicalPlug : all logs are output in a *.log file, whose
		 * filename by default matches the executable name, with a raw 
		 * encoding, all logs in chronological order. Associated 
		 * command-line option : '--classicalPlug'.
		 *    - HTMLPlug : all logs are output in a set of HTML files 
		 * gathered by a generated frameset, where each log channel has a 
		 * dedicated HTML page. Associated command-line option :
		 * '--HTMLPlug'.
		 *
		 */
		enum KnownPlugs 
		{
			consolePlug,
			classicalPlug,
			HTMLPlug							
		} ;
	
		
		/**
		 * The LogHolder object is made to be instanciated as an automatic
		 * variable in main() functions, so that it is automatically
		 * in charge of the Log system's life cycle. 
		 *
		 * Besides, it allows to select the log plug to use at run-time,
		 * thanks the command-line plug options that may be passed to the
		 * executable and, therefore, to the LogHolder instance.
		 *
		 * If this program has been called with '--HTMLPlug' as parameter
		 * in the command line, then the HTML log plug with be selected
		 * instead of the classical raw text plug ('--classicalPlug') or the
		 * console plug ('--consolePlug'), and log messages will be output
		 * accordingly. The classical plug is the default one.
		 *
		 * The LogHolder instance should preferably by created outside the
		 * try/catch pair to ensure it is destroyed after all automatic
		 * variables (which must be within the try/catch pair), since their
		 * destruction might result in log messages. Thus no operation on 
		 * a LogHolder instance should result in an exception, instead an
		 * emergency shutdown is triggered.
		 *
		 * @example : 
		 * <pre>
		 * int main( int argc, char * argv[] )
		 * {
		 * 
		 *   LogHolder log( argc, argv ) ;
		 *  
		 *   try 
		 *   { 
		 *
		 *     // ensures LogHolder will be the last to be deallocated.
		 * 
		 *     ... put your program here, automatic variables included ...
		 *
		 *   } 
		 *   catch( etc. )
		 *   {
		 *	   ...
		 *   }	
		 *
		 * }
		 * </pre>
	 	 *
		 *
	 	 */
		class LogHolder : public TextDisplayable
		{
	
				
			public:
			
			
				/**
				 * Creates a LogHolder.
				 *
				 * Reads the command-line arguments, and uses them to 
				 * choose a specific LogPlug, if specified, otherwise uses
				 * default log plug. 
				 *
				 * Command-line parameters are left untouched (read-only). 
				 *
				 * Only the first log plug specification found in
				 * command-line is taken into account.
				 *
				 * @param argCount The number of arguments (argc).
				 *
				 * @param arguments The arguments themselves (argv).
				 *
				 * @note There is no point in using more than one instance 
				 * of LogHolder in a process.
				 *
				 * @throw LogException if the log system could not be
				 * correctly started.
				 *
		 		 */
				LogHolder( int argCount, const char * const arguments[] )
					throw( LogException ) ;
			
	
				/// Basic virtual destructor.
				virtual ~LogHolder() throw() ;


								
				/**
				 * Returns a user-friendly description of the state of 
				 * this object.
				 *
				 * @see TextDisplayable, Displayable
				 * @see Ceylan::VerbosityLevels
				 *
				 */
				virtual const std::string toString( 
						Ceylan::VerbosityLevels level = Ceylan::high )
					const throw() ;
								
								
				/**
				 * Command-line option which is to be used to select the
				 * console plug.	
				 *
				 */		
				static const std::string ConsolePlugOption ;
				
								
				/**
				 * Command-line option which is to be used to select the
				 * classical plug, which is the default plug.	
				 *
				 */		
				static const std::string ClassicalPlugOption ;
				
				
				/**
				 * Command-line option which is to be used to select the HTML
				 * plug.		
				 */		
				static const std::string HTMLPlugOption ;
										
				
				/**
				 * The default plug, chosen if no specific plug is 
				 * requested thanks to command-line parameters.
				 *
				 */
				static KnownPlugs DefaultPlug ;
				
				
				
			protected:	 		
			
				/// Records the chosen plug, in order to be able to stop it.
				KnownPlugs _chosenPlug ;
				
				
				
			private:
			
			
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogHolder( const LogHolder & source ) throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogHolder & operator = ( const LogHolder & source ) throw() ;
				
								
		} ;

	}

}

#endif // CEYLAN_LOG_HOLDER_H_
