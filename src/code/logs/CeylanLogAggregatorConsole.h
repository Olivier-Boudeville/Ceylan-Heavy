#ifndef CEYLAN_LOG_AGGREGATOR_CONSOLE_H_
#define CEYLAN_LOG_AGGREGATOR_CONSOLE_H_


#include "CeylanLogAggregator.h"  // for LogAggregator
#include "CeylanDisplayable.h"    // for VerbosityLevels

#include <iosfwd>                 // for ostream
#include <string>



namespace Ceylan
{


	namespace System
	{
	
		// LogAggregatorConsole instances aggregate their log into files.
		class File ;
	}
	
	
	namespace Log
	{
	
	
		// They are aggregated by aggregators.
		class LogMessage ;
		
		
		/**
		 * This class implements the LogAggregator interface thanks to
		 * console-based direct output, hence with no file being written. 
		 *
		 * @note This is the simpliest log aggregator, useful whenever 
		 * the Ceylan library is built with no file descriptor support.
		 * 
		 * Log messages are stored in chronological order (based on 
		 * the time they were received, not on their respective timestamp),
		 * all in a row, each prefixed by its channel name. 
		 *
		 * @see LogAggregator
		 *
		 */
		class LogAggregatorConsole : public LogAggregator
		{
		
		
			public:
							
			
				/**
				 * Describes the three standard streams :
				 *		- Output is the standard output stream, which is
				 * the default standard stream being used
				 *		- Error is the standard error stream
				 *		- Log is the standard log stream
				 *
				 * The difference between Log and Error is that Log is 
				 * buffered wheread Error is not.
				 *
				 */
				enum StandardStream { Output, Error, Log } ; 
				
				
				/**
				 * Constructs a LogAggregatorConsole which will output 
				 * incoming log messages in console-based output. 
				 *
				 * The logs are stored with a raw encoding, as opposed 
				 * to HTML encoding for example.
				 *
				 * @param consoleStream tells to which standard stream
				 * logs should be written. The default is the standard
				 * output stream, Output.
				 * 
				 * @param immediateWrite tells whether the aggregator 
				 * should write log messages as soon as they are 
				 * received (the safe and default behaviour).
				 *
				 * @param useGlobalLevelOfDetail tells whether log 
				 * channels levels of detail are to be overriden by 
				 * the aggregator-wide one.
				 *
				 * @param beSmart tells whether this aggregator should
				 * be smart and auto-correct messages with faulty 
				 * classnames.
				 *
				 */
				explicit LogAggregatorConsole( 
					StandardStream consoleStream 
						= LogAggregatorConsole::Output, 
					bool immediateWrite = true,	
					bool useGlobalLevelOfDetail = true,
					bool beSmart = true ) throw( LogAggregatorException ) ;
					
							
				/**
				 * Virtual destructor.
				 *
				 * @note If this aggregator has been set to be smart,
				 * it will automatically trigger log aggregation upon 
				 * deletion.
				 *
				 */
				virtual ~LogAggregatorConsole() throw() ;
		
						 
				/**
				 * Aggregates all channel and log messages informations
				 * in the console.
				 *
				 * @note this method does nothing if the immediate write
				 * flag is turned on : work should be already done.
				 *
				 */
				virtual void aggregate() throw( LogAggregatorException ) ;


		        /**
		         * Classify and stores <b>message</b> internally, 
				 * according to the corresponding channels they contain.
		         *
				 * @note this implementation, if immediateWrite mode was
				 * selected, will immediatly output this message on the
				 * console.
				 *
		         * @param message the log message to be stored, the 
				 * aggregator takes ownership of it.
		         *
		         */		
				virtual void store( LogMessage & message ) 
					throw( LogException ) ; 
						
										
	            /**
	             * Returns a user-friendly description of the state of
				 * this object.
	             *
				 * @param level the requested verbosity level.
				 *
				 * @note Text output format is determined from overall
				 * settings.
				 *
				 * @see TextDisplayable
				 *
	             */
				virtual const std::string toString( 
						Ceylan::VerbosityLevels level = Ceylan::high )
					const throw() ;


			
			protected:
								
					
				/**
				 * Internal method used to perform the effective 
				 * output of log channels on the console.
				 *
				 * @param channel the log channel to output on the
				 * console.
				 *
				 */
				virtual void write( const LogChannel & channel ) 
					const throw( LogException ) ;
			
			
				/**
				 * Internal method used to perform the effective 
				 * writing of log messages on the console.
				 *
				 * @param message the log message to write
				 *
				 * @note this method is mainly used when immediate 
				 * writing mode is set.
				 *
				 */
				virtual void write( const LogMessage & message ) const throw() ;
			
			
				/// The identifier of the standard steam being used.
				StandardStream _streamNumber ;
			
				
				/**
				 * The output stream where log messages should be written.
				 *
				 */
				std::ostream * _outputStream ; 
				
								
				/**
				 * Tells whether log messages should be written to log 
				 * file as soon as they are received.
				 *
				 */
				bool _immediateWrite ; 
				
				
				/**
				 * Tells whether a global (aggregator-wide) level of 
				 * detail for channels should be used.
				 *
				 * @note if set, will override log channels levels of details.
				 *
				 */
				bool _useGlobalLevelOfDetail ; 
				
				
				/**
				 * Defines the aggregator-wide level of detail, used if
				 * _useGlobalLevelOfDetail is set.
				 *
				 */
				LevelOfDetail _globalLevelOfDetail ;



		private:
		
		
				/**
				 * Copy constructor made private to ensure that it will
				 * be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * constructor is called, implicitly or not.
				 * 
				 */			 
				LogAggregatorConsole( const LogAggregatorConsole & source )
					throw() ;
			
			
				/**
				 * Assignment operator made private to ensure that it 
				 * will be never called.
				 *
				 * The compiler should complain whenever this undefined
				 * operator is called, implicitly or not.
				 * 
				 */			 
				LogAggregatorConsole & operator = ( 
					const LogAggregatorConsole & source ) throw() ;
				
				
		} ;
	
	
	}


}


#endif // CEYLAN_LOG_AGGREGATOR_CONSOLE_H_
